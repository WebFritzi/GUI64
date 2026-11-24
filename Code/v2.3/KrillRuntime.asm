; ROM bank 5: installer, App loader and shared KERNAL fallback. Loader by Krill, repository version 194.
; Public RAM entries are in KrillGate.asm; direct calls here require bank 5,
; $01=$37, stopped GUI IRQ/sprites, no open KERNAL files and no nested gate call.
; $64C2-$64FF holds installer/App state; games copy live state below $0801.
; Installer saves $E0/$E1 and $E8-$EB; both GUI load paths also preserve $E0-$E7.
; Install returns A=status, X=drive type, C=error; $FB/$FC permit fallback.

; Initialize after startup's graphics copy, before the first installer call.
Krill_ROMInit   lda #0
                sta Krill_NativeActive
                sta KRILL_MDAPPMODE
                sta Krill_GameInstalling
                lda #$ff
                sta Krill_LastStatus
                sta Krill_LastWarning
                sta Krill_DriveType
                lda Krill_SavedP
                lsr             ; Init preserves the caller carry as well
                lda #$ff
                rts

Krill_ROMInstall
                tax
                cld
                lda Krill_NativeActive
                bne Krill_Invalid
                txa
                cmp #8
                bcc Krill_Invalid
                cmp #31
                bcc Krill_InstallValid
Krill_Invalid   lda #$fd
                sta Krill_LastStatus
                lda #1
                sta Krill_ResultCarry
                jmp Krill_Return
Krill_InstallValid
                sta Krill_Device
                jsr Krill_SaveContext
                ; UninstallIRQ also disables sprite DMA during KERNAL IEC I/O.
                lda $dd00
                sta Krill_SavedDD00
                and #3
                sta $dd00
                lda $dd02
                sta Krill_SavedDD02
                lda #$3f
                sta $dd02
                ; Installer and drive images stay visible in the same ROM bank.
                ; Valid KERNAL channel state; this workspace may change.
                lda #0
                sta $90
                sta $94
                sta $98
                sta $99
                sta $9d
                lda #3
                sta $9a
                ; Status/warning are not needed until the installer returns.
                ; Reuse their two bytes for its temporary progress callback.
                lda #<Krill_InstallProgress
                sta Krill_LastStatus
                lda #>Krill_InstallProgress
                sta Krill_LastWarning
                lda Krill_Device
                sta $ba
                jsr KRILL_CORE_INSTALL
                sta Krill_LastStatus
                stx Krill_DriveType
                php
                pla
                and #1
                sta Krill_ResultCarry
                lda KRILL_MDWARNING
                sta Krill_LastWarning
                lda Krill_LastStatus
                bne +
                lda #1
                sta Krill_NativeActive
+               lda Krill_NativeActive
                bne +
                jsr Krill_RestoreSerial
+               jsr Krill_RestoreContext
                lda #0          ; installer scratch begins with drive type, not App mode
                sta KRILL_MDAPPMODE
                jmp Krill_Return

Krill_Return    ldx Krill_ResultCarry
                cpx #1
                lda Krill_LastStatus
                ldx Krill_DriveType
                rts

; Called directly by ROM control; the cartridge stays mapped for the caller.
Krill_ROMUninstall
                lda Krill_NativeActive
                beq +
                jsr KRILL_CORE_UNINSTALL
                jsr Krill_RestoreSerial
                lda #0
                sta Krill_NativeActive
+               lda Krill_SavedP
                lsr             ; public Uninstall preserves the caller carry
                rts

; Called from ROM only at installer checkpoints with A/X/flags disposable.
; Y stays intact for the fast-upload loop. Raw installer calls have no dialog.
Krill_InstallProgress
                lda LOAD_TYPE
                cmp #2
                bne +
                tya             ; keep the fast-upload index intact
                pha
                jsr PaintHourglass
                pla
                tay
                rts
+               ora Krill_GameInstalling
                beq +
                jmp LoadMotion
+               rts

Krill_RestoreSerial
                lda Krill_SavedDD00
                sta $dd00
                lda Krill_SavedDD02
                sta $dd02
                rts

Krill_SaveContext
                ; CIA sources are already disabled by GUI64. Do not touch timers.
                lda $e0
                sta Krill_SavedZP
                lda $e1
                sta Krill_SavedZP+1
                ldx #3
-               lda $e8,x
                sta Krill_SavedZP+2,x
                dex
                bpl -
                rts

Krill_RestoreContext
                lda Krill_SavedZP
                sta $e0
                lda Krill_SavedZP+1
                sta $e1
                ldx #3
-               lda Krill_SavedZP+2,x
                sta $e8,x
                dex
                bpl -
                rts


; The raw API retains its optional explicit destination and returns core status.
Krill_ROMLoadRaw
                ldx KRILL_BLOCKDESTLO
                ldy KRILL_BLOCKDESTLO+1
                lda Krill_SavedP
                lsr
                jmp KRILL_CORE_LOADRAW

; CRT App loader. Installer scratch is reused after Krill_Install returns.
; Called through LoadFile after PrepareLoad selected the file and drew its dialog.
!if APP_START != KRILL_MDAPPSTART { !error "Krill App start differs from GUI64" }
!if APP_END+1 != KRILL_MDAPPLIMIT { !error "Krill App limit differs from GUI64" }
!if FREEMEM != $6300 { !error "Krill discard page differs from FREEMEM" }
Krill_AppSavedZP = KRILL_INSTALL_STATE_RAM+2 ; $64D5-$64DC
Krill_AppStatus = KRILL_INSTALL_STATE_RAM+11
Krill_AppNative = KRILL_INSTALL_STATE_RAM+12
Krill_AppNext = KRILL_INSTALL_STATE_RAM+13
Krill_AppProgressReady = KRILL_INSTALL_STATE_RAM+15

Krill_ROMLoadApp
                ; The installer advances two segments at its progress checkpoints.
                lda #$ff
                sta MOTION_COUNTER
                lda CurDeviceNo
                jsr Krill_ROMInstall
                bcc Krill_AppInstalled
                lda #5          ; device not present / installation failed
                sta error_code
                lda error_code
                rts
Krill_AppInstalled
                sei
                cld
                ldx #7
-               lda $e0,x
                sta Krill_AppSavedZP,x
                dex
                bpl -
                lda Krill_NativeActive
                sta Krill_AppNative
                ; Upstream installation may select another available device.
                lda $ba
                cmp CurDeviceNo
                beq Krill_AppSelected
                lda #5
                sta error_code
                jsr Krill_ROMUninstall
                jmp Krill_AppRestore
Krill_AppSelected
                lda Krill_AppNative
                bne Krill_AppFast
                jmp Krill_AppFallback
Krill_AppFast   lda #1
                sta KRILL_MDAPPMODE
                lda #0
                sta KRILL_MDAPPERROR
                sta Krill_AppProgressReady
                sta error_code
                lda #$ff
                sta MOTION_COUNTER
                ldx #<Str_FileName
                ldy #>Str_FileName
                clc
                jsr KRILL_CORE_OPENFILE
                bcc Krill_AppPoll
                jmp Krill_AppTransferError
Krill_AppPoll   jsr KRILL_CORE_POLLBLOCK
                sta Krill_AppStatus
                bcs Krill_AppDone
                lda KRILL_MDAPPERROR
                bne Krill_AppBoundsError
                jsr Krill_AppProgress
                jmp Krill_AppPoll
Krill_AppDone   lda Krill_AppStatus
                bne Krill_AppTransferError
                lda KRILL_LOADADDRHI
                cmp #>APP_START
                bcc Krill_AppBoundsError
                cmp #>(APP_END+1)
                bcs Krill_AppBoundsError
                lda KRILL_ENDADDRLO
                cmp KRILL_LOADADDRLO
                lda KRILL_ENDADDRHI
                sbc KRILL_LOADADDRHI
                bcc Krill_AppBoundsError
                bne +
                lda KRILL_ENDADDRLO
                cmp KRILL_LOADADDRLO
                beq Krill_AppBoundsError ; empty payload is not executable
+               lda KRILL_LOADADDRLO
                sta jmp_app+1
                lda KRILL_LOADADDRHI
                sta jmp_app+2
                jmp Krill_AppFinishNative
Krill_AppBoundsError
                lda #32
                sta error_code
                jmp Krill_AppFinishNative
Krill_AppTransferError
                cmp #$fe
                beq +
                lda #4          ; file not found / read error
                !byte $2c       ; skip LDA #5
+               lda #5
                sta error_code
Krill_AppFinishNative
                jsr KRILL_CORE_CLOSEFILE
                jsr Krill_ROMUninstall
                lda #0
                sta KRILL_MDAPPMODE
                lda error_code
                bne Krill_AppRestore
                jsr FillMotion
                jmp Krill_AppRestore

; Keep the existing bytewise bounded App reader for unsupported drives.
; The installer already stopped GUI IRQ/sprites for both transfer paths.
; BackToNormal restarts them after success or error. CIA sources stay off.
Krill_AppFallback
                jsr KernalLoadFileBody
Krill_AppRestore
                sei
                ldx #7
-               lda Krill_AppSavedZP,x
                sta $e0,x
                dex
                bpl -
                lda error_code
                rts

; END_ADDRESS_API is a high-water mark for out-of-order blocks. Reuse the
; existing 18-step indicator; this is approximate progress from directory size.
Krill_AppProgress
                lda KRILL_ENDADDRHI
                beq Krill_AppProgressDone
                lda Krill_AppProgressReady
                bne Krill_AppProgressCompare
                lda KRILL_LOADADDRLO
                sta Krill_AppNext
                lda KRILL_LOADADDRHI
                sta Krill_AppNext+1
                lda #1
                sta Krill_AppProgressReady
                jsr Krill_AppProgressAdvance
Krill_AppProgressCompare
                lda MOTION_COUNTER
                cmp #17
                beq Krill_AppProgressDone
                lda KRILL_ENDADDRLO
                cmp Krill_AppNext
                lda KRILL_ENDADDRHI
                sbc Krill_AppNext+1
                bcc Krill_AppProgressDone
                jsr LoadMotion
                jsr Krill_AppProgressAdvance
                jmp Krill_AppProgressCompare
Krill_AppProgressDone
                rts
Krill_AppProgressAdvance
                clc
                lda Krill_AppNext
                adc BYTES_PER_PROGBIT_LO
                sta Krill_AppNext
                lda Krill_AppNext+1
                adc BYTES_PER_PROGBIT_HI
                sta Krill_AppNext+1
                rts

; Shared bytewise PRG reader and progress display.
; KERNAL fallback executed in ROM bank 5.
KernalLoadFileBody
                lda #$02      ; logical number
                ldx CurDeviceNo ; device number
                ldy #$00      ; secondary address
                jsr SETLFS    ; set file parameters
                ldx #<Str_FileName;load_fn
                ldy #>Str_FileName;load_fn
                lda load_length
                jsr SETNAM
                ;
                jsr OPEN        ; open file
                bcc +
                ; Error
                sta error_code
                jmp load_close
+               lda #$ff
                sta MOTION_COUNTER
                ldx #$02
                jsr CHKIN       ; set input device
                ;
                jsr CHRIN       ; read lobyte of start address
                tax
                jsr CHRIN       ; read hibyte of start address
                tay
                jsr READST
                and #$bf       ; EOF alone permits the empty-payload check below.
                beq +
                lda #4
                sta error_code
                jmp load_close
+
                lda LOAD_TYPE
                beq ++
                ; App wants to load
                ; Make sure Addr(X/Y) is between APP_START and APP_END+1
                cpy #>APP_START
                bcc +
                cpy #>(APP_END+1)
                bcc ++
+               ; Addr(X/Y) not valid --> error
ld_err          lda #32
                sta error_code
                jmp load_close
++              ; Use Addr(X/Y) as destination address
                stx $ae
                stx jmp_app+1
                stx jmp_boot+1
                sty $af
                sty jmp_app+2
                sty jmp_boot+2
                ; Load file
                jsr LoadMotionDone; set $fd/$fe
                ldy #0
-               jsr READST
                bne load_eof    ; either EOF or read error
                jsr CHRIN       ; get a byte from file
                jsr Krill_FileStore ; includes RAM under I/O and game bounds
                bcs ld_err
                dec $fd
                bne +
                dec $fe
                bpl +
                jsr LoadMotion
+               inc $ae
                bne +
                inc $af
                ; Apps must not exceed APP_END
                lda LOAD_TYPE
                beq +
                lda $af
                cmp #>(APP_END+1)
                bcc +
                ; Pointer has reached $d000
                ; This is OK only if EOF has already been reached
                jsr READST
                bne load_eof
                beq ld_err
                
+               jmp -           ; next byte
load_eof
                pha
                lda $ae
                cmp jmp_boot+1
                bne +
                lda $af
                cmp jmp_boot+2
                bne +
                pla
                jmp ld_err      ; Header without payload is not executable.
+               pla
                and #$40        ; end of file?
                bne +
                lda #4
                sta error_code
+               jsr FillMotion
load_close      jsr CLRCHN      ; end data input/output of file
                lda #$02
                jmp CLOSE       ; close file

LoadMotion      ldx MOTION_COUNTER
                inx
                cpx #18
                bcs LoadMotionDone
                stx MOTION_COUNTER
!ifdef WIN{
                lda #CL_DARKBLUE
                sta CLRMEM+451,x
}
!ifdef MAC{
                lda #CL_DARKGRAY
                sta CLRMEM+491,x
}
LoadMotionDone  lda BYTES_PER_PROGBIT_LO
                sta $fd
                lda BYTES_PER_PROGBIT_HI
                sta $fe
                rts

FillMotion      ldx MOTION_COUNTER
                bpl +
                ldx #0
+
-               
!ifdef WIN{
                lda #CL_DARKBLUE
                sta CLRMEM+451,x
}
!ifdef MAC{
                lda #CL_DARKGRAY
                sta CLRMEM+491,x
}
                inx
                cpx #18
                bcc -
                ;
                ldx #80
-               jsr Pause
                dex
                bpl -
                rts
