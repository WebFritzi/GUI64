; Bank 6, copy-only control. Apps/games/Viewer continue using bank 5.
; Reuse the installer tail after installation; no App/Viewer RAM is borrowed.
Krill_CopyName       = FREEMEM+$100 ; encoded first track, sector+1, zero
Krill_CopyGUIZP      = FREEMEM+$103 ; original $E0-$E8 across the whole operation
Krill_CopySavedZP    = KRILL_INSTALL_STATE_RAM
Krill_CopyBlocks     = KRILL_INSTALL_STATE_RAM+9
Krill_CopyOpened     = KRILL_INSTALL_STATE_RAM+10
Krill_CopyEnd        = KRILL_INSTALL_STATE_RAM+11
Krill_CopyWritten    = KRILL_INSTALL_STATE_RAM+14
Krill_CopyIndex      = KRILL_INSTALL_STATE_RAM+20
Krill_CopyEOF        = KRILL_INSTALL_STATE_RAM+24
Krill_CopyProgressLeft = KRILL_INSTALL_STATE_RAM+25
Krill_CopyTimeout    = KRILL_INSTALL_STATE_RAM+26
Krill_CopySourceOpen = KRILL_INSTALL_STATE_RAM+28
Krill_CopyUploadAt   = KRILL_INSTALL_STATE_RAM+29
Krill_CopyUploadN    = KRILL_INSTALL_STATE_RAM+30
Krill_CopyWakeClock  = KRILL_INSTALL_STATE_RAM+31
Krill_CopyDevice     = KRILL_INSTALL_STATE_RAM+32
Krill_CopyAnyWritten = KRILL_INSTALL_STATE_RAM+34 ; bit 0: written, bit 4: helper retained, bit 5: helper buffer 4, bit 6: cleanup pending, bit 7: KERNAL target
Krill_CopyBufferHi   = KRILL_INSTALL_STATE_RAM+33
; Upload work bytes are free while the native writer is running.
Krill_CopyFastCount  = Krill_CopyUploadN
Krill_CopyWaitMask   = Krill_CopyUploadAt
Krill_CopyWaitValue  = Krill_CopyWakeClock
Krill_CopyQuiet      = Krill_GameInstalling
Krill_CopyBufferSize = 20*254
!if Krill_CopyGUIZP+9 > Krill_RuntimeData { !error "Copy saved ZP overlaps runtime" }
!if Krill_CopyAnyWritten >= WND_HEAP { !error "Copy state exceeds FREEMEM" }
!if STRING_LIST_DRIVEB-STRING_LIST_DRIVEA < Krill_CopyBufferSize { !error "Copy buffer A too small" }
!if APP_START-STRING_LIST_DRIVEB < Krill_CopyBufferSize { !error "Copy buffer B too small" }

Krill_ROMCopy
                cld
                ldx #8
-               lda $e0,x
                sta Krill_CopyGUIZP,x
                dex
                bpl -
                lda $dd00
                sta Krill_SavedDD00
                and #3
                sta $dd00
                lda $dd02
                sta Krill_SavedDD02
                lda #$3f
                sta $dd02
                lda #0
                sta $90
                sta $94
                sta $98
                sta $99
                sta $9d
                sta error_code
                sta Krill_NativeActive
                sta Krill_CopyQuiet
                jsr Krill_CopyInitState
                lda #3
                sta $9a
                lda DiskToCopyFrom
                cmp DiskToCopyTo
                bne +
                jmp Krill_CopyBadSource
+
                ldx DiskToCopyTo+1
                cpx #2
                bcc +
                jmp Krill_CopyBadSource
+
                ; The existing GUI clipboard filename remains unchanged.
                ldx #0
-               lda Str_FileName,x
                beq +
                sta write_fn,x
                inx
                cpx #17
                bcc -
Krill_CopyBadSource
                lda #4
                sta error_code
                jmp Krill_CopyFinish
+               stx copypastelength
                txa
                bne +
                jmp Krill_CopyBadSource
+
                ldy #0
-               lda write_appendix,y
                sta write_fn,x
                inx
                iny
                cpy #4
                bcc -
                stx write_length
                lda #0
                sta write_fn,x
                ; Native pause/resume is currently implemented for 1541 pairs.
                ; Confirm model AND actual code execution before uploading it.
                lda DiskToCopyFrom
                jsr Krill_CopyProbe
                bcc +
                jmp Krill_CopyFallback
+
                lda DiskToCopyTo
                jsr Krill_CopyProbe
                bcc +
                jmp Krill_CopyFallback
+
                lda DiskToCopyFrom
                jsr Krill_CopyFindStart
                bcc +
                jmp Krill_CopyFallback
+
                jsr Krill_CopyUploadTarget
                bcc +
                jmp Krill_CopyFallback
+
                lda #1
                sta Krill_CopyQuiet
                lda #<Krill_CopyNoProgress
                sta Krill_LastStatus
                lda #>Krill_CopyNoProgress
                sta Krill_LastWarning
                lda DiskToCopyFrom
                sta $ba
                jsr Krill_CopyInstall
                sta Krill_LastStatus
                bcc +
                jmp Krill_CopyFallback
+
                cmp #0
                beq +
                jmp Krill_CopyFallback
+
                lda #1
                sta Krill_NativeActive
Krill_CopySelectedCheck
                lda $ba
                cmp DiskToCopyFrom
                beq Krill_CopySelected
                jsr Krill_CopyArmTarget
                jsr Krill_CopyUninstall
                lda #0
                sta Krill_NativeActive
                lda #5
                sta error_code
                jmp Krill_CopyFinish
Krill_CopySelected
                jsr Krill_CopyArmTarget
                jsr Krill_CopyInitState
                lda #3
                sta KRILL_MDAPPMODE
                lda #0
                sta KRILL_LOADADDRLO
                lda Krill_CopyBufferHi
                sta KRILL_LOADADDRHI
                ldx #<Krill_CopyName
                ldy #>Krill_CopyName
                sec
                jsr KRILL_CORE_OPENFILE
                bcc Krill_CopyReadNext
                jmp Krill_CopyReadError

Krill_CopyInitState
                lda #0
                sta Krill_CopyOpened
                sta Krill_CopySourceOpen
                sta Krill_CopyEOF
                sta Krill_CopyAnyWritten
Krill_CopyResetBuffer
                lda #0
                sta Krill_CopyBlocks
                sta Krill_CopyEnd
                sta Krill_CopyWritten
                ldx DiskToCopyTo+1
                lda StringListDrvHi,x
                sta Krill_CopyBufferHi
                sta Krill_CopyEnd+1
                sta Krill_CopyWritten+1
                rts

Krill_CopyReadNext
                jsr Krill_CopyWaitPark
                bcc +
                jmp Krill_CopyReadError
+
                lda #$10
                jsr Krill_CopyWake
; Bound a stalled read inside a batch, where there is no per-block park wait.
; Reuse target-output scratch bytes; no additional C64 RAM is reserved.
Krill_CopyReadDeadline
                lda #0
                sta Krill_CopyWaitValue
                sta Krill_CopyWaitMask
                lda #1
                sta Krill_CopyTimeout
Krill_CopyPoll
                lda #0
                sta Krill_ViewerReady
                jsr KRILL_CORE_POLLBLOCK
                bcc Krill_CopyBlock
                cmp #0
                beq +
                jmp Krill_CopyReadError
+
                inc Krill_CopyEOF
                jsr Krill_CopyWaitPark
                bcc +
                jmp Krill_CopyReadError
+
                jsr Krill_CopyWrite
                jmp Krill_CopyFinish
Krill_CopyBlock
                lda Krill_ViewerReady
                bne Krill_CopyReceived
                dec Krill_CopyWaitValue
                bne Krill_CopyPoll
                dec Krill_CopyWaitMask
                bne Krill_CopyPoll
                dec Krill_CopyTimeout
                bne Krill_CopyPoll
                jmp Krill_CopyReadError
Krill_CopyReceived
                lda Krill_CopyEnd
                sta $ae
                lda Krill_CopyEnd+1
                sta $af
                ldy Krill_ViewerIndex
-               iny
                beq +
                lda FREEMEM,y
                ldx #0
                sta ($ae,x)
                inc $ae
                bne -
                inc $af
                jmp -
+               lda $ae
                sta Krill_CopyEnd
                lda $af
                sta Krill_CopyEnd+1
                inc Krill_CopyBlocks
                lda Krill_CopyBlocks
                cmp #20
                bcs +
                jmp Krill_CopyReadDeadline
+
                jsr Krill_CopyWaitPark
                bcc +
                jmp Krill_CopyReadError
+
                jsr Krill_CopyWrite
                lda error_code
                beq +
                jmp Krill_CopyFinish
+
                jsr Krill_CopyResetBuffer
                jsr Krill_CopyUploadTarget
                bcc +
                jmp Krill_CopyWriteError
+
                jsr Krill_CopyArmTarget
                jsr Krill_CopyRestoreNative
                lda #$10
                jsr Krill_CopyWake
                jmp Krill_CopyReadDeadline
Krill_CopyReadError
                jsr Krill_CopySaveNative
                lda Krill_NativeActive
                beq +
                lda Krill_CopyTimeout
                bne +
                ; The copy drive checks released ATN between disk read attempts.
                ; Do not wait for a block/close handshake after our timeout.
                lda #0
                sta Krill_NativeActive
                jsr Krill_CopyWakeTarget
                ; Let the aborted 1541 finish its reset before new DOS traffic.
                lda #10
                sta Krill_CopyTimeout
-               jsr Krill_CopyLongDelay
                dec Krill_CopyTimeout
                bne -
+               lda #4
                sta error_code
                jmp Krill_CopyFinish
Krill_CopyWriteError
                lda #31
                sta error_code
                jmp Krill_CopyFinish

; Generic drives keep KERNAL input/output; file open/close is shared in ROM.
Krill_CopyFallback
                lda #$fb
                sta Krill_LastStatus
                jsr Krill_CopyWakeTarget
                jsr Krill_CopyInitState
                jsr PrepareSourceCh
                ldx DiskToCopyFrom
                jsr SETLFS_OPEN
                bcc +
                jmp Krill_CopyReadError
+
                inc Krill_CopySourceOpen
                lda DiskToCopyFrom
                jsr Krill_CopyCheckDOS
                bcc +
                jmp Krill_CopyReadError
+
Krill_CopyKernalRead
                lda Krill_CopyEnd
                sta $ae
                lda Krill_CopyEnd+1
                sta $af
                ldx #3
                jsr CHKIN
                bcc +
                jmp Krill_CopyReadError
+
-               jsr READST
                bne Krill_CopyKernalEOF
                jsr CHRIN
                ldy #0
                sta ($ae),y
                inc $ae
                bne +
                inc $af
+               lda $ae
                cmp #<Krill_CopyBufferSize
                bne -
                lda Krill_CopyBufferHi
                clc
                adc #>Krill_CopyBufferSize
                cmp $af
                bne -
                beq Krill_CopyKernalWrite
Krill_CopyKernalEOF
                and #$bf
                beq +
                jmp Krill_CopyReadError
+
                inc Krill_CopyEOF
Krill_CopyKernalWrite
                lda $ae
                sta Krill_CopyEnd
                lda $af
                sta Krill_CopyEnd+1
                jsr CLRCHN
                jsr Krill_CopyWrite
                lda error_code
                beq +
                jmp Krill_CopyFinish
+
                lda Krill_CopyEOF
                beq +
                jmp Krill_CopyFinish
+
                jsr Krill_CopyResetBuffer
                jmp Krill_CopyKernalRead

; The source must be parked before entering. The write channel stays open.
Krill_CopySaveNative
                ldx #8
-               lda $e0,x
                sta Krill_CopySavedZP,x
                dex
                bpl -
                rts
Krill_CopyWrite
                jsr Krill_CopySaveNative
                jsr Krill_CopyWakeTarget
                lda Krill_CopyOpened
                bne Krill_CopyWriteOpened
                lda write_length
                ldx #<write_fn
                ldy #>write_fn
                jsr SETNAM
                lda #2
                ldx DiskToCopyTo
                tay
                jsr SETLFS_OPEN
                bcc +
                jmp Krill_CopyWriteFail
+
                inc Krill_CopyOpened
                lda DiskToCopyTo
                jsr Krill_CopyCheckDOS
                bcc +
                jmp Krill_CopyWriteDOSFail
+
Krill_CopyWriteOpened
                lda Krill_NativeActive
                beq Krill_CopyKernalOutput
                lda Krill_CopyAnyWritten
                bmi Krill_CopyKernalOutput
                jmp Krill_CopyFastWrite

Krill_CopyKernalOutput
                lda Krill_CopyWritten
                cmp Krill_CopyEnd
                bne +
                lda Krill_CopyWritten+1
                cmp Krill_CopyEnd+1
                bne +
                rts
+               lda #254
                sta Krill_CopyProgressLeft
Krill_CopyWriteSelect
                ldx #2
                jsr CHKOUT
                bcc +
                jmp Krill_CopyWriteFail
+
                lda Krill_CopyWritten
                sta $ae
                lda Krill_CopyWritten+1
                sta $af
                jsr Krill_CopyStageRAM
Krill_CopyWriteLoop
                lda $ae
                cmp Krill_CopyEnd
                bne +
                lda $af
                cmp Krill_CopyEnd+1
                beq Krill_CopyWriteEnd
+               jsr READST
                beq +
                jmp Krill_CopyWriteFail
+
                lda #254
                sec
                sbc Krill_CopyProgressLeft
                tay
                lda FREEMEM,y
                jsr CHROUT
                lda Krill_CopyAnyWritten
                ora #1
                sta Krill_CopyAnyWritten
                inc $ae
                bne +
                inc $af
+               dec Krill_CopyProgressLeft
                bne Krill_CopyWriteLoop
                jsr Krill_CopyWrittenPointer
                jsr CLRCHN
                jsr Krill_CopyProgress
                lda #254
                sta Krill_CopyProgressLeft
                jmp Krill_CopyWriteSelect
Krill_CopyWriteEnd
                jsr Krill_CopyWrittenPointer
                jsr CLRCHN
                lda Krill_CopyProgressLeft
                cmp #254
                beq +
                jsr Krill_CopyProgress
+               rts
Krill_CopyWrittenPointer
                lda $ae
                sta Krill_CopyWritten
                lda $af
                sta Krill_CopyWritten+1
                rts

Krill_CopyWriteDOSFail
                cmp #26
                bne Krill_CopyWriteFail
                lda #30
                sta error_code
                rts
Krill_CopyWriteFail
                lda #31
                sta error_code
                rts

; Close DOS channels while the source is parked. Quiet the target again
; before native close/uninstall, so its DOS ATN acknowledgement cannot interfere.
Krill_CopyFinish
                jsr Krill_CopyWakeTarget
                jsr CLRCHN
                lda Krill_CopyAnyWritten
                and #$40
                bne +
                jsr Krill_CopyReleaseHelper ; free cache before DOS may allocate it on CLOSE
+               lda Krill_CopySourceOpen
                beq +
                lda #3
                jsr CLOSE
                lda #14
                jsr CLOSE
+               lda Krill_CopyOpened
                beq +
                lda #2
                jsr CLOSE
                lda error_code
                bne Krill_CopyCheckFinalDOS
                lda Krill_CopyAnyWritten
                bne Krill_CopyCheckFinalDOS
                jsr Krill_CopyEmptyTarget
Krill_CopyCheckFinalDOS
                lda DiskToCopyTo
                jsr Krill_CopyCheckDOS
                bcc +
                cmp #26
                bne Krill_CopyCloseWriteError
                lda #30
                bne Krill_CopyCloseSetError
Krill_CopyCloseWriteError
                lda error_code
                bne +
                lda #31
Krill_CopyCloseSetError
                sta error_code
+               lda #15
                jsr CLOSE
                lda Krill_NativeActive
                beq Krill_CopyRestore
                lda Krill_CopyAnyWritten
                and #$40
                beq +
                lda DiskToCopyTo
                sta Krill_CopyDevice
                lda #5
                ldx #<Krill_CopyFastRecoverME
                ldy #>Krill_CopyFastRecoverME
                jsr Krill_CopyCommand
                jsr Krill_CopyLongDelay
                lda Krill_CopyAnyWritten
                and #$af ; receiver recovery released the helper as well
                sta Krill_CopyAnyWritten
+               jsr Krill_CopyUploadTarget
                jsr Krill_CopyArmTarget
                jsr Krill_CopyRestoreNative
                lda #$10
                jsr Krill_CopyWake
                lda Krill_CopyEOF
                bne +
                jsr KRILL_CORE_CLOSEFILE
+               jsr Krill_CopyUninstall
                lda #0
                sta Krill_NativeActive
                sta Krill_CopyQuiet
                jsr Krill_CopyLongDelay
Krill_CopyRestore
                lda Krill_SavedDD00
                sta $dd00
                lda Krill_SavedDD02
                sta $dd02
                lda #0
                sta KRILL_MDAPPMODE
                sta Krill_CopyQuiet
                ldx #8
-               lda Krill_CopyGUIZP,x
                sta $e0,x
                dex
                bpl -
                ldx DiskToCopyTo+1
                stx CurDeviceInd
                lda DiskToCopyTo
                sta CurDeviceNo
                lda error_code
                rts
Krill_CopyRestoreNative
                ldx #8
-               lda Krill_CopySavedZP,x
                sta $e0,x
                dex
                bpl -
                lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #3
                sta $dd02
                rts

; DOS status: carry set on error, A=numeric DOS code (or $FF if unavailable).
; Source status uses LFN 14, target status LFN 15 (both SA 15).
; Keep each command channel open until that drive's data channel is closed.
Krill_CopyCheckDOS
                sta Krill_CopyDevice
                lda #0
                jsr SETNAM
                ldx Krill_CopyDevice
                cpx DiskToCopyFrom
                beq +
                lda #15
                bne ++
+               lda #14
++              sta Krill_CopyUploadN
                ldy #15
                jsr SETLFS_OPEN
                bcc +
                cmp #2          ; already-open command channel on this same drive
                bne Krill_CopyDOSFail
+               ldx Krill_CopyUploadN
                jsr CHKIN
                bcs Krill_CopyDOSFail
                jsr CHRIN
                sec
                sbc #"0"
                asl
                sta Krill_CopyIndex
                asl
                asl
                clc
                adc Krill_CopyIndex
                sta Krill_CopyIndex
                jsr CHRIN
                sec
                sbc #"0"
                clc
                adc Krill_CopyIndex
                pha
Krill_CopyDOSDrain
                jsr READST
                bne +
                jsr CHRIN
                cmp #13
                bne Krill_CopyDOSDrain
+               jsr CLRCHN
                pla
                cmp #1
                rts
Krill_CopyDOSFail
                jsr CLRCHN
                lda #$ff
                sec
                rts

; Read-only model signature plus an execute/read-back test. Unsupported devices
; take the KERNAL path; no 1541-specific silence program is sent to them.
Krill_CopyProbe
                sta Krill_CopyDevice
                lda #6
                ldx #<Krill_CopyModelMR
                ldy #>Krill_CopyModelMR
                jsr Krill_CopyCommand
                bcs Krill_CopyProbeFail
                jsr Krill_CopyTalk
                jsr $ffa5
                cmp #"4"
                bne Krill_CopyProbeUntalk
                jsr $ffa5 ; consume the second ROM byte, as Krill getmodel does
                jsr $ffab
                lda #Krill_CopyProbeCodeEnd-Krill_CopyProbeCode
                ldx #<Krill_CopyProbeCode
                ldy #>Krill_CopyProbeCode
                jsr Krill_CopyCommand
                bcs Krill_CopyProbeFail
                lda #5
                ldx #<Krill_CopyProbeME
                ldy #>Krill_CopyProbeME
                jsr Krill_CopyCommand
                bcs Krill_CopyProbeFail
                lda #6
                ldx #<Krill_CopyProbeMR
                ldy #>Krill_CopyProbeMR
                jsr Krill_CopyCommand
                bcs Krill_CopyProbeFail
                jsr Krill_CopyTalk
                jsr $ffa5
                pha
                jsr $ffab
                pla
                cmp #$a5
                beq Krill_CopyProbeOK
Krill_CopyProbeFail
                sec
                rts
Krill_CopyProbeUntalk
                jsr $ffab
                sec
                rts
Krill_CopyProbeOK
                clc
                rts
Krill_CopyTalk
                lda #0
                sta $90
                lda Krill_CopyDevice
                jsr $ffb4
                lda #$6f
                jmp $ff96
Krill_CopyCommand
                sta Krill_CopyUploadN
                stx $ae
                sty $af
                lda #0
                sta $90
                lda Krill_CopyDevice
                jsr $ffb1
                lda #$6f
                jsr $ff93
                ldy #0
-               lda ($ae),y
                jsr $ffa8
                iny
                cpy Krill_CopyUploadN
                bcc -
                jsr $ffae
                lda $90
                cmp #1
                rts
Krill_CopyModelMR !pet "m-r"
                !word $e5c6
                !byte 2
Krill_CopyProbeCode !pet "m-w"
                !word $0300
                !byte 16,$a9,$a5,$8d,$0f,$03,$60
                !fill 10,0
Krill_CopyProbeCodeEnd
Krill_CopyProbeME !pet "m-e"
                !word $0300
Krill_CopyProbeMR !pet "m-r"
                !word $030f
                !byte 1

; Before installation, find the physical start in a DOS raw directory stream.
; DOS supplies 254 bytes per sector, skipping each sector's two link bytes.
; This does not alter GUI64's existing directory reader or parsed records.
Krill_CopyFindStart
                sta Krill_CopyDevice
                lda #1
                ldx #<Krill_CopyInitName
                ldy #>Krill_CopyInitName
                jsr SETNAM
                lda #15
                ldx Krill_CopyDevice
                tay
                jsr SETLFS_OPEN
                lda #15
                jsr CLOSE
                lda #1
                ldx #<dirname
                ldy #>dirname
                jsr SETNAM
                lda #3
                ldx Krill_CopyDevice
                ldy #2
                jsr SETLFS_OPEN
                bcs Krill_CopyFindFail
                ldx #3
                jsr CHKIN
                bcs Krill_CopyFindFail
                jsr Krill_CopyDirectoryBlock ; skip BAM payload
                bcs Krill_CopyFindFail
                lda #18
                sta Krill_CopyBlocks
Krill_CopyFindSector
                jsr Krill_CopyDirectoryBlock
                bcs Krill_CopyFindFail
                ldx #0
Krill_CopyFindEntry
                stx Krill_CopyIndex
                lda FREEMEM,x
                and #7
                beq Krill_CopyFindNext
                cmp #4
                bcs Krill_CopyFindNext
                txa
                clc
                adc #3
                tax
                ldy #0
-               cpy copypastelength
                bcc +
                lda #$a0
                bne ++
+               lda Str_FileName,y
++              cmp FREEMEM,x
                bne Krill_CopyFindNext
                inx
                iny
                cpy #16
                bcc -
                ldx Krill_CopyIndex
                lda FREEMEM+1,x
                beq Krill_CopyFindNext
                sta Krill_CopyName
                lda FREEMEM+2,x
                clc
                adc #1
                sta Krill_CopyName+1
                lda #0
                sta Krill_CopyName+2
                clc
                bcc Krill_CopyFindClose
Krill_CopyFindNext
                lda Krill_CopyIndex
                clc
                adc #32
                tax
                bne Krill_CopyFindEntry
                dec Krill_CopyBlocks
                bne Krill_CopyFindSector
Krill_CopyFindFail
                sec
Krill_CopyFindClose
                php
                jsr CLRCHN
                lda #3
                jsr CLOSE
                plp
                rts
Krill_CopyDirectoryBlock
                ldx #0
-               jsr READST
                bne +
                jsr CHRIN
                sta FREEMEM,x
                inx
                cpx #254
                bcc -
                clc
                rts
+               sec
                rts
Krill_CopyInitName !pet "i"
Krill_CopyEmptyBP !pet "b-p:2,0"


; CBM DOS creates CR in a new sequential file even when no byte was written.
; For an empty source, change only the newly created target file's last-sector
; length byte from 2 to 1, using DOS direct access. Model/execute verification
; and a fresh filename lookup precede this; no BAM/directory sector is written.
Krill_CopyEmptyTarget
                lda DiskToCopyTo
                jsr Krill_CopyProbe
                bcs Krill_CopyEmptyFail
                lda DiskToCopyTo
                jsr Krill_CopyFindStart
                bcs Krill_CopyEmptyFail
                lda #1
                ldx #<Krill_CopyBufferName
                ldy #>Krill_CopyBufferName
                jsr SETNAM
                lda #2
                ldx DiskToCopyTo
                tay
                jsr SETLFS_OPEN
                bcs Krill_CopyEmptyFail
                ldx #7
-               lda Krill_CopyU1,x
                sta FREEMEM,x
                dex
                bpl -
                ldx #7
                lda Krill_CopyName
                jsr Krill_CopyDecimal
                lda #","
                sta FREEMEM,x
                inx
                lda Krill_CopyName+1
                sec
                sbc #1
                jsr Krill_CopyDecimal
                stx Krill_CopyBlocks ; length survives low-level command helpers
                jsr Krill_CopySectorCommand
                bcs Krill_CopyEmptyClose
                lda #7
                ldx #<Krill_CopyEmptyBP
                ldy #>Krill_CopyEmptyBP
                jsr Krill_CopyCommand
                ldx #2
                jsr CHKOUT
                bcs Krill_CopyEmptyClose
                lda #0
                jsr CHROUT
                lda #1
                jsr CHROUT
                jsr CLRCHN
                lda #"2"
                sta FREEMEM+1
                jsr Krill_CopySectorCommand
                bcs Krill_CopyEmptyClose
                jsr Krill_CopyProgress
                clc
Krill_CopyEmptyClose
                php
                jsr CLRCHN
                lda #2
                jsr CLOSE
                plp
                bcc +
Krill_CopyEmptyFail
                lda #31
                sta error_code
+               rts
Krill_CopySectorCommand
                lda Krill_CopyBlocks
                ldx #<FREEMEM
                ldy #>FREEMEM
                jsr Krill_CopyCommand
                lda DiskToCopyTo
                jmp Krill_CopyCheckDOS
Krill_CopyDecimal
                ldy #"0"
-               cmp #10
                bcc +
                sec
                sbc #10
                iny
                bne -
+               pha
                tya
                sta FREEMEM,x
                inx
                pla
                clc
                adc #"0"
                sta FREEMEM,x
                inx
                rts
Krill_CopyBufferName !pet "#"
Krill_CopyU1 !pet "u1:2,0,"

; Wait for the source to offer the next block, with the target still quiet.
Krill_CopyWaitPark
                lda #$37
                sta $dd02
                lda $dd00
                and #3
                sta $dd00
                lda #8
                sta Krill_CopyTimeout
Krill_CopyParkOuter
                ldx #0
                ldy #0
Krill_CopyParkLoop
                lda $dd00
                and #$c0
                cmp #$40
                beq Krill_CopyParkReady
                dex
                bne Krill_CopyParkLoop
                dey
                bne Krill_CopyParkLoop
                dec Krill_CopyTimeout
                bne Krill_CopyParkOuter
                sec
                rts
Krill_CopyParkReady
                clc
                rts

; Four slow ATN edges at a fixed CLK level distinguish control from native data.
; A=$10 resumes source; A=0 releases target and leaves source parked.
Krill_CopyWake
                sta Krill_CopyWakeClock
                lda #$3f
                sta $dd02
                lda Krill_SavedDD00
                and #3
                ora Krill_CopyWakeClock
                ora #8
                sta $dd00
                jsr Krill_CopyDelay
                ldx #4
Krill_CopyWakeLoop
                lda $dd00
                and #$3b
                eor #8
                sta $dd00
                txa
                pha
                jsr Krill_CopyDelay
                pla
                tax
                dex
                bne Krill_CopyWakeLoop
                jsr Krill_CopyDelay
                lda Krill_CopyWakeClock
                beq Krill_CopyWakeDOS
                lda #$37
                sta $dd02
                lda Krill_SavedDD00
                and #3
                sta $dd00
                rts
Krill_CopyWakeDOS
                lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #$3f
                sta $dd02
                jmp Krill_CopyLongDelay
Krill_CopyArmTarget
                lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #3
                sta $dd02
Krill_CopyLongDelay
                lda #30
                sta Krill_CopyIndex
- jsr Krill_CopyDelay
                dec Krill_CopyIndex
                bne -
                rts
Krill_CopyDelay
                ldy #3
- ldx #$ff
-- dex
                bne --
                dey
                bne -
                rts


; Target wake without a native source also handles installation falling back.
Krill_CopyWakeTarget
                lda Krill_CopyQuiet
                beq +
                lda Krill_NativeActive
                beq Krill_CopyWakeUninstalled
                ; Re-arm the passive drive before requesting its DOS return.
                ; Native traffic need not leave one uninterrupted timer interval.
                jsr Krill_CopyArmTarget
                lda #0
                jsr Krill_CopyWake
                lda #0
                sta Krill_CopyQuiet
+               rts
Krill_CopyWakeUninstalled
                jsr Krill_CopyArmTarget
                lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #$3f
                sta $dd02
                jsr Krill_CopyLongDelay
                lda #0
                sta Krill_CopyQuiet
                rts

Krill_CopyUploadTarget
                lda #0
                sta $90
                sta Krill_CopyUploadAt
Krill_CopyUploadPage
                lda DiskToCopyTo
                jsr $ffb1
                lda #$6f
                jsr $ff93
                ldx #0
- lda Krill_CopyMW,x
                jsr $ffa8
                inx
                cpx #3
                bne -
                lda Krill_CopyUploadAt
                jsr $ffa8
                lda #3
                jsr $ffa8
                lda #Krill_CopyTargetEnd-Krill_CopyTargetCode
                sec
                sbc Krill_CopyUploadAt
                cmp #32
                bcc +
                lda #32
+ sta Krill_CopyUploadN
                jsr $ffa8
                ldy #0
- tya
                clc
                adc Krill_CopyUploadAt
                tax
                lda Krill_CopyTargetCode,x
                jsr $ffa8
                iny
                cpy Krill_CopyUploadN
                bcc -
                jsr $ffae
                lda Krill_CopyUploadAt
                clc
                adc Krill_CopyUploadN
                sta Krill_CopyUploadAt
                cmp #Krill_CopyTargetEnd-Krill_CopyTargetCode
                bcc Krill_CopyUploadPage
                lda DiskToCopyTo
                jsr $ffb1
                lda #$6f
                jsr $ff93
                ldx #0
- lda Krill_CopyME,x
                jsr $ffa8
                inx
                cpx #5
                bne -
                jsr $ffae
                lda #1
                sta Krill_CopyQuiet
                lda $90
                cmp #1
                rts

Krill_CopyMW !pet "m-w"
Krill_CopyME !pet "m-e"
                !word $0303
Krill_CopyTargetCode
                !pseudopc $0300 {
                !zone Krill_CopyTarget {

                jmp ArmIRQ
TargetQuiet
                php
                pha
                txa
                pha
                tya
                pha
                cli
JobsIdle
                lda $00
                ora $01
                ora $02
                ora $03
                ora $04
                bmi JobsIdle
                sei
                lda $00
                pha
                lda $3f
                pha
                lda $1800
                pha
                lda $1802
                pha
                lda $180e
                pha
                lda $1c0b
                pha
                lda $1c0e
                pha
                lda $1c06
                pha
                lda $1c07
                pha
                tsx
                stx SavedSP+1
                lda #$1a
                sta $1802
                lda #$7f
                sta $180e
                sta $1c0e
                lda #$c0
                sta $1c0e
                lda #$40
                sta $1c0b
                lda #$d0
                sta $00
                lda #0
                sta $3f
                ldx #0
                ldy #$10
                lda #$ff
                sta $1c04
                stx $1800
ArmLow
                bit $1800
                bpl ArmLow
                sty $1800
                sta $1c05
                cli
ArmHigh
                bit $1800
                bmi ArmHigh
                stx $1800
                sei
                jmp ArmLow
ArmIRQ
                sei
SavedSP ldx #0
                txs
                lda #<Restore
                sta $0301
                lda #>Restore
                sta $0302
                lda #$d0
                sta $00
                ldx #0
                ldy #$10
                lda #$ff
RunHigh
                bit $1800
                bmi RunHigh
                stx $1800
                sta $1c05
                cli
RunLow
                bit $1800
                bpl RunLow
                sty $1800
                sei
                jmp RunHigh
Restore
                sei
                ldx SavedSP+1
                txs
                lda #$7f
                sta $180e
                sta $1c0e
                pla
                sta $1c07
                pla
                sta $1c06
                pla
                ora #$80 ; restore saved interrupt sources in SET mode
                sta $1c0e
                pla
                sta $1c0b
                pla
                ora #$80 ; restore saved interrupt sources in SET mode
                sta $180e
                pla
                sta $1802
                pla
                sta $1800
                pla
                sta $3f
                pla
                sta $00
                pla
                tay
                pla
                tax
                pla
                plp
                rts
TargetEnd = *
                }
                }
Krill_CopyTargetEnd


; Fast target stream. C64 control executes entirely from bank 6.
; DOS allocates/closes the file. Full data blocks use the copy-only raw
; writer without immediate read-back verification. DOS CLOSE still writes
; and verifies the final partial block and directory/BAM metadata.
Krill_CopyFastWrite
                lda Krill_CopyWritten
                cmp Krill_CopyEnd
                bne +
                lda Krill_CopyWritten+1
                cmp Krill_CopyEnd+1
                bne +
                rts
+               jsr Krill_CopyRawCheck
                bcc +
                ; Keep the source installed/parked; only target output falls back.
                lda Krill_CopyAnyWritten
                ora #$80
                sta Krill_CopyAnyWritten
                jmp Krill_CopyKernalOutput
+               jsr Krill_CopyFastUploadTarget
                bcc +
                jmp Krill_CopyWriteFail
+               lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #$3f
                sta $dd02
                lda #0
                ldx #$40
                jsr Krill_CopyFastWait
                bcc +
                jmp Krill_CopyFastFailed
+               jsr Krill_CopyFastReady
                bcc +
                jmp Krill_CopyFastFailed
+
                jsr Krill_CopyFastPacketStart
                bcc +
                jmp Krill_CopyFastFailed
+
                lda Krill_CopyWritten
                sta $ae
                lda Krill_CopyWritten+1
                sta $af
                jsr Krill_CopyStageRAM
                lda Krill_CopyAnyWritten
                ora #1
                sta Krill_CopyAnyWritten
Krill_CopyFastLoop
                lda FREEMEM,y
                jsr Krill_CopySendByte
                bcs Krill_CopyFastFailed
                iny
                cpy Krill_CopyFastCount
                bne Krill_CopyFastLoop
                tya
                clc
                adc $ae
                sta $ae
                bcc +
                inc $af
+               jsr Krill_CopyWrittenPointer
                lda Krill_SavedDD00
                and #3
                sta $dd00 ; drive writes while the C64 paints progress
                jsr Krill_CopyProgress
                lda Krill_CopyWritten
                cmp Krill_CopyEnd
                bne +
                lda Krill_CopyWritten+1
                cmp Krill_CopyEnd+1
                beq Krill_CopyFastDone
+               jsr Krill_CopyFastReady
                bcs Krill_CopyFastFailed
                jsr Krill_CopyFastPacketStart
                jsr Krill_CopyStageRAM
                jmp Krill_CopyFastLoop
Krill_CopyFastDone
                jsr Krill_CopyFastReady
                bcs Krill_CopyFastFailed
                jsr Krill_CopyFastStart
                bcs Krill_CopyFastFailed
                lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #$c0
                ldx #$c0
                jsr Krill_CopyFastWait
                bcs Krill_CopyFastFailed
Krill_CopyFastSuccess
                jmp Krill_CopyDelay

Krill_CopyFastFailed
                lda Krill_SavedDD00
                and #3
                sta $dd00
                jsr Krill_CopyLongDelay ; let a live receiver leave its byte loop
                lda Krill_CopyAnyWritten
                ora #$40              ; deferred drive cleanup after DOS CLOSE
                sta Krill_CopyAnyWritten
                jmp Krill_CopyWriteFail

Krill_CopyFastReady
                lda Krill_SavedDD00
                and #3
                sta $dd00
                lda #$40
                ldx #$c0
                jmp Krill_CopyFastWait
Krill_CopyFastPacketStart
                lda Krill_SavedDD00
                and #3
                ora #8
                sta Krill_CopyTXBase
                sta $dd00
                lda Krill_CopyEnd
                sec
                sbc Krill_CopyWritten
                tax
                lda Krill_CopyEnd+1
                sbc Krill_CopyWritten+1
                bne +
                cpx #254
                bcc ++
+               ldx #254
++              stx Krill_CopyFastCount
                clc
                rts

Krill_CopyFastStart
                jsr Krill_CopyFastPacketStart
                lda #$80
                ldx #$80
Krill_CopyFastWait
                sta Krill_CopyWaitValue
                stx Krill_CopyWaitMask
                lda #8
                sta Krill_CopyTimeout
Krill_CopyFastWaitOuter
                ldx #0
                ldy #0
Krill_CopyFastWaitLoop
                lda $dd00
                and Krill_CopyWaitMask
                cmp Krill_CopyWaitValue
                beq Krill_CopyFastWaitOK
                dex
                bne Krill_CopyFastWaitLoop
                dey
                bne Krill_CopyFastWaitLoop
                dec Krill_CopyTimeout
                bne Krill_CopyFastWaitOuter
                sec
                rts
Krill_CopyFastWaitOK
                lda #0
                sta Krill_CopyTimeout
                clc
                rts

; Copy-only two-bit sender. Page-aligned ROM tables encode four pairs.
; Pair writes are 25 C64 cycles apart, fitting both PAL and NTSC windows.
Krill_CopyTXBase = Krill_CopyWaitValue
Krill_CopySendByte
                tax
                bit $dd00
                bpl Krill_CopyByteRaster
                lda #0
                sta Krill_CopyTimeout
Krill_CopyByteReady
                bit $dd00
                bpl Krill_CopyByteRaster
                sec
                sbc #1
                bne Krill_CopyByteReady
                dec Krill_CopyTimeout
                bne Krill_CopyByteReady
                sec
                rts
; Keep the screen visible. Start only where a badline cannot split the byte.
Krill_CopyByteRaster
                lda $d012
                cmp #45
                bcc Krill_CopyByteStart
                cmp #248
                bcs Krill_CopyByteStart
                sec
                sbc $d011
                and #7
                beq Krill_CopyByteRaster
                cmp #5
                bcs Krill_CopyByteRaster
Krill_CopyByteStart
                lda Krill_CopyTXBase
                ora #$10
                sta $dd00
                !fill 4,$ea
                lda Krill_CopyPair0,x
                ora Krill_CopyTXBase
                sta $dd00
                bit $00
                !fill 5,$ea
                lda Krill_CopyPair1,x
                ora Krill_CopyTXBase
                sta $dd00
                bit $00
                !fill 5,$ea
                lda Krill_CopyPair2,x
                ora Krill_CopyTXBase
                sta $dd00
                bit $00
                !fill 5,$ea
                lda Krill_CopyPair3,x
                ora Krill_CopyTXBase
                sta $dd00
                !fill 12,$ea
                lda Krill_CopyTXBase
                sta $dd00
                clc
                rts
                !align 255,0
Krill_CopyPair0
                !for .v,0,255 { !byte ((.v & 1) << 5) | ((.v & 2) << 3) }
Krill_CopyPair1
                !for .v,0,255 { !byte ((.v & 4) << 3) | ((.v & 8) << 1) }
Krill_CopyPair2
                !for .v,0,255 { !byte ((.v & 16) << 1) | ((.v & 32) >> 1) }
Krill_CopyPair3
                !for .v,0,255 { !byte ((.v & 64) >> 1) | ((.v & 128) >> 3) }

; Check before uploading: receiver 0 plus helper 2 or 4 must both be free.
; An unknown physical-write ROM uses the existing KERNAL target writer.
; Bit 7 of AnyWritten keeps this decision for all following portions.
Krill_CopyRawCheck
                lda DiskToCopyTo
                sta Krill_CopyDevice
                lda #6
                ldx #<Krill_CopyRawBufferMR
                ldy #>Krill_CopyRawBufferMR
                jsr Krill_CopyCommand
                bcs Krill_CopyRawDenied
                jsr Krill_CopyTalk
                jsr $ffa5
Krill_CopyRawBufferStatus
                pha
                jsr $ffab
                pla
                ; Receiver always needs buffer 0. Prefer helper 2, else 4.
                tax
                and #1
                bne Krill_CopyRawDenied
                lda Krill_CopyAnyWritten
                and #$10
                beq Krill_CopyRawChoose
                ; Our helper remains reserved between portions; keep its slot.
                lda Krill_CopyAnyWritten
                and #$20
                beq +
                txa
                and #$10
                bne Krill_CopyRawSelectedCached
                beq Krill_CopyRawDenied
+               txa
                and #4
                beq Krill_CopyRawDenied
Krill_CopyRawSelectedCached
                jmp Krill_CopyRawAllowed
Krill_CopyRawChoose
                txa
                and #4
                beq Krill_CopyRawUse2
                txa
                and #$10
                bne Krill_CopyRawDenied
                lda Krill_CopyAnyWritten
                ora #$20
                bne Krill_CopyRawSelected
Krill_CopyRawUse2
                lda Krill_CopyAnyWritten
                and #$df
Krill_CopyRawSelected
                sta Krill_CopyAnyWritten
                and #1
                bne Krill_CopyRawAllowed
                lda #6
                ldx #<Krill_CopyRawROMMR
                ldy #>Krill_CopyRawROMMR
                jsr Krill_CopyCommand
                bcs Krill_CopyRawDenied
                jsr Krill_CopyTalk
                lda #0
                sta Krill_CopyIndex
Krill_CopyRawCompare
                jsr $ffa5
Krill_CopyRawByte
                ldx Krill_CopyIndex
                cmp Krill_CopyRawExpected,x
                bne Krill_CopyRawUntalkDenied
                inc Krill_CopyIndex
                lda Krill_CopyIndex
                cmp #CopyRawPhysicalEnd-CopyRawWrite
                bcc Krill_CopyRawCompare
                jsr $ffab
Krill_CopyRawAllowed
                clc
                rts
Krill_CopyRawUntalkDenied
                jsr $ffab
Krill_CopyRawDenied
                sec
                rts
Krill_CopyRawBufferMR !pet "m-r"
                !word $024f
                !byte 1
Krill_CopyRawROMMR !pet "m-r"
                !word $f575
                !byte CopyRawPhysicalEnd-CopyRawWrite
Krill_CopyRawExpected = Krill_CopyRawHelperCode + CopyRawWrite - $0500

Krill_CopyFastUploadTarget
                lda #3
                sta Krill_CopyIndex
                lda #0
                sta $90
                sta Krill_CopyUploadAt
Krill_CopyFastUploadPage
                ; Select ROM pages without adding C64 state or RAM code.
                ldx #0
                lda Krill_CopyAnyWritten
                and #$20
                beq +
                ldx #2
+               lda Krill_CopyIndex
                cmp #3
                beq +
                inx
+               lda Krill_CopyImageLo,x
                clc
                adc Krill_CopyUploadAt
                sta $ae
                lda Krill_CopyImageHi,x
                adc #0
                sta $af
                lda DiskToCopyTo
                jsr $ffb1
                lda #$6f
                jsr $ff93
                ldx #0
- lda Krill_CopyMW,x
                jsr $ffa8
                inx
                cpx #3
                bne -
                lda Krill_CopyUploadAt
                jsr $ffa8
                lda Krill_CopyIndex
                jsr $ffa8
                lda #32
                sta Krill_CopyUploadN
                jsr $ffa8
                ldy #0
-               lda ($ae),y
                jsr $ffa8
                iny
                cpy Krill_CopyUploadN
                bcc -
                jsr $ffae
                lda Krill_CopyUploadAt
                clc
                adc Krill_CopyUploadN
                sta Krill_CopyUploadAt
                bne Krill_CopyFastUploadPage
                lda Krill_CopyIndex
                cmp #3
                bne Krill_CopyFastUploadDone
                lda Krill_CopyAnyWritten
                and #$10
                bne Krill_CopyFastUploadDone
Krill_CopyFastUploadHelper
                lda Krill_CopyAnyWritten
                and #$20
                beq +
                lda #7
                bne ++
+               lda #5
++              sta Krill_CopyIndex
                jmp Krill_CopyFastUploadPage
Krill_CopyFastUploadDone
                lda DiskToCopyTo
                jsr $ffb1
                lda #$6f
                jsr $ff93
                ldx #0
- lda Krill_CopyFastME,x
                jsr $ffa8
                inx
                cpx #5
                bne -
                lda Krill_CopyEnd
                sec
                sbc Krill_CopyWritten
                pha
                lda Krill_CopyEnd+1
                sbc Krill_CopyWritten+1
                pha
                tsx
                lda $0102,x
                jsr $ffa8
                pla
                jsr $ffa8
                pla
                jsr $ffae

                lda $90
                cmp #1
                bcs +
                lda Krill_CopyAnyWritten
                ora #$10
                sta Krill_CopyAnyWritten
                clc
+               rts

; Normal completion releases the pinned helper before DOS CLOSE can reuse it.
; Failed receivers use the established deferred recovery after DOS CLOSE.
Krill_CopyReleaseHelper
                lda Krill_CopyAnyWritten
                and #$10
                beq Krill_CopyReleaseDone
                lda DiskToCopyTo
                sta Krill_CopyDevice
                lda Krill_CopyAnyWritten
                and #$20
                beq +
                ldx #<Krill_CopyRelease4
                ldy #>Krill_CopyRelease4
                bne ++
+               ldx #<Krill_CopyRelease2
                ldy #>Krill_CopyRelease2
++              lda #5
                jsr Krill_CopyCommand
                jsr Krill_CopyLongDelay
                lda Krill_CopyAnyWritten
                and #$ef
                sta Krill_CopyAnyWritten
Krill_CopyReleaseDone
                rts
Krill_CopyRelease2 !pet "m-e"
                !word Krill_CopyReleaseEntry2
Krill_CopyRelease4 !pet "m-e"
                !word Krill_CopyReleaseEntry4

Krill_CopyImageLo !byte <Krill_CopyFastTargetCode,<Krill_CopyRawHelperCode,<Krill_CopyFastTargetCode4,<Krill_CopyRawHelperCode4
Krill_CopyImageHi !byte >Krill_CopyFastTargetCode,>Krill_CopyRawHelperCode,>Krill_CopyFastTargetCode4,>Krill_CopyRawHelperCode4
Krill_CopyFastRecoverME !pet "m-e"
                !word CopyWriterRestore
Krill_CopyFastME !pet "m-e"
                !word $0300
; $0205/$0206 are the extra M-E length bytes. File-buffer operations do
; not use the command buffer. A non-local DOS error abandons this routine;
; the C64 timeout then invokes CopyWriterRestore through a fresh M-E command.
; Same receiver address and size; helper buffer follows actual DOS allocation.
!zone Krill_CopyDrive2 {
.RawBuffer = 2
    !source "KrillCopyDrive.asm"
Krill_CopyFastTargetCode = .FastTargetCode
Krill_CopyFastTargetEnd = .FastTargetEnd
Krill_CopyRawHelperCode = .RawHelperCode
Krill_CopyRawHelperEnd = .RawHelperEnd
CopyWriterRestore = .CopyWriterRestore
CopyRawWrite = .CopyRawWrite
CopyRawPhysicalEnd = .CopyRawPhysicalEnd
Krill_CopyRawWrite2 = .CopyRawWrite
Krill_CopyRawReleased2 = .CopyRawReleased
Krill_CopyReleaseEntry2 = .CopyRawRelease
CopyRawReleased = .CopyRawReleased
}
!zone Krill_CopyDrive4 {
.RawBuffer = 4
    !source "KrillCopyDrive.asm"
Krill_CopyFastTargetCode4 = .FastTargetCode
Krill_CopyRawHelperCode4 = .RawHelperCode
!if .CopyWriterRestore != CopyWriterRestore { !error "Copy recovery addresses differ" }
Krill_CopyRawWrite4 = .CopyRawWrite
Krill_CopyRawReleased4 = .CopyRawReleased
Krill_CopyReleaseEntry4 = .CopyRawRelease
}
Krill_CopyROMEnd = *
