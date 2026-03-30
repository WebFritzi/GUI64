;WIN
MAC

!ifdef WIN{
!to "gui64.win.d64",d64,"gui64.win","gui64 disk"
!source "Constants_win.asm"
!source "Macros_win.asm"
} else ifdef MAC{
!to "gui64.mac.d64",d64,"gui64.mac","gui64 disk"
!source "Constants_mac.asm"
!source "Macros_mac.asm"
}


!zone AutoRun
;*=$0300
;; Standard C64 vectors
;!byte $8b, $e3, $83, $a4, $7c, $a5, $1a, $a7, $e4, $a7, $86, $ae, $00, $00, $00, $00, $4c, $48, $b2, $00
;!byte $31, $ea, $66, $fe, $47, $fe, $4a, $f3, $91, $f2, $0e, $f2, $50, $f2, $33, $f3, $57, $f1
*=$0326; CHROUT vector ($0326)
!byte <autostart, >autostart
;*** $0328 - $032f
!byte $ed,$f6,$3e,$f1,$2f,$f3,$66,$fe
;*** $0330 - $030b
!byte $a5,$f4,$ed,$f5,$00,$00,$00,$00,$00,$00,$00,$00
;; Super Snapshot v5 vectors
;;*** $0328 - $032f    
;!byte $ed,$f6,$3e,$f1,$2f,$f3,$66,$fe
;;*** $0330 - $030b
;!byte $fd,$de,$f1,$de,$00,$00,$00,$00,$00,$00,$00,$00

;*** $033c = Cassette buffer
*=$033c
BOOT            ; Clear screen
                ; (that's why code is here and not in default screen RAM)
                jsr CLRSCR
                jmp ($02)

; Run BASIC program at $0801
RUN             ; Clear screen
                ; (that's why code is here and not in default screen RAM)
                jsr CLRSCR
                lda #0
                sta $0800
                jsr $A533 ; Re-link program
                jsr $A659 ; Reset CLR, TXTPTR
                jmp $A7AE ; Jump into interpreter loop

autostart       ; No kernal messages ("SEARCHING FOR ..." etc)
                lda #0
                sta $9d
                ; Restore std kernal vectors
                jsr RESTOR
                ; Disable system interrupt
                jsr Disable_CIA_IRQ
                ; Initialize CIA timer
                jsr TODInit

                ; Bank out BASIC, I/O, and Kernal
                lda #$34 ; RAM / RAM / RAM
                sta $01
                ; Copy desktop chars and sprites to CHARBASE
                lda #<GraphicsData
                sta $fb
                lda #>GraphicsData
                sta $fc
                lda #<CHARBASE
                sta $fd
                lda #>CHARBASE
                sta $fe
                ;
                ldx #13
                jsr CopyBlockFBtoFD

                ; Bank in I/O
                lda #$35 ; RAM / IO / RAM
                sta  $01
                
!ifdef WIN{
                ; Copy taskbar chars to TASKCHARBASE
                lda #<TaskCharsData
                sta $fb
                lda #>TaskCharsData
                sta $fc
                lda #<TASKCHARBASE
                sta $fd
                lda #>TASKCHARBASE
                sta $fe
                ;
                ldx #4
                jsr CopyBlockFBtoFD
}

                ; Check for Ultimate + CMD Interf
                jsr IsUltimateCMD
                
                ;lda #1
                
                sta bIsUltimate
                beq +
                lda #1
                sta IconAvailable+2
                ; Get Ultimate's time and retrieve settings values
                jsr UltGetTime
                jsr UltReadSettings

+               jsr SetGlobals
                jsr RepaintAll

!ifdef WIN{
                jsr SetTaskbarColors
                jsr PaintTaskbar
                jsr SetBkgPattern

                jsr SetGraphicsEnvironment

                lda $d018
                sta desktop_d018
                and #%11110001
                ora #TASKCHARSHI
                sta taskbar_d018
} else ifdef MAC {
                jsr MenubarToScreen
                jsr SetGraphicsEnvironment
}

                jsr InstallIRQ

!zone MainLoop
MainLoop        lda exit_code
                beq MainLoop
                ;
                ldx #0
                stx exit_code
                ;
                cmp #EC_RBTNPRESS
                beq RBtnPress
                cmp #EC_RBTNRELEASE
                beq RBtnRelease
                cmp #EC_LBTNPRESS
                beq LBtnPress
                cmp #EC_LBTNRELEASE
                beq LBtnRelease
                cmp #EC_MOUSEMOVE
                beq Moved
                cmp #EC_GAMEEXIT
                beq Exit
                cmp #EC_DBLCLICK
                beq BtnDblClick
                cmp #EC_SCROLLWHEELDOWN
                beq ScrollDown
                cmp #EC_SCROLLWHEELUP
                beq ScrollUp
                cmp #EC_KEYPRESS
                beq KeyPress
                cmp #EC_LLBTNPRESS
                beq LongLBtnPress
                cmp #EC_RUNFILE
                beq RunFile
                cmp #EC_BOOTFILE
                beq BootFile
                cmp #EC_TOOLTIP
                beq Tooltip
                jmp MainLoop
                
dumm            !byte 0; Uncomment if indirect jumps occur

RBtnPress       jsr OnRBtnPress
                jmp MainLoop

RBtnRelease     jsr OnRBtnRelease
                jmp MainLoop

LBtnPress       jsr OnLBtnPress
                jmp MainLoop

LongLBtnPress   jsr OnLongLBtnPress
                jmp MainLoop

LBtnRelease     jsr OnLBtnRelease
                jmp MainLoop

BtnDblClick     jsr OnDblClick
                jmp MainLoop

Moved           jsr OnMouseMove
                jmp MainLoop

ScrollDown      jsr OnScrollWheel
                jmp MainLoop

ScrollUp        jsr OnScrollWheel
                jmp MainLoop

KeyPress        jsr OnKeyPress
                jmp MainLoop

Tooltip         jsr OnTooltip
                jmp MainLoop

Exit            ; Number of chars in keyboard buffer
                jsr DeinstallIRQ
                jsr Enable_CIA_IRQ
                jsr SetC64Defaults
                ; Reset to BASIC
                jmp ($a000)

BootFile        jsr PrepareLoad
                jsr LoadFile
                lda error_code
                beq +
                ; Error
                jmp load_error
+               ; No error
                jsr SetC64Defaults
                lda #0
                sta $0800
                sta $0801
                sta $0802
                pla
                pla
                pla
                pla
                pla
                pla
                ;
                jmp BOOT

RunFile         jsr PrepareLoad
                jsr LoadFile
                lda error_code
                beq +
load_error      ; Error
                jsr Disable_CIA_IRQ
                jsr InstallIRQ
                jsr ShowDiskError
                jmp MainLoop
+               ; No error
                ldx $ae
                ldy $af
                stx $2d   ; Set pointer in zeropage to end of
                sty $2e   ; BASIC program (a.k.a. start of variables)
                jsr SetC64Defaults
                jmp RUN

!zone LoadRoutines
load_fn         !pet "0123456789abcdef",0
error_code      !byte 0
load_length     !byte 0
; Addresses in zero page
BYTES_PER_PROGBIT_LO= $5c
BYTES_PER_PROGBIT_HI= $5d
MOTION_COUNTER      = $5e
; Loads file in Str_FileName to its address
LoadFile        lda #$02      ; logical number
                ldx CurDeviceNo ; device number
                ldy #$00      ; secondary address
                jsr SETLFS    ; set file parameters
                ldx #<load_fn
                ldy #>load_fn
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
                jsr CHRIN       ; read start address LSB
                sta $ae
                sta $02         ; for BOOT
                jsr CHRIN       ; read start address MSB
                sta $af
                sta $03         ; for BOOT
                ; Load file
                ldy #0
-               jsr READST
                bne load_eof    ; either EOF or read error
                jsr CHRIN       ; get a byte from file
                STA ($ae),Y     ; write byte to memory
                dec $fd
                bne +
                dec $fe
                bpl +
                jsr LoadMotion
+               inc $ae
                bne +
                inc $af
+               jmp -           ; next byte
load_eof        and #$40        ; end of file?
                bne +
                lda #4
                sta error_code
+               jsr FillMotion
load_close      jsr CLRCHN      ; end data input/output of file
                lda #$02
                jsr CLOSE       ; close file
                rts

LoadMotion      inc MOTION_COUNTER
                ldx MOTION_COUNTER
!ifdef WIN{
                lda #CL_DARKBLUE
                sta CLRMEM+451,x
} else ifdef MAC{
                lda #CL_DARKGRAY
                sta CLRMEM+491,x
}
                lda BYTES_PER_PROGBIT_LO
                sta $fd
                lda BYTES_PER_PROGBIT_HI
                sta $fe
                rts

FillMotion      ldx MOTION_COUNTER
!ifdef WIN{
-               lda #CL_DARKBLUE
                sta CLRMEM+451,x
} else ifdef MAC{
-               lda #CL_DARKGRAY
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

; Uses y only
Pause           ldy #0
-               dey
                bne -
                rts

; Fills load_fn and load_length
PrepareLoad     +SelectControl 1
                jsr GetFile
                lda res
                bne +
                pla
                pla
                jmp MainLoop
+               ; Compute bytes per progress bit
                lda FileSizeHex
                sta $fd
                lda #9
                sta $fc
                jsr DivideFDbyFC
                ;
                lsr $fd
                ror $fe
                lda $fd
                sta BYTES_PER_PROGBIT_HI
                lda $fe
                sta BYTES_PER_PROGBIT_LO
                sta $fd
                lda BYTES_PER_PROGBIT_HI
                sta $fe
                ;
                jsr GetCurDeviceNo
                jsr ShowLoadDlg
                jsr error_codeTo0
                ; Copy Str_FileName to load_fn
                ldx #16
-               lda Str_FileName,x
                sta load_fn,x
                dex
                bpl -
                ; Compute file name length
                ldx #$ff
-               inx
                lda load_fn,x
                bne -
                stx load_length
                ;
                jsr UninstallIRQ
!ifdef WIN{
                jsr FakeTaskbar
}
                jmp Enable_CIA_IRQ

SetC64Defaults  lda #1
                sta $dc0e
                ; Set $01
                lda #55
                sta $01
                ;
                jsr StdGraphics
                ;
                lda #0
                sta 198
                ;lda #64
                ;sta $dc05
                jsr $FDDD; set timer
                rts

Enable_CIA_IRQ  lda #%11111111
                sta $dc0d
                lda $dc0d
                rts

; Get back to standard C64 graphics settings
StdGraphics     lda #0
                sta VIC+21
-               lda $d012
                bne -
                lda $d016
                and #%11101111
                sta $d016
                lda #CL_LIGHTBLUE
                sta FRAMECOLOR
                lda #CL_DARKBLUE
                sta BKGCOLOR
                ; Reset graphics environment
                ; Char set and screen ram
                lda #21
                sta $d018
                ; VIC bank
                lda $dd02
                ora #%00000011
                sta $dd02
                lda $dd00
                and #%11111100
                ora #%00000011
                sta $dd00
                ;
                lda #$04
                sta 648
                lda #0
                sta SPR_PRIORITY
                rts

!zone MainBody
                !source "KeyMouseJoy_win.asm"
                !source "Math_win.asm"
                !source "UltimateFunctions.asm"
                !source "Window_UltimateBrowser.asm"

!ifdef WIN{
                !source "Events_win.asm"
                !source "IRQ_win.asm"
                !source "Graphics_win.asm"
                !source "NoGUI_win.asm"
                !source "WindowFunctions_win.asm"
                !source "ControlFunctions_win.asm"
                !source "PaintFunctions_win.asm"
                !source "ControlPaintFunctions_win.asm"
                !source "StringRoutines_win.asm"
                !source "WindowManagement_win.asm"
                !source "TaskBar_win.asm"
                !source "StdWindowProc_win.asm"
                !source "MenuFunctions_win.asm"
                !source "Window_Drive_win.asm"
                !source "SD2IEC_win.asm"
                !source "Window_Settings_win.asm"
                !source "Window_Viewer_win.asm"
                !source "Window_Dialogs_win.asm"
                !source "DialogProcs_win.asm"
                !source "DiskOperations_win.asm"
                !source "Data_win.asm"
                !source "StringsAndControls_win.asm"
                !source "Clock_win.asm"
                !source "Icons_win.asm"
} else ifdef MAC{
                !source "Events_mac.asm"
                !source "IRQ_mac.asm"
                !source "Graphics_mac.asm"
                !source "NoGUI_mac.asm"
                !source "WindowFunctions_mac.asm"
                !source "ControlFunctions_mac.asm"
                !source "PaintFunctions_mac.asm"
                !source "ControlPaintFunctions_mac.asm"
                !source "StringRoutines_mac.asm"
                !source "WindowManagement_mac.asm"
                !source "StdWindowProc_mac.asm"
                !source "MenuFunctions_mac.asm"
                !source "Window_Drive_mac.asm"
                !source "SD2IEC_mac.asm"
                !source "Window_Settings_mac.asm"
                !source "Window_Viewer_mac.asm"
                !source "Window_Dialogs_mac.asm"
                !source "DialogProcs_mac.asm"
                !source "DiskOperations_mac.asm"
                !source "Data_mac.asm"
                !source "StringsAndControls_mac.asm"
                !source "Clock_mac.asm"
                !source "Icons_mac.asm"
}

; ATTENTION: PATH_A_EX is at WIN: $7000, MAC: $6900

!zone Appendix
; The following is copied to $d000 at GUI64 startup
!ifdef WIN{
GraphicsData    !bin "chars41_win.bin"
SpriteData      !source "Sprites_win.asm"
TaskCharsData   !bin "TaskbarChars6_win.bin"
} else ifdef MAC {
GraphicsData    !bin "chars4_mac.bin"
SpriteData      !source "Sprites_mac.asm"
}