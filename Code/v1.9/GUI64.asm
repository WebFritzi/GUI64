!to "gui64.d64",d64
!source "Constants.asm"
!source "Macros.asm"

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
                sta $01
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

                jsr SetGlobals
                jsr RepaintAll
                jsr SetTaskbarColors
                jsr PaintTaskbar
                jsr SetBkgPattern
                jsr SetGraphicsEnvironment

                lda $d018
                sta desktop_d018
                and #%11110001
                ora #TASKCHARSHI
                sta taskbar_d018

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
                jmp ($a000);$fce2

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
load_fn         !pet "0123456789abcdef",0;,0,0,0,0
error_code      !byte 0
load_length     !byte 0
;BytesPerProgBit !byte 0,0
;motion_counter  !byte 0
BYTES_PER_PROGBIT_LO= $57
BYTES_PER_PROGBIT_HI= $58
MOTION_COUNTER      = $59
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
+               lda #3
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

LoadMotion      ldx MOTION_COUNTER
                lda #221
                sta SCRMEM+880,x
                lda #239
                sta SCRMEM+880+40,x
                lda #238
                sta SCRMEM+880+80,x
                inc MOTION_COUNTER
                lda BYTES_PER_PROGBIT_LO
                sta $fd
                lda BYTES_PER_PROGBIT_HI
                sta $fe
                rts

FillMotion      ldx MOTION_COUNTER
-               lda #221
                sta SCRMEM+880,x
                lda #239
                sta SCRMEM+880+40,x
                lda #238
                sta SCRMEM+880+80,x
                inx
                cpx #39
                bcc -
                ldx #80
-               jsr Pause
                dex
                bpl -
                rts

Pause           ldy #0
-               dey
                bne -
                rts

; Fills load_fn and load_length
PrepareLoad     jsr GetFile
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
                ldx #5
                lda #0
-               asl $fe
                rol $fd
                rol
                dex
                bpl -
                ; DO NOT CHANGE FDFE AGAIN BEFORE START OF LoadFile
                sta BYTES_PER_PROGBIT_HI
                sta $fe
                lda $fd
                sta BYTES_PER_PROGBIT_LO
                ;
                jsr GetCurDeviceNo
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
                jsr FakeTaskbar
                jsr FakeTaskbarBox
                jsr UninstallIRQ
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
                !source "Events.asm"
                !source "KeyMouseJoy.asm"
                !source "IRQ.asm"
                !source "Graphics.asm"
                !source "NoGUI.asm"
                !source "WindowFunctions.asm"
                !source "ControlFunctions.asm"
                !source "PaintFunctions.asm"
                !source "ControlPaintFunctions.asm"
                !source "StringRoutines.asm"
                !source "WindowManagement.asm"
                !source "TaskBar.asm"
                !source "StdWindowProc.asm"
                !source "MenuFunctions.asm"
                !source "DriveWindow.asm"
                !source "SD2IEC.asm"
                !source "SettingsWindow.asm"
                !source "ViewerWindow.asm"
                !source "ShowDialogs.asm"
                !source "DialogProcs.asm"
                !source "DiskOperations.asm"
                !source "Math.asm"
                !source "Data.asm"
                !source "StringsAndControls.asm"

; ATTENTION: PATH_A_EX is at $6500

!zone Appendix
; The following is copied to $d000 at GUI64 startup
;*=GRAPHICSDATA
GraphicsData    !bin "chars39.bin"
SpriteData      !source "Sprites.asm"
TaskCharsData   !bin "TaskbarChars6.bin"
