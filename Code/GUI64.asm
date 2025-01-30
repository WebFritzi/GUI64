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
two_bytes       !byte 0,0 ; First 2 bytes of file

BOOT            ; Clear screen
                ; (that's why code is here and not in default screen RAM)
                jsr CLRSCR
                jmp (two_bytes)

RUN             ; Run BASIC program at $0801
                ; Clear screen (that's why code is here)
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
                ; Bank out BASIC and Kernal
                lda #53 ; RAM / IO / RAM
                sta $01

                ; Copy chars and sprites to $E000
                lda #<GraphicsData
                sta $fb
                lda #>GraphicsData
                sta $fc
                lda #<CHARBASE
                sta $fd
                lda #>CHARBASE
                sta $fe
                ;
                ldx #17
--              ldy #$00
-               lda ($fb),y
                sta ($fd),y
                dey
                bne -
                inc $fc
                inc $fe
                dex
                bne --

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

Exit            ; Number of chars in keyboard buffer
                jsr DeinstallIRQ
                jsr Enable_CIA_IRQ
                jsr SetC64Defaults
                ; Reset to BASIC
                jmp ($a000);$fce2

BootFile        jsr PrepareLoad
                jsr ReadTwoBytes
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
                lda #$01
                sta two_bytes
                lda #$08
                sta two_bytes+1
                jsr LoadFile
                lda error_code
                beq +
load_error      ; Error
                jsr InstallIRQ
                jsr ShowDiskError
                jmp MainLoop
+               ; No error
                stx $2D   ; Zeiger auf Start der Variablen
                sty $2E   ; (a.k.a. "Ende des Programms") setzen
                jsr SetC64Defaults
                jmp RUN

!zone LoadRoutines
load_appendix   !pet ",p,p"
load_fn         !pet "0123456789abcdef",0,0,0,0,0
error_code      !byte 0
load_length     !byte 0
; Loads file in Str_FileName to its address
LoadFile        lda load_length
                ldx #<load_fn
                ldy #>load_fn
                jsr SETNAM
                lda #1
                ldx CurDeviceNo
                ldy #0
                jsr SETLFS
                lda #0
                ; Load to address
                ldx two_bytes
                ldy two_bytes+1
                jsr LOAD
                bcc +
                ; Accumulator contains BASIC error code
                ; most likely errors:
                ; A = $05 (DEVICE NOT PRESENT)
                ; A = $04 (FILE NOT FOUND)
                ; A = $1D (LOAD ERROR)
                ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
                sta error_code
+               rts

PrepareLoad     jsr GetFileName
                lda res
                bne +
                pla
                pla
                jmp MainLoop
                ;
+               jsr BlackTaskbar
                jsr UninstallIRQ
                jsr Enable_CIA_IRQ
                ;
                jsr GetCurDeviceNo
                lda #0
                sta error_code
                ; Copy Str_FileName to load_fn
                ldx #16
-               lda Str_FileName,x
                sta load_fn,x
                dex
                bpl -
                ; Place X index at end of load_fn
                ldx #$ff
-               inx
                lda load_fn,x
                bne -
                ; Append ",p,p" to end of load_fn
                ldy #0
-               lda load_appendix,y
                sta load_fn,x
                inx
                iny
                cpy #4
                bcc -
                stx load_length
                rts

; Read first two bytes from file to two_bytes
ReadTwoBytes    lda load_length
                ldx #<load_fn
                ldy #>load_fn
                jsr SETNAM
                lda #$02      ; file number 2
                ldx CurDeviceNo
                ldy #$02      ; secondary address 2
                jsr SETLFS
                jsr OPEN
                bcc +
                ; Error
                sta error_code
                lda #$02      ; file number 2
                jsr CLOSE
                jsr CLRCHN
                rts
+               ; No error
                ldx #$02      ; file number 2
                jsr CHKIN     ; file 2 now used as input
                ;
                jsr CHRIN     ; get a byte from file
                sta two_bytes
                jsr CHRIN     ; get a byte from file
                sta two_bytes+1
                lda #$02      ; file number 2
                jsr CLOSE
                jsr CLRCHN
                rts

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
                !source "SettingsWindow.asm"
                !source "ViewerWindow.asm"
                !source "DialogFunctions.asm"
                !source "DiskOperations.asm"
                !source "Math.asm"
                !source "Data.asm"
                !source "StringsAndControls.asm"

; ATTENTION: FREEMEM is at $5f00

!zone Appendix
; The following is copied to $e000 at GUI64 startup
;*=GRAPHICSDATA
GraphicsData    !bin "chars33.bin"
                !bin "TaskbarChars6.bin"
SpriteData      !source "Sprites.asm"