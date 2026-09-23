WIN
;MAC

!ifdef WIN{
!to "gui64.win.d64",d64,"gui64.win","gui64 disk"
!source "Constants_win.asm"
} else ifdef MAC{
!to "gui64.mac.d64",d64,"gui64.mac","gui64 disk"
!source "Constants_mac.asm"
}
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
                ; Init zero page
                lda #0
                ldx #$7f
-               sta $10,x
                dex
                bpl -
                lda #>WND_HEAP
                sta WindowOnHeap+1
                lda #$ff
                sta CurrentWindow
                sta CurMenuItem
                ; Initialize CIA timer
                jsr GetWiC64Time
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
                ; Check for Ultimate + CMD Interface
                lda #0
                sta IconAvailable+2
                jsr U_IsUltimateCMD

                ;lda #1

                sta bIsUltimate
                beq +
                inc IconAvailable+2
                ; Get Ultimate's time and retrieve settings values
                jsr UltGetTime
                jsr UltReadSettings
                ; Send target #2 to root
                ldx #2
                stx U_CurTarget
                jsr U_GotoRoot

+               jsr SetGlobals
                jsr RepaintGUI
!ifdef WIN{
                jsr SetBkgPattern

                ; Set graphics environment
                ; Tell CIA that data comes in at bits 0,1
                lda $dd02
                ora #%00000011
                sta $dd02
                ; Choose VIC bank at VIC_BANK
                lda $dd00
                and #%11111100
!if (3-((>VICBANK)/64)) {
                ora #(3-((>VICBANK)/64))
}
                sta $dd00
                ; Choose char set at CHAR_BASE
                lda #(((>(SCRMEM-VICBANK))*4)+((>(CHARBASE-VICBANK))/4))
                sta $d018
                
                sta desktop_d018
                ;and #%11110001
                ;ora #TASKCHARSHI
                ;sta taskbar_d018
                lda #(((>(SCRMEM-VICBANK))*4)+TASKCHARSHI)
                sta taskbar_d018
} else ifdef MAC{
                ; Set graphics environment
                ; Tell CIA that data comes in at bits 0,1
                lda $dd02
                ora #%00000011
                sta $dd02
                ; Choose VIC bank at VIC_BANK
                lda $dd00
                and #%11111100
!if (3-((>VICBANK)/64)) {
                ora #(3-((>VICBANK)/64))
}
                sta $dd00
                ; Choose char set at CHAR_BASE
                lda #(((>(SCRMEM-VICBANK))*4)+((>(CHARBASE-VICBANK))/4))
                sta $d018
}
                ;
                jsr InstallIRQ

!zone MainLoop
MainLoop        ldx exit_code
                beq +
                lda #0
                sta exit_code
                dex
                lda EventsLo,x
                sta call_event+1
                lda EventsHi,x
                sta call_event+2
call_event      jsr $ffff
                
+               ldx exit_code2
                beq MainLoop
                lda #0
                sta exit_code2
                cpx #EC_TIMER
                bne MainLoop
                jsr App_TimerHandler
                jmp MainLoop

!ifdef WIN{
EventsLo        !byte <OnRBtnPress, <OnRBtnRelease, <OnLBtnPress, <OnLBtnRelease, <OnMouseMove
                !byte <Exit, <OnDblClick, <OnScrollWheel, <OnScrollWheel, <OnKeyPress, <RunFile
                !byte <BootFile, <OnTooltip, <OnApp
EventsHi        !byte >OnRBtnPress, >OnRBtnRelease, >OnLBtnPress, >OnLBtnRelease, >OnMouseMove
                !byte >Exit, >OnDblClick, >OnScrollWheel, >OnScrollWheel, >OnKeyPress, >RunFile
                !byte >BootFile, >OnTooltip, >OnApp
}
!ifdef MAC{
EventsLo        !byte <OnRBtnPress, <OnRBtnRelease, <OnLBtnPress, <OnLBtnRelease, <OnMouseMove
                !byte <Exit, <OnDblClick, <OnScrollWheel, <OnScrollWheel, <OnKeyPress, <RunFile
                !byte <BootFile, <OnTooltip, <OnApp
EventsHi        !byte >OnRBtnPress, >OnRBtnRelease, >OnLBtnPress, >OnLBtnRelease, >OnMouseMove
                !byte >Exit, >OnDblClick, >OnScrollWheel, >OnScrollWheel, >OnKeyPress, >RunFile
                !byte >BootFile, >OnTooltip, >OnApp
}
                
;dumm            !byte 0; Uncomment if indirect jumps occur

Exit            pla
                pla
                ; Number of chars in keyboard buffer
                jsr DeinstallIRQ
                jsr Enable_CIA_IRQ
                jsr SetC64Defaults
                ; Reset to BASIC
                jmp ($a000)

OnApp           lda #1
                sta LOAD_TYPE
                jsr RunBootFile
                bne load_err
                ; No error
                jsr BackToNormal
                jsr APP_START
                jmp RepaintGUI

RunFile         pla
                pla
                lda #0
                sta LOAD_TYPE
                jsr RunBootFile
                bne load_error
                ; No error
                jsr SetC64Defaults
                ldx $ae
                ldy $af
                stx $2d   ; Set pointer in zeropage to end of
                sty $2e   ; BASIC program (a.k.a. start of variables)
                jmp RUN

BackToNormal    jsr KillDialog
                jsr Disable_CIA_IRQ
                jsr InstallIRQ
                rts

load_err        jsr BackToNormal
                jsr ShowDiskError
                rts

load_error      jsr load_err
                jmp MainLoop

BootFile        pla
                pla
                lda #0
                sta LOAD_TYPE
                jsr RunBootFile
                bne load_error
                ; No error
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

RunBootFile     jsr PrepareLoad
                jsr LoadFile
                lda error_code
                rts

!zone LoadRoutines
;load_fn         !pet "0123456789abcdef",0
;error_code      !byte 0
load_length     !byte 0
; Addresses in zero page
BYTES_PER_PROGBIT_LO= $b0
BYTES_PER_PROGBIT_HI= $b1
MOTION_COUNTER      = $b2
LOAD_TYPE           = $b3
; Loads file in Str_FileName to its address
; Input:
; LOAD_TYPE = 0/1
; 0: Load to address given by first two bytes of file
; 1: Load app to APP_START
LoadFile        lda #$02      ; logical number
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
                lda LOAD_TYPE
                beq +
                ; Load to APP_START
                ldx #<APP_START
                ldy #>APP_START ; non-zero
+               ; Use Addr(X/Y) as destination address
                stx $ae
                stx $02
                sty $af
                sty $03
                ; Load file
                ldy #0
-               jsr READST
                bne load_eof    ; either EOF or read error
                jsr CHRIN       ; get a byte from file
                sta ($ae),y     ; write byte to memory
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
                jmp CLOSE       ; close file

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
PrepareLoad     jsr SelectControl1
                jsr GetFile
                lda res
                bne +
                pla
                pla
                jmp MainLoop
+               ; Copy Str_FileName to load_fn
                ; and retrieve length
                sty load_length
;-               lda Str_FileName,y
;                sta load_fn,y
;                dey
;                bpl -
                ; Compute bytes per progress bit
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
                ;
!ifdef WIN{
                jsr UninstallIRQ_FakeTB
} else ifdef MAC{
                jsr UninstallIRQ
}
                jmp Enable_CIA_IRQ

SetC64Defaults  ldx #1
                stx $dc0e
                dex
                stx VIC+21
                stx SPR_PRIORITY
                stx 198; chars in keyboard buffer
                ; Set $01
                lda #55
                sta $01
                ; Initialize BASIC-RAM/ZP:
                ; - Copy CHRGET to $73
                ; - Set TXTTAB ($2B/$2C)
                ; - Set MEMSIZ/FRETOP
                ; - Descriptor stack and so on...
                jsr $E3BF
                ; Standard C64 graphics
-               lda $d012
                bne -
                lda #8
                sta $d016
                lda #CL_LIGHTBLUE
                sta FRAMECOLOR
                lda #CL_DARKBLUE
                sta BKGCOLOR
                ; Char set and screen ram
                lda #21
                sta $d018
                ; VIC bank
                lda $dd00
                ora #%00000011
                sta $dd00
                ;
                jmp $FDDD; set timer

Enable_CIA_IRQ  lda #%11111111
                sta $dc0d
                lda $dc0d
                rts

;; Get back to standard C64 graphics settings
;StdGraphics     lda $d012
;                bne StdGraphics
;                lda #0
;                sta VIC+21
;                sta SPR_PRIORITY
;                lda #8
;                sta $d016
;                ;
;                lda #CL_LIGHTBLUE
;                sta FRAMECOLOR
;                lda #CL_DARKBLUE
;                sta BKGCOLOR
;                ; Char set and screen ram
;                lda #21
;                sta $d018
;                ; VIC bank
;                lda $dd00
;                ora #%00000011
;                sta $dd00
;                ;
;                ;lda #$04
;                ;sta 648
;                rts

!zone MainBody
                !source "KeyMouseJoy.asm"
                !source "Math.asm"
                !source "Clock.asm"
                !source "Window_UltimateBrowser.asm"
                !source "Icons.asm"
                !source "NoGUI.asm"
                !source "WindowFunctions.asm"
                !source "SD2IEC.asm"
                !source "PaintFunctions.asm"
                !source "ControlFunctions.asm"
                !source "StringRoutines.asm"
                !source "WindowManagement.asm"
                !source "UltimateLib.asm"
                !source "WiC64_Time.asm"
!ifdef WIN{
                !source "Events_win.asm"
                !source "IRQ_win.asm"
                !source "Graphics_win.asm"
                !source "ControlPaintFunctions_win.asm"
                !source "TaskBar_win.asm"
                !source "StdWindowProc_win.asm"
                !source "MenuFunctions_win.asm"
                !source "StringsAndControls_win.asm"
                !source "Window_Drive_win.asm"
                !source "Window_Settings_win.asm"
                !source "Window_Viewer_win.asm"
                !source "Window_Dialogs_win.asm"
                !source "DialogProcs_win.asm"
                !source "DiskOperations_win.asm"
                !source "Data_win.asm"
} else ifdef MAC{
                !source "Events_mac.asm"
                !source "IRQ_mac.asm"
                !source "Graphics_mac.asm"
                !source "ControlPaintFunctions_mac.asm"
                !source "StdWindowProc_mac.asm"
                !source "MenuFunctions_mac.asm"
                !source "StringsAndControls_mac.asm"
                !source "Window_Drive_mac.asm"
                !source "Window_Settings_mac.asm"
                !source "Window_Viewer_mac.asm"
                !source "Window_Dialogs_mac.asm"
                !source "DialogProcs_mac.asm"
                !source "DiskOperations_mac.asm"
                !source "Data_mac.asm"
}
                !source "API_Functions.asm"

; Prog End
; WIN: 5D68,   MAC: 5890?

*=$5f00
GUI_CreateWindow                jmp CreateWindow
GUI_CreateWindowEx              jmp CreateWindowEx
GUI_KillCurWindow               jmp KillCurWindow
GUI_StdWndProc                  jmp StdWndProc
GUI_IsInCurControl              jmp IsInCurControl
GUI_Repaint                     jmp RepaintGUI
GUI_GetDesign                   jmp GetDesign
GUI_UpdateControl               jmp UpdateControl
!ifdef MAC{
GUI_SelectCtrl_AddBits          jmp SelCtrl_AddBits
}
!ifdef WIN{
GUI_SelectCtrl_AddBits          jmp EmptyWndProc; just rts
}
GUI_SetCtrlStringList           jmp SetCtrlStringList
GUI_SetCtrlString               jmp SetCtrlString
GUI_SelectControl               jmp SelectControl
GUI_SelectControl0              jmp SelectControl0
GUI_SelectControl1              jmp SelectControl1
GUI_SelectControl2              jmp SelectControl2
GUI_SelectControl3              jmp SelectControl3
GUI_SelectControl4              jmp SelectControl4
GUI_UpdateWindow                jmp UpdateWindow
GUI_SetEditSLInfo               jmp SetEditSLInfo
GUI_RegisterChars               jmp App_RegisterChars
GUI_SetCtrlActionsRoutine       jmp App_SetCtrlActionsRoutine
GUI_SetPaintCtrlsRoutine        jmp App_SetPaintCtrlsRoutine
GUI_AddBufWidthToFD             jmp AddBufWidthToFD
GUI_AddBufWidthTo02             jmp AddBufWidthTo02
GUI_IsInCurMenu                 jmp IsInCurMenu
GUI_GetCurMenuID                jmp App_GetCurMenuID
GUI_ShowMessage                 jmp ShowMessage
GUI_GetMousePosInWnd            jmp GetMousePosInWnd
GUI_GetCSTMWindowColor          jmp App_GetCSTMWindowColor
GUI_GetCSTMDesktopColor         jmp App_GetCSTMDesktopColor
GUI_StartTimer                  jmp App_StartTimer
GUI_StopTimer                   jmp App_StopTimer
GUI_InitTimer                   jmp App_SetTimerHandler
GUI_RepaintCurWindow            jmp App_RepaintCurWindow
GUI_FindWndByType               jmp IsWndTypePresent
GUI_SelectTopWindow             jmp SelectTopWindow
GUI_AddControl                  jmp AddControl

; ATTENTION: APP_START is at $6000
!warnon W1000


!zone Appendix
; The following is copied to $d000 at GUI64 startup
!ifdef WIN{
GraphicsData    !bin "chars50_win.bin"
SpriteData      !source "Sprites.asm"
TaskCharsData   !bin "TaskbarChars6_win.bin"
} else ifdef MAC {
GraphicsData    !bin "chars12_mac.bin"
SpriteData      !source "Sprites.asm"
}