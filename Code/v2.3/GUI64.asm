!ifndef MAC {
    WIN = 1
}

; Addresses in zero page for file loading
BYTES_PER_PROGBIT_LO= $b0
BYTES_PER_PROGBIT_HI= $b1
MOTION_COUNTER      = $b2
LOAD_TYPE           = $b3

!ifdef WIN{
                !source "Constants_win.asm"
}
!ifdef MAC{
                !source "Constants_mac.asm"
}
!source "Macros.asm"
                !source "build/krill/symbols.asm"

; RAM payload for the Magic Desk startup copier; KERNAL vectors stay untouched.
*=$033c

; Run BASIC program at $0801
RUN             ; Clear screen
                ; (that's why code is here and not in default screen RAM)
                jsr CLRSCR
                lda #0
                sta $0800
                jsr $A533 ; Re-link program
                jsr $A659 ; Reset CLR, TXTPTR
                jmp $A7AE ; Jump into interpreter loop

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
                jmp InstallIRQ

load_err        jsr BackToNormal
                jmp ShowDiskError

load_error      jsr load_err
                jmp MainLoop

BootFile        jmp BootBasic

; Load-address operand retained for the shared PRG readers and legacy callers.
; The Boot option now hands LOAD to BASIC and never executes this jump.
jmp_boot        jmp $FFFF

RunBootFile     jsr PrepareLoad
                jsr LoadFile
                lda error_code
                rts

;load_fn         !pet "0123456789abcdef",0
;error_code      !byte 0
load_length     !byte 0
; Loads file in Str_FileName to its address
; Input:
; LOAD_TYPE = 0/1
; 0: Load to address given by first two bytes of file
; 1: Load app with check of first two bytes of file
LoadFile
                lda LOAD_TYPE
                beq Krill_LoadGame
                jmp Krill_LoadApp

                !source "KrillGate.asm"

; Uses y only
Pause           ldy #0
-               dey
                bne -
                rts


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

; One writable Krill core for Apps and games, protected below game RAM.
!if * > KRILL_RESIDENT_RAM { !error "Game return/start code overlaps Krill core" }
* = KRILL_RESIDENT_RAM
!bin "build/krill/loader.bin"
KrillResidentEnd = *
!if KrillResidentEnd > KRILL_GAME_STATE { !error "Krill core overlaps low-RAM state" }

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
                ;
                jsr GetCurDeviceNo
                jsr ShowLoadDlg
                jsr error_codeTo0
                ;
                rts             ; Installer gate stops GUI IRQ/sprites for Apps and games.

KrillLowCodeEnd = *
!if KrillLowCodeEnd > KRILL_GAME_STATE { !error "Load preparation overlaps low-RAM state" }

                ; One-time startup reuses the later game-state area below $0801.
                !source "Startup.asm"

                !source "KeyMouseJoy.asm"
                !source "Math.asm"
                !source "Clock.asm"
                !source "Window_UltimateBrowser.asm"
                !source "Icons.asm"
                !source "NoGUI.asm"
                !source "WindowFunctions.asm"

; Boot leaves GUI64 before loading: BASIC executes LOAD"name",device,1.
; KERNAL initialization clears low RAM and the normal screen at $0400-$07E7.
; This code and Str_FileName must survive until the BASIC input loop owns LOAD.
!if * < $0801 { !error "BASIC boot handoff must be above KERNAL workspace" }

BootBasic       jsr SelectControl1
                jsr GetFile
                lda res
                bne +
                rts             ; No file: return through the normal event dispatcher.
+               jsr GetCurDeviceNo
                sta BootBasicDevice+1 ; CurDeviceNo is reused by BASIC initialization.
                jsr UninstallIRQ
                jsr Krill_Uninstall
                sei
                cld
                ldx #$fb
                txs             ; Discard GUI/event callers; use BASIC's normal stack.
                lda #$37
                sta $01
                lda #$80
                sta $de00       ; RAM test must see normal RAM instead of cartridge ROM.
                ; Use public entries for the installed KERNAL, including JiffyDOS.
                jsr $ff84       ; IOINIT: SID, CIAs and serial bus.
                jsr $ff87       ; RAMTAS: KERNAL workspace and normal memory limits.
                jsr RESTOR      ; Restore all KERNAL vectors, including CHROUT/LOAD.
                jsr $ff81       ; CINT: normal VIC bank, screen editor and IRQ timer.
                jsr $e453       ; Restore BASIC vectors, including the direct-mode loop.
                jsr $e3bf       ; BASIC RAM, CHRGET and memory pointers.
                jsr $a644       ; NEW: empty program and reset BASIC variable state.

                ldx #0
-               lda BootBasicPrefix,x
                beq +
                jsr CHROUT
                inx
                bne -
+               ldx #0
-               lda Str_FileName,x
                beq +
                jsr CHROUT
                inx
                bne -
+               lda #$22
                jsr CHROUT
                lda #$2c
                jsr CHROUT
BootBasicDevice ldx #0
                lda #0
                jsr $bdcd       ; BASIC prints the selected device as an unsigned integer.
                ldx #0
-               lda BootBasicSuffix,x
                beq +
                jsr CHROUT
                inx
                bne -
+               lda #13
                sta $0277       ; Only RETURN: the screen editor reads the printed command.
                lda #1
                sta $c6
                lda #$80
                sta $9d         ; Normal direct-mode KERNAL messages.
                ldx #$fb
                txs
                cli
                jmp ($0302)     ; Standard BASIC input loop; no GUI return, RUN or load jump.

BootBasicPrefix !pet "load",$22,0
BootBasicSuffix !pet ",1",19,0    ; HOME leaves the cursor on the LOAD command.
                !source "SD2IEC.asm"
                !source "PaintFunctions.asm"
                !source "ControlFunctions.asm"
                !source "StringRoutines.asm"
                !source "WindowManagement.asm"
                !source "UltimateLib.asm"
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
}
!ifdef MAC{
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

; First unused address after all engine code and the shared CRT loader.
EngineCodeEnd = *
!if EngineCodeEnd > $5f00 { !error "Engine overlaps API jump table" }

*=$5f00
; !unused_label_warn_off
GUI_CreateWindow                jmp CreateWindow
GUI_CreateWindowEx              jmp CreateWindowEx
GUI_KillCurWindow               jmp KillCurWindow
GUI_StdWndProc                  jmp StdWndProc
GUI_IsInCurControl              jmp IsInCurControl
GUI_Repaint                     jmp RepaintGUI
GUI_GetDesign                   jmp GetDesign
GUI_UpdateControl               jmp UpdateControl
GUI_SelectCtrl_AddBits          jmp SelCtrl_AddBits
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
GUI_CurWndToScreen
GUI_RepaintCurWindow            jmp PaintCurWndViaBufToScreen
GUI_FindWndByType               jmp IsWndTypePresent
GUI_SelectTopWindow             jmp SelectTopWindow
GUI_AddControl                  jmp AddControl
GUI_SetFrameHandler             jmp App_SetFrameHandler
GUI_StartFrameHandling          jmp App_StartFrameHandler
GUI_StopFrameHandling           jmp App_StopFrameHandler
GUI_MapOutIO                    jmp MapOutIO
GUI_MapInIO                     jmp MapInIO
GUI_GetScreenMem                jmp App_GetScreenMem
GUI_PaintBox                    jmp App_PaintBox
GUI_ShowYesNoDlg                jmp ShowYesNoDlg
GUI_ShowErrorMsg                jmp ShowErrorMsg
GUI_SetModalAddress             jmp App_SetModalAddress
GUI_GetDialogResult             jmp App_GetDialogResult
GUI_ShowWndAsDlg                jmp App_ShowWndAsDlg
GUI_KillDialog                  jmp KillDialog
GUI_GetLBtnFirePressed          jmp App_GetLBtnPressed
GUI_CurWndRowToScreen           jmp CurWndRowToScreen
GUI_GetTopWindow                jmp App_GetTopWindow
GUI_GetWndInfo                  jmp App_GetWndInfo
GUI_GetPaintBuf                 jmp App_GetPaintBuf
GUI_GetPressedKeys              jmp App_GetPressedKeys
GUI_BeginDiskIO                 jmp App_BeginDiskIO
GUI_EndDiskIO                   jmp App_EndDiskIO
GUI_GetAppDevice                jmp App_GetAppDevice
GUI_SelectWindow                jmp SelectWindow
; !unused_label_warn_on

; ATTENTION: PATH_A_EX is at $6000

; Appendix
; The following is copied to $d000 at GUI64 startup
!ifdef WIN{
GraphicsData    !bin "chars52_win.bin"
SpriteData      !source "Sprites.asm"
}
!ifdef MAC {
GraphicsData    !bin "chars14_mac.bin"
SpriteData      !source "Sprites.asm"
}

; Defines 4 hourglass states (2 chars per state)
HourglassChars  !byte $ff,$ff,$8f,$ff,$7e,$7e,$3c,$18
                !byte $18,$24,$42,$42,$81,$81,$ff,$ff
                !byte $ff,$ff,$81,$87,$7e,$7e,$3c,$18
                !byte $18,$24,$42,$42,$81,$bd,$ff,$ff
                !byte $ff,$ff,$81,$81,$42,$7e,$3c,$18
                !byte $18,$24,$42,$42,$bd,$ff,$ff,$ff
                !byte $ff,$ff,$81,$81,$42,$42,$24,$18
                !byte $18,$24,$42,$7e,$ff,$ff,$ff,$ff

!ifdef WIN{
TaskCharsData   !bin "TaskbarChars6_win.bin"
}

; Temporary assembler output appended after graphics, extracted by the CRT builder.
; These bytes are placed in banks 5/6 and excluded from the boot-time RAM image.
; Bank 6 also includes the shared guard and startup-only ROM functions.
Krill_ROMImage = *
Krill_ROMStart = KRILL_BANK_END
!pseudopc Krill_ROMStart {
                !source "KrillRuntime.asm"
Krill_ROMEnd = *
!if Krill_ROMEnd > $a000 { !error "Krill control exceeds ROM bank 5" }
}
Krill_ROMSize = * - Krill_ROMImage
Krill_GameROMImage = *
!pseudopc $8000 {
                !source "KrillGames.asm"
Krill_GameControlEnd = *
!if Krill_GameControlEnd > KRILL_MDGUARDBODY { !error "Game control overlaps shared ROM guard" }
                !fill KRILL_MDGUARDBODY - *, $ff
                !bin "build/krill/guard.bin"
Startup_ROMStart = *
                !source "StartupROM.asm"
Startup_ROMEnd = *
                !source "DriveDetectROM.asm"
                !source "KrillViewer.asm"
                !source "KrillCopy.asm"
Krill_GameROMEnd = *
!if Krill_GameROMEnd > $a000 { !error "Game/startup code exceeds ROM bank 6" }
}
Krill_GameROMSize = * - Krill_GameROMImage
