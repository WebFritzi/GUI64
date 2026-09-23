;=============================================== OnTooltip
OnTooltip       lda ProgramMode
                cmp #PM_MENU
                beq ++
                jsr IsInCurWnd
                bcc +
                ; In current window
                lda WindowType
                cmp #WT_ULTIMATE
                bne ++
                ; In Ultimate window
                jsr IsInCurControl
                bcc ++
                jmp UltShowTooltip
+               ; Not in current window
                lda MouseInfo+1
                beq ++
                jsr WindowFromPos
                lda res
                bpl ++
                ; Not in any window
                jsr IsInAnyIcon
                bcc ++
                ; In some icon
                ldx res
                cpx #3
                bcs ++
                ;lda PathsLo,x
                ;sta $fd
                ;lda PathsHi,x
                ;sta $fe
                lda #0
                sta $fd
                txa
                ;clc
                adc #>PATH_A_EX
                sta $fe
                txa
                lsr
                sta Param0
                jsr GetMouseInfo
                ldy MouseInfo
                iny
                ldx MouseInfo+5
                inx
                jsr ShowTooltip
-
++              rts

;PathsLo         !byte <PATH_A_EX,<PATH_B_EX,<PATH_U_EX
;PathsHi         !byte >PATH_A_EX,>PATH_B_EX,>PATH_U_EX

;=============================================== OnKeyPress
OnKeyPress      ; Check if CMD + joystick button is pressed
                lda actkey
                cmp #$fa
                bne +
                jsr JoyDecoder
                bcs +
                jmp OnRBtnRelease
+               lda AllocedWindows
                beq -
                ldx #EC_KEYPRESS
                jmp PassToWndProcNM

OnScrollWheel   lda ProgramMode
                cmp #PM_MENU
                beq +
                jsr IsInCurWnd
                bcc +
                inx
PassToWndProcNM ldy #PM_NORMAL
PassToWndProc   stx wndParam0
                sty wndParam1
                jmp (WindowProc)
+               rts

;============================================= OnDblClick
OnDblClick      jsr OnLBtnPress
                lda ProgramMode
                beq +
                rts
+               ; Only processed in curr wnd and desktop
                jsr GetMouseInfo
                jsr IsInCurWnd
                bcc ++
                ; In current window
                jsr IsInTitleBar
                bcs +
                ; Not in title bar
                ldx #EC_DBLCLICK
                jmp PassToWndProcNM
+               ; In title bar
                jsr IsInCloseSymbol
                bcs +++
                lda WindowBits
                and #BIT_WND_RESIZABLE
                beq +++
                jsr MaximizeCurWnd
                jmp RepaintAll
++              ; Not in current window
                jsr WindowFromPos
                lda res
                cmp #$ff
                beq +
                ; in another wnd
+++             rts
+               ; Not in a window
                jsr IsInAnyIcon
                bcc ++
                ; In some icon
                ldx res
                cpx #2
                bcs +
                ; In drive icon A or B
                inx; X = WT_DRIVE_A or WT_DRIVE_B
                stx Param0
                jsr IsWndTypePresent
                bcs driveWndExists
                ldx res
                inx
                txa
                jmp createDriveWnd
+               ; Neither in drive icon A nor B
                bne ++
                ; In Ultimate icon
                lda #WT_ULTIMATE
                sta Param0
                jsr IsWndTypePresent
                bcs driveWndExists
                jsr CreateUltWnd
                jsr UltShowDir
-
++              rts
createDriveWnd  jsr CreateDriveWnd
                lda res
                beq -
                jsr ShowDirectory
                lda error_code
                bne -
                lda ProgramMode
                bne -
                ; Adjust height
                lda #24
                sec
                sbc WindowPosY
                sta DrvWndMaxHeight
                lda num_files
                clc
                adc #3
                cmp DrvWndMaxHeight
                bcc +
                lda DrvWndMaxHeight
                jmp set_wh
+               cmp #7
                bcs set_wh
                lda #7
set_wh          sta WindowHeight
                jsr UpdateWindow
                jmp RepaintAll
driveWndExists  stx Param0
                jsr SelectTopWindow
                jmp RepaintAll

DrvWndMaxHeight !byte 0

;============================================= OnButtonPress
; Right Button------------------------------------------------
OnRBtnPress     jsr RemoveTooltip
                lda ProgramMode
                bne +++
                jsr IsInCurWnd
                bcs +
                ; Not in cur wnd
                jsr ActivateWndFromPos
                lda res
                bmi +++
+               ; In cur wnd
                jsr IsInTitleBar
                bcs +
                ; Not in title bar
                ldx #EC_RBTNPRESS
                jmp PassToWndProcNM
+               ; In title bar
                lda WindowType
                cmp #WT_DLG
                bcs +++
                ; It's not a dialog
                lda WindowBits
                and #BIT_WND_IS_ICONIZED
                beq +
                ; De-Iconize
                jsr DeiconizeCurWnd
                jmp RepaintAll
+               ; Iconize
                jsr IconizeCurWnd
                jmp RepaintAll
+++             rts

; Left Button-------------------------------------------------
OnLBtnPress     jsr RemoveTooltip
                jsr GetMouseInfo
                lda ProgramMode
                beq ClickInNormalMode
                bpl ClickInMM
                jmp ClickInDlgMode

ClickInMM       jmp ClickInMenuMode

ClickInNormalMode
                jsr IsInCurWnd
                bcc Clk_NotCurWnd
ClickInCurWnd   ; In cur window
                jsr IsInTitleBar
                bcc ++++
                ; In title bar
                jsr IsInCloseSymbol
                bcc +++
                ; In close symbol
                jmp KillDialog
+++             ; Not in close symbol -> Prepare drag
                lda WindowPosX
                sta DragOldPosX
                lda WindowPosY
                sta DragOldPosY
                ldx WindowWidth
                dex
                stx DragObjWidthMinus1
                ldx WindowHeight
                dex
                stx DragObjHeightMinus1
                jsr SetDragAnchor
                lda #0
                sta DragType
                sta DragObjType; DOT_WINDOW
                lda #1
                sta MayDrag
                rts
SetDragAnchor   lda MouseInfo
                sta DragAnchorX
                lda MouseInfo+5
                sta DragAnchorY
                rts
++++            ; Not in title bar
                lda CurrentCursor
                cmp #CUR_DEFAULT
                beq +
                cmp #CUR_CARRET
                beq +
                jsr SetDragAnchor
                lda #1
                sta DragType; resize
                sta MayDrag
                rts
+               ; Not in title bar, no resize
                ldx #EC_LBTNPRESS
                jmp PassToWndProcNM
Clk_NotCurWnd   ; Not in current window
                jsr IsInSystemMenu
                bcc Clk_NotSysMenu
                ; In system menu item
                lda #1
                sta bSystemMenuOn
                lda CSTM_MenuSelClr
                sta CLRMEM
                sta CLRMEM+1
                sta CLRMEM+2
                lda #PM_MENU
                sta ProgramMode
                jsr SelectSysMenu
                jmp PaintSysOrWndManMenu; PaintSystemMenu
Clk_NotSysMenu  jsr IsInMenubar
                bcs ClickInMenubar
                jsr IsInClock
                bcs ClickInClock
                lda MouseInfo+1
                bne +
                jmp ClickInWndManItem
+               ; Not in: cur wnd, menu bar
                jsr ActivateWndFromPos
                lda res
                bmi +++
                jmp ClickInNormalMode
+++             ; Not in: any window, menu bar
                jsr IsInAnyIcon
                bcc +
                ; In some icon -> Prepare drag icon
                lda res
                sta CurrentIcon
                asl
                tax
                lda CSTM_Icons,x
                sta DragOldPosX
                inx
                lda CSTM_Icons,x
                sta DragOldPosY
                lda #3
                sta DragObjWidthMinus1
                lda #2
                sta DragObjHeightMinus1
                jsr SetDragAnchor
                lda #0
                sta DragType; Reposition
                lda #1
                sta MayDrag
                sta DragObjType; DOT_ICON
+               rts
ClickInClock    jmp ShowClockDialog
ClickInMenubar  ; In menu bar
                lda AllocedWindows
                beq +++
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +++
                jsr SelectControl0
                jsr SelMenubarEntry
                lda res
                bmi +++
                jsr MenubarToScreen
                ; Add check mark if necessary
                lda WindowType
                cmp #WT_DRIVE_A
                beq +
                cmp #WT_DRIVE_B
                bne ++
+               lda #WM_PRESSMENUBAR
                sta wndParam0
                lda #PM_NORMAL
                sta wndParam1
                jsr DriveWndProc
                ;
++              jsr SelectControl0
                jsr Menubar_ShowMenu
                lda #PM_MENU
                sta ProgramMode
+++             rts

ClickInWndManItem
                lda AllocedWindows
                beq +
                lda #1
                sta bWndManMenuOn
                lda CSTM_MenuSelClr
                sta CLRMEM+31
                sta CLRMEM+32
                sta CLRMEM+33
                lda #PM_MENU
                sta ProgramMode
                jsr BuildWndManMenu
                jsr SelectWndManMenu
                jsr PaintSysOrWndManMenu;PaintWndManMenu
+               rts

; Activates window which is clicked on (NOT! curr wnd)
ActivateWndFromPos
                jsr WindowFromPos
                lda res
                bmi +
                ldy #0
                lda ($fb),y
                sta Param0
                jsr SelectTopWindow
                ;jsr MenubarToScreen
                ;jmp RepaintAll
                jmp RepaintGUI
+               rts

ClickInMenuMode lda bSystemMenuOn
                beq ++
                ; System menu shown
                lda #0
                sta bSystemMenuOn
                lda #PM_NORMAL
                sta ProgramMode
                jsr RepaintAll
                jsr IsInCurMenu
                bcc +
                ; Clicked in system menu
                jsr ClickedOnSysMen
+               lda #CL_WHITE
                sta CLRMEM
                sta CLRMEM+1
                sta CLRMEM+2
                rts
++              ; System menu not shown
                lda bWndManMenuOn
                beq ++
                ; Wnd man menu shown
                lda #0
                sta bWndManMenuOn
                lda #PM_NORMAL
                sta ProgramMode
                jsr RepaintAll
                jsr IsInCurMenu
                bcc +
                ; Clicked in wnd man menu
                jsr ClickedOnWndManMen
+               lda #CL_WHITE
                sta CLRMEM+31
                sta CLRMEM+32
                sta CLRMEM+33
                rts
++              ; Both menus not shown
                ldx #EC_LBTNPRESS
                ldy #PM_MENU
                jmp PassToWndProc

ClickedOnSysMen jsr GetMenuItem
                lda res
                bne ++
                ; Clicked on Settings
                lda #WT_SETTINGS
                sta Param0
                jsr IsWndTypePresent
                bcs +++
                jsr CreateSettingsWindow
                jmp RepaintAll
++              cmp #2
                beq ++++
                cmp #1
                bne +
                ; Clicked on About
                ldx #<Str_Mess_GUI64
                ldy #>Str_Mess_GUI64
                jmp ShowMessage
+               jmp U_Reboot
+++             rts
++++            ; Clicked on BASIC
                lda #<Str_Dlg_BASIC
                sta $fd
                lda #>Str_Dlg_BASIC
                sta $fe
                lda #<mod_res2
                sta ModalAddress
                lda #>mod_res2
                sta ModalAddress+1
                jmp ShowAreYouSureDlg
mod_res2        lda DialogResult
                cmp #1
                bne ++
                lda #EC_GAMEEXIT
                sta exit_code
++              rts

ClickedOnWndManMen
                jsr GetMenuItem
                ldx res
                lda FREEMEM+256,x
                sta Param0
                jsr SelectTopWindow
                jmp RepaintAll

ClickInDlgMode  jsr IsInCurWnd
                bcc +
                jmp ClickInCurWnd
+               rts

;============================================= OnButtonRelease
; Right Button------------------------------------------------
OnRBtnRelease   jsr GetMouseInfo
                lda ProgramMode
                beq +
-               rts
+               jsr IsInCurWnd
                bcc +
                ; In cur window
                ldx #EC_RBTNRELEASE
                jmp PassToWndProcNM
+               ; Not in cur window
                jsr WindowFromPos
                lda res
                bpl -
                ; Not in any window
                jsr IsInAnyIcon
                bcc -

;                bcs +
                
;                jsr APP_START
;                jmp RepaintGUI
                
;+

                ldx res
                stx ZP_60; icon clicked
                cpx #2
                bcs -
                stx CurDeviceInd
                lda CSTM_DevNumbers,x
                sta CurDeviceNo
                jsr CreateDevNoDlg; res used
                ; Position of dev no dialog
                lda ZP_60
                asl
                tay
                iny
                ldx CSTM_Icons,y
                inx
                txa
                clc
                adc #6; dialog height
                cmp #25
                bcc +
                ldx #18
+               stx WindowPosY
                dey
                ldx CSTM_Icons,y
                inx
                inx
                inx
                txa
                clc
                adc #10; dialog width
                cmp #41
                bcc +
                lda CSTM_Icons,y
                ;sec
                sbc #9
                tax
+               stx WindowPosX
                jsr UpdateWindow
                jsr SetDevNoLabels
                jmp ShowTheDialog

; Left Button-------------------------------------------------
OnLBtnRelease   lda MayDrag
                beq +++
                lda #0
                sta MayDrag
                ldx IsDragging
                beq ++
                sta IsDragging
                lda DragObjType
                beq +
                jsr DoesIconOverlap
                bcc ++
                ; Icons do overlap
                lda CurrentIcon
                asl
                tax
                lda DragOldPosX
                sta CSTM_Icons,x
                inx
                lda DragOldPosY
                sta CSTM_Icons,x
                jmp RepaintAll
+               lda WindowFocCtrl
                jsr SelectControl
++              rts
+++             ; No drag
                lda ControlPressed
                beq +
                ldx #EC_LBTNRELEASE
                jmp PassToWndProcNM
+               rts

;============================================= OnMouseMove
;MovInMenuModeP  jmp MovInMenuMode

OnMouseMove     jsr RemoveTooltip
                jsr GetMouseInfo
                lda ProgramMode
                beq MovInNrmDlgMode
                bpl MovInMenuMode;MovInMenuModeP

MovInNrmDlgMode ; Moved in normal AND Dlg mode
                lda bFirePressed
                bne +
                lda IsLBtnPressed
                beq ++
+               ; Mouse button is pressed
                lda ControlPressed
                beq +
Jmp_StdWnd      ; Control is being pressed
                ldx #EC_MOUSEMOVE
                jmp PassToWndProcNM
+               ; No control being pressed
                lda MayDrag
                beq +
                lda #1
                sta IsDragging
                jsr Drag
+               rts
++              ; Mouse button NOT pressed
                jsr IsInCurWnd
                bcc ++
                ; Moved in cur wnd
                lda WindowBits
                and #BIT_WND_RESIZABLE
                beq ++
                ; Check if in lower right corner
                lda WindowPosX
                clc
                adc WindowWidth
                tax
                dex
                cpx MouseInfo
                bne ++
                lda WindowPosY
                clc
                adc WindowHeight
                tax
                dex
                cpx MouseInfo+5
                bne ++
                ; Is in lower right corner
                ldx WindowBits
                txa
                and #BIT_WND_FIXEDWIDTH
                beq +
                ldy #CUR_RESIZENS
                jmp SetCursor
+               txa
                and #BIT_WND_FIXEDHEIGHT
                beq +
                ldy #CUR_RESIZEWE
                jmp SetCursor
+               ldy #CUR_RESIZENWSE
                jmp SetCursor
++              ; Not in lower right corner
                ldy #CUR_DEFAULT
                jsr SetCursor
                lda AllocedWindows
                bne Jmp_StdWnd
                rts

MovInMenuMode   lda bSystemMenuOn
                beq +++
                ; System menu is shown
                jsr IsInCurMenu
                bcc ++
                ; In system menu
                jsr GetMenuItem
                lda res
                sta Param0
                jmp SelectSysMenuItem
++              ; Not in system menu
                lda CurMenuItem
                bmi +
                lda #$ff
                sta CurMenuItem
                jsr PaintSysOrWndManMenu;PaintSystemMenu
+               rts
+++             lda bWndManMenuOn
                beq +++
                ; Window manager menu is shown
                jsr IsInCurMenu
                bcc ++
                ; In wnd man menu
                jsr GetMenuItem
                lda res
                sta Param0
                jmp SelectWndManMenuItem
++              ; Not in wnd man menu
                lda CurMenuItem
                bmi +
                lda #$ff
                sta CurMenuItem
                jsr PaintSysOrWndManMenu;PaintWndManMenu
+               rts
+++             ; Is in menu mode of cur wnd
                lda AllocedWindows
                bne +
-               rts
+               ldx #EC_MOUSEMOVE
                ldy #PM_MENU
                jmp PassToWndProc

; Expects Param0 filled with item
SelectSysMenuItem
                lda Param0
                bmi -
                cmp CurMenuItem
                beq -
                sta CurMenuItem
                jsr PaintSysOrWndManMenu;PaintSystemMenu
                ;
                jmp SelMenuItemTail

; Expects Param0 filled with item
SelectWndManMenuItem
                lda Param0
                bmi -
                cmp CurMenuItem
                beq -
                sta CurMenuItem
                jsr PaintSysOrWndManMenu;PaintWndManMenu
                ;
                jmp SelMenuItemTail