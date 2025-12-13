;=============================================== OnTooltip
OnTooltip       lda ProgramMode
                cmp #PM_MENU
                beq ++
                jsr IsInCurWnd
                bne ++
                lda MouseInfo+1
                beq ++
                jsr WindowFromPos
                lda res
                bpl ++
                jsr IsInDriveIcon_A
                beq +
                ; In drive A
                lda #<PATH_A_EX;Str_Tooltip_DrvA
                sta $fd
                lda #>PATH_A_EX;Str_Tooltip_DrvA
                sta $fe
                jmp DoTooltip
+               ; Not in drive A
                jsr IsInDriveIcon_B
                beq ++
                lda #<PATH_B_EX;Str_Tooltip_DrvB
                sta $fd
                lda #>PATH_B_EX;Str_Tooltip_DrvB
                sta $fe
DoTooltip       jsr GetMouseInfo
                ldy MouseInfo
                ldx MouseInfo+5
                lda #0
                sta Param
                jsr ShowTooltip
++              rts

;=============================================== OnKeyPress
OnKeyPress      ; Check if CMD + joystick button is pressed
                lda actkey
                cmp #$fa
                bne +
                jsr JoyDecoder
                bcs +
                jmp OnRBtnRelease
+               lda AllocedWindows
                beq back_to_ml
                lda #EC_KEYPRESS
                sta wndParam
                jmp JustPassOn

OnLongLBtnPress
OnScrollWheel   sta wndParam; EC_LLBTNPRESS / EC_SCROLLUP/DOWN
                lda ProgramMode
                cmp #PM_MENU
                beq back_to_ml
                jsr IsInCurWnd
                beq back_to_ml
JustPassOn      lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)
back_to_ml      rts

;============================================= OnDblClick
OnDblClick      jsr OnLBtnPress
                lda ProgramMode
                beq +
                rts
+               ; Only processed in curr wnd and desktop
                jsr GetMouseInfo
                jsr IsInCurWnd
                beq ++
                ; In current window
                jsr IsInTitleBar
                bne +
                ; Not in title bar
                lda #EC_DBLCLICK
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)
+               ; In title bar
                jsr IsInCloseSymbol
                bne +++
                lda WindowBits
                and #BIT_WND_RESIZABLE
                beq +++
                jsr MaximizeCurWnd
                jmp RepaintAll
++              ; Not in current window
                ;jsr IsInClock
                ;beq +
                ;; In clock
                ;jmp ShowClockDialog
                ;;
;+              
                jsr WindowFromPos
                lda res
                cmp #$ff
                beq +
                ; in another wnd
+++             rts
+               ; Not in a window
                jsr IsInDriveIcon_A
                beq ++
                ; In drive icon A
                lda #WT_DRIVE_A
                sta Param
                jsr IsWndTypePresent
                bne driveWndExists
                ;jsr DeactivateWnd
                lda #WT_DRIVE_A
                jmp createDriveWnd
++              ; Not in drive icon A
                jsr IsInDriveIcon_B
                bne +
                rts
+               ; In drive icon B
                lda #WT_DRIVE_B
                sta Param
                jsr IsWndTypePresent
                bne driveWndExists
                ;jsr DeactivateWnd
                lda #WT_DRIVE_B
createDriveWnd  jsr CreateDriveWnd
                lda res
                beq ++
                jsr ShowDirectory
                lda error_code
                bne ++
                lda ProgramMode
                bne ++
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
                jsr RepaintAll
                jmp ++
driveWndExists  ldy #WNDSTRUCT_HANDLE
                lda ($fb),y
                sta Param
                jsr SelectTopWindow
                jsr RepaintAll
++              rts

DrvWndMaxHeight !byte 0

;============================================= OnButtonPress
; Right Button------------------------------------------------
OnRBtnPress     jsr RemoveTooltip
                lda ProgramMode
                bne +++
                jsr IsInCurWnd
                bne +
                ; Not in cur wnd
                jsr ActivateWndFromPos
                lda res
                bmi +++
+               ; In cur wnd
                jsr IsInTitleBar
                beq +++
                ; In title bar
                lda WindowType
                cmp #WT_DLG
                bcs +++
                ; It's not a dialog
                lda WindowBits
                and #BIT_WND_IS_ICONIZED
                beq +
                ; De-Iconize
                jsr DeiconizeCurWnd
                jmp ++
+               ; Iconize
                jsr IconizeCurWnd
++              jmp RepaintAll
+++             rts

; Left Button-------------------------------------------------
OnLBtnPress     jsr RemoveTooltip
                jsr GetMouseInfo
                lda ProgramMode
                beq ClickInNormalMode
                bpl ClickInMM
                jmp ClickInDM

ClickInMM       jmp ClickInMenuMode
ClickInDM       jmp ClickInDlgMode

ClickInNormalMode
                jsr IsInCurWnd
                bne ClickInCurWnd
                jsr IsInSystemMenu
                bne ClickInSysMenu
                jsr IsInMenubar
                bne ClickInMenubar
                jsr IsInClock
                bne ClickInClock
                ; Not in cur window nor in menu bar
                jsr ActivateWndFromPos
                lda res
                bmi +++
                jmp ClickInNormalMode
+++             rts
ClickInClock    jmp ShowClockDialog
ClickInMenubar  ; In menu bar
                lda AllocedWindows
                beq +++
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +++
                +SelectControl 0
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
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jsr DriveWndProc
                ;
++              +SelectControl 0
                jsr Menubar_ShowMenu
                lda #PM_MENU
                sta ProgramMode
+++             rts
ClickInSysMenu  ; In system menu item
                lda #1
                sta bSystemMenuOn
                lda CSTM_MenuSelClr
                sta CLRMEM
                sta CLRMEM+1
                sta CLRMEM+2
                lda #PM_MENU
                sta ProgramMode
                jsr SelectSysMenu
                jsr PaintSystemMenu
                rts
ClickInCurWnd   ; In cur window
                jsr IsInTitleBar
                beq ++++
                ; In title bar
                jsr IsInCloseSymbol
                beq +++
                ; In close symbol
                jsr KillCurWindow
                lda #PM_NORMAL
                sta ProgramMode
                jmp RepaintAll
+++             ; Not in close symbol
                lda WindowPosX
                sta OldPosX
                lda MouseInfo
                sta DragWndAnchorX
                lda #0
                sta DragType
                lda #1
                sta MayDragWnd
                rts
++++            ; Not in title bar
                lda CurrentCursor
                cmp #CUR_DEFAULT
                beq +
                cmp #CUR_CARRET
                beq +
                lda MouseInfo
                sta DragWndAnchorX
                lda MouseInfo+5
                sta DragWndAnchorY
                lda #1
                sta DragType
                sta MayDragWnd
                rts
+               ; Not in title bar, no resize
                lda #EC_LBTNPRESS
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)

                ; Not on desktop
; Activates window which is clicked on (NOT! curr wnd)
ActivateWndFromPos
                jsr WindowFromPos
                lda res
                bmi +
                ldy #0
                lda ($fb),y
                sta Param
                jsr SelectTopWindow
                jsr MenubarToScreen
                jmp RepaintAll
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
                beq +
                ; Clicked in system menu
                jsr ClickedOnSysMen
+               lda #CL_WHITE
                sta CLRMEM
                sta CLRMEM+1
                sta CLRMEM+2
                rts
++              ; System menu not shown
                lda #EC_LBTNPRESS
                sta wndParam
                lda #PM_MENU
                sta wndParam+1
                jmp (WindowProc)

ClickedOnSysMen jsr GetMenuItem
                lda res
                bne ++
                ; Settings
                lda #WT_SETTINGS
                sta Param
                jsr IsWndTypePresent
                bne +++
                jsr CreateSettingsWindow
                jmp RepaintAll
++              cmp #2
                beq ++++
                ; About
                +ShowMessage <Str_Mess_GUI64, >Str_Mess_GUI64
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

ClickInDlgMode  jsr IsInCurWnd
                beq +
                jmp ClickInCurWnd
+               rts

;============================================= OnButtonRelease
; Right Button------------------------------------------------
OnRBtnRelease   jsr GetMouseInfo
                lda ProgramMode
                beq +
-               rts
+               jsr IsInCurWnd
                bne -
                ; Not in cur window
                jsr WindowFromPos
                lda res
                bpl -
                ; Not in any window
                jsr IsInDriveIcon_A
                beq +
                ; In drive icon A
                ldx #0
                jmp ++
+               jsr IsInDriveIcon_B
                beq -
                ; In drive icon B
                ldx #1
++              stx CurDeviceInd
                lda DeviceNumbers,x
                sta CurDeviceNo
                jsr CreateDevNoDlg
                lda MouseInfo+5
                sta WindowPosY
                jsr UpdateWindow
                jsr SetDevNoLabels
                jmp ShowTheDialog

; Left Button-------------------------------------------------
OnLBtnRelease   lda MayDragWnd
                beq ++
                lda IsWndDragging
                beq +
                lda #0
                sta IsWndDragging
                sta MayDragWnd
                lda WindowFocCtrl
                jsr SelectControl
                rts
+               lda #0
                sta MayDragWnd
                rts
++              ; No drag
                lda ControlPressed
                beq +
                lda #EC_LBTNRELEASE
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)
+               rts

;============================================= OnMouseMove
MovInMenuModeP  jmp MovInMenuMode

OnMouseMove     jsr RemoveTooltip
                jsr GetMouseInfo
                lda ProgramMode
                beq MovInNrmDlgMode
                bpl MovInMenuModeP

MovInNrmDlgMode ; Moved in normal AND Dlg mode
                lda bFirePressed
                bne +
                lda IsLBtnPressed
                beq ++
+               ; Mouse button is pressed
                lda ControlPressed
                beq +
Jmp_StdWnd      lda #EC_MOUSEMOVE
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)
+               lda MayDragWnd
                beq +
                lda #1
                sta IsWndDragging
                jsr Drag
+               rts
++              ; Mouse button NOT pressed
                jsr IsInCurWnd
                beq ++
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
                +SetCursor CUR_RESIZENS
                rts
+               txa
                and #BIT_WND_FIXEDHEIGHT
                beq +
                +SetCursor CUR_RESIZEWE
                rts
+               +SetCursor CUR_RESIZENWSE
                rts
++              ; Not in lower right corner
                +SetCursor CUR_DEFAULT
                lda AllocedWindows
                beq +
                jmp Jmp_StdWnd
+               rts

MovInMenuMode   lda bSystemMenuOn
                beq +++
                ; System menu is shown
                jsr IsInCurMenu
                beq ++
                ; In system menu
                jsr GetMenuItem
                lda res
                sta Param
                jsr SelectSysMenuItem
                rts
++              ; Not in system menu
                lda CurMenuItem
                bmi +
                lda #$ff
                sta CurMenuItem
                jsr PaintSystemMenu
+               rts
+++             ; Is in menu mode of cur wnd
                lda AllocedWindows
                bne +
                rts
+               lda #EC_MOUSEMOVE
                sta wndParam
                lda #PM_MENU
                sta wndParam+1
                jmp (WindowProc)

; Expects Param filled with item
SelectSysMenuItem
                lda Param
                bmi ++
                cmp CurMenuItem
                beq ++
                sta CurMenuItem
                jsr PaintSystemMenu
                ;
                ldx Param
                inx
                jsr SelectMenuLine
                ldx Param
                bne +
                jsr SelectMenuLine
+               inx
                inx
                inx
                cpx CurMenuHeight
                bne ++
                dex
                jsr SelectMenuLine
++              rts