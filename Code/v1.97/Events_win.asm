;=============================================== OnTooltip
OnTooltip       lda ProgramMode
                cmp #PM_MENU
                beq ++
                jsr IsInTaskbar
                beq +
                ; In task bar
                jsr IsInTaskBtns
                beq ++
                jsr GetTaskBtnIndex
                ldx res
                bmi ++
                lda TaskBtnHandles,x
                sta Param
                jsr GetTaskBtnPos
                pha
                jsr GetWindowAddr
                jsr IsDriveWindow
                sta Param
                ldy #8
                lda ($fb),y
                sta $fd
                iny
                lda ($fb),y
                sta $fe
                pla
                tay
                ldx #19
                jsr ShowTooltip
+               ; Not in task bar
                jsr IsInCurWnd
                bne ++
                jsr WindowFromPos
                lda res
                bpl ++
                
                jsr IsInAnyIcon
                beq ++
                ldx res
                cpx #2
                bcs +
                ; In drive icon A or B
                lda PathsLo,x
                sta $fd
                lda PathsHi,x
                sta $fe
                jmp DoTooltip
+               ; Neither in drive icon A nor B
                rts
DoTooltip       jsr GetMouseInfo
                ldy MouseInfo
                iny
                ldx MouseInfo+1
                inx
                lda #0
                sta Param
                jsr ShowTooltip
++              rts

PathsLo         !byte <PATH_A_EX,<PATH_B_EX
PathsHi         !byte >PATH_A_EX,>PATH_B_EX

;=============================================== OnKeyPress
OnKeyPress      ; Check if CMD + joystick button is pressed
                lda actkey
                cmp #$fa
                bne +
                jsr JoyDecoder
                bcs +
                jsr OnRBtnRelease
+               lda VisibleWindows
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
+               ; Dbl clicked in title bar
                jsr IsInMinMaxClose
                bne +
                lda WindowBits
                and #BIT_WND_CANMAXIMIZE
                beq +
                jsr MaximizeCurWnd
                jmp RepaintAll
++              ; Not in current window
                jsr WindowFromPos
                lda res
                cmp #$ff
                beq +
                ; in another wnd
                rts
+               ; Not in a window
                jsr IsInTaskbar
                beq ++
                ; In taskbar
                lda MouseInfo
                cmp #34
                bcs +
                rts
+               ; In clock
                jsr ShowClockDialog
                rts
++              ; On desktop
                jsr IsInAnyIcon
                beq ++
                ; In some icon
                ldx res
                cpx #2
                bcs +
                ; In drive icon A or B
                inx; X = WT_DRIVE_A or WT_DRIVE_B
                stx Param
                jsr IsWndTypePresent
                bne driveWndExists
                ldx res
                inx
                txa
                jmp createDriveWnd
+               ; Neither in icon A nor B
                bne ++
                ; In Ultimate icon
                lda #WT_ULTIMATE
                sta Param
                jsr IsWndTypePresent
                bne driveWndExists
                jsr CreateUltWnd
                jsr RepaintAll
                jsr PaintTaskbar
++              rts
createDriveWnd  jsr CreateDriveWnd
                lda res
                beq ++
                jsr ShowDirectory
                lda error_code
                bne ++
                lda ProgramMode
                bne ++
                ; Adjust height
                lda num_files
                clc
                adc #4
                cmp #DRVWND_HEIGHT
                bcc +
                lda #DRVWND_HEIGHT
                jmp set_wh
+               cmp #7
                bcs set_wh
                lda #7
set_wh          sta WindowHeight
                ldx CurrentWindow
                sta WndDefHeight,x
                jsr UpdateWindow
                jsr RepaintAll
                jmp ++
driveWndExists  stx Param
                jsr SelectTopWindow
                jsr RestoreCurWnd
                jsr RepaintAll
                jmp PaintTaskbar
++              rts

;============================================= OnButtonPress
; Right Button------------------------------------------------
OnRBtnPress     jmp RemoveTooltip

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
                jsr IsInStartBtn
                beq +
                ; In Start button
                lda #1
                sta StartBtnPushed
                lda #PM_MENU
                sta ProgramMode
                jmp PaintCbmMenu
+               ; Not in Start button
                jsr IsInTaskbar
                beq ++
                ; In task bar
                jsr IsInTaskBtns
                bne +
                ; Not in task buttons
                lda MouseInfo+3
                cmp #249
                bne +++
                lda MouseInfo+4
                beq +++
                lda MouseInfo+2
                cmp #$57
                bne +++
                jsr MinimizeAll
                jsr PaintTaskbar
                jmp RepaintAll
+               ; In task buttons
                jsr GetTaskBtnIndex
                ldx res
                bmi +++
                lda TaskBtnHandles,x
                sta Param
                cmp WndPriorityList
                bne +
                lda CurrentWindow
                bmi +
                jmp minimize
                ;
+               lda Param
                jsr ChangeActiveWnd
                jmp RestoreCurWnd
++              ; Not in task bar
                jsr IsInCurWnd
                bne ClickInCurWnd
                ; Not in cur window
                jsr WindowFromPos
                lda res
                bmi +++
                jsr IsWndVisible
                beq +++
                ldy #0
                lda ($fb),y
                jsr ChangeActiveWnd
                jmp ClickInNormalMode
+++             ; Not in: any window nor in taskbar
                jsr IsInAnyIcon
                beq +
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
                lda #4
                sta DragObjectWidth
                lda #3
                sta DragObjectHeight
                lda MouseInfo
                sta DragAnchorX
                lda MouseInfo+1
                sta DragAnchorY
                lda #0
                sta DragType; Reposition
                lda #1
                sta MayDrag
                sta DragObjectType; DOT_ICON
+               rts
ClickInCurWnd   ; In cur window
                jsr IsInTitleBar
                beq ++++
                ; In title bar
                jsr IsInMinMaxClose
                beq +++
                ; In min/max/close symbol
                cmp #3
                bne +
minimize        ; In minimize symbol
                jsr MinimizeCurWnd
                jsr RepaintAll
                jmp PaintTaskbar
                ;
+               cmp #2
                bne ++
                ; In maximize symbol
                lda WindowBits
                and #BIT_WND_ISMAXIMIZED
                beq +
                jsr CurWnd_SetDefSize
                jmp rep
+               jsr MaximizeCurWnd
rep             jmp RepaintAll
++              ; In close symbol
                jsr KillCurWindow
                jsr RepaintAll
                jsr PaintTaskbar
                lda #PM_NORMAL
                sta ProgramMode
                rts
+++             ; Not in min/max/close
                lda WindowPosX
                sta DragOldPosX
                lda WindowPosY
                sta DragOldPosY
                lda WindowWidth
                sta DragObjectWidth
                lda WindowHeight
                sta DragObjectHeight
                lda MouseInfo
                sta DragAnchorX
                lda MouseInfo+1
                sta DragAnchorY
                lda #0
                sta DragType
                sta DragObjectType; DOT_WINDOW
                lda #1
                sta MayDrag
                rts
++++            ; Not in title bar
                lda CurrentCursor
                cmp #CUR_DEFAULT
                beq +
                cmp #CUR_CARRET
                beq +
                lda MouseInfo
                sta DragAnchorX
                lda MouseInfo+1
                sta DragAnchorY
                lda #1
                sta DragType
                sta MayDrag
                rts
+               ; Not in title bar, no resize
                lda #EC_LBTNPRESS
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)
                ; Not on desktop
ChangeActiveWnd ; Activates window with handle in Param
                sta Param
                jsr DeactivateWnd
                jsr SelectTopWindow
                jsr RestoreCurWnd
                jsr PaintCurWindow
                jsr WindowToScreen
                jmp PaintTaskbar

ClickInMenuMode lda StartBtnPushed
                bne CloseStartMenu
                ; Start button not pushed
                lda #EC_LBTNPRESS
                sta wndParam
                lda #PM_MENU
                sta wndParam+1
                jmp (WindowProc)
CloseStartMenu  lda #0
                sta StartBtnPushed
                sta MayHighlight
                sta ProgramMode
                lda #$ff
                sta OldMenuItem
                jsr PaintCbmMenu
                jsr RepaintAll
                jsr IsInStartMenu
                beq +
                ; Clicked in StartMenu
                lda MenuItem
                bne ++
                ; Clicked on Settings
                lda #WT_SETTINGS
                sta Param
                jsr IsWndTypePresent
                bne +
                jsr DeactivateWnd
                jsr CreateSettingsWindow
                jsr PaintCurWindow
                jsr WindowToScreen
                jsr PaintTaskbar
+               rts
++              cmp #2
                beq ClickedOnBASIC
                ; Clicked on About
                +JMPShowMessage <Str_Mess_GUI64, >Str_Mess_GUI64
ClickedOnBASIC  ; Clicked on BASIC
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
                jsr IsInAnyIcon
                beq -
                ldx res
                stx dummy+1; icon clicked
                cpx #2
                bcs -
                stx CurDeviceInd
                lda DeviceNumbers,x
                sta CurDeviceNo
                jsr CreateDevNoDlg; res used
                ; Position of dev no dialog
                lda dummy+1
                asl
                tay
                iny
                ldx CSTM_Icons,y
                inx
                txa
                clc
                adc #8; dialog height
                cmp #23
                bcc +
                ldx #14
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
                sec
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
                lda DragObjectType
                beq +
                jsr DoesIconOverlap
                beq ++
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
                ; Mouse button is pressed
+               lda ControlPressed
                beq +
Jmp_StdWnd      lda #EC_MOUSEMOVE
                sta wndParam
                lda #PM_NORMAL
                sta wndParam+1
                jmp (WindowProc)
+               lda MayDrag
                beq +
                lda #1
                sta IsDragging
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
                cpx MouseInfo+1
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
                lda VisibleWindows
                beq +
                jmp Jmp_StdWnd
+               rts

MovInMenuMode   lda StartBtnPushed
                bne +
                ; Is in menu mode of cur wnd
                lda #EC_MOUSEMOVE
                sta wndParam
                lda #PM_MENU
                sta wndParam+1
                jmp (WindowProc)
+               ; Start Button pushed
                jsr IsInStartMenu
                bne +
                lda #0
                sta MayHighlight
                lda #$ff
                sta OldMenuItem
                jsr PaintCbmMenu
                rts
+               ; Move in start menu
                jsr GetYCoordCmdMenu
                sta dummy
                ;
                lda MouseInfo+3
                sec
                sbc dummy
                lsr
                lsr
                lsr
                lsr
                sta MenuItem
                cmp #CbmMenuItems
                bcc +
                lda OldMenuItem
                sta MenuItem
-               rts
                ;
+               cmp OldMenuItem
                beq -
                ;
                sta OldMenuItem
                ; Paint CBM menu
                jsr PaintCbmMenu
                ; Find pos in color mem
                lda MenuItem
                asl
                clc
                adc #23
                sec
                sbc #CbmMenuHeight
                tax
                lda ScrTabLo,x
                sta $02
                lda ClrTabHi,x
                sta $03
                lda #1
                jsr AddTo02
                lda #0
                sta MayHighlight
                ldy #7
-               lda CSTM_ActiveClr
                sta ($02),y
                dey
                bpl -
                lda #1
                sta MayHighlight
                rts