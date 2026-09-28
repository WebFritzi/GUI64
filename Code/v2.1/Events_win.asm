;=============================================== OnTooltip
OnTooltip       lda ProgramMode
                cmp #PM_MENU
                beq ++
                jsr IsInTaskbar
                bcc +
                ; In task bar
                jsr IsInTaskBtns
                bcc ++
                jsr GetTaskBtnIndex
                tax
                bmi ++
                lda TaskBtnHandles,x
                sta Param0
                ; Get task btn pos
                stx $fd
                lda TaskBtnWidth
                sta $fe
                jsr MultiplyFDbyFE
                txa
                clc
                adc #3
                ;
                pha
                jsr GetWindowAddr
                jsr IsDriveWindow
                sta Param0
                ldy #8
                jsr AddrInFBtoFD
                pla
                tay
                ldx #19
                jmp ShowTooltip
+               ; Not in task bar
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
                ldx MouseInfo+1
                inx
                jmp ShowTooltip
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
                jsr OnRBtnRelease
+               lda VisibleWindows
                beq +; rts in OnScrollWheel
                ldx #EC_KEYPRESS
                jmp PassToWndProcNM

OnScrollWheel   lda ProgramMode
                cmp #PM_MENU
                beq +
                jsr IsInCurWnd
                bcc +
                inx; exit_code = EC_SCROLLWHEELUP/DOWN
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
                bcc ++
                ; In taskbar
                lda MouseInfo
                cmp #34
                bcs +
                rts
+               ; In clock
                jmp ShowClockDialog
++              ; On desktop
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
+               ; Neither in icon A nor B
                bne ++
                ; In Ultimate icon
                lda #WT_ULTIMATE
                sta Param0
                jsr IsWndTypePresent
                bcs driveWndExists
                jsr CreateUltWnd
                jsr UltShowDir
                jmp PaintTaskbar
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
                jmp RepaintAll
driveWndExists  stx Param0
                jsr SelectTopWindow
                jsr RestoreCurWnd
                jmp RepaintGUI
++              rts

;============================================= OnButtonPress
; Right Button------------------------------------------------
OnRBtnPress     jsr RemoveTooltip
                lda ProgramMode
                bne +++
                jsr IsInCurWnd
                bcc +++
                ; In cur wnd
                jsr IsInTitleBar
                bcs +++
                ; Not in title bar
                ldx #EC_RBTNPRESS
                jmp PassToWndProcNM
+++             rts

; Left Button-------------------------------------------------
OnLBtnPress     jsr RemoveTooltip
                jsr GetMouseInfo
                lda ProgramMode
                beq ClickInNormalMode
                bpl ClickInMM

                ; ClickInDlgMode
                jsr IsInCurWnd
                bcc +
                jmp ClickInCurWnd
+               rts

ClickInMM       jmp ClickInMenuMode

ClickInNormalMode
                ; Is in start btn?
                lda MouseInfo+4
                bne +
                lda MouseInfo+2
                cmp #47
                bcs +
                cmp #26
                bcc +
                lda MouseInfo+3
                cmp #248
                bcs +
                cmp #230
                bcc +
                ; In Start button
                lda #PM_MENU
                sta ProgramMode
                sta StartBtnPushed
                jmp PaintCbmMenu
+               ; Not in Start button
                jsr IsInTaskbar
                bcc ++
                ; In task bar
                jsr IsInTaskBtns
                bcs +
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
                tax
                bmi +++
                lda TaskBtnHandles,x
                sta Param0
                cmp WndPriorityList
                bne +
                lda CurrentWindow
                bpl minimize
                ;
+               lda Param0
                jsr ChangeActiveWnd
                jmp RestoreCurWnd
++              ; Not in task bar
                jsr IsInCurWnd
                bcs ClickInCurWnd
                ; Not in cur window
                jsr WindowFromPos
                lda res
                bmi +++
                ldy #WNDSTRUCT_BITS
                lda ($fb),y
                and #BIT_WND_ISMINIMIZED
                bne +++
                ;jsr IsWndVisible
                ;bcc +++
                ldy #0
                lda ($fb),y
                jsr ChangeActiveWnd
                jmp ClickInNormalMode
+++             ; Not in: any window nor in taskbar
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
SetDragAnchor   lda MouseInfo
                sta DragAnchorX
                lda MouseInfo+1
                sta DragAnchorY
                rts
ClickInCurWnd   ; In cur window
                jsr IsInTitleBar
                bcc ++++
                ; In title bar
                jsr IsInMinMaxClose
                beq +++
                ; In min/max/close symbol
                cmp #3
                bne +
minimize        ; In minimize symbol
                jsr MinimizeCurWnd
                jmp RepaintGUI
                ;
+               cmp #2
                bne ++
                ; In maximize symbol
                lda WindowBits
                and #BIT_WND_ISMAXIMIZED
                beq +
                jsr CurWnd_SetDefSize
                jmp RepaintAll
+               jsr MaximizeCurWnd
                jmp RepaintAll
++              ; In close symbol
                lda #PM_NORMAL
                sta ProgramMode
                jmp KillCurWnd_RepaintGUI
+++             ; Not in min/max/close
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
++++            ; Not in title bar
                lda CurrentCursor
                cmp #CUR_DEFAULT
                beq +
                cmp #CUR_CARRET
                beq +
                jsr SetDragAnchor
                lda #1
                sta DragType
                sta MayDrag
                rts
+               ; Not in title bar, no resize
                ldx #EC_LBTNPRESS
                jmp PassToWndProcNM
                ; Not on desktop
ChangeActiveWnd ; Activates window with handle in Param0
                sta Param0
                jsr DeactivateWnd
                jsr SelectTopWindow
                jsr RestoreCurWnd
                jsr PaintCurWndViaBufToScreen
                jmp PaintTaskbar

ClickInMenuMode lda StartBtnPushed
                bne CloseStartMenu
                ; Start button not pushed
                ldx #EC_LBTNPRESS
                ldy #PM_MENU
                jmp PassToWndProc
CloseStartMenu  lda #0
                sta StartBtnPushed
                sta MayHighlight
                sta ProgramMode
                lda #$ff
                sta OldMenuItem
                jsr PaintCbmMenu
                jsr RepaintAll
                jsr IsInStartMenu
                bcc +
                ; Clicked in StartMenu
                lda MenuItem
                bne ++
                ; Clicked on Settings
                lda #WT_SETTINGS
                sta Param0
                jsr IsWndTypePresent
                bcs +
                jsr DeactivateWnd
                jsr CreateSettingsWindow
                jsr PaintCurWndViaBufToScreen
                jmp PaintTaskbar
+               rts
++              cmp #2
                beq ClickedOnBASIC
                cmp #1
                bne +
                ; Clicked on About
                ldx #<Str_Mess_GUI64
                ldy #>Str_Mess_GUI64
                jmp ShowMessage
+               jmp U_Reboot
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
                jmp SelectControl
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
                ; Mouse button is pressed
+               lda ControlPressed
                beq +
Jmp_StdWnd      ldx #EC_MOUSEMOVE
                jmp PassToWndProcNM
+               lda MayDrag
                beq +
                lda #1
                sta IsDragging
                jmp Drag
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
                cpx MouseInfo+1
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
                lda VisibleWindows
                bne Jmp_StdWnd
                rts

MovInMenuMode   lda StartBtnPushed
                bne +
                ; Is in menu mode of cur wnd
                ldx #EC_MOUSEMOVE
                ldy #PM_MENU
                jmp PassToWndProc
+               ; Start Button pushed
                jsr IsInStartMenu
                bcs +
                lda #0
                sta MayHighlight
                lda #$ff
                sta OldMenuItem
                jmp PaintCbmMenu
+               ; Move in start menu
                jsr GetYCoordCmdMenu
                sta ZP_5F
                ;
                lda MouseInfo+3
                sec
                sbc ZP_5F
                lsr
                lsr
                lsr
                lsr
                sta MenuItem
                cmp CbmMenuItems
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
                sbc CbmMenuHeight
                tax
                lda ScrTabLo,x
                sta $02
                ;lda ClrTabHi,x
                lda ScrTabHi,x
                eor #$3c
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