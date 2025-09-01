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
                lda ProgramMode
                bne ++
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
                iny
                ldx MouseInfo+1
                inx
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
                jsr IsInDriveIcon_A
                beq ++
                ; In drive A
                lda #WT_DRIVE_A
                sta Param
                jsr IsWndTypePresent
                bne driveWndExists
                jsr DeactivateWnd
                lda #WT_DRIVE_A
                jmp createDriveWnd
++              ; Not in drive A
                jsr IsInDriveIcon_B
                bne +
                rts
+               ; In drive B
                lda #WT_DRIVE_B
                sta Param
                jsr IsWndTypePresent
                bne driveWndExists
                jsr DeactivateWnd
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
driveWndExists  ldy #WNDSTRUCT_HANDLE
                lda ($fb),y
                sta Param
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
                jsr GenerateReservedForSM
                jsr InvertReserved_DT
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
+++             rts
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
+++             ; Not in min/max/close symbols
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
                lda MouseInfo+1
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
                bne ClickedOnReset
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
ClickedOnReset  ; Clicked on Reset
                lda #<Str_Dlg_Reset
                sta $fd
                lda #>Str_Dlg_Reset
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
                lda MouseInfo+1
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
                ; Mouse button is pressed
+               lda ControlPressed
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
+               jsr GetYCoordCmdMenu
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
                ; Result should be 0, 1, or 2
                cmp #CbmMenuItems
                bcc +
                lda OldMenuItem
                sta MenuItem
                rts
                ;
+               cmp OldMenuItem
                bne +
                rts
                ;
+               sta OldMenuItem
                ; Paint CBM menu and the chars in DT_Reserved
                jsr PaintCbmMenu
                lda #22
                sec
                sbc #CbmMenuHeight
                sta dummy
                inc dummy
                lda MenuItem
                asl
                clc
                adc dummy
                tax
                lda ScrTabLo,x
                sta $fb
                lda ScrTabHi,x
                sta $fc
                lda #1
                jsr AddToFB
                lda $fb
                sta $02
                lda $fc
                sec
                sbc #>SCRMEM_MINUS_CLRMEM
                sta $03
                ;
                lda #0
                sta MayHighlight
                lda MenuItem
                asl
                asl
                asl
                clc
                adc #DT_Reserved_Char
                tax
                ldy #0
-               txa
                sta ($fb),y
                lda #1
                sta ($02),y
                inx
                iny
                cpy #8
                bcc -
                lda #1
                sta MayHighlight
                rts