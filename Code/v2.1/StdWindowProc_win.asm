; Needs wndParam0 filled with exit code
; and wndParam1 filled with ProgramMode
StdWndProc      jsr GetMousePosInWnd
                ; Checks event
;                ldx wndParam0
;                dex
;                lda WndProcEventsLo,x
;                sta go_event+1
;                lda WndProcEventsHi,x
;                sta go_event+2
;go_event        jmp $ffff
                lda wndParam0
                cmp #EC_LBTNPRESS
                beq StdWnd_LBPress
                cmp #EC_LBTNRELEASE
                beq StdWnd_LBRel
                cmp #EC_RBTNPRESS
                beq StdWnd_RBPress
                cmp #EC_SCROLLWHEELDOWN
                beq StdW_ScrolWheel
                cmp #EC_SCROLLWHEELUP
                beq StdW_ScrolWheel
                cmp #EC_KEYPRESS
                beq StdW_KeyPress
                cmp #EC_MOUSEMOVE
                beq StdW_MMoveProc;StdWnd_MMove
EmptyWndProc    rts

;WndProcEventsLo !byte <StdWnd_LBPress, <StdWnd_LBRel, <StdWnd_MMove, 0, <StdWnd_DblClk 
;                !byte <StdW_ScrolWheel, <StdW_ScrolWheel, <StdW_KeyPress
;WndProcEventsHi !byte >StdWnd_LBPress, >StdWnd_LBRel, >StdWnd_MMove, 0, >StdWnd_DblClk 
;                !byte >StdW_ScrolWheel, >StdW_ScrolWheel, >StdW_KeyPress

;StdW_LBPresProc jmp StdWnd_LBPress
;StdW_LBRelProc  jmp StdWnd_LBRel
StdW_MMoveProc  jmp StdWnd_MMove
;StdW_ScrWhlProc jmp StdW_ScrolWheel
;StdW_KeyPrsProc jmp StdW_KeyPress

;StdWnd_DblClk   rts


StdW_ScrolWheel jsr GetCtrlFromPos
                lda res
                bmi +
                jsr SelectControl
                jmp ControlsProc
+               rts
;----------------------------------------------
StdW_KeyPress   jmp ControlsProc
;----------------------------------------------
StdWnd_LBRel    ; Left button released
                lda wndParam1
                bne +
                ; Release in normal mode
                lda ControlPressed
                beq +
                ; Some control is pressed
                lda #0
                sta ControlPressed
                jmp ControlsProc
+               rts
;----------------------------------------------
StdWnd_LBPress  ; Left button pressed
                lda wndParam1
                beq Std_ClickInNM
                bpl Std_ClickInMM
                rts

StdWnd_RBPress  lda ProgramMode
                bne +
                jsr GetCtrlFromPos
                lda res
                bmi +
                lda ControlIndex
                sta WindowFocCtrl
                jsr UpdateWindow
+               rts

Std_ClickInNM   lda MousePosInWndY
                bpl no_menu
                ; Clicked in menu bar
                jsr SelectControl0
                jsr SelMenubarEntry
                lda res
                bmi ++
                jsr Menubar_ShowMenu
                lda #PM_MENU
                sta ProgramMode
                rts
no_menu         ; Not in menu bar
                jsr GetCtrlFromPos
                lda res
                bmi ++
                lda ControlIndex
                sta WindowFocCtrl
                jsr UpdateWindow
                lda ControlType
                cmp #CT_BUTTON
                bne +
                lda #1
                sta ControlPressed
+               jmp ControlsProc
++              rts
Std_ClickInMM   lda ControlType
                cmp #CT_MENUBAR
                bne +
                lda #$ff
                sta ControlHilIndex
                jsr UpdateControl
                jmp ++
+               cmp #CT_COLORPICKER
                bne ++
                jsr ControlsProc
++              lda #PM_NORMAL
                sta ProgramMode
                jmp RepaintAll
;----------------------------------------------
StdWnd_MMove    ; Mouse has moved
                lda wndParam1
                beq Std_MovInNMDM
                bpl Std_MovInMM

Std_MovInNMDM   ; Moved in normal AND dialog mode
                lda ControlPressed
                beq +++
                ; Control is pressed
                jsr IsInCurControl
                bcc +
                ; Is in cur ctrl
                lda ControlBits
                and #BIT_CTRL_ISPRESSED
                bne ++
                ; and not pressed, then press
                lda ControlBits
                ora #BIT_CTRL_ISPRESSED
                sta ControlBits
                jsr UpdateControl
                jmp ControlsProc
+               ; Is not in cur ctrl
                lda ControlBits
                and #BIT_CTRL_ISPRESSED
                beq ++
                ; and pressed, then release
                lda ControlBits
                and #($ff-BIT_CTRL_ISPRESSED)
                sta ControlBits
                jsr UpdateControl
                jmp ControlsProc
++              rts
+++             ; Control is not pressed
                lda ControlIndex
                pha
                jsr GetCtrlFromPos
                lda res
                sta Param0
                lda ControlType
                cmp #CT_EDIT_SL
                bne +
                jsr IsInCtrlMiddle
                bcc +
                ldy #CUR_CARRET
                jsr SetCursor
+               pla
                jmp SelectControl
Std_MovInMM     jsr IsInCurMenu
                bcc ++
                ; In cur menu
                lda CurMenuID
                cmp #ID_MENU_COLORPICKER
                beq +++
                ; Regular menu
                jsr GetMenuItem
                lda res
                sta Param0
                jmp SelectMenuItem
++              ; Not in cur menu
                lda CurMenuItem
                bmi +++
                lda #$ff
                sta CurMenuItem
                lda CurMenuType
                bne +++
                jmp Menubar_ShowMenu
+++             rts