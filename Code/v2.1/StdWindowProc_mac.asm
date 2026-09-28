; Needs wndParam0 filled with exit code
; and wndParam1 filled with ProgramMode
StdWndProc      jsr GetMousePosInWnd
                ; Checks event
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
                beq StdWnd_MMove
EmptyWndProc    rts

;StdW_LBPresProc jmp StdWnd_LBPress
;StdW_LBRelProc  jmp StdWnd_LBRel
;StdW_MMoveProc  jmp StdWnd_MMove
;StdW_ScrWhlProc jmp StdW_ScrolWheel
;StdW_KeyPrsProc jmp StdW_KeyPress


StdW_ScrolWheel jsr GetCtrlFromPos
                lda res
                bmi +
                jsr SelectControl
                jsr ControlsProc
+               rts
;----------------------------------------------
StdW_KeyPress   jmp ControlsProc
;----------------------------------------------
StdWnd_LBRel    ; Left button released
                lda wndParam1
                bne +
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

Std_ClickInNM   jsr GetCtrlFromPos
                lda res
                bmi ++
                jsr SelectControl
                lda ControlIndex
                sta WindowFocCtrl
                jsr UpdateWindow
                lda ControlType
                cmp #CT_BUTTON
                bne +
                lda #1
                sta ControlPressed
+               jsr ControlsProc
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
++              ;jsr RepaintAll
;                lda #PM_NORMAL
;                sta ProgramMode
;                jmp MenubarToScreen
                lda #PM_NORMAL
                sta ProgramMode
                jmp RepaintGUI
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
                beq +
                ; Regular menu
                jsr GetMenuItem
                lda res
                sta Param0
                jsr SelectMenuItem
+               rts
++              ; Not in cur menu
                lda CurMenuItem
                bmi +
                lda #$ff
                sta CurMenuItem
                lda CurMenuType
                bne +
                jsr Menubar_ShowMenu
+               rts