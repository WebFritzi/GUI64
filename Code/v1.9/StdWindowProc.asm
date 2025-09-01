wndParam        !byte 0,0
dumm            !byte 0

; Needs wndParam filled with exit code
; and wndParam+1 filled with ProgramMode
StdWndProc      jsr GetMousePosInWnd
                ; Checks event
                lda wndParam
                cmp #EC_LBTNPRESS
                beq StdW_LBPresProc
                cmp #EC_LBTNRELEASE
                beq StdW_LBRelProc
                cmp #EC_MOUSEMOVE
                beq StdW_MMoveProc
                cmp #EC_SCROLLWHEELDOWN
                beq StdW_ScrWhlProc
                cmp #EC_SCROLLWHEELUP
                beq StdW_ScrWhlProc
                cmp #EC_KEYPRESS
                beq StdW_KeyPrsProc
                cmp #EC_LLBTNPRESS
                beq StdW_LLBtnPress
                rts

StdW_LBPresProc jmp StdWnd_LBPress
StdW_LBRelProc  jmp StdWnd_LBRel
StdW_MMoveProc  jmp StdWnd_MMove
StdW_ScrWhlProc jmp StdW_ScrolWheel
StdW_KeyPrsProc jmp StdW_KeyPress
StdW_LLBtnPress jmp no_menu


StdWnd_LBPress  ; Left button pressed
                lda wndParam+1
                beq Std_ClickInNM
                bpl Std_ClickInMM
                rts
Std_ClickInNM   lda MousePosInWndY
                bpl no_menu
                ; Clicked in menu bar
                +SelectControl 0
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
+               jsr ControlsProc
++              rts
Std_ClickInMM   lda ControlType
                cmp #CT_MENUBAR
                bne +
                lda #$ff
                sta ControlHilIndex
                jsr UpdateControl
                jmp ++
+               lda ControlType
                cmp #CT_COLORPICKER
                bne ++
                jsr ControlsProc
++              jsr RepaintAll
                lda #PM_NORMAL
                sta ProgramMode
                rts
;----------------------------------------------
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
                lda wndParam+1
                beq Std_RelInNM
                bpl Std_RelInMM
                jmp Std_RelInDM
Std_RelInNM     lda ControlPressed
                bne +
                ; No control pressed;
                rts
+               ; Some control is pressed
                lda #0
                sta ControlPressed
                jmp ControlsProc
Std_RelInMM     ;
                rts
Std_RelInDM     ;
                rts
;----------------------------------------------
StdWnd_MMove    ; Mouse has moved
                lda wndParam+1
                beq Std_MovInNMDM
                bpl Std_MovInMM

Std_MovInNMDM   ; Moved in normal AND dialog mode
                lda ControlPressed
                beq +++
                ; Control is pressed
                jsr IsInCurControl
                beq +
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
                lda #BIT_CTRL_ISPRESSED
                eor #%11111111
                and ControlBits
                sta ControlBits
                jsr UpdateControl
                jmp ControlsProc
++              rts
+++             ; Control is not pressed
                lda ControlIndex
                pha
                jsr GetCtrlFromPos
                lda res
                sta Param
                lda ControlType
                cmp #CT_EDIT_SL
                bne +
                jsr IsInCtrlMiddle
                beq +
                +SetCursor CUR_CARRET
+               pla
                jmp SelectControl
Std_MovInMM     jsr IsInCurMenu
                beq ++
                ; In cur menu
                lda CurMenuID
                cmp #ID_MENU_COLORPICKER
                beq +
                ; Regular menu
                jsr GetMenuItem
                lda res
                sta Param
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