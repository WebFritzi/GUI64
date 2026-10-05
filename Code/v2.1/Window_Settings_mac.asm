CreateSettingsWindow
                ldx #<Wnd_Settings
                ldy #>Wnd_Settings
                jsr CreateWindowEx
                lda res
                bne +
                rts
                ; Color Pickers
+               ldx #5
-               txa
                jsr SelectControl
                ldy CSTM_TitleClr,x
                jsr SetCtrlColor
                dex
                bpl -
                ; Radio Button Group
                lda #6
                jsr SelectControl
                lda CSTM_DeskPattern
                sta ControlHilIndex
                ldx #<Str_Settings_RBG
                ldy #>Str_Settings_RBG
                lda #2
                jsr SetCtrlStringList
                ; Apply Button
                ldy #10
                lda #(BIT_CTRL_DBLFRAME_RGT+BIT_CTRL_DBLFRAME_BTM)
                jsr SelCtrl_AddBits
                ;lda #10
                ;jsr SelectControl
                ;lda #(BIT_CTRL_DBLFRAME_RGT+BIT_CTRL_DBLFRAME_BTM)
                ;ora ControlBits
                ;sta ControlBits
                ;lda #ID_BTN_APPLY
                ;sta ControlID
                ;jsr UpdateControl
                ; OK Button
                ldy #11
                lda #(BIT_CTRL_DBLFRAME_LFT+BIT_CTRL_DBLFRAME_RGT+BIT_CTRL_DBLFRAME_BTM)
                jsr SelCtrl_AddBits
                ;lda #11
                ;jsr SelectControl
                ;lda #(BIT_CTRL_DBLFRAME_LFT+BIT_CTRL_DBLFRAME_RGT+BIT_CTRL_DBLFRAME_BTM)
                ;ora ControlBits
                ;sta ControlBits
                ;lda #ID_BTN_OK
                ;sta ControlID
                ;jsr UpdateControl
                ;
                jmp MenubarToScreen

; Needs wndParam0 filled with exit code
SettingsWndProc jsr StdWndProc
                ;
                lda wndParam0
                cmp #EC_LBTNRELEASE
                beq +
-               rts
+               jsr IsInCurControl
                bcc -
                ; Mouse button released
                lda ControlIndex
                cmp #11; OK-Button
                bne ++
                ; "OK" was pressed
                jsr ApplySettings
                lda bIsUltimate
                beq +
                jsr UltSaveSettings
+               jmp KillCurWnd_RepaintAll
++              cmp #10; Apply-Button
                bne -
ApplySettings   ; "Apply" was pressed
                ; Set custom colors (repaints at the end)
                ldx #5
-               txa
                jsr SelectControl
                lda ControlColor
                sta CSTM_TitleClr,x
                dex
                bpl -
                ; Set desktop pattern (repaints at the end)
                lda #6
                jsr SelectControl
                lda ControlHilIndex
                sta CSTM_DeskPattern
                ;
                jmp RepaintAll