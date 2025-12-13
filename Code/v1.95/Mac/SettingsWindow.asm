CreateSettingsWindow
                lda #<Wnd_Settings
                sta $fb
                lda #>Wnd_Settings
                sta $fc
                jsr CreateWindow
                lda res
                bne +
                rts
                ; Color Pickers
+               ldx #5
-               txa
                jsr SelectControl
                lda CSTM_TitleClr,x
                sta ControlColor
                jsr UpdateControl
                dex
                bpl -
                ; Radio Button Group
                +SelectControl 6
                +ControlSetStringList <Str_Settings_RBG, >Str_Settings_RBG, 2
                +ControlSetHilIndex CSTM_DeskPattern
                ; Apply Button
                +SelectControl 10
                lda #(BIT_CTRL_DBLFRAME_RGT+BIT_CTRL_DBLFRAME_BTM)
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_APPLY
                ; OK Button
                +SelectControl 11
                lda #(BIT_CTRL_DBLFRAME_LFT+BIT_CTRL_DBLFRAME_RGT+BIT_CTRL_DBLFRAME_BTM)
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_OK
                ;
                jsr MenubarToScreen
                rts

; Needs wndParam filled with exit code
SettingsWndProc jsr StdWndProc
                ;
                lda wndParam
                cmp #EC_LBTNRELEASE
                beq +
-               rts
+               jsr IsInCurControl
                beq -
                ; Mouse button released
                lda ControlID
                cmp #ID_BTN_OK
                bne +
                ; "OK" was pressed
                jsr ApplySettings
                jsr KillCurWindow
                jmp RepaintAll
+               cmp #ID_BTN_APPLY
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
                +SelectControl 6
                lda ControlHilIndex
                sta CSTM_DeskPattern
                jmp RepaintAll