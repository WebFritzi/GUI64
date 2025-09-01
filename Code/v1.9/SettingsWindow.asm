CreateSettingsWindow
                lda #<Wnd_Settings
                sta $fb
                lda #>Wnd_Settings
                sta $fc
                jsr CreateWindow
                lda res
                bne +
                rts
+               ldx #5
-               txa
                jsr SelectControl
                lda CSTM_ActiveClr,x
                sta ControlColor
                jsr UpdateControl
                dex
                bpl -
                ;
                +SelectControl 6
                +ControlSetStringList <Str_Settings_RBG, >Str_Settings_RBG, 2
                +ControlSetHilIndex CSTM_DeskPattern
                ;
                +SelectControl 10
                +ControlSetID ID_BTN_APPLY
                ;
                +SelectControl 11
                +ControlSetID ID_BTN_OK
                ;
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
                jsr RepaintAll
                jmp PaintTaskbar
+               cmp #ID_BTN_APPLY
                bne -
ApplySettings   ; "Apply" was pressed
                ; Set custom colors (repaints at the end)
                lda WindowCtrlPtr
                sta $fb
                lda WindowCtrlPtr+1
                sta $fc
                ldx #0
                ldy #CTRLSTRUCT_COLOR
-               lda ($fb),y
                sta CSTM_ActiveClr,x
                tya
                clc
                adc #16
                tay
                inx
                cpx #6
                bcc -
                ; Set desktop pattern (repaints at the end)
                tya
                clc
                adc #6
                tay
                lda ($fb),y
                sta CSTM_DeskPattern
                ;
                jsr SetBkgPattern
                jmp RepaintAll