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
                ldy CSTM_ActiveClr,x
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
                ;
                ;lda #10
                ;ldx #ID_BTN_APPLY
                ;jsr SelCtrlAndSetID
                ;
                ;lda #11
                ;ldx #ID_BTN_OK
                ;jmp SelCtrlAndSetID

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
+               jmp KillCurWnd_RepaintGUI
++              cmp #10; Apply-Button
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
                jsr SetBkgPattern
                ;
                jmp RepaintAll