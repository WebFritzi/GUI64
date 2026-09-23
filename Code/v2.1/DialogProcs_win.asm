;-------------------------------------------------
; Dialog procs
;
DoStdWProcAndCheckLBtnRelease
                jsr StdWndProc
                lda wndParam0
                cmp #EC_LBTNRELEASE
                rts

DoStdWProcAndCheckLBtnReleaseInCtrl
                jsr DoStdWProcAndCheckLBtnRelease
                beq +
                clc
                rts
+               jmp IsInCurControl

NewFileDlgProc  jsr DoStdWProcAndCheckLBtnRelease
                bne ++
                ; LEFT BUTTON RELEASED
                jsr IsInCurControl
                bcs +
-               rts
+               lda ControlIndex
                cmp #4; OK-Button
                bne check_cancel
create_ok       ; Pressed "OK" button
                jsr SelectControl3
                lda ControlHilIndex
                bmi -
                bne +
                ; It's a directory
                jsr CreateDirectory
                jmp chk_create_err
+               ; It's an image
                jsr CreateImageFile
chk_create_err  lda error_code
                beq kill
DlgDiskErr      ; Error creating image/folder
                jsr KillDialog
                jsr InstallIRQ
                jmp ShowDiskError
kill            jsr KillDialog
                jmp ShowDirectory
check_cancel    cmp #5; Cancel-Button
                bne ++++
                ; Pressed "Cancel" button
                jmp CloseDlg
                ;
++              cmp #EC_LBTNPRESS
                bne +++
                ; Button pressed (only process ListBox)
                jsr IsInCurControl
                bcc ++++
                lda ControlIndex
                cmp #3
                bne ++++
                ; Pressed in Images ListBox
                ; Adjust size of edit box
                jsr ClearImageStr
                lda ControlHilIndex
                sta ZP_5F
                jsr SelectControl1

                ;ldx ZP_5F; needed again below
;                bmi ++++
;                bne +
;                lda #21
;                sta ControlWidth
;                lda #0
;                ldx #16
;                bne ++; jmp ++
;+               lda #15
;                sta ControlWidth
;                lda #0
;                ldx #12
;++              jsr SetEditSLInfo

                ldx ZP_5F; needed again below
                bmi ++++
                beq nf_directory
                lda #15
                ldx #12
                bne nf_set_edit; jmp nf_set_edit
nf_directory    lda #21
                ldx #16
nf_set_edit     sta ControlWidth
                lda #0
                jsr SetEditSLInfo

                ; Place file extension string
                ldx ZP_5F
                inx
                txa
                asl
                asl
                tax
                dex
                ldy #3
-               lda Str_NewFile_Imgs,x
                sta Ctrl_NF_ImgType,y
                dex
                dey
                bpl -
                jsr PaintCurWndViaBufToScreen
+++             jsr Cmp_ReturnKey
                bne ++++
                jmp create_ok
++++            rts

old_dev_ind     !byte 0
old_cur_wnd     !byte 0
DevNoDlgProc    jsr DoStdWProcAndCheckLBtnReleaseInCtrl
                ;bne +++
                ;jsr IsInCurControl
                bcc +++
                lda ControlIndex
                cmp #2; Ok-Button
                bne +++
                ; Pressed "OK" in device number dialog
                jsr SelectControl1
                lda ControlIndex+UPDOWN_DIGIT_HI
                asl                         ; *2
                sta ZP_5F
                asl                         ; *4
                asl                         ; *8
                adc ZP_5F                   ; *10
                adc ControlIndex+UPDOWN_DIGIT_LO
                ;
                pha
                jsr CloseDlg
                pla
                cmp CurDeviceNo
                beq ++
                ; New dev no is different
                ldx CurDeviceInd
                stx old_dev_ind
                sta CSTM_DevNumbers,x
                ;
                tax
                lda #1
                sta bMayRoot-8,x
                ;
                ldx old_dev_ind
                inx
                stx Param0
                ; If window is open ...
                jsr IsWndTypePresent
                bcc ++
                ; ... update it
                stx Param0
                lda CurrentWindow
                sta old_cur_wnd
                jsr DeactivateWnd
                jsr SelectWindow
                jsr ShowDirectory
                lda ProgramMode
                bmi +
                lda old_cur_wnd
                sta CurrentWindow
+               jmp PaintTaskbar
++              jmp RepaintAll
+++             rts

FormatDlgProc   jsr DoStdWProcAndCheckLBtnRelease
                beq +
                jsr Cmp_ReturnKey
                beq format_ok
                rts
+               ; LBtnRelease
                jsr IsInCurControl
                bcc +++
                lda ControlIndex
                cmp #4; OK-Button
                bne ++
format_ok       ; "OK" pressed
                jsr FormatDisk
                jsr KillDialog
                lda error_code
                beq +
                jsr InstallIRQ
                jmp ShowDiskError
+               jmp ShowDirectory
++              cmp #3; Cancel-Button
                beq CloseDlg; "Cancel" pressed 
+++             rts

RenameDlgProc   jsr DoStdWProcAndCheckLBtnRelease
                beq +
                jsr Cmp_ReturnKey
                beq rename_ok
                rts
+               ; LBTNRELEASE
                jsr IsInCurControl
                bcc ++++
                lda ControlIndex
                cmp #5; OK-Button
                bne +++
rename_ok       ; "OK" pressed
                lda WindowBitsEx
                and #BIT_EX_WND_ISDISK
                bne ++
                ; Rename file
                jsr RenameFile
                jmp after
++              ; Rename disk
                jsr RenameDisk
                lda error_code
                beq after
                ; Error rename disk
                jmp DlgDiskErr
after           jsr KillDialog
                jmp ShowDirectory
+++             cmp #4; Cancel-Button
                beq CloseDlg
++++            rts

MessageDlgProc  jsr DoStdWProcAndCheckLBtnReleaseInCtrl
                ;bne ++
                ;jsr IsInCurControl
                bcc ++
                lda ControlIndex
                cmp #1
                bne ++; OK-Button index is 0
CloseDlg        jsr KillDialog
                jsr PaintTaskbar
                ldx IsMessageModal
                lda #0
                sta IsMessageModal
                txa
                beq ++
                jmp (ModalAddress)
++              rts

KillDialog      lda #PM_NORMAL
                sta ProgramMode
                jsr KillCurWindow
                jmp RepaintAll

ClockDlgProc    jsr DoStdWProcAndCheckLBtnReleaseInCtrl
                ;bne +
                ;jsr IsInCurControl
                bcc +
                lda ControlIndex
                cmp #2; Set-Button
                bne +
                ; Pressed "Set" in clock dialog
                jsr SelectControl0
                lda ControlIndex+UPDOWN_DIGIT_HI
                sta Clock
                lda ControlIndex+UPDOWN_DIGIT_LO
                sta Clock+1
                jsr SelectControl1
                lda ControlIndex+UPDOWN_DIGIT_HI
                sta Clock+2
                lda ControlIndex+UPDOWN_DIGIT_LO
                sta Clock+3
                jsr SetTOD
                beq CloseDlg
+               rts

YesNoDlgProc    jsr DoStdWProcAndCheckLBtnReleaseInCtrl
                ;bne +++
                ;jsr IsInCurControl
                bcc +++
                lda ControlIndex
                cmp #2; Yes-Button
                bne +
                ; Clicked on "Yes"
                lda #1
                bne ++; jmp ++
+               cmp #1; No-Button
                bne +++
                ; Clicked on "No"
                lda #0
++              sta DialogResult
                jsr CloseDlg
                jmp (ModalAddress)
+++             rts