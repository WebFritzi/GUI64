;-------------------------------------------------
; Dialog procs
;
EmptyWndProc    rts

NewFileDlgProc  jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                bne ++
                ; LEFT BUTTON RELEASED
                jsr IsInCurControl
                bne +
-               rts
+               lda ControlID
                cmp #ID_BTN_OK
                bne check_cancel
create_ok       ; Pressed "OK" button
                +SelectControl 3
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
                ; Error creating image/folder
                jsr KillDialog
                jsr InstallIRQ
                jmp ShowDiskError
kill            jsr KillDialog
                jmp ShowDirectory
check_cancel    cmp #ID_BTN_CANCEL
                bne ++++
                ; Pressed "Cancel" button
                jmp CloseDlg
                ;
++              cmp #EC_LBTNPRESS
                bne +++
                ; Button pressed (only process ListBox)
                jsr IsInCurControl
                beq ++++
                lda ControlIndex
                cmp #3
                bne ++++
                ; Pressed in Images ListBox
                ; Adjust size of edit box
                jsr ClearImageStr
                lda ControlHilIndex
                sta dummy
                +SelectControl 1
                ldx dummy; needed again below
                bmi ++++
                bne +
                lda #21
                sta ControlWidth
                lda #0
                ldx #16
                jsr SetCarretInfo
                jmp ++
+               lda #15
                sta ControlWidth
                lda #0
                ldx #12
                jsr SetCarretInfo
++              jsr UpdateControl
                ; Place file extension string
                ldx dummy
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
                jsr PaintCurWindow
                jsr WindowToScreen
+++             cmp #EC_KEYPRESS
                bne ++++
                lda actkey
                cmp #$fc; return
                bne ++++
                jmp create_ok
++++            rts

DevNoDlgProc    jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                bne +++
                jsr IsInCurControl
                beq +++
                lda ControlID
                cmp #ID_BTN_OK
                bne +++
                ; Pressed "OK" in device number dialog
                +SelectControl 1
                ldx ControlIndex+CTRLSTRUCT_DIGIT_HI
                lda TimesTen,x
                sta dummy
                lda ControlIndex+CTRLSTRUCT_DIGIT_LO
                clc
                adc dummy
                sta dummy
                ;
                cmp CurDeviceNo
                beq ++
                ; New dev no is different
                ldx CurDeviceInd
                stx old_dev_ind
                sta DeviceNumbers,x
                ;
                tax
                lda #1
                sta bMayRoot,x
                ;
                jsr CloseDlg
                ldx old_dev_ind
                inx
                stx Param
                ; If window is open ...
                jsr IsWndTypePresent
                beq +++
                ; ... update it
                stx Param
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
++              jmp CloseDlg
+++             rts

TimesTen        !byte 0,10,20
old_dev_ind     !byte 0
old_cur_wnd     !byte 0

DiskInfoDlgProc jmp MessageDlgProc

CopyFileDlgProc jmp StdWndProc

FormatDlgProc   jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                beq +
                cmp #EC_KEYPRESS
                bne +++
                lda actkey
                cmp #$fc; return
                bne +++
                jmp format_ok
+               jsr IsInCurControl
                beq +++
                lda ControlID
                cmp #ID_BTN_OK
                bne ++
format_ok       ; "OK" pressed
                jsr FormatDisk
                jsr KillCurWindow
                jsr RepaintAll
                lda #PM_NORMAL
                sta ProgramMode
                lda error_code
                beq +
                jsr InstallIRQ
                jmp ShowDiskError
+               jmp ShowDirectory
++              cmp #ID_BTN_CANCEL
                bne +++
                ; "Cancel" pressed
                jmp CloseDlg
+++             rts

RenameDlgProc   jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                beq +
                cmp #EC_KEYPRESS
                bne ++++
                lda actkey
                cmp #$fc; return
                bne ++++
                jmp rename_ok
+               ; LBTNRELEASE
                jsr IsInCurControl
                beq ++++
                lda ControlID
                cmp #ID_BTN_OK
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
                jsr KillDialog
                jsr InstallIRQ
                jmp ShowDiskError
after           jsr KillDialog
                jmp ShowDirectory
+++             cmp #ID_BTN_CANCEL
                bne ++++
                jmp CloseDlg
++++            rts

KillDialog      jsr KillCurWindow
                jsr RepaintAll
                lda #PM_NORMAL
                sta ProgramMode
                rts

MessageDlgProc  jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                bne ++
                jsr IsInCurControl
                beq ++
                lda ControlID
                cmp #ID_BTN_OK
                bne ++
CloseDlg        jsr KillDialog
                jsr PaintTaskbar
                ldx IsMessageModal
                lda #0
                sta IsMessageModal
                txa
                beq ++
                jmp (ModalAddress)
++              rts

ClockDlgProc    jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                bne +
                jsr IsInCurControl
                beq +
                lda ControlID
                cmp #ID_BTN_SET
                bne +
                ; Pressed "Set" in clock dialog
                +SelectControl 0
                lda ControlIndex+CTRLSTRUCT_DIGIT_HI
                sta Clock
                lda ControlIndex+CTRLSTRUCT_DIGIT_LO
                sta Clock+1
                +SelectControl 1
                lda ControlIndex+CTRLSTRUCT_DIGIT_HI
                sta Clock+2
                lda ControlIndex+CTRLSTRUCT_DIGIT_LO
                sta Clock+3
                jsr SetTOD
                jmp CloseDlg
+               rts

YesNoDlgProc    jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                bne +++
                jsr IsInCurControl
                beq +++
                lda ControlID
                cmp #ID_BTN_YES
                bne +
                ; Clicked on "Yes"
                lda #1
                jmp ++
+               cmp #ID_BTN_NO
                bne +++
                ; Clicked on "No"
                lda #0
++              sta DialogResult
                jsr CloseDlg
                jmp (ModalAddress)
+++             rts