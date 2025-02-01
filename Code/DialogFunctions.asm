;-------------------------------------------------
; Dialog procs
;
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
                lda ControlParent+CTRLSTRUCT_DIGIT_HI
                tax
                lda TimesTen,x
                sta dummy
                lda ControlParent+CTRLSTRUCT_DIGIT_LO
                clc
                adc dummy
                sta dummy
                ;
                cmp CurDeviceNo
                beq ++
                ldx CurDeviceInd
                txa
                and #%00000001
                eor #%00000001
                tay
                lda dummy
                cmp DeviceNumbers,y
                bne +
                ; Device numbers equal
                jsr KillDialog
                +ShowMessage <Str_Mess_SameDev, >Str_Mess_SameDev
                rts
+               sta DeviceNumbers,x
                jsr CloseDlg
                jmp ShowDirectory
++              jmp CloseDlg
+++             rts

DiskInfoDlgProc jmp MessageDlgProc

CopyFileDlgProc jsr StdWndProc
                lda wndParam
                cmp #EC_LBTNRELEASE
                bne ++
                jsr IsInCurControl
                beq ++
                lda ControlType
                cmp #CT_BUTTON
                beq +
                rts
+               jmp CloseDlg
                

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
                lda #GM_NORMAL
                sta GameMode
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
                lda #GM_NORMAL
                sta GameMode
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
                lda ControlParent+CTRLSTRUCT_DIGIT_HI
                sta Clock
                lda ControlParent+CTRLSTRUCT_DIGIT_LO
                sta Clock+1
                +SelectControl 1
                lda ControlParent+CTRLSTRUCT_DIGIT_HI
                sta Clock+2
                lda ControlParent+CTRLSTRUCT_DIGIT_LO
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


;-------------------------------------------------
; Show dialog routines
;

; Is called at the end of each show_dialog function
ShowTheDialog   jsr PaintCurWindow
                jsr WindowToScreen
                lda #GM_DIALOG
                sta GameMode
                rts

; Shows dialog with text "Are you sure?"
; Expects:
; * title string in FDFE
; * address to jump to right after yes/no decision in ModalAddress
ShowAreYouSureDlg
                lda #<Str_Mess_Sure
                sta $fb
                lda #>Str_Mess_Sure
                sta $fc
                jmp ShowYesNoDlg

AdjustCtrlPos   ldx StringHeight
                inx
                stx ControlPosY
                ldx StringWidth
                inx
                txa
                sec
                sbc ControlWidth
                sta ControlPosX
                jmp UpdateControl

; Shows question dlg with multi-line string in FBFC
; and title in FDFE
ShowYesNoDlg    lda $fb
                sta dummy
                lda $fc
                sta dummy+1
                jsr DeactivateWnd
                +CreateWindowByData <Wnd_Dlg_YesNo, >Wnd_Dlg_YesNo
                +SetCurWndTitleByte $fd,$fe
                ;
                jsr AddMLLabelToDlg; Also adjusts window width and height
                ;
                +AddControl <Ctrl_YN_NoBtn, >Ctrl_YN_NoBtn
                jsr AdjustCtrlPos
                +ControlSetID ID_BTN_NO
                ;
                +AddControl <Ctrl_YN_YesBtn, >Ctrl_YN_YesBtn
                jsr AdjustCtrlPos
                lda ControlPosX
                sec
                sbc #5
                sta ControlPosX
                jsr UpdateControl
                +ControlSetID ID_BTN_YES
                ;
                jmp ShowTheDialog

CreateMsgDlg    jsr DeactivateWnd
                lda $fb
                sta dummy
                lda $fc
                sta dummy+1
                ;
                +CreateWindowByData <Wnd_Dlg_ShowMess, >Wnd_Dlg_ShowMess
                ;
                jsr AddMLLabelToDlg; Also adjusts window width and height
                ;
                +AddControl <Ctrl_SM_OkBtn, >Ctrl_SM_OkBtn
                jsr AdjustCtrlPos
                +ControlSetID ID_BTN_OK
                rts

; Shows message with multi-line string in FBFC
ShowMessage     jsr CreateMsgDlg
                jmp ShowTheDialog

; Shows error message with multi-line string in FBFC
ShowErrorMsg    jsr CreateMsgDlg
                +SetCurWndTitle <Str_Dlg_Error, >Str_Dlg_Error
                jmp ShowTheDialog

; Shows copy file dialog
ShowDeviceNoDlg lda WindowOnHeap
                sta $a0
                lda WindowOnHeap+1
                sta $a1
                ;
                jsr GetCurDeviceNo
                jsr DeactivateWnd
                lda #<Wnd_Dlg_DevNo
                sta $fb
                lda #>Wnd_Dlg_DevNo
                sta $fc
                jsr CreateWindow
                ; Set dialog position
                ldy #WNDSTRUCT_POSX
                lda ($a0),y
                clc
                adc #8
                sta WindowPosX
                iny
                lda ($a0),y
                sta WindowPosY
                jsr UpdateWindow
                ; Set label "A:" or "B:"
                +SelectControl 0
                lda CurDeviceInd
                tax
                clc
                adc #$61
                sta Ctrl_DN_DevInd
                ; UpDown
                +SelectControl 1
                lda ControlBits
                ora #BIT_CTRL_DBLFRAME_TOP
                sta ControlBits
                lda DeviceNumbers,x
                sta file_size
                lda #0
                sta file_size+1
                jsr ConvertToDec
                lda file_size_dec
                and #%11110000
                lsr
                lsr
                lsr
                lsr
                sta ControlParent+CTRLSTRUCT_DIGIT_HI
                lda file_size_dec
                and #%00001111
                sta ControlParent+CTRLSTRUCT_DIGIT_LO
                lda #CL_WHITE
                sta ControlColor
                lda #8
                sta ControlParent+CTRLSTRUCT_LOWERLIMIT
                lda #$30
                sta ControlParent+CTRLSTRUCT_UPPERLIMIT                
                jsr UpdateControl
                ; OK button
                +SelectControl 2
                +ControlSetID ID_BTN_OK
                jmp ShowTheDialog

; Shows copy file dialog
ShowCopyFileDlg jsr DeactivateWnd
                lda #<Wnd_Dlg_CopyFile
                sta $fb
                lda #>Wnd_Dlg_CopyFile
                sta $fc
                jsr CreateWindow
                ; Progressbar
                +SelectControl 0
                lda FileSizeHex
                sta ControlParent+CTRLSTRUCT_MAX_LO
                lda FileSizeHex+1
                sta ControlParent+CTRLSTRUCT_MAX_HI
                lda #0
                sta ControlParent+CTRLSTRUCT_VAL_LO
                sta ControlParent+CTRLSTRUCT_VAL_HI
                jsr UpdateControl
                ; Filename label
                +SelectControl 1
                lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                +ControlSetString <Str_FileName, >Str_FileName, 0
                ; Label "From A/B to B/A" label
                +SelectControl 2
                lda DiskToCopyFrom+1
                clc
                adc #$61
                ldx #10
                sta Ctrl_CF_Label2,x
                lda DiskToCopyTo+1
                clc
                adc #$61
                ldx #15
                sta Ctrl_CF_Label2,x
                jmp ShowTheDialog

; Shows disk info dialog
ShowDiskInfoDlg jsr GetCurDeviceNo
                jsr DeactivateWnd
                ;
                lda #<Wnd_Dlg_DiskInfo
                sta $fb
                lda #>Wnd_Dlg_DiskInfo
                sta $fc
                jsr CreateWindow
                ;
                +SelectControl 3
                +ControlSetColorVal CL_DARKBLUE
                +SelectControl 4
                +ControlSetColorVal CL_WHITE
                +SelectControl 11
                +ControlSetID ID_BTN_OK
                ;
                ldx CurDeviceInd
                +SelectControl 0
                lda DiskSizeHexLo,x
                sta ControlParent+CTRLSTRUCT_MAX_LO
                sec
                sbc BlocksFreeHexLo,x
                sta ControlParent+CTRLSTRUCT_VAL_LO
                lda DiskSizeHexHi,x
                sta ControlParent+CTRLSTRUCT_MAX_HI
                sbc BlocksFreeHexHi,x
                sta ControlParent+CTRLSTRUCT_VAL_HI
                jsr UpdateControl
                ldy #2
                lda WriteProtected,x
                bne +
-               lda Str_No,y
                sta Ctrl_DI_Label7+5,y
                dey
                bpl -
                jmp ++
+               ;
-               lda Str_Yes,y
                sta Ctrl_DI_Label7+5,y
                dey
                bpl -
++              ldx #3
                lda CurDeviceInd
                bne +
                ; Drive A                
-               lda Str_DriveTypeA,x
                sta Ctrl_DI_Label6+5,x
                lda Str_DiskSizeA,x
                sta Ctrl_DI_Label9+5,x
                lda Str_OccupiedA,x
                sta Ctrl_DI_Label10+5,x
                lda Str_BlocksFreeA,x
                sta Ctrl_DI_Label11+5,x
                lda Str_NumFilesA,x
                sta Ctrl_DI_Label8+5,x
                dex
                bpl -                
                jmp ShowTheDialog
+               ; Drive B
-               lda Str_DriveTypeB,x
                sta Ctrl_DI_Label6+5,x
                lda Str_DiskSizeB,x
                sta Ctrl_DI_Label9+5,x
                lda Str_OccupiedB,x
                sta Ctrl_DI_Label10+5,x
                lda Str_BlocksFreeB,x
                sta Ctrl_DI_Label11+5,x
                lda Str_NumFilesB,x
                sta Ctrl_DI_Label8+5,x
                dex
                bpl -
                jmp ShowTheDialog

; Shows format dialog
ShowFormatDlg   jsr GetCurDeviceNo
                jsr DeactivateWnd
                ;
                lda #<Wnd_Dlg_Format
                sta $fb
                lda #>Wnd_Dlg_Format
                sta $fc
                jsr CreateWindow
                ;
                +SelectControl 1
                lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                ldx #15
                lda #32
-               sta Str_FilenameEdit,x
                dex
                bpl -
                lda #<Forbidden
                sta ControlParent+CTRLSTRUCT_FORBIDDEN
                lda #>Forbidden
                sta ControlParent+CTRLSTRUCT_FORBIDDEN+1
                +ControlSetString <Str_FilenameEdit, >Str_FilenameEdit, 0
                +EditSetCarretInfo 0, 16
                ;
                +SelectControl 2
                +ControlSetStringList <Str_Dlg_For_RBG, >Str_Dlg_For_RBG, 2
                +ControlSetHilIndexVal 0
                ;
                +SelectControl 3
                +ControlSetID ID_BTN_CANCEL
                +SelectControl 4
                +ControlSetID ID_BTN_OK
                ;
                jmp ShowTheDialog

; Shows rename dialog
; Requires:
; * A: 0 for disk and 1 for file
; * Str_FileName
ShowRenameDlg   pha
                jsr DeactivateWnd
                jsr GetCurDeviceNo
                ; Create the dialog
                lda #<Wnd_Dlg_Rename
                sta $fb
                lda #>Wnd_Dlg_Rename
                sta $fc
                jsr CreateWindow
                ; Put filename/diskname into label
                +SelectControl 1
                lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                +ControlSetString <Str_FileName, >Str_FileName, 0
                ; Prepare edit_sl control
                +SelectControl 3
                lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                ldx #15
                lda #32
-               sta Str_FilenameEdit,x
                dex
                bpl -
                lda #<Forbidden
                sta ControlParent+CTRLSTRUCT_FORBIDDEN
                lda #>Forbidden
                sta ControlParent+CTRLSTRUCT_FORBIDDEN+1
                +ControlSetString <Str_FilenameEdit, >Str_FilenameEdit, 0
                +EditSetCarretInfo 0, 16
                ;
                +SelectControl 4
                +ControlSetID ID_BTN_CANCEL
                ;
                +SelectControl 5
                +ControlSetID ID_BTN_OK
                ;
                pla ; 0 for disk and 1 for file
                beq +
                ; Rename file
                +SetCurWndTitle <Str_Dlg_Ren_File, >Str_Dlg_Ren_File
                +DelBitExFromCurWnd BIT_EX_WND_ISDISK
                +SelectControl 0
                +ControlSetString <Str_Mess_OldFile, >Str_Mess_OldFile, 0
                +SelectControl 2
                +ControlSetString <Str_Mess_NewFile, >Str_Mess_NewFile, 0
                jmp ++
+               ; Rename disk
                +SetCurWndTitle <Str_Dlg_Ren_Disk, >Str_Dlg_Ren_Disk
                +AddBitExToCurWnd BIT_EX_WND_ISDISK
                +SelectControl 0
                +ControlSetString <Str_Mess_OldDisk, >Str_Mess_OldDisk, 0
                +SelectControl 2
                +ControlSetString <Str_Mess_NewDisk, >Str_Mess_NewDisk, 0
                ;
++              jmp ShowTheDialog

ShowClockDialog jsr DeactivateWnd
                lda #<Wnd_Dlg_Clock
                sta $fb
                lda #>Wnd_Dlg_Clock
                sta $fc
                jsr CreateWindow
                ;
                +SelectControl 0
                lda #CL_WHITE
                sta ControlColor
                lda #0
                sta ControlParent+CTRLSTRUCT_LOWERLIMIT
                lda #$23
                sta ControlParent+CTRLSTRUCT_UPPERLIMIT
                lda Clock
                sta ControlParent+CTRLSTRUCT_DIGIT_HI
                lda Clock+1
                sta ControlParent+CTRLSTRUCT_DIGIT_LO
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT)
                ora ControlBits
                sta ControlBits
                jsr UpdateControl
                ;
                +SelectControl 1
                lda #CL_WHITE
                sta ControlColor
                lda #0
                sta ControlParent+CTRLSTRUCT_LOWERLIMIT
                lda #$59
                sta ControlParent+CTRLSTRUCT_UPPERLIMIT
                lda Clock+2
                sta ControlParent+CTRLSTRUCT_DIGIT_HI
                lda Clock+3
                sta ControlParent+CTRLSTRUCT_DIGIT_LO
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT)
                ora ControlBits
                sta ControlBits
                jsr UpdateControl
                ;
                +SelectControl 2
                +ControlSetID ID_BTN_SET
                ;
                jmp ShowTheDialog

; Adds a multiline label at pos (1,1) to dialog
; with string at address (dummy,dummy+1)
; and adjusts window geometry !!!!
AddMLLabelToDlg lda dummy
                sta $fb
                lda dummy+1
                sta $fc
                jsr GetStringInfo
                ldx StringWidth
                inx
                inx
                stx WindowWidth
                lda #40
                sec
                sbc StringWidth
                tax
                dex
                dex
                txa
                lsr
                sta WindowPosX
                ldx StringHeight
                inx
                inx
                inx
                inx
                inx
                inx
                stx WindowHeight
                lda #22
                sec
                sbc WindowHeight
                lsr
                sta WindowPosY
                jsr UpdateWindow
                ;
                +AddControl <Ctrl_Dlg_Label, >Ctrl_Dlg_Label
                +ControlSetWidthHeight StringWidth, StringHeight
                lda dummy
                sta $02
                lda dummy+1
                sta $03
                lda #0
                sta Param
                jmp SetCtrlString