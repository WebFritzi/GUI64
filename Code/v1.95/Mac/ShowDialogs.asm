;-------------------------------------------------
; Show dialog routines
;
DeactivateWnd   jsr PaintCurWndDeac
                jsr WindowToScreen
                rts

; Is called at the end of each show_dialog function
ShowTheDialog   jsr PaintCurWindow
                jsr WindowToScreen
                lda #PM_DIALOG
                sta ProgramMode
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
                inx
                txa
                sec
                sbc ControlWidth
                sta ControlPosX
                jmp UpdateControl

SaveFBFD_Deact  lda $fd
                pha
                lda $fe
                pha
                lda $fb
                pha
                lda $fc
                pha
                jsr DeactivateWnd
                pla
                sta dummy+1
                pla
                sta dummy
                pla
                sta $fe
                pla
                sta $fd
                rts

; Shows question dlg with multi-line string in FBFC
; and title in FDFE
ShowYesNoDlg    jsr SaveFBFD_Deact
                +CreateWindowByData <Wnd_Dlg_YesNo, >Wnd_Dlg_YesNo
                +SetCurWndTitleByte $fd,$fe
                ;
                jsr AddMLLabelToDlg; Also adjusts window width and height
                ;
                +AddControl <Ctrl_YN_NoBtn, >Ctrl_YN_NoBtn
                jsr AdjustCtrlPos
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_LFT)
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_NO
                ;
                +AddControl <Ctrl_YN_YesBtn, >Ctrl_YN_YesBtn
                jsr AdjustCtrlPos
                lda ControlPosX
                sec
                sbc #5
                sta ControlPosX
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_BTM)
                ora ControlBits
                sta ControlBits
                jsr UpdateControl
                +ControlSetID ID_BTN_YES
                ;
                jmp ShowTheDialog

; Creates message with multi-line string in FBFC
CreateMsgDlg    jsr SaveFBFD_Deact
                ;
                +CreateWindowByData <Wnd_Dlg_ShowMess, >Wnd_Dlg_ShowMess
                ;
                jsr AddMLLabelToDlg; Also adjusts window width and height
                ;
                +AddControl <Ctrl_SM_OkBtn, >Ctrl_SM_OkBtn
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_BTM)
                ora ControlBits
                sta ControlBits
                jsr AdjustCtrlPos
                +ControlSetID ID_BTN_OK
                rts

IsMessageModal  !byte 0

; Shows message with multi-line string in FBFC
ShowMessage     jsr CreateMsgDlg
                jmp ShowTheDialog

; Shows error message with multi-line string in FBFC
ShowErrorMsg    jsr CreateMsgDlg
                +SetCurWndTitle <Str_Dlg_Error, >Str_Dlg_Error
                jmp ShowTheDialog

;-----------------------------------------
CreateDevNoDlg  jsr DeactivateWnd
                lda #<Wnd_Dlg_DevNo
                sta $fb
                lda #>Wnd_Dlg_DevNo
                sta $fc
                jsr CreateWindow
                +SelectControl 1
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_BTM)
                jsr SetControlBits
                +SelectControl 2
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_BTM)
                jsr SetControlBits
                rts

; Sets control bits in A and updates control
SetControlBits  ora ControlBits
                sta ControlBits
                jsr UpdateControl
                rts

; Shows device number dialog
ShowDeviceNoDlg jsr GetCurDeviceNo
                ;
                lda WindowOnHeap
                sta $a0
                lda WindowOnHeap+1
                sta $a1
                ;
                jsr CreateDevNoDlg
                ;
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
                jsr SetDevNoLabels
                jmp ShowTheDialog

; Sets label and updown in DevNoDlg
; Requires CurDeviceInd filled
SetDevNoLabels  ; Set label "A:" or "B:"
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
                sta ControlIndex+CTRLSTRUCT_DIGIT_HI
                lda file_size_dec
                and #%00001111
                sta ControlIndex+CTRLSTRUCT_DIGIT_LO
                lda #CL_WHITE
                sta ControlColor
                lda #8
                sta ControlIndex+CTRLSTRUCT_LOWERLIMIT
                lda #$29
                sta ControlIndex+CTRLSTRUCT_UPPERLIMIT                
                jsr UpdateControl
                ; OK button
                +SelectControl 2
                +ControlSetID ID_BTN_OK
                rts
;-----------------------------------------

ClearImageStr   ldx #15
                lda #32
-               sta Str_ImageEdit,x
                dex
                bpl -
                rts

; Shows new file dialog
ShowNewFileDlg  jsr DeactivateWnd
                lda #<Wnd_Dlg_NewFile
                sta $fb
                lda #>Wnd_Dlg_NewFile
                sta $fc
                jsr CreateWindow
                ; Set up image name edit
                jsr ClearImageStr
                ldx #3
-               lda Str_NewFile_Imgs,x
                sta Ctrl_NF_ImgType,x
                dex
                bpl -
                +SelectControl 1
                lda #<Forbidden
                sta ControlIndex+CTRLSTRUCT_FORBIDDEN
                lda #>Forbidden
                sta ControlIndex+CTRLSTRUCT_FORBIDDEN+1
                +ControlSetString <Str_ImageEdit, >Str_ImageEdit, 0
                lda #0
                ldx #16
                jsr SetCarretInfo
                ; Set up list view
                +SelectControl 3
                lda #CL_WHITE
                sta ControlColor
                lda #<StrLst_FileNew
                sta ControlStrings
                lda #>StrLst_FileNew
                sta ControlStrings+1
                lda #0
                sta ControlHilIndex
                sta ControlTopIndex
                +ControlSetID ID_LB_IMAGES
                ;
                +SelectControl 4
                +ControlSetID ID_BTN_OK
                ;
                +SelectControl 5
                +ControlSetID ID_BTN_CANCEL
                jmp ShowTheDialog

; Shows a load dialog when loading and running a file
ShowLoadDlg     jsr DeactivateWnd
                lda #<Wnd_Dlg_Load
                sta $fb
                lda #>Wnd_Dlg_Load
                sta $fc
                jsr CreateWindow
                ;
                +SelectControl 0
                lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                +ControlSetString <Str_FileName, >Str_FileName, 0
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
                sta ControlIndex+CTRLSTRUCT_MAX_LO
                lda FileSizeHex+1
                sta ControlIndex+CTRLSTRUCT_MAX_HI
                lda #0
                sta ControlIndex+CTRLSTRUCT_VAL_LO
                sta ControlIndex+CTRLSTRUCT_VAL_HI
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
                lda DiskHasError,x
                beq +
                rts
+               jsr DeactivateWnd
                ;
                lda #<Wnd_Dlg_DiskInfo
                sta $fb
                lda #>Wnd_Dlg_DiskInfo
                sta $fc
                jsr CreateWindow
                ;
                +SelectControl 3
                +ControlSetColorVal CL_DARKGRAY
                +SelectControl 4
                +ControlSetColorVal CL_WHITE
                +SelectControl 11
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_BTM)
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_OK
                ;
                ldx CurDeviceInd
                +SelectControl 0
                lda DiskSizeHexLo,x
                sta ControlIndex+CTRLSTRUCT_MAX_LO
                sec
                sbc BlocksFreeHexLo,x
                sta ControlIndex+CTRLSTRUCT_VAL_LO
                lda DiskSizeHexHi,x
                sta ControlIndex+CTRLSTRUCT_MAX_HI
                sbc BlocksFreeHexHi,x
                sta ControlIndex+CTRLSTRUCT_VAL_HI
                jsr UpdateControl
                ;
                ldy WriteProtected,x; y = 0, 1, or 2
                lda Str_WriteProtLo,y
                sta smc_wp+1
                lda Str_WriteProtHi,y
                sta smc_wp+2
                ldx #3
smc_wp          lda $ffff,x
                sta Ctrl_DI_Label7+5,x
                dex
                bpl smc_wp
                ;
                ldx #3
                ldy CurDeviceInd
                tya
                asl
                asl
                clc
                adc #3
                tay
-               lda Str_DriveType,y
                sta Ctrl_DI_Label6+5,x
                lda Str_DiskSize,y
                sta Ctrl_DI_Label9+5,x
                lda Str_Occupied,y
                sta Ctrl_DI_Label10+5,x
                lda Str_BlocksFree,y
                sta Ctrl_DI_Label11+5,x
                lda Str_NumFiles,y
                sta Ctrl_DI_Label8+5,x
                dey
                dex
                bpl -                
                jmp ShowTheDialog

Str_WriteProtLo !byte <Str_No, <Str_Yes, <Str_DriveTypes
Str_WriteProtHi !byte >Str_No, >Str_Yes, >Str_DriveTypes

; Shows format dialog
ShowFormatDlg   jsr GetCurDeviceNo
                lda DiskHasError,x
                beq +
                rts
+               jsr DeactivateWnd
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
                sta ControlIndex+CTRLSTRUCT_FORBIDDEN
                lda #>Forbidden
                sta ControlIndex+CTRLSTRUCT_FORBIDDEN+1
                +ControlSetString <Str_FilenameEdit, >Str_FilenameEdit, 0
                lda #0
                ldx #16
                jsr SetCarretInfo
                ;
                +SelectControl 2
                +ControlSetStringList <Str_Dlg_For_RBG, >Str_Dlg_For_RBG, 2
                +ControlSetHilIndexVal 0
                ;
                +SelectControl 3
                lda #BIT_CTRL_DBLFRAME_RGT
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_CANCEL
                +SelectControl 4
                lda #BIT_CTRL_DBLFRAME_LFT
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_OK
                ;
                jmp ShowTheDialog

; Shows rename dialog
; Requires:
; * A: 0 for disk and 1 for file
; * Str_FileName
ShowRenameDlg   pha
                jsr GetCurDeviceNo
                lda DiskHasError,x
                beq +
                pla
                rts
+               jsr DeactivateWnd
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
                sta ControlIndex+CTRLSTRUCT_FORBIDDEN
                lda #>Forbidden
                sta ControlIndex+CTRLSTRUCT_FORBIDDEN+1
                +ControlSetString <Str_FilenameEdit, >Str_FilenameEdit, 0
                lda #0
                ldx #16
                jsr SetCarretInfo
                ;
                +SelectControl 4
                lda #BIT_CTRL_DBLFRAME_RGT
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_CANCEL
                ;
                +SelectControl 5
                lda #BIT_CTRL_DBLFRAME_LFT
                ora ControlBits
                sta ControlBits
                +ControlSetID ID_BTN_OK
                ;
                pla ; 0 for disk and 1 for file
                beq +
                ; Rename file
                +SetCurWndTitle <Str_Dlg_Ren_File, >Str_Dlg_Ren_File
                +DelBitFromCurWnd BIT_WND_ISDISK
                +SelectControl 0
                +ControlSetString <Str_Mess_OldFile, >Str_Mess_OldFile, 0
                +SelectControl 2
                +ControlSetString <Str_Mess_NewFile, >Str_Mess_NewFile, 0
                jmp ++
+               ; Rename disk
                +SetCurWndTitle <Str_Dlg_Ren_Disk, >Str_Dlg_Ren_Disk
                +AddBitToCurWnd BIT_WND_ISDISK
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
                sta ControlIndex+CTRLSTRUCT_LOWERLIMIT
                lda #$23
                sta ControlIndex+CTRLSTRUCT_UPPERLIMIT
                lda Clock
                sta ControlIndex+CTRLSTRUCT_DIGIT_HI
                lda Clock+1
                sta ControlIndex+CTRLSTRUCT_DIGIT_LO
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_TOP)
                ora ControlBits
                sta ControlBits
                jsr UpdateControl
                ;
                +SelectControl 1
                lda #CL_WHITE
                sta ControlColor
                lda #0
                sta ControlIndex+CTRLSTRUCT_LOWERLIMIT
                lda #$59
                sta ControlIndex+CTRLSTRUCT_UPPERLIMIT
                lda Clock+2
                sta ControlIndex+CTRLSTRUCT_DIGIT_HI
                lda Clock+3
                sta ControlIndex+CTRLSTRUCT_DIGIT_LO
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_TOP)
                ora ControlBits
                sta ControlBits
                jsr UpdateControl
                ;
                +SelectControl 2
                lda ControlBits
                ora #(BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_BTM)
                sta ControlBits
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
                sbc WindowWidth
                ;tax
                ;dex
                ;dex
                ;txa
                lsr
                sta WindowPosX
                ldx StringHeight
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