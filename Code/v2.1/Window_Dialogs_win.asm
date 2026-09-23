;-------------------------------------------------
; Show dialog routines
;

; Is called at the end of each show_dialog function
ShowTheDialog   lda #PM_DIALOG
                sta ProgramMode
                jmp PaintCurWndViaBufToScreen

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

AddCtrl_Adjust  jsr AddControl
                jmp AdjustCtrlPos

; Shows question dlg with multi-line string in FBFC
; and title in FDFE
ShowYesNoDlg    lda $fb
                sta ZP_5F
                lda $fc
                sta ZP_60
                jsr DeactivateWnd
                ldx #<Wnd_Dlg_YesNo
                ldy #>Wnd_Dlg_YesNo
                jsr CreateWindow
                ldx $fd
                ldy $fe
                jsr SetCurWndTitle
                ;
                jsr AddMLLabelToDlg; Also adjusts window width and height
                ;
                ldx #<Ctrl_YN_NoBtn
                ldy #>Ctrl_YN_NoBtn
                jsr AddCtrl_Adjust
                ;
                ldx #<Ctrl_YN_YesBtn
                ldy #>Ctrl_YN_YesBtn
                jsr AddCtrl_Adjust
                lda ControlPosX
                sec
                sbc #5
                sta ControlPosX
                jsr UpdateControl
                ;
                jmp ShowTheDialog

CreateMsgDlg    jsr DeactivateWnd
                lda $fb
                sta ZP_5F
                lda $fc
                sta ZP_60
                ;
                ldx #<Wnd_Dlg_ShowMess
                ldy #>Wnd_Dlg_ShowMess
                jsr CreateWndNoRes
                ;
                jsr AddMLLabelToDlg; Also adjusts window width and height
                ;
                ldx #<Ctrl_SM_OkBtn
                ldy #>Ctrl_SM_OkBtn
                ;lda #ID_BTN_OK
                jmp AddCtrl_Adjust

IsMessageModal  !byte 0

; Shows message with multi-line string in Addr(X,Y)
ShowMessage     stx $fb
                sty $fc
                jsr CreateMsgDlg
                jmp ShowTheDialog

; Shows error message with multi-line string in FBFC
ShowErrorMsg    stx $fb
                sty $fc
                jsr CreateMsgDlg
                ldx #<Str_Dlg_Error
                ldy #>Str_Dlg_Error
                jsr SetCurWndTitle
                jmp ShowTheDialog

;-----------------------------------------
CreateDevNoDlg  jsr DeactivateWnd
                ldx #<Wnd_Dlg_DevNo
                ldy #>Wnd_Dlg_DevNo
                jmp CreateWindowEx

; Shows device number dialog
ShowDeviceNoDlg jsr GetCurDeviceNo
                ;
                ;lda WindowOnHeap
                ;sta $a0
                ;lda WindowOnHeap+1
                ;sta $a1
                lda WindowPosY
                pha
                lda WindowPosX
                pha
                ;
                jsr CreateDevNoDlg
                ;
                ; Set dialog position
                ;ldy #WNDSTRUCT_POSX
                ;lda ($a0),y
                pla
                clc
                adc #8
                sta WindowPosX
                ;iny
                ;lda ($a0),y
                pla
                sta WindowPosY
                jsr UpdateWindow
                jsr SetDevNoLabels
                jmp ShowTheDialog

; Sets label and updown in DevNoDlg
; Requires CurDeviceInd filled
SetDevNoLabels  ; Set label "A:" or "B:"
                jsr SelectControl0
                lda CurDeviceInd
                tax
                clc
                adc #$61
                sta Ctrl_DN_DevInd
                ; UpDown
                jsr SelectControl1
                lda ControlBits
                ora #BIT_CTRL_DBLFRAME_TOP
                sta ControlBits
                lda CSTM_DevNumbers,x
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
                sta ControlIndex+UPDOWN_DIGIT_HI
                lda file_size_dec
                and #%00001111
                sta ControlIndex+UPDOWN_DIGIT_LO
                lda #8
                sta ControlIndex+UPDOWN_LOWERLIMIT
                lda #$29
                sta ControlIndex+UPDOWN_UPPERLIMIT
                jmp UpdateControl
                ;; OK button
                ;lda #2
                ;ldx #ID_BTN_OK
                ;jmp SelCtrlAndSetID
;-----------------------------------------

ClearImageStr   ldx #15
                lda #32
-               sta Str_DialogEdit,x
                dex
                bpl -
                rts

; Shows new file dialog
ShowNewFileDlg  jsr DeactivateWnd
                ldx #<Wnd_Dlg_NewFile
                ldy #>Wnd_Dlg_NewFile
                jsr CreateWindowEx
                ; Set up image name edit
                jsr ClearImageStr
                ldx #3
-               lda Str_NewFile_Imgs,x
                sta Ctrl_NF_ImgType,x
                dex
                bpl -
                jsr SelectControl1
                jsr PrepareEditCommon
                ; Set up list view
                ;lda #3
                ;ldx #ID_LB_IMAGES
                ;jsr SelCtrlAndSetID
                jsr SelectControl3
                lda #0
                sta ControlHilIndex
                ;sta ControlTopIndex
                lda #5
                ldx #<StrLst_FileNew
                ldy #>StrLst_FileNew
                jsr SetCtrlStringList
                ;
                ;lda #4
                ;ldx #ID_BTN_OK
                ;jsr SelCtrlAndSetID
                ;
                ;lda #5
                ;ldx #ID_BTN_CANCEL
                ;jsr SelCtrlAndSetID
                ;
                jmp ShowTheDialog

SetFileNameUC   lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                ldx #<Str_FileName
                ldy #>Str_FileName
                jmp SetCtrlString

; Shows a load dialog when loading and running a file
ShowLoadDlg     jsr DeactivateWnd
                ldx #<Wnd_Dlg_Load
                ldy #>Wnd_Dlg_Load
                jsr CreateWindowEx
                ;
                jsr SelectControl0
                jsr SetFileNameUC
                jmp ShowTheDialog

; Shows copy file dialog
ShowCopyFileDlg jsr DeactivateWnd
                ldx #<Wnd_Dlg_CopyFile
                ldy #>Wnd_Dlg_CopyFile
                jsr CreateWindowEx
                ; Progressbar
                jsr SelectControl0
                lda FileSizeHex
                sta ControlIndex+PROGBAR_MAX_LO
                lda FileSizeHex+1
                sta ControlIndex+PROGBAR_MAX_HI
                lda #0
                sta ControlIndex+PROGBAR_VAL_LO
                sta ControlIndex+PROGBAR_VAL_HI
                jsr UpdateControl
                ; Filename label
                jsr SelectControl1
                jsr SetFileNameUC
                ; Label "From A/B to B/A" label
                jsr SelectControl2
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

; Calls GetCurDeviceNo and loads DiskHasError,x to A
GetCurDiskError jsr GetCurDeviceNo
                lda DiskHasError,x
                rts

; Shows disk info dialog
ShowDiskInfoDlg jsr GetCurDiskError
                beq +
                rts
+               jsr DeactivateWnd
                ;
                ldx #<Wnd_Dlg_DiskInfo
                ldy #>Wnd_Dlg_DiskInfo
                jsr CreateWindowEx
                ; Colorbox label 1
                jsr SelectControl4
                ldy #CL_DARKBLUE
                jsr SetCtrlColor
                ; Colorbox label 2
                lda #5
                jsr SelectControl
                ldy #CL_WHITE
                jsr SetCtrlColor
                ; Progressbar
                ldx CurDeviceInd
                jsr SelectControl0
                lda DiskSizeHexLo,x
                sta ControlIndex+PROGBAR_MAX_LO
                sec
                sbc BlocksFreeHexLo,x
                sta ControlIndex+PROGBAR_VAL_LO
                lda DiskSizeHexHi,x
                sta ControlIndex+PROGBAR_MAX_HI
                sbc BlocksFreeHexHi,x
                sta ControlIndex+PROGBAR_VAL_HI
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
ShowFormatDlg   jsr GetCurDiskError
                beq +
                rts
+               jsr DeactivateWnd
                ;
                ldx #<Wnd_Dlg_Format
                ldy #>Wnd_Dlg_Format
                jsr CreateWindowEx
                ;
                jsr SelectControl1
                jsr PrepareEditSL
                ;
                jsr SelectControl2
                lda #0
                sta ControlHilIndex
                ldx #<Str_Dlg_For_RBG
                ldy #>Str_Dlg_For_RBG
                lda #2
                jsr SetCtrlStringList
                ;
                ;lda #3
                ;ldx #ID_BTN_CANCEL
                ;jsr SelCtrlAndSetID
                ;
                ;lda #4
                ;ldx #ID_BTN_OK
                ;jsr SelCtrlAndSetID
                ;
                jmp ShowTheDialog
                
PrepareEditSL   lda #BIT_CTRL_UPPERCASE
                sta ControlBits
                jsr ClearImageStr
PrepareEditCommon
                lda #<Forbidden
                sta ControlIndex+EDITSL_FORBIDDEN
                lda #>Forbidden
                sta ControlIndex+EDITSL_FORBIDDEN+1
                ldx #<Str_DialogEdit
                ldy #>Str_DialogEdit
                jsr SetCtrlString
                lda #0
                ldx #16
                jmp SetEditSLInfo

; Shows rename dialog
; Requires:
; * A: 0 for disk and 1 for file
; * Str_FileName
ShowRenameDlg   pha
                jsr GetCurDiskError
                beq +
                pla
                rts
+               jsr DeactivateWnd
                ; Create the dialog
                ldx #<Wnd_Dlg_Rename
                ldy #>Wnd_Dlg_Rename
                jsr CreateWindowEx
                ; Put filename/diskname into label
                jsr SelectControl1
                jsr SetFileNameUC
                ; Prepare edit_sl control
                jsr SelectControl3
                jsr PrepareEditSL
                ;
                ;lda #4
                ;ldx #ID_BTN_CANCEL
                ;jsr SelCtrlAndSetID
                ;
                ;lda #5
                ;ldx #ID_BTN_OK
                ;jsr SelCtrlAndSetID
                ;
                pla; 0 for disk and 1 for file
                beq +
                ; Rename file
                ldx #<Str_Dlg_Ren_File
                ldy #>Str_Dlg_Ren_File
                jsr SetCurWndTitle
                lda WindowBitsEx
                and #($ff-BIT_EX_WND_ISDISK)
                sta WindowBitsEx
                jsr UpdateWindow
                jsr SelectControl0
                ldx #<Str_Mess_OldFile
                ldy #>Str_Mess_OldFile
                jsr SetCtrlString
                jsr SelectControl2
                ldx #<Str_Mess_NewFile
                ldy #>Str_Mess_NewFile
                jsr SetCtrlString
                jmp ShowTheDialog
+               ; Rename disk
                ldx #<Str_Dlg_Ren_Disk
                ldy #>Str_Dlg_Ren_Disk
                jsr SetCurWndTitle
                lda WindowBitsEx
                ora #BIT_EX_WND_ISDISK
                sta WindowBitsEx
                jsr UpdateWindow
                jsr SelectControl0
                ldx #<Str_Mess_OldDisk
                ldy #>Str_Mess_OldDisk
                jsr SetCtrlString
                jsr SelectControl2
                ldx #<Str_Mess_NewDisk
                ldy #>Str_Mess_NewDisk
                jsr SetCtrlString
                jmp ShowTheDialog

ClockUpperLimits!byte $23,0,$59
ShowClockDialog jsr DeactivateWnd
                ldx #<Wnd_Dlg_Clock
                ldy #>Wnd_Dlg_Clock
                jsr CreateWindowEx
                ;
                ;jsr SelectControl0
                ;;lda #0
                ;;sta ControlIndex+CTRLSTRUCT_LOWERLIMIT
                ;lda #$23
                ;sta ControlIndex+CTRLSTRUCT_UPPERLIMIT
                ;lda Clock
                ;sta ControlIndex+CTRLSTRUCT_DIGIT_HI
                ;lda Clock+1
                ;sta ControlIndex+CTRLSTRUCT_DIGIT_LO
                ;lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT)
                ;sta ControlBits
                ;ldy #CL_WHITE
                ;jsr SetCtrlColor
                ;;
                ;jsr SelectControl1
                ;;lda #0
                ;;sta ControlIndex+CTRLSTRUCT_LOWERLIMIT
                ;lda #$59
                ;sta ControlIndex+CTRLSTRUCT_UPPERLIMIT
                ;lda Clock+2
                ;sta ControlIndex+CTRLSTRUCT_DIGIT_HI
                ;lda Clock+3
                ;sta ControlIndex+CTRLSTRUCT_DIGIT_LO
                ;lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT)
                ;sta ControlBits
                ;ldy #CL_WHITE
                ;jsr SetCtrlColor
                
                ldx #2
-               txa
                lsr
                jsr SelectControl   ; A=1 bei X=2, A=0 bei X=0

                lda ClockUpperLimits,x
                sta ControlIndex+UPDOWN_UPPERLIMIT

                lda Clock,x
                sta ControlIndex+UPDOWN_DIGIT_HI
                lda Clock+1,x
                sta ControlIndex+UPDOWN_DIGIT_LO

                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_LFT)
                sta ControlBits

                ;ldy #CL_WHITE
                ;jsr SetCtrlColor
                jsr UpdateControl

                dex
                dex
                bpl -
                ;
                ;lda #2
                ;ldx #ID_BTN_SET
                ;jsr SelCtrlAndSetID
                ;
                jmp ShowTheDialog

; Adds a multiline label at pos (1,1) to dialog
; with string at address (ZP_5F,ZP_60)
; and adjusts window geometry !!!!
AddMLLabelToDlg lda ZP_5F
                sta $fb
                lda ZP_60
                sta $fc
                jsr GetStringInfo
                iny
                iny
                sty WindowWidth
                lda #40
                sec
                sbc WindowWidth
                lsr
                sta WindowPosX
                txa; StringHeight
                clc
                adc #6
                sta WindowHeight
                lda #22
                sec
                sbc WindowHeight
                lsr
                sta WindowPosY
                jsr UpdateWindow
                ;
                ldx #<Ctrl_Dlg_Label
                ldy #>Ctrl_Dlg_Label
                jsr AddControl
                lda StringWidth
                sta ControlWidth
                lda StringHeight
                sta ControlHeight
                ldx ZP_5F
                ldy ZP_60
                jmp SetCtrlString