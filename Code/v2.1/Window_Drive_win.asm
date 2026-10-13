DriveWndLo      !byte <Wnd_DriveA,<Wnd_DriveB
DriveWndSamePage = (>Wnd_DriveA == >Wnd_DriveB)
!if DriveWndSamePage == 0 {
DriveWndHi      !byte >Wnd_DriveA,>Wnd_DriveB
}

; Creates a drive wnd with type in A
CreateDriveWnd  ;cmp #WT_DRIVE_A
;                bne +
;                ; Drive A
;                ldx #<Wnd_DriveA
;                ldy #>Wnd_DriveA
;                jmp ++
;+               ; Drive B
;                ldx #<Wnd_DriveB
;                ldy #>Wnd_DriveB
;++              jsr CreateWindow
                tay
                ldx DriveWndLo-1,y
!if DriveWndSamePage == 0 {
                lda DriveWndHi-1,y
} else {
                lda #>Wnd_DriveA
}
                tay
                jsr CreateWindow
                lda res
                beq +++
                ;
                jsr GetCurDeviceInd
                ; Menu bar
                ldx #<Ctrl_Drv_Menubar
                ldy #>Ctrl_Drv_Menubar
                jsr AddControl
                ldx #<Str_DriveMenubar
                ldy #>Str_DriveMenubar
                lda #3
                jsr SetMenu_AddMaximizedFileListBox
                ldx CurDeviceInd
                lda ControlOnHeap
                sta FileListBoxesLo,x
                lda ControlOnHeap+1
                sta FileListBoxesHi,x
                ldy #CL_WHITE
                jsr SetCtrlColor
                ;
                lda #0
                sta ShowFileSizes,x
                ;
                jmp UpdateWindow
+++             rts

DriveWndProc    lda wndParam0
                cmp #EC_LBTNPRESS
                bne +++
                lda wndParam1
                bne +++
                jsr GetMousePosInWnd
                lda MousePosInWndY
                bpl +++
                jsr GetCurDeviceInd
                ldy #32
                lda ShowFileSizes,x
                beq +
                ldy #227
+               sty Menu_Drive_View+3
                ldy #32
                lda ShowLowerCase,x
                beq +
                ldy #227
+               sty Menu_Drive_View+15
                ;
+++             jsr StdWndProc
                ;
                lda wndParam1
                beq DriveWnd_NM
                bmi +++
                ; In menu mode
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne +++
                ; Mouse btn pressed in MM
                jsr IsInCurMenu
                bcc +++
                lda CurMenuID
                cmp #ID_MENU_FILE
                beq FileMenuClicked
                bcc DiskMenuClicked
                jmp ViewMenuClicked
DriveWnd_NM     ; In normal mode
                jsr ChkBrowserAction
                beq DblClickAction
+++             rts
;                lda wndParam0
;                cmp #EC_DBLCLICK
;                bne ++
;                ; Double clicked in normal mode
;                jsr SelectControl1
;                jsr IsInCurControl
;                bcc +++
;                jsr FLSB_GetMouseArea
;                cpy #1
;                beq DblClickAction
;                rts
;++              jsr Cmp_ReturnKey
;                ; Return pressed
;                beq DblClickAction
;+++             rts

FileMenuClicked ldx CurMenuItem
MenuDispatch    lda MenuActionHi,x
                pha                
                lda MenuActionLo,x
                pha
                rts

DiskMenuClicked lda CurMenuItem
                clc
                adc #8
                tax
                bne MenuDispatch

is_dnp          !byte 0
DblClickAction  lda #0
                sta is_dnp
                jsr GetFile
                sty ZP_5F
                jsr GetCurDeviceNo
                lda Str_FileType
                cmp #"S"; SEQ
                beq GoViewFile
                cmp #"U"; USR
                beq GoViewFile
                cmp #"R"; REL
                beq GoViewFile
                cmp #"D"; DEL
                beq +++
                cmp #"F"; folder
                bne ++
                ; It's a folder
                jsr ChangeDir
                lda #0
                jmp Thereafter
++              cmp #"P"; PRG
                bne +++
                ; It's a PRG file
                jsr HasFileExt
                bcc +
                jsr IsTxtExt
                bcs GoViewFile
                jsr IsGUIExt
                bcs GoLoadApp
+               ldx CurDeviceInd
                lda IsDiskDrive,x
                bne ++; RUN
                ; It's not a disk drive
                lda IsDiskImage,x
                bne ++; RUN
                ; We're not in a disk image (hence in a folder)
                jsr HasFileExt
                bcc ++; RUN
                jsr IsValidFileExt; .d64, .d71, .d81, .dnp
                ;sta file_ext
                beq ++; RUN
                ;jsr IsPRGExt; .prg
                ;bcs ++; RUN
                ;ldx #<Str_Mess_NoValidFileType
                ;ldy #>Str_Mess_NoValidFileType
                ;jmp ShowMessage
                pha
                jsr ChangeDir
                ;lda file_ext
                pla
                jmp Thereafter
++              ; RUN if disk drive or if file has no (valid) file ext
                lda #EC_RUNFILE
                sta exit_code
+++             rts

; Called after ChangeDir
; Requires value (0/1) in A
; A=0: Changed to folder
; A=1,2,3,4: Changed to disk image d64, d71, d81, dnp
Thereafter      ldx error_code
                beq +
                jmp ShowDiskError
+               ldx CurDeviceInd
                sta IsDiskImage,x
                jmp ShowDirectory

GoViewFile      jmp ActionViewFile

GoLoadApp       lda #EC_APP
                sta exit_code
                rts             

; Checks if there is a dot at the end of file name indicating a
; file extension of 3 or 4 chars
; Result in carry
HasFileExt      lda ZP_5F; filename length
                sec
                sbc #5
                bmi +
                tay
                iny
                lda Str_FileName,y
                cmp #"."
                beq ++
+               clc
++              rts

; Result in carry
IsGUIExt        ldx #4
                bne CheckExt3

; Result in carry
IsTxtExt        ldx #0
                
; Result in carry
CheckExt3       ldy ZP_5F
-               dey
                lda Str_FileName,y
                ora #$20
                cmp TXT,x
                bne +
                inx
                lda TXT,x
                bne -
                ;sec
                rts
+               clc
                rts

TXT             !byte $74,$78,$74,0     ; txt backwards
                !byte $69,$75,$67,0     ; gui backwards

; Decides on whether file name of PRG ends with ".d64", ".d71", ".d81", or ".dnp"
; Output in A:
; 0 : no, 1: d64, 2: d71, 3: d81, 4: dnp
IsValidFileExt  ldy ZP_5F
                dey
                dey
                dey
                lda Str_FileName,y
                ora #$80
                cmp #$c4              ; "D"
                bne no_valid_ext
                ; C=1 durch CMP-Gleichheit.
                ; INY und LDA verändern C nicht.
                iny
                lda Str_FileName,y
                sbc #"6"              ; 6->0, 7->1, 8->2, N->$18
                tax
                cmp #3
                bcc check_last        ; D64/D71/D81
                cmp #$18              ; "N"-"6"
                bne no_valid_ext
                ldx #3                ; DNP
check_last      iny
                lda Str_FileName,y
                cmp ExtLast,x
                bne no_valid_ext
                inx                   ; 0..3 -> EXT_D64..EXT_DNP
                cpx #EXT_DNP
                bne +
                stx is_dnp
+               txa                   ; wichtig: Z=0 bei allen gültigen Typen
                rts
no_valid_ext    lda #0
                rts

ExtLast         !byte "4","1","1","P"

;IsValidFileExt  ldy ZP_5F
;                dey
;                lda Str_FileName,y
;                sta ZP_5F           ; last letter
;                dey                 ; middle letter
;                ldx #4
;-               lda ZP_5F
;                cmp ExtLast-1,x
;                bne +
;                lda Str_FileName,y
;                cmp ExtMiddle-1,x
;                beq found_ext
;+               dex
;                bne -
;no_valid_ext    lda #0
;                rts
;found_ext       dey
;                lda Str_FileName,y
;                ora #$80
;                cmp #$c4            ; D/d
;                bne no_valid_ext
;                ;
;                txa                 ; 1=D64 ... 4=DNP
;                cpx #EXT_DNP
;                bne +
;                sta is_dnp
;+               rts

;ExtLast         !byte "4","1","1","P"
;ExtMiddle       !byte "6","7","8","N"

;IsValidFileExt  ldy ZP_5F
;                dey
;                dey
;                dey
;                lda Str_FileName,y
;                ora #$80
;                cmp #$c4 ;"D"
;                bne ++
;                iny
;                ; Check for d64
;                lda Str_FileName,y
;                cmp #"6"
;                bne +
;                iny
;                lda Str_FileName,y
;                cmp #"4"
;                bne ++
;                lda #EXT_D64
;                rts
;+               ; Check for d71/d81
;                iny
;                lda Str_FileName,y
;                cmp #"1"
;                bne check_dnp
;                dey
;                lda Str_FileName,y
;                cmp #"7"
;                bne check_8
;                lda #EXT_D71
;                rts
;check_8         cmp #"8"
;                bne ++
;                lda #EXT_D81
;                rts
;check_dnp       cmp #"P"
;                bne ++
;                dey
;                lda Str_FileName,y
;                cmp #"N"
;                bne ++
;                lda #EXT_DNP
;                sta is_dnp
;                rts
;++              lda #0
;                rts

SetDrvWndWidth  jsr GetCurDeviceInd
                ldx CurDeviceInd
                lda #20
                ldy ShowFileSizes,x
                bne ++
                ldy Max_Fn_Len_Plus2,x
                cpy #18
                bcc +++
--              lda #21
                bne +++
++              ldy Max_Fn_Len_Plus2,x
                cpy #13
                bcc +++
                beq --
                tya
                clc
                adc #8
+++             sta WindowWidth
                ldx CurrentWindow
                sta WndDefWidth,x
                ; Possibly correct WindowPosX after setting WindowWidth
                clc
                adc WindowPosX
                cmp #41
                bcc +
                lda #40
                ;sec
                sbc WindowWidth
                sta WindowPosX
+               jmp UpdateWindow

MenuActionLo    ; File menu
                !byte <(ActionNewFile-1), <(ActionCutFile-1), <(ActionCopyFile-1), <(ActionPasteFile-1)
                !byte <(ActionDelete-1), <(ActionRenamFile-1), <(ActionViewFile-1), <(ActionBootFile-1)
                ; Disk menu
                !byte <(ShowDirectory-1), <(ShowDeviceNoDlg-1), <(ShowDiskInfoDlg-1), <(ShowFormatDlg-1)
                !byte <(ActionRenamDisk-1), <(KillCurWnd_RepaintGUI-1)
MenuActionHi    ; File menu
                !byte >(ActionNewFile-1), >(ActionCutFile-1), >(ActionCopyFile-1), >(ActionPasteFile-1)
                !byte >(ActionDelete-1), >(ActionRenamFile-1), >(ActionViewFile-1), >(ActionBootFile-1)
                ; Disk menu
                !byte >(ShowDirectory-1), >(ShowDeviceNoDlg-1), >(ShowDiskInfoDlg-1), >(ShowFormatDlg-1)
                !byte >(ActionRenamDisk-1), >(KillCurWnd_RepaintGUI-1)

ActionCutFile   jsr ActionCopyFile
                lda CanCopy
                beq +
                sta IsCut
+               rts

ActionBootFile  lda #EC_BOOTFILE
                sta exit_code
                rts

KillCurWnd_RepaintGUI
                jsr KillCurWindow
                jmp RepaintGUI

SortRoutinesLo  !byte <SortByName, <SortByType, <SortBySize
SortRoutinesHi  !byte >SortByName, >SortByType, >SortBySize

ViewMenuClicked lda CurMenuItem
                cmp #ID_MI_SHOWSIZES
                bne +++
                ; Clicked on "Show Sizes"
                jsr GetCurDeviceInd
                jsr SelectControl1
                lda ControlBitsEx
                eor #BIT_EX_CTRL_SHOWSIZES
                sta ControlBitsEx
                and #BIT_EX_CTRL_SHOWSIZES
                ;ldx CurDeviceInd
                sta ShowFileSizes,x
                ;
                jsr SetDrvWndWidth
-               jsr UpdateControl
                jmp RepaintAll
+++             cmp #ID_MI_LOWERCASE
                bne +
                ; Clicked on "Lower case"
                jsr GetCurDeviceInd
                jsr SelectControl1
                lda ControlBitsEx
                eor #BIT_EX_CTRL_LOWERCASE
                sta ControlBitsEx
                and #BIT_EX_CTRL_LOWERCASE
                ;ldx CurDeviceInd
                sta ShowLowerCase,x
                bpl -; jmp -
+               ; Clicked on a sort menu item
                sec
                sbc #2
                cmp #3
                bcs +
                tax
                lda SortRoutinesLo,x
                sta sort+1
                lda SortRoutinesHi,x
                sta sort+2
                jsr GetCurDeviceInd
                lda DiskHasError,x
                bne +
                ldy #CUR_HOURGLASS
                jsr SetCursor
sort            jsr $ffff
                ldy #CUR_DEFAULT
                jsr SetCursor
                jmp RepaintAll
+               rts

SelCtrl1_Exit_if_NoSel
                jsr SelectControl1
                lda ControlHilIndex
                cmp ControlNumStr
                bcc +
                pla
                pla
+               rts

ActionViewFile  jsr GetCurDeviceNo
                jsr SelCtrl1_Exit_if_NoSel
                jsr GetFile
                ; Show viewer window
                jsr CreateViewerWnd
                jsr PaintCurWndViaBufToScreen
                ; Read file from disk into buffer
                jsr ReadFileToViewerBuf
                lda error_code
                beq +
                jmp ShowDiskError
+               jmp RepaintGUI

ThrowSpaceError ldx #<Str_Mess_NoSpace
                ldy #>Str_Mess_NoSpace
                jmp ShowMessage

ActionPasteFile lda CanCopy
                bne +
                rts
+               jsr GetCurDeviceNo
                stx DiskToCopyTo+1
                sta DiskToCopyTo
                ; Check if there is enough space on disk
                lda BlocksFreeHexHi,x
                cmp FileSizeHex+1
                bcc ThrowSpaceError
                bne ++
                lda BlocksFreeHexLo,x
                cmp FileSizeHex
                bcc ThrowSpaceError
++              ; Do paste
                jsr ShowCopyFileDlg
                jsr CopyPasteFile
                jsr KillCurWindow; kills dialog
                lda error_code
                beq +
                ; Error
                jsr ShowDiskError
                lda WindowBitsEx
                ora #BIT_EX_WND_ISERRMSG
                sta WindowBitsEx
                jsr UpdateWindow
                jsr bttr
                jmp InstallIRQ
+               lda IsCut
                beq ++
                lda DiskToCopyFrom
                sta CurDeviceNo
                jsr deletefile
                ldx DiskToCopyFrom+1
                stx CurDeviceInd
                ;lda StringListDrvLo,x
                ;sta $fb
                ;lda StringListDrvHi,x
                ;sta $fc
                jsr load_dir
                ldx DiskToCopyFrom+1
                lda FileListBoxesLo,x
                sta $fb
                lda FileListBoxesHi,x
                sta $fc
                ldy #CTRLSTRUCT_NUMSTRINGS
                lda num_files
                sta ($fb),y
                jsr GetDiskValues
++              jsr bttr
                jsr RepaintAll
                jmp ShowDirectory

bttr            lda #0
                sta CanCopy
                sta IsCut
                sta ProgramMode
                rts

ActionNewFile   jsr GetCurDiskError
                beq +
                rts
+               lda IsDiskDrive,x
                bne +
                lda IsDiskImage,x
                bne +
                jmp ShowNewFileDlg
+               ldx #<Str_Mess_NoNew
                ldy #>Str_Mess_NoNew
                jmp ShowMessage

ActionCopyFile  lda #0
                sta CanCopy
                sta IsCut
                jsr SelCtrl1_Exit_if_NoSel
                jsr GetFile
                lda Str_FileType
                sta write_appendix+1
                ;
                jsr GetCurDeviceNo
                sta DiskToCopyFrom
                stx DiskToCopyFrom+1
                lda #1
                sta CanCopy
                rts

ActionRenamDisk jsr GetCurDeviceNo
                lda Str_Title_DrvLo,x
                sta $fb
                lda Str_Title_DrvHi,x
                sta $fc
                ldy #15
-               lda ($fb),y
                sta Str_FileName,y
                dey
                bpl -
                lda #0
                jmp ShowRenameDlg

ActionRenamFile jsr SelCtrl1_Exit_if_NoSel
                jsr GetFile
                lda #1
                jmp ShowRenameDlg

ActionDelete    jsr SelCtrl1_Exit_if_NoSel
                jsr GetCurDeviceNo
                lda #<Str_Dlg_Delete
                sta $fd
                lda #>Str_Dlg_Delete
                sta $fe
                lda #<mod_res1
                sta ModalAddress
                lda #>mod_res1
                sta ModalAddress+1
                jmp ShowAreYouSureDlg
mod_res1        lda DialogResult
                cmp #1
                bne ++
                jsr DeleteFile
                lda error_code
                beq +
                jmp ShowDiskError
+               jmp ShowDirectory
++              rts

; Does the following:
; * Copies filename string of highlighted file in list view to Str_FileName
; * Copies file size to FileSizeHex
; * Copies file type to Str_FileType
; Expects: control FileListScrollBox selected
; Output:
; 1 (success) or 0 (error) in res
; string length is in Y
GetFile         lda #0
                sta res
                ; Get pointer to string list
                sta $fb
                ldx WindowType
                dex
                cpx #2
                bcs ++
                lda StringListDrvHi,x
                sta $fc
                ; Find filename location (FBFC)
                ldx ControlHilIndex
                beq +
                cpx ControlNumStr
                bcs ++
-               lda #FILE_RECORD_LENGTH
                jsr AddToFB
                dex
                bne -
+               ; Copy file size to FileSizeHex
                ldy #0
                lda ($fb),y
                sta FileSizeHex
                iny
                lda ($fb),y
                sta FileSizeHex+1
                ; Get file type
                ldy #19
                lda ($fb),y
                sta Str_FileType
                ; Copy filename to Str_FileName
                lda #2
                jsr AddToFB
                ldy #$ff
-               iny
                lda ($fb),y
                sta Str_FileName,y
                bne -
                inc res
++              rts

;==============================================================
;
;  SORTING ALGORITHMS
;
;==============================================================

; Used algorithm: MinSort (also known as SelectionSort)
;
; procedure MinSort(a[]):
; for pos = 0 to length-2
;   minpos = pos
;   minval = a[pos]
;   for j = pos+1 to length-1
;     if a[j] < minval then
;       minpos = j
;       minval = a[j]
;   if minpos != pos then
;     swap a[pos],a[minpos]

; Labels for sorting
;length_files         !byte 0; ZP
length_folders       !byte 0
minpos               !byte 0
length               !byte 0
length_minus_1       !byte 0
sort_pos             !byte 0

; Format of an entry:
; 2 bytes: size
; 16 bytes: name
; 1 byte: not used
; 1 byte: type

; Finds start address of files
; Sets vars length_files (no of files), length_folders (no of folders)
; and FDFE = Start of files
;     FBFC = Start of folders
; Must call GetCurDeviceInd beforehands
FindFoldersAndFiles
                ldy CurDeviceInd
                lda NumStrings,y
                sta length_files
                lda #0
                sta length_folders
                sta $fb
                sta $fd
                lda StringListDrvHi,y
                sta $fc
                sta $fe
                lda IsDiskDrive,y
                bne ++
                ; It's not a real disk drive
                ldy #(FILE_RECORD_LENGTH-1)
-               lda ($fd),y
                cmp #$46; "F" (folder)
                bne ++
                lda #FILE_RECORD_LENGTH
                jsr AddToFD
                inc length_folders
                dec length_files
                bne -
++              rts

MinSort_1       stx minpos
                stx ZP_5F
                ; FDFE must be at pos
                ; 0405 is minval
                ; 0203 runs through records
                lda $fd
                sta $04
                clc
                adc #FILE_RECORD_LENGTH
                sta $02
                lda $fe
                sta $05
                adc #0
                sta $03
                inx; X = j
                rts

SwapFD04        ldy #(FILE_RECORD_LENGTH-1)
-               lda ($fd),y
                sta $06
                lda ($04),y
                sta ($fd),y
                lda $06
                sta ($04),y
                dey
                bpl -
                rts

; Sorts list at FDFE by position in sort_pos (2 for name, 19 for type)
; Required: length and sort_pos
SortFDFE        ldx length
                beq +++
                dex
                beq +++
                stx length_minus_1
                ;-----------------------------
                ; MinSort
                ;
                ldx #0; X = pos
--              jsr MinSort_1
-               ; Comparison
                ; if $0203 < $0405 then minpos = posof($02) = X and $0405 = $0203
                ldy sort_pos
                lda ($04),y
                and #%01111111
                sta $06
                lda ($02),y
                and #%01111111
                cmp $06
                bcs +
                stx minpos
                jsr C_0203_To_0405
+               lda #FILE_RECORD_LENGTH
                jsr AddTo02
                inx
                cpx length
                bcc -
                ;
                jsr SortFinishPass
                bcc --
+++             rts

; Sorts folders and files by name
SortByName      jsr FindFoldersAndFiles
                lda #2
                sta sort_pos
                lda length_files
                sta length
                jsr SortFDFE
                lda length_folders
                sta length
                jsr FBFC_To_FDFE
                jmp SortFDFE

StartSortFiles  jsr FindFoldersAndFiles
                ldx length_files
                beq +
                dex
                beq +
                stx length_minus_1
                rts
+               ; one more rts
                pla 
                pla
                rts

; Sorts files by type (_D_EL, _P_RG, _R_EL, _S_EQ, _U_SR)
SortByType      jsr StartSortFiles
                lda length_files
                sta length
                lda #(FILE_RECORD_LENGTH - 1)
                sta sort_pos
                jmp SortFDFE

; Sorts files (not folders!) in string list of CurDeviceInd by size
; Must call GetCurDeviceInd beforehands
SortBySize      jsr StartSortFiles
                ;-----------------------------
                ; MinSort (in fact: MaxSort)
                ;
                ldx #0; X = pos
--              jsr MinSort_1
-               ; Comparison
                ; if $0203 > $0405 then minpos = posof($02) = X and $0405 = $0203
                ldy #1
                lda ($04),y
                cmp ($02),y
                bcc +
                bne ++
                dey
                lda ($04),y
                cmp ($02),y
                bcs ++
+               stx minpos
                jsr C_0203_To_0405
++              lda #FILE_RECORD_LENGTH
                jsr AddTo02
                inx
                cpx length_files
                bcc -
                ;
                jsr SortFinishPass
                bcc --
                rts

SortFinishPass  ldx ZP_5F ; X = pos
                cpx minpos
                beq +
                jsr SwapFD04
+               ; increment pos
                lda #FILE_RECORD_LENGTH
                jsr AddToFD
                inx
                cpx length_minus_1
                rts