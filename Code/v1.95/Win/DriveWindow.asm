; Creates a drive wnd with type in A
CreateDriveWnd  cmp #WT_DRIVE_A
                bne +
                ; Drive A
                +CreateWindowByData <Wnd_DriveA, >Wnd_DriveA
                lda res
                bne ++
                rts
+               ; Drive B
                +CreateWindowByData <Wnd_DriveB, >Wnd_DriveB
                lda res
                beq +++
++              ;
                jsr GetCurDeviceInd
                ; Menu
                +AddControl <Ctrl_Drv_Menubar, >Ctrl_Drv_Menubar
                +ControlSetStringList <Str_DriveMenubar, >Str_DriveMenubar, 3
                +MenubarAddMenuList <DriveMenubar, >DriveMenubar
                ; FileListBox
                +AddControl <Ctrl_Drv_FLB, >Ctrl_Drv_FLB
                lda #BIT_CTRL_ISMAXIMIZED
                sta ControlBits
                ldx CurDeviceInd
                lda ControlOnHeap
                sta FileListBoxesLo,x
                lda ControlOnHeap+1
                sta FileListBoxesHi,x
                ;
                lda ControlBitsEx
                and #%01111111
                ora ShowFileSizes,x
                sta ControlBitsEx
                ;
                jsr UpdateWindow
                +ControlSetColorVal CL_WHITE
+++             rts

DriveWndProc    lda wndParam
                cmp #EC_LBTNPRESS
                bne +++
                lda wndParam+1
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
                lda wndParam+1
                beq DriveWnd_NM
                bmi +++
                ; In menu mode
                lda wndParam
                cmp #EC_LBTNPRESS
                bne +++
                ; Mouse btn pressed in MM
                jsr IsInCurMenu
                beq +++
                lda CurMenuID
                cmp #ID_MENU_DISK
                bne +
                jmp DiskMenuClicked
+               cmp #ID_MENU_FILE
                bne +
                jmp FileMenuClicked
+               jmp ViewMenuClicked
DriveWnd_NM     ; In normal mode
                lda wndParam
                cmp #EC_DBLCLICK
                bne ++
                ; Double clicked in normal mode
                +SelectControl 1
                jsr IsInCurControl
                beq +++
                jsr FLSB_GetMouseArea
                cpy #1
                bne +++
                jmp DblClickAction
++              cmp #EC_KEYPRESS
                bne +++
                ; Key pressed in normal mode
                lda actkey
                cmp #$fc
                bne +++
                ; Return pressed
                jmp DblClickAction
+++             rts

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
                lda WindowType
                cmp #WT_DRIVE_A
                bne +
                lda #<STRING_LIST_DRIVEA
                sta $fb
                lda #>STRING_LIST_DRIVEA
                sta $fc
                jmp ++
+               lda #<STRING_LIST_DRIVEB
                sta $fb
                lda #>STRING_LIST_DRIVEB
                sta $fc
++              ; Find filename location (FBFC)
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
                beq +
                sta Str_FileName,y
                jmp -
+               sta Str_FileName,y
                lda #1
                sta res
++              rts

is_dnp          !byte 0
file_ext        !byte 0

DblClickAction  lda #0
                sta is_dnp
                jsr GetFile
                sty HasFileExt+1; self-modifying code below
                sty IsPRGExt+1; self-modifying code below
                sty IsValidFileExt+1; self-modifying code below
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
                ldx CurDeviceInd
                lda IsDiskDrive,x
                bne ++; RUN
                ; It's not a disk drive
                lda IsDiskImage,x
                bne ++; RUN
                ; We're not in a disk image (hence in a directory)
                jsr HasFileExt
                beq ++; RUN
                jsr IsValidFileExt; .d64, .d71, .d81, .dnp
                sta file_ext
                bne +
                jsr IsPRGExt; .prg
                bne ++; RUN
                +JMPShowMessage <Str_Mess_NoValidFileType, >Str_Mess_NoValidFileType
+               jsr ChangeDir
                lda file_ext
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

; Checks if there is a dot at the end of file name indicating a
; file extension of up to 4 chars
HasFileExt      ldy #$FF; Is filled in DblClickAction and is the fn string length
                dey
                dey
                bmi +
                ldx #3
-               lda Str_FileName,y
                cmp #"."
                beq ++
                dey
                dex
                bpl -
+               lda #0
                rts
++              lda #1
                rts

IsPRGExt        ldy #$FF; Is filled in DblClickAction and is the fn length
                dey
                lda Str_FileName,y
                cmp #$47 ;"g"
                beq +
                cmp #$67 ;"G"
                bne ++
+               dey
                lda Str_FileName,y
                cmp #$52 ;"r"
                beq +
                cmp #$72 ;"R"
                bne ++
+               dey
                lda Str_FileName,y
                cmp #$50 ;"p"
                beq +
                cmp #$70 ;"P"
                bne ++
+               lda #1
                rts
++              lda #0
                rts

; Decides on whether file name of PRG ends with ".d64", ".d71", ".d81", or ".dnp"
; Output in A:
; 0 : no, 1: d64, 2: d71, 3: d81, 4: dnp
IsValidFileExt  ldy #$FF; Is filled in DblClickAction and is the fn length
                dey
                dey
                dey
                lda Str_FileName,y
                cmp #$44 ;"d"
                beq +
                cmp #$c4 ;"D"
                bne ++
+               iny
                ; Check for d64
                lda Str_FileName,y
                cmp #"6"
                bne +
                iny
                lda Str_FileName,y
                cmp #"4"
                bne ++
                lda #EXT_D64
                rts
+               ; Check for d71/d81
                iny
                lda Str_FileName,y
                cmp #"1"
                bne check_dnp
                dey
                lda Str_FileName,y
                cmp #"7"
                bne check_8
                lda #EXT_D71
                rts
check_8         cmp #"8"
                bne ++
                lda #EXT_D81
                rts
check_dnp       cmp #"P"
                bne ++
                dey
                lda Str_FileName,y
                cmp #"N"
                bne ++
                lda #EXT_DNP
                sta is_dnp
                rts
++              lda #0
                rts

ViewMenuClicked lda CurMenuItem
                cmp #ID_MI_SHOWSIZES
                bne +++
                ; Clicked on "Show Sizes"
                jsr GetCurDeviceInd
                +SelectControl 1
                lda ControlBitsEx
                eor #BIT_EX_CTRL_SHOWSIZES
                sta ControlBitsEx
                and #BIT_EX_CTRL_SHOWSIZES
                ldx CurDeviceInd
                sta ShowFileSizes,x
                ;
                jsr SetDrvWndWidth
-               jsr UpdateControl
                jmp RepaintAll
+++             cmp #ID_MI_LOWERCASE
                bne +
                ; Clicked on "Lower case"
                jsr GetCurDeviceInd
                +SelectControl 1
                lda ControlBitsEx
                eor #BIT_EX_CTRL_LOWERCASE
                sta ControlBitsEx
                and #BIT_EX_CTRL_LOWERCASE
                ldx CurDeviceInd
                sta ShowLowerCase,x
                jmp -
+               cmp #ID_MI_SORTBYNAME
                bne +
                ; Clicked on "Sort by name"
                jsr GetCurDeviceInd
                lda DiskHasError,x
                bne ++
                jsr ShowSortDlg
                jsr SortByName
                jsr KillDialog
                jmp RepaintAll
+               cmp #ID_MI_SORTBYTYPE
                bne +
                ; Clicked on "Sort by type"
                jsr GetCurDeviceInd
                lda DiskHasError,x
                bne ++
                jsr ShowSortDlg
                jsr SortByType
                jsr KillDialog
                jmp RepaintAll
+               cmp #ID_MI_SORTBYSIZE
                bne +
                ; Clicked on "Sort by size"
                jsr GetCurDeviceInd
                lda DiskHasError,x
                bne ++
                jsr ShowSortDlg
                jsr SortBySize
                jsr KillDialog
                jmp RepaintAll
+               rts

SetDrvWndWidth  jsr GetCurDeviceInd
                ldx CurDeviceInd
                lda #20
                ldy ShowFileSizes,x
                bne ++
                ldy Max_Fn_Len_Plus2,x
                cpy #18
                bcc +
--              lda #21
-
+               sta WindowWidth
                ldx CurrentWindow
                sta WndDefWidth,x
                jmp CorrectWinPosX
++              ldy Max_Fn_Len_Plus2,x
                cpy #13
                bcc -
                beq --
                tya
                clc
                adc #8
                sta WindowWidth
                ldx CurrentWindow
                sta WndDefWidth,x
CorrectWinPosX  ; Possibly correct WindowPosX after setting WindowWidth
                clc
                adc WindowPosX
                cmp #41
                bcc +
                lda #40
                sec
                sbc WindowWidth
                sta WindowPosX
+               jmp UpdateWindow

FileMenuClicked lda CurMenuItem
                cmp #ID_MI_FILENEW
                bne +
                ; Clicked on "New"
                jmp ActionNewFile
+               cmp #ID_MI_FILECUT
                bne ++
                ; Clicked on "Cut"
                jsr ActionCopyFile
                lda CanCopy
                beq +
                sta IsCut
+               rts
++              cmp #ID_MI_FILECOPY
                bne +
                ; Clicked on "Copy"
                jmp ActionCopyFile
+               cmp #ID_MI_FILEPASTE
                bne +
                ; Clicked on "Paste"
                jmp ActionPasteFile
+               cmp #ID_MI_FILEDELETE
                bne +
                ; Clicked on "Delete"
                jmp ActionDelete
+               cmp #ID_MI_FILERENAME
                bne +
                ; Clicked on "Rename"
                jmp ActionRenamFile
+               cmp #ID_MI_FILEVIEW
                bne +
                ; Clicked on "View"
                jmp ActionViewFile
+               cmp #ID_MI_FILEBOOT
                bne +
                ; Clicked on "Boot"
                lda #EC_BOOTFILE
                sta exit_code
+               rts

DiskMenuClicked lda CurMenuItem
                cmp #ID_MI_DISKREFRESH
                bne +
                ; Clicked on "Refresh"
                jmp ShowDirectory
+               cmp #ID_MI_DEVICENO
                bne +
                ; Clicked on "Device No"
                jmp ShowDeviceNoDlg
+               cmp #ID_MI_DISKINFO
                bne +
                ; Clicked on "Info"
                jmp ShowDiskInfoDlg
+               cmp #ID_MI_DISKFORMAT
                bne +
                ; Clicked on "Format"
                jmp ShowFormatDlg
+               cmp #ID_MI_DISKRENAME
                bne +
                ; Clicked on "Rename"
                jmp ActionRenamDisk
+               cmp #ID_MI_DISKCLOSE
                bne +
                ; Clicked on "Close"
                jsr KillCurWindow
                jsr RepaintAll
                jsr PaintTaskbar
+               rts

ActionViewFile  jsr GetCurDeviceNo
                +SelectControl 1
                lda ControlHilIndex
                cmp ControlNumStr
                bcs ++
                jsr GetFile
                ; Show viewer window
                jsr CreateViewerWnd
                lda #<FILEVIEWERBUF_START
                sta ViewerEOF
                lda #>FILEVIEWERBUF_START
                sta ViewerEOF+1
                jsr PaintCurWindow
                jsr WindowToScreen; RepaintAll
                ; Read file from disk into buffer
                jsr ReadFileToViewerBuf
                lda error_code
                beq +
                jmp ShowDiskError
+               jsr RepaintAll
++              rts

;                ; Read file from disk into buffer
;                jsr GetFile
;                jsr ReadFileToViewerBuf
;                lda error_code
;                beq +
;                jmp ShowDiskError
;+               jsr PaintTaskbar
;                ; Show viewer window
;                jsr CreateViewerWnd
;                jsr RepaintAll
;                jsr PaintTaskbar
;++              rts

ThrowSpaceError +ShowMessage <Str_Mess_NoSpace, >Str_Mess_NoSpace
                rts

ActionPasteFile lda CanCopy
                bne +
                rts
+               jsr GetCurDeviceNo
                stx DiskToCopyTo+1
                sta DiskToCopyTo
                ;cmp DiskToCopyFrom
                ;bne +
                ;+ShowMessage <Str_Mess_NoSameDisk, >Str_Mess_NoSameDisk
                ;rts
;+               
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
                +AddBitExToCurWnd BIT_EX_WND_ISERRMSG
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
                ldx DiskToCopyFrom+1;dummy
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

ActionNewFile   jsr GetCurDeviceNo
                lda DiskHasError,x
                beq +
                rts
+               lda IsDiskDrive,x
                bne +
                lda IsDiskImage,x
                bne +
                jmp ShowNewFileDlg
+               +JMPShowMessage <Str_Mess_NoNew, >Str_Mess_NoNew

ActionCopyFile  lda #0
                sta CanCopy
                sta IsCut
                +SelectControl 1
                lda ControlHilIndex
                cmp ControlNumStr
                bcs +
                jsr GetFile
                lda Str_FileType
                sta write_appendix+1
                ;
                jsr GetCurDeviceNo
                sta DiskToCopyFrom
                stx DiskToCopyFrom+1
                lda #1
                sta CanCopy
+               rts

ActionRenamDisk jsr GetCurDeviceNo
                ldx CurDeviceInd
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

ActionRenamFile +SelectControl 1
                lda ControlHilIndex
                cmp ControlNumStr
                bcs ++
                jsr GetFile
                lda #1
                jsr ShowRenameDlg
++              rts

ActionDelete    +SelectControl 1
                lda ControlHilIndex
                cmp ControlNumStr
                bcs ++
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
+               jsr ShowDirectory
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
length_files         !byte 0
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
                lda StringListDrvLo,y
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
                beq ++
                jmp -
++              rts

MinSort_1       stx minpos
                stx dummy
                ; FDFE must be at pos
                ; 0405 is minval
                lda $fd
                sta $04
                lda $fe
                sta $05
                ; 0203 runs through records
                lda $fd
                clc
                adc #FILE_RECORD_LENGTH
                sta $02
                lda $fe
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
                lda $02
                sta $04
                lda $03
                sta $05
+               lda #FILE_RECORD_LENGTH
                jsr AddTo02
                inx
                cpx length
                bcc -
                ;
                ldx dummy ; X = pos
                cpx minpos
                beq +
                jsr SwapFD04
+               ; increment pos
                lda #FILE_RECORD_LENGTH
                jsr AddToFD
                inx
                cpx length_minus_1
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
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                jsr SortFDFE
                rts

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
                jsr SortFDFE
                rts

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
                lda $02
                sta $04
                lda $03
                sta $05
++              lda #FILE_RECORD_LENGTH
                jsr AddTo02
                inx
                cpx length_files
                bcc -
                ;
                ldx dummy ; X = pos
                cpx minpos
                beq +
                jsr SwapFD04
+               ; increment pos
                lda #FILE_RECORD_LENGTH
                jsr AddToFD
                inx
                cpx length_minus_1
                bcc --
                rts