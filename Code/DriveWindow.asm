; Creates a drive wnd with type in A
CreateDriveWnd  cmp #WT_DRIVE_A
                bne +
                ; Drive A
                +CreateWindowByData <Wnd_DriveA, >Wnd_DriveA
                lda res
                beq +++
                jmp ++
+               ; Drive B
                +CreateWindowByData <Wnd_DriveB, >Wnd_DriveB
                lda res
                beq +++
++              ;
                jsr GetCurDeviceInd
                +AddControl <Ctrl_Drv_Menubar, >Ctrl_Drv_Menubar
                +ControlSetStringList <Str_DriveMenubar, >Str_DriveMenubar, 3
                +MenubarAddMenuList <DriveMenu, >DriveMenu
                ;
                +AddControl <Ctrl_Drv_FLB, >Ctrl_Drv_FLB
                lda #BIT_CTRL_ISMAXIMIZED
                sta ControlBits
                +ControlSetColorVal CL_WHITE
                ldx CurDeviceInd
                lda ControlOnHeap
                sta FileListBoxesLo,x
                lda ControlOnHeap+1
                sta FileListBoxesHi,x
+++             rts

; Needs wndParam filled with exit code
DriveWndProc    jsr StdWndProc
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
+               jmp OptsMenuClicked
DriveWnd_NM     ; In normal mode
                lda wndParam
                cmp #EC_DBLCLICK
                bne ++
                ; Double clicked in normal mode
                +SelectControl 1
                jsr IsInCurControl
                beq +++
                jsr GetMousePosInWnd
                ldx MousePosInWndX
                inx
                cpx ControlWidth
                beq +++ ; In scrollbar
                ldx MousePosInWndY
                beq +++ ; Above files
                inx
                cpx ControlHeight
                bcs +++ ; Below files
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

; Copies filename string of highlighted file in list view
; to Str_FileName
; Expects: control FileListScrollBox selected
; Output:
; 1 (success) or 0 (error) in res
; string length is in Y
GetFileName     lda #0
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
++              ; Find Filename location (FBFC)
                ldx ControlHilIndex
                beq +
                cpx ControlNumStr
                bcs ++
-               lda #32
                jsr AddToFB
                dex
                bne -
+               ; Copy file size to FileSizeHex
                ldy #30
                lda ($fb),y
                sta FileSizeHex
                iny
                lda ($fb),y
                sta FileSizeHex+1
                ; Get first two letters of file type
                ldy #18
                lda ($fb),y
                sta Str_FileType
                iny
                lda ($fb),y
                sta Str_FileType+1
                ; Copy filename to Str_FileName
                ldy #$ff
-               iny
                lda ($fb),y
                cmp #1
                beq +
                sta Str_FileName,y
                jmp -
+               lda #0
                sta Str_FileName,y
                lda #1
                sta res
++              rts

DblClickAction  jsr GetFileName
                sty IsValidFileExt+1
                jsr GetCurDeviceNo
                lda Str_FileType
                cmp #"S";SEQ
                bne +
                jmp ActionViewFile
+               cmp #"U"; USR
                bne +
                jmp ActionViewFile
+               ldx CurDeviceInd
                cmp #"D"; DIR or DEL
                bne ++
                lda IsDiskDrive,x
                bne +++
                lda Str_FileType+1
                cmp #"E"; DEL
                beq +++
                jsr ChangeDir
                jmp Thereafter
++              cmp #"P"; PRG
                bne +++
                lda IsDiskDrive,x
                bne +
                jsr IsValidFileExt
                beq +
                jsr ChangeDir
                jmp Thereafter
+               ; RUN
                lda #EC_RUNFILE
                sta exit_code
+++             rts

Thereafter      lda error_code
                beq +
                jmp ShowDiskError
+               jmp ShowDirectory

; Decides on whether file name of PRG ends with ".d64"
IsValidFileExt  ldy #$FF; Is filled in DblClickAction and is the fn length
                dey
                dey
                dey
                dey
                lda Str_FileName,y
                cmp #"."
                bne ++
                iny
                lda Str_FileName,y
                cmp #"D"
                beq +
                cmp #"d"
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
                jmp is_valid
+               ; Check for d71/d81
                iny
                lda Str_FileName,y
                cmp #"1"
                bne +
                dey
                lda Str_FileName,y
                cmp #"7"
                beq is_valid
                cmp #"8"
                bne ++
                jmp is_valid
+               ; Check for dnp
                cmp #"P"
                bne ++
                dey
                lda Str_FileName,y
                cmp #"N"
                bne ++
                inc $d020
is_valid        lda #1
                rts
++              lda #0
                rts

OptsMenuClicked lda CurMenuItem
                cmp #ID_MI_SHOWSIZES
                bne +
                ; Clicked on "Show Sizes"
                rts
+               ;Clicked on "GUI64 Info"
                +ShowMessage <Str_Mess_GUI64, >Str_Mess_GUI64
                rts

FileMenuClicked lda CurMenuItem
                cmp #ID_MI_FILECUT
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
+               cmp #ID_MI_FILERUN
                bne +
                ; Clicked on "Run"
                lda #EC_RUNFILE
                sta exit_code
                rts
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
                ; Read file from disk into buffer
                jsr GetFileName
                jsr ReadFileToViewerBuf
                lda error_code
                beq +
                jmp ShowDiskError
+               jsr PaintTaskbar
                ; Show viewer window
                jsr CreateViewerWnd
                jsr RepaintAll
                jsr PaintTaskbar
++              rts

ThrowSpaceError +ShowMessage <Str_Mess_NoSpace, >Str_Mess_NoSpace
                rts

ActionPasteFile lda CanCopy
                bne +
                rts
+               jsr GetCurDeviceNo
                lda CurDeviceInd
                sta DiskToCopyTo+1
                lda CurDeviceNo
                sta DiskToCopyTo
                cmp DiskToCopyFrom
                bne +
                +ShowMessage <Str_Mess_NoSameDisk, >Str_Mess_NoSameDisk
                rts
+               ; Check if there is enough space on disk
                ldx DiskToCopyTo+1
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
                lda StringListDrvLo,x
                sta $fb
                lda StringListDrvHi,x
                sta $fc
                stx dummy
                jsr LoadDir
                jsr ManipulStrList
                ldx dummy
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
                lda #GM_NORMAL
                sta GameMode
                rts

ActionCopyFile  lda #0
                sta CanCopy
                sta IsCut
                +SelectControl 1
                lda ControlHilIndex
                cmp ControlNumStr
                bcs +
                jsr GetFileName
                lda Str_FileType
                sta write_appendix+1
                ;
                jsr GetCurDeviceNo
                lda CurDeviceNo
                sta DiskToCopyFrom
                lda CurDeviceInd
                sta DiskToCopyFrom+1
                lda #1
                sta CanCopy
+               rts

ActionRenamDisk jsr GetCurDeviceNo
                ldx CurDeviceInd
                lda Str_Title_DrvLo,x
                sta $fb
                lda Str_Title_DrvHi,x
                sta $fc
                lda #2
                jsr AddToFB
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
                jsr GetFileName
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