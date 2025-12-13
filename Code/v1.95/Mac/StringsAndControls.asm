;------------------------------
; Custom strings as PETSCII
; Lower/upper case
;Str_Mess_Error  !pet "Error code: xx\\Error message:\illegal device number",0
;Str_Mess_NoSameDisk !pet "Files cannot be copied\from a disk to itself.",0
;Str_Mess_SameDev!pet "Drives A and B cannot have\the same device number.",0
Str_Title_Settings    
                !pet "Settings",0
Str_Title_Viewer!pet "File View",0
Str_Settings_RBG!pet "Solid",0,"Dithered",0
Str_No          !pet "  No"
Str_Yes         !pet " Yes"
; Dialog titles
Str_Dlg_Info    !pet "Info",0
Str_Dlg_Delete  !pet "Delete",0
Str_Dlg_BASIC   !pet "BASIC",0
Str_Dlg_Clock   !pet "Clock",0
Str_Dlg_Ren_File!pet "Rename File",0
Str_Dlg_Ren_Disk!pet "Rename Disk",0
Str_Dlg_For_Disk!pet "Format Disk",0
Str_Dlg_DiskInfo!pet "Disk Info",0
Str_Dlg_CopyFile!pet "Copying File",0
Str_Dlg_Error   !pet "Disk Error",0
Str_Dlg_DevNo   !pet "Dev No",0
Str_Dlg_NewFile !pet "New Folder or Image",0
Str_Dlg_Load    !pet "Loading File",0
; Dialog labels
Str_Err_WritProt!pet "DISK WRITE PROTECTEd"; err code 30
Str_Err_NA      !pet "UNKNOWN ERROr"; err code 31
Str_Mess_Error  !pet "illegal device number",0
Str_Mess_NoSpace!pet "There is not enough space\on this disk.",0
Str_Mess_NoValidFileType !pet "File type not supported.",0
Str_Mess_NoNew  !pet "Not available in real\disk drives or images.",0
Str_Mess_Sure   !pet "Are you sure?",0
Str_Mess_OldFile!pet "Old filename:",0
Str_Mess_NewFile!pet "New filename:",0
Str_Mess_OldDisk!pet "Old diskname:",0
Str_Mess_NewDisk!pet "New diskname:",0
Str_Mess_GUI64  !pet 221,222,223," GUI64 v",VERSION
                !pet "\",224,225,226," by WebFritzi  \    ",96," 2025",0
Str_Dlg_For_RBG !pet "Fast format",0,"Full format",0
Str_TooManyFiles!pet "GUI64 only shows 255\of the xxx files.",0
StrLst_FileNew  !pet "Folder",0,"d64 Image",0,"d71 Image",0,"d81 Image",0,"dnp Image",0,0
Str_NewFile_Imgs!pet "    ",".d64",".d71",".d81",".dnp"
;------------------------------
; Custom strings as PETSCII
; Only upper case
Str_Title_DriveA!pet "a-"
Str_Title_DrvA  !pet "no disk         ",0
Str_Title_DriveB!pet "b-"
Str_Title_DrvB  !pet "no disk         ",0
Str_LoadingUC   !pet "loading...",0
Str_Disk_Error  !pet "disk error",0
Str_DirUp       !pet 0,0,"..",0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,"f"
;"f ..",1,"                  0",0,0,0,0,0,0,0,0

;==============================
; Window and controls data
;------------------------------
; Drive window
Wnd_DriveA      !byte WT_DRIVE_A, %00000111, 4, 1, 20, 7, <Str_Title_DriveA, >Str_Title_DriveA, <DriveWndProc, >DriveWndProc
Wnd_DriveB      !byte WT_DRIVE_B, %00000111, 6, 3, 20, 7, <Str_Title_DriveB, >Str_Title_DriveB, <DriveWndProc, >DriveWndProc
Ctrl_Drv_Menubar!byte CT_MENUBAR, 0, 0, 0, 0
                !pet 0
Ctrl_Drv_FLB    !byte CT_FILELISTSCROLLBOX, 0, 0, 24, 9
                !pet 0
;------------------------------
; Settings window
Wnd_Settings    !byte WT_SETTINGS, %00001100, 9, 2, 21, 20, <Str_Title_Settings, >Str_Title_Settings, <SettingsWndProc, >SettingsWndProc
                !byte CT_COLORPICKER, 17, 3, 2, 1
                !pet 0
                !byte CT_COLORPICKER, 17, 4, 2, 1
                !pet 0
                !byte CT_COLORPICKER, 17, 5, 2, 1
                !pet 0
                !byte CT_COLORPICKER, 17, 6, 2, 1
                !pet 0
                !byte CT_COLORPICKER, 17, 7, 2, 1
                !pet 0
                !byte CT_COLORPICKER, 17, 8, 2, 1
                !pet 0
                !byte CT_RADIOBUTTONGROUP, 2, 13, 15, 2
                !pet 0
                !byte CT_FRAME, 1, 1, 19, 9
                !pet "Colors",0
                !byte CT_LABEL_ML, 2, 3, 14, 1
                !pet "Window Title\Selection\Menu Selection\Window\Desktop\Button",0
                !byte CT_FRAME, 1, 11, 19, 5
                !pet "Desktop Pattern",0
                !byte CT_BUTTON, 7, 16, 9, 3
                !pet " Apply ",0
                !byte CT_BUTTON, 15, 16, 6, 3
                !pet " OK ",0
                ; Final zero byte
                !byte 0
;------------------------------
; File viewer window
Wnd_FileViewer  !byte WT_FILEVIEW, %00000011, 9, 3, 19, 14, <Str_Title_Viewer, >Str_Title_Viewer, <ViewerWndProc, >ViewerWndProc
                !byte CT_MENUBAR, 0, 0, 0, 0
                !pet 0
                !byte CT_TEXTVIEWBOX, 0, 0, 21, 12
                !pet 0
                ; Final zero byte
                !byte 0

;==============================
; Dialog and controls data
;------------------------------
;------------------------------
; Show Message
Wnd_Dlg_ShowMess!byte WT_DLG_INFO, %00001100, 1, 1, 1, 1, <Str_Dlg_Info, >Str_Dlg_Info, <MessageDlgProc, >MessageDlgProc
Ctrl_SM_OkBtn   !byte CT_BUTTON, 1, 1, 6, 3
                !pet " OK ",0
;------------------------------
; YesNo Dialog
Wnd_Dlg_YesNo   !byte WT_DLG_YESNO, %00001100, 1, 1, 1, 1, 0, 0, <YesNoDlgProc, >YesNoDlgProc
Ctrl_Dlg_Label  !byte CT_LABEL_ML, 1, 1, 1, 1
                !pet 0
Ctrl_YN_NoBtn   !byte CT_BUTTON, 1, 1, 6, 3
                !pet " No ",0
Ctrl_YN_YesBtn  !byte CT_BUTTON, 1, 1, 7, 3
                !pet " Yes ",0
;------------------------------
; Load Dialog
Wnd_Dlg_Load    !byte WT_DLG_LOAD, %00001100, 10, 7, 20, 7, <Str_Dlg_Load, >Str_Dlg_Load, <EmptyWndProc, >EmptyWndProc
                !byte CT_LABEL, 1, 1, 16, 1
                !pet 0
                !byte CT_PROGRESSBAR, 1, 3, 18, 1, 0
                ; Final zero byte
                !byte 0
;------------------------------
; Disk Info Dialog
Wnd_Dlg_DiskInfo!byte WT_DLG_DISKINFO, %00001100, 10, 3, 20, 14, <Str_Dlg_DiskInfo, >Str_Dlg_DiskInfo, <DiskInfoDlgProc, >DiskInfoDlgProc
                !byte CT_PROGRESSBAR, 1, 1, 18, 1, 0
                !byte CT_LABEL_ML, 1, 7, 13, 1
                !pet "Files\Drive Type\Write protect",0
                !byte CT_LABEL, 1, 3, 13, 1
                !pet "Size (blocks)",0
                !byte CT_COLBOXLABEL, 1, 4, 11, 1
                !pet "Occupied",0                
                !byte CT_COLBOXLABEL, 1, 5, 11, 1
                !pet "Available",0
Ctrl_DI_Label6  !byte CT_LABEL, 15, 8, 4, 1; Type
                !pet "xxxx",0
Ctrl_DI_Label7  !byte CT_LABEL, 15, 9, 3, 1; Write prot
                !pet "xxxx",0
Ctrl_DI_Label8  !byte CT_LABEL, 15, 7, 4, 1; Files
                !pet "xxxx",0
Ctrl_DI_Label9  !byte CT_LABEL, 15, 3, 4, 1; Size
                !pet "xxxx",0
Ctrl_DI_Label10 !byte CT_LABEL, 15, 4, 4, 1; Occupied
                !pet "xxxx",0
Ctrl_DI_Label11 !byte CT_LABEL, 15, 5, 4, 1; Available
                !pet "xxxx",0
                !byte CT_BUTTON, 12, 10, 8, 3
                !pet "  OK  ",0
                ; Final zero byte
                !byte 0
;------------------------------
; Format disk Dialog
Wnd_Dlg_Format  !byte WT_DLG_FORMAT, %00001100, 10, 4, 21, 12, <Str_Dlg_For_Disk, >Str_Dlg_For_Disk, <FormatDlgProc, >FormatDlgProc
                !byte CT_LABEL, 2, 1, 9, 1
                !pet "Diskname:",0
                !byte CT_EDIT_SL, 1, 2, 19, 3
                !pet 0
                !byte CT_RADIOBUTTONGROUP, 2, 5, 13, 2
                !pet 0
                !byte CT_BUTTON, 5, 7, 10, 3
                !pet " Cancel ",0
                !byte CT_BUTTON, 14, 7, 6, 3
                !pet " OK ",0
                ; Final zero byte
                !byte 0
;------------------------------
; Rename Dialog
Wnd_Dlg_Rename  !byte WT_DLG_RENAME, %00001100, 10, 4, 21, 14, 0, 0, <RenameDlgProc, >RenameDlgProc
                !byte CT_LABEL, 1, 1, 13, 1
                !pet 0
                !byte CT_LABEL, 2, 3, 16, 1
                !pet 0
                !byte CT_LABEL, 1, 5, 13, 1
                !pet 0
                !byte CT_EDIT_SL, 1, 6, 19, 3
                !pet 0
                !byte CT_BUTTON, 5, 9, 10, 3
                !pet " Cancel ",0
                !byte CT_BUTTON, 14, 9, 6, 3
                !pet " OK ",0
                ; Final zero byte
                !byte 0
;------------------------------
; Copy File Dialog
Wnd_Dlg_CopyFile!byte WT_DLG_COPYFILE, %00001100, 11, 4, 18, 9, <Str_Dlg_CopyFile, >Str_Dlg_CopyFile, <CopyFileDlgProc, >CopyFileDlgProc
                !byte CT_PROGRESSBAR, 1, 1, 16, 1, 0
                !byte CT_LABEL, 1, 3, 16, 1
                !pet  0
Ctrl_CF_Label2  !byte CT_LABEL, 1, 5, 13, 1
                !pet  "From A to B",0; 10 and 15
                ; Final zero byte
                !byte 0
;------------------------------
; Device No Dialog
Wnd_Dlg_DevNo   !byte WT_DLG_DEVNO, %00001100, 27, 0, 10, 6, <Str_Dlg_DevNo, >Str_Dlg_DevNo, <DevNoDlgProc, >DevNoDlgProc
                !byte CT_LABEL, 2, 1, 2, 1
Ctrl_DN_DevInd  !pet  "A:",0
                !byte CT_UPDOWN, 4, 0, 5, 3
                !pet 0
                !byte CT_BUTTON, 2, 2, 6, 3
                !pet " OK ",0
                ; Final zero byte
                !byte 0
;------------------------------
; New File Dialog
Wnd_Dlg_NewFile !byte WT_DLG_NEWFILE, %00001100, 7, 2, 27, 15, <Str_Dlg_NewFile, >Str_Dlg_NewFile, <NewFileDlgProc, >NewFileDlgProc
                !byte CT_LABEL, 1, 2, 10, 1
                !pet "Name",0
                !byte CT_EDIT_SL, 5, 1, 21, 3;15
                !pet 0
                !byte CT_LABEL, 1, 5, 10, 1
                !pet "Type",0
                !byte CT_LISTBOX, 6, 5, 11, 7
                !pet 0
                !byte CT_BUTTON, 18, 5, 8, 3
                !pet "  OK  ",0
                !byte CT_BUTTON, 18, 8, 8, 3
                !pet "Cancel",0
                !byte CT_LABEL, 20, 2, 4, 1
Ctrl_NF_ImgType !pet ".d64",0
                ; Final zero byte
                !byte 0
;------------------------------
; Clock Dialog
Wnd_Dlg_Clock   !byte WT_DLG_CLOCK, %00001100, 31,0,9,6, <Str_Dlg_Clock, >Str_Dlg_Clock, <ClockDlgProc, >ClockDlgProc
                !byte CT_UPDOWN, 0, 0, 5, 3
                !pet 0
                !byte CT_UPDOWN, 4, 0, 5, 3
                !pet 0
                !byte CT_BUTTON, 0, 2, 9, 3
                !pet "  Set  ",0
                ; Final zero byte
                !byte 0

;===================================================
; Menus
; Format: ID, max_str_len, item_count, StringList
Menu_System     !pet ID_MENU_SYSTEM,8,3,"Settings",0,"About",0,"BASIC",0
Menu_ColorPicker!pet ID_MENU_COLORPICKER,2,16,"0",0,"1",0,"2",0,"3",0,"4",0,"5",0,"6",0,"7",0,"8",0,"9",0,"A",0,"B",0,"C",0,"D",0,"E",0,"F",0
; Drive Window
Str_DriveMenubar!pet "Disk",0,"File",0,"View",0
Menu_Drive_Disk !pet ID_MENU_DISK,9,6,"Refresh",0,"Device No",0,"Info",0,"Format",0,"Rename",0,"Close",0
Menu_Drive_File !pet ID_MENU_FILE,6,8,"New",0,"Cut",0,"Copy",0,"Paste",0,"Delete",0,"Rename",0,"View",0,"Boot",0
Menu_Drive_View !pet ID_MENU_OPTS,13,5," File Sizes",0," Lower Case",0," Sort by Name",0," Sort by Type",0," Sort by Size",0
                ;!pet 230,230,230,230,230,230,230,230,230,230,230,230,230,0," GUI64 Info",0
; Viewer Window
Str_ViewMenubar !pet "View As",0
Menu_View_File  !pet ID_MENU_FILE,8,4,"Text ABC",0,"Text abc",0,"Hex",0,"Close",0
;--------------------
; Menu bars (menu lists)
DriveMenubar    !word Menu_Drive_Disk, Menu_Drive_File, Menu_Drive_View
ViewerMenubar   !word Menu_View_File