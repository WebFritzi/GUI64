;------------------------------
; Custom strings as PETSCII
; Lower/upper case
Str_Settings    !pet "Settings",0
Str_Settings_RBG!pet "Solid",0,"Dotted",0
Str_DriveMenubar!pet "Disk",0,"File",0
Str_No          !pet " No"
Str_Yes         !pet "Yes"
; Dialog titles
Str_Dlg_Info    !pet "Information",0
Str_Dlg_Delete  !pet "Delete",0
Str_Dlg_Reset   !pet "Reset",0
Str_Dlg_Clock   !pet "Clock",0
Str_Dlg_Ren_File!pet "Rename File",0
Str_Dlg_Ren_Disk!pet "Rename Disk",0
Str_Dlg_For_Disk!pet "Format Disk",0
Str_Dlg_DiskInfo!pet "Disk Info",0
Str_Dlg_CopyFile!pet "Copy File",0
Str_Dlg_Error   !pet "Disk Error",0
; Dialog labels
Str_Err_WritProt!pet "DISK WRITE PROTECTEd"; err code 30
Str_Err_NA      !pet "NOT AVAILABLe"; err code 31
Str_Mess_Error  !pet "Error code: xx\\Error message:\illegal device number",0
Str_Mess_NoSpace!pet "There is not enough space\on this disk.",0
Str_Mess_NoSameDisk !pet "Files cannot be copied\from a disk to itself.",0
Str_Mess_MaxWnd !pet "The number of minimizable\windows is limited to 7.",0
Str_Mess_Sure   !pet "Are you sure?",0
Str_Mess_OldFile!pet "Old filename:",0
Str_Mess_NewFile!pet "New filename:",0
Str_Mess_OldDisk!pet "Old diskname:",0
Str_Mess_NewDisk!pet "New diskname:",0
Str_Dlg_For_RBG !pet "Fast format",0,"Full format",0
Str_Loading     !pet "Loading...",0
;------------------------------
; Custom strings as PETSCII
; Only upper case
Str_Title_Drive8!pet "8-no disk         ",0
Str_Title_Drive9!pet "9-no disk         ",0
Str_LoadingUC   !pet "loading...      ",0
;==============================
; Window and controls data
;------------------------------
; Drive window
Wnd_Drive8      !byte WT_DRIVE_8, %00110110, 6, 1, 29, 7, <Str_Title_Drive8, >Str_Title_Drive8, <DriveWndProc, >DriveWndProc
Wnd_Drive9      !byte WT_DRIVE_9, %00110110, 9, 4, 29, 7, <Str_Title_Drive9, >Str_Title_Drive9, <DriveWndProc, >DriveWndProc
Ctrl_Drv_Menubar!byte CT_MENUBAR, 0, 0, 0, 0
                !pet 0
Ctrl_Drv_FLB    !byte CT_FILELISTSCROLLBOX, 0, 0, 27, 9
                !pet 0
;------------------------------
; Settings window
Wnd_Settings    !byte WT_SETTINGS, %00101100, 9, 1, 21, 20, <Str_Settings, >Str_Settings, <SettingsWndProc, >SettingsWndProc
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
                !byte CT_RADIOBUTTONGROUP, 2, 12, 15, 2
                !pet 0
                !byte CT_FRAME, 1, 1, 19, 8
                !pet "Colors",0
                !byte CT_LABEL, 2, 3, 12, 1
                !pet "Title (act.)",0
                !byte CT_LABEL, 2, 4, 14, 1
                !pet "Title (inact.)",0
                !byte CT_LABEL, 2, 5, 14, 1
                !pet "Selection",0
                !byte CT_LABEL, 2, 6, 6, 1
                !pet "Window",0
                !byte CT_LABEL, 2, 7, 7, 1
                !pet "Desktop",0
                !byte CT_FRAME, 1, 10, 19, 5
                !pet "Desktop Pattern",0
                !byte CT_BUTTON, 8, 15, 7, 3
                !pet "Apply",0
                !byte CT_BUTTON, 16, 15, 4, 3
                !pet "OK",0
                ; Final zero byte
                !byte 0
;==============================
; Dialog and controls data
;------------------------------
;------------------------------
; Show Message
Wnd_Dlg_ShowMess!byte WT_DLG_INFO, %00001100, 1, 1, 1, 1, <Str_Dlg_Info, >Str_Dlg_Info, <MessageDlgProc, >MessageDlgProc
                !byte CT_LABEL_ML, 1, 1, 1, 1
                !pet 0
Ctrl_SM_OkBtn   !byte CT_BUTTON, 1, 1, 4, 3
                !pet "OK",0
;------------------------------
; YesNo Dialog
Wnd_Dlg_YesNo   !byte WT_DLG_YESNO, %00001100, 1, 1, 1, 1, 0, 0, <YesNoDlgProc, >YesNoDlgProc
Ctrl_Dlg_Label  !byte CT_LABEL_ML, 1, 1, 1, 1
                !pet 0
Ctrl_YN_NoBtn   !byte CT_BUTTON, 1, 1, 4, 3
                !pet "No",0
Ctrl_YN_YesBtn  !byte CT_BUTTON, 1, 1, 5, 3
                !pet "Yes",0
;------------------------------
; Disk Info Dialog
Wnd_Dlg_DiskInfo!byte WT_DLG_DISKINFO, %00001100, 10, 3, 20, 15, <Str_Dlg_DiskInfo, >Str_Dlg_DiskInfo, <DiskInfoDlgProc, >DiskInfoDlgProc
                !byte CT_PROGRESSBAR, 1, 1, 18, 1, 0
                !byte CT_LABEL, 1, 8, 13, 1
                !pet "Drive Type",0
                !byte CT_LABEL, 1, 9, 13, 1
                !pet "Write protect",0
                !byte CT_LABEL, 1, 7, 13, 1
                !pet "Files",0
                !byte CT_LABEL, 1, 3, 13, 1
                !pet "Size (blocks)",0
                !byte CT_COLBOXLABEL, 1, 4, 11, 1
                !pet "Occupied",0                
                !byte CT_COLBOXLABEL, 1, 5, 11, 1
                !pet "Available",0
Ctrl_DI_Label6  !byte CT_LABEL, 15, 8, 4, 1; Type
                !pet "xxxx",0
Ctrl_DI_Label7  !byte CT_LABEL, 16, 9, 3, 1; Write prot
                !pet "xxx",0
Ctrl_DI_Label8  !byte CT_LABEL, 15, 7, 4, 1; Files
                !pet "xxxx",0
Ctrl_DI_Label9  !byte CT_LABEL, 15, 3, 4, 1; Size
                !pet "xxxx",0
Ctrl_DI_Label10 !byte CT_LABEL, 15, 4, 4, 1; Occupied
                !pet "xxxx",0
Ctrl_DI_Label11 !byte CT_LABEL, 15, 5, 4, 1; Available
                !pet "xxxx",0
                !byte CT_BUTTON, 13, 10, 6, 3
                !pet " OK ",0
                ; Final zero byte
                !byte 0
;------------------------------
; Format disk Dialog
Wnd_Dlg_Format  !byte WT_DLG_FORMAT, %00001100, 10, 5, 21, 13, <Str_Dlg_For_Disk, >Str_Dlg_For_Disk, <FormatDlgProc, >FormatDlgProc
                !byte CT_LABEL, 2, 1, 9, 1
                !pet "Diskname:",0
                !byte CT_EDIT_SL, 1, 2, 19, 3
                !pet 0
                !byte CT_RADIOBUTTONGROUP, 2, 5, 13, 2
                !pet 0
                !byte CT_BUTTON, 6, 8, 8, 3
                !pet "Cancel",0
                !byte CT_BUTTON, 15, 8, 4, 3
                !pet "OK",0
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
                !byte CT_BUTTON, 6, 9, 8, 3
                !pet "Cancel",0
                !byte CT_BUTTON, 15, 9, 4, 3
                !pet "OK",0
                ; Final zero byte
                !byte 0
;------------------------------
; Copy File Dialog
Wnd_Dlg_CopyFile!byte WT_DLG_COPYFILE, %00001100, 11, 4, 18, 9, <Str_Dlg_CopyFile, >Str_Dlg_CopyFile, <CopyFileDlgProc, >CopyFileDlgProc
                !byte CT_PROGRESSBAR, 1, 1, 16, 1, 0
                !byte CT_LABEL, 1, 3, 16, 1
                !pet  0
Ctrl_CF_Label2  !byte CT_LABEL, 1, 5, 13, 1
                !pet  "From #8 to #9",0; 11 and 17
                ; Final zero byte
                !byte 0
;------------------------------
; Clock Dialog
Wnd_Dlg_Clock   !byte WT_DLG_CLOCK, %00001100, 31,13,9,9, <Str_Dlg_Clock, >Str_Dlg_Clock, <ClockDlgProc, >ClockDlgProc
                !byte CT_UPDOWN, 0, 1, 5, 3
                !pet 0
                !byte CT_UPDOWN, 4, 1, 5, 3
                !pet 0
                !byte CT_BUTTON, 1, 4, 7, 3
                !pet " Set ",0
                ; Final zero byte
                !byte 0

; Menus
; Format: ID, max_str_len, item_count, StringList
Menu_Start      !pet ID_MENU_START,8,3,"Settings",0," ",0,"Reset   ",0
Menu_ColorPicker!pet ID_MENU_COLORPICKER,2,16,"0",0,"1",0,"2",0,"3",0,"4",0,"5",0,"6",0,"7",0,"8",0,"9",0,"A",0,"B",0,"C",0,"D",0,"E",0,"F",0
Menu_Disk       !pet ID_MENU_DISK,7,5,"Refresh",0,"Info",0,"Format",0,"Rename",0,"Close",0
Menu_File       !pet ID_MENU_FILE,6,7,"Cut",0,"Copy",0,"Paste",0,"Delete",0,"Rename",0,"Run",0, "Boot",0
; Menu lists
DriveMenus      !word Menu_Disk, Menu_File