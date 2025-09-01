CurDeviceInd    !byte 0; 0 for drive A, 1 for drive B
CurDeviceNo     !byte 8
;-------------------------------
bTooltipOn      !byte 0
dummy           !byte 0,0
Param           !byte 0,0
Clock           !byte 0,0,0,0
MayShowClock    !byte 1
StartBtnPushed  !byte 0
MenuItem        !byte 0
OldMenuItem     !byte 255
MayHighlight    !byte 0
MouseInfo       !byte 0,0,0,0,0;MouseInfo: xScr,yScr,x,y,xHiByte
ProgramMode     !byte PM_NORMAL; 0: normal, 1: menu, 255: dialog
exit_code       !byte 0
res             !byte 0 ;return value for various functions
DialogResult    !byte 0
ModalAddress    !byte 0,0
MapWidth        !byte 0
MapHeight       !byte 0
GapFrom         !byte 0
GapTo           !byte 0
WndAddressInBuf !byte 0,0
BufWidth        !byte 0
BufHeight       !byte 0
CurrentCursor   !byte 0
Point           !byte 0,0
MousePosInWndX  !byte 0
MousePosInWndY  !byte 0
StringWidth     !byte 0
StringHeight    !byte 0
ControlPressed  !byte 0
; Copy info --------------------
CanCopy         !byte 0
IsCut           !byte 0
DiskToCopyFrom  !byte 0,0 ;DevNo, Ind
DiskToCopyTo    !byte 0,0 ;DevNo, Ind
; FileName is in Str_FileName
FileSizeHex     !byte 0,0
; Task bar ---------------------
TaskBtnPos      !byte 0
TaskBtnWidth    !byte 0
TaskBtnPressed  !byte 0
;---- Window dragging-----------
MayDragWnd      !byte 0
IsWndDragging   !byte 0
DragType        !byte 0
DragWndAnchorX  !byte 0
DragWndAnchorY  !byte 0
OldPosX         !byte 0
NewPosX         !byte 0
NewPosY         !byte 0
NewHeight       !byte 0
;---- Changable values --------
CSTM_ActiveClr  !byte CL_ORANGE
CSTM_DeactiveClr!byte CL_MIDGRAY
CSTM_SelectClr  !byte CL_LIGHTGREEN
CSTM_MenuSelClr !byte CL_MIDGRAY
CSTM_WindowClr  !byte CL_LIGHTGRAY
CSTM_DesktopClr !byte CL_LIGHTBLUE
CSTM_DeskPattern!byte 0; 0 for solid, 1 for dotted
;---- Window data -------------
WindowOnHeap    !byte 0,0
; Complete window struct for current window
CurrentWindow   !byte 255 ; Index/handle of current window (0-15)
WindowType      !byte 0
WindowBitsEx    !byte 0; see constants for documentation
WindowBits      !byte 0; see constants for documentation
WindowPosX      !byte 0
WindowPosY      !byte 0
WindowWidth     !byte 0
WindowHeight    !byte 0
WindowTitleStr  !byte 0,0
WindowCtrlPtr   !byte 0,0
WindowNumCtrls  !byte 0
WindowFocCtrl   !byte 0
WindowProc      !byte 0,0
;------------------------------
AllocedWindows  !byte 0
MinableWindows  !byte 0
VisibleWindows  !byte 0
;------------------------------
window_counter  !byte 0
control_counter !byte 0
;---- Control data ------------
ControlOnHeap   !byte 0,0
; Complete control struct for current control
ControlIndex    !byte 0
ControlType     !byte 0
ControlColor    !byte 0
ControlPosX     !byte 0
ControlPosY     !byte 0
ControlWidth    !byte 0
ControlHeight   !byte 0
ControlBits     !byte 0; see constants for documentation
ControlHilIndex !byte 0
ControlTopIndex !byte 0
ControlNumStr   !byte 0
ControlStrings  !byte 0,0
ControlID       !byte 0
ControlBitsEx   !byte 0; see constants for documentation
;------------------------------
; Menu info
CurrentMenu     !byte 0,0 ; ptr to cur menu
CurMenuType     !byte 0
CurMenuID       !byte 0
CurMenuPosX     !byte 0
CurMenuPosY     !byte 0
CurMenuWidth    !byte 0
CurMenuHeight   !byte 0
CurMenuItem     !byte $ff
;------------------------------
EofWndHeap      !word WND_HEAP ; next free address on window heap
EofCtrlsHeap    !word CONTROL_HEAP ; next free address on controls heap
;------------------------------
; Tables
WndPriorityList !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
WndDefWidth     !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
WndDefHeight    !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ClrTabHi        !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
BufScrTabHi     !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07
BufClrTabHi     !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
TaskBtnWidths   !byte 0,11,11,10,7,6,5,4
TaskBtnHandles  !byte 0,0,0,0,0,0,0
; For drives A and B
StringListDrvLo !byte <STRING_LIST_DRIVEA, <STRING_LIST_DRIVEB
StringListDrvHi !byte >STRING_LIST_DRIVEA, >STRING_LIST_DRIVEB
Str_Title_DrvLo !byte <Str_Title_DrvA, <Str_Title_DrvB
Str_Title_DrvHi !byte >Str_Title_DrvA, >Str_Title_DrvB
BlocksFreeHexLo !byte 0,0
BlocksFreeHexHi !byte 0,0
DiskSizeHexLo   !byte 0,0
DiskSizeHexHi   !byte 0,0
WriteProtected  !byte 0,0
ShowFileSizes   !byte 0,0; BIT_EX_CTRL_SHOWSIZES, BIT_EX_CTRL_SHOWSIZES
ShowLowerCase   !byte 0,0
Max_Fn_Len_Plus2!byte 0,0; (maximal filename length) + 2
FileListBoxesLo !byte 0,0
FileListBoxesHi !byte 0,0
DrvSymLeft      !byte 41,2
DrvSymRight     !byte 37,3
DrvSymTop       !byte 39,117
DeviceNumbers   !byte 8,9
IsDiskDrive     !byte 1,1; 0 for non-disk drives (SD2IEC etc.)
IsDiskImage     !byte 0,0
DnpEndPosInPath !byte 0,0; if =0, then there is no dnp file in path
PathLength      !byte 0,0
NumStrings      !byte 0,0
; DiskInfo
Str_DriveType   !pet "0000","0000"
Str_DiskSize    !pet "0000","0000"
Str_Occupied    !pet "0000","0000"
Str_BlocksFree  !pet "1111","1111"
Str_NumFiles    !pet "2222","2222"
;------------------------------
; Per device no (0 - 29)
bMayRoot        !byte 0,0,0,0,0,0,0,0 ; 0 - 7 not used
                !byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
;------------------------------
; Custom error strings beyond 1-29 (BASIC errors)
CustomErrorsLO  !byte <Str_Err_WritProt, <Str_Err_NA
CustomErrorsHI  !byte >Str_Err_WritProt, >Str_Err_NA
; Variable strings
Str_FileName    !pet "0123456789abcdef",0
Str_FileType    !pet "x"
Str_FilenameEdit!pet "0123456789abcdef",0
Str_ImageEdit   !pet "0123456789abcdef",0