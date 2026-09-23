;CurDeviceInd    !byte 0; 0 for drive A, 1 for drive B (ZP)
;CurDeviceNo     !byte 8 ZP
;-------------------------------
bTooltipOn      !byte 0
bIsUltimate     !byte 0
;Clock           !byte 0,0,0,0 ZP
StartBtnPushed  !byte 0
;MenuItem        !byte 0 ZP
OldMenuItem     !byte 255
MayHighlight    !byte 0
;MouseInfo       !byte 0,0,0,0,0;MouseInfo: xScr,yScr,x,y,xHiByte (ZP)
;ProgramMode     !byte PM_NORMAL; 0: normal, 1: menu, 255: dialog (ZP)
;exit_code       !byte 0 ZP
exit_code2      !byte 0
DialogResult    !byte 0
ModalAddress    !byte 0,0
MapWidth        !byte 0
MapHeight       !byte 0
GapFrom         !byte 0
GapTo           !byte 0
;WndAddressInBuf !byte 0,0 ZP
;BufWidth        !byte 0 ZP
;BufHeight       !byte 0 ZP
CurrentCursor   !byte 0
Point           !byte 0,0
;MousePosInWndX  !byte 0 ZP
;MousePosInWndY  !byte 0 ZP
;StringWidth     !byte 0 ZP
StringHeight    !byte 0
;ControlPressed  !byte 0 ZP
; Copy info --------------------
;CanCopy         !byte 0 ZP
IsCut           !byte 0
;DiskToCopyFrom  !byte 0,0 ;DevNo, Ind ZP
DiskToCopyTo    !byte 0,0 ;DevNo, Ind
; FileName is in Str_FileName
;FileSizeHex     !byte 0,0 ZP
; Task bar ---------------------
TaskBtnPos      !byte 0
;TaskBtnWidth    !byte 0 ZP
TaskBtnPressed  !byte 0
;---- CBM Menu -----------------
CbmMenuWidth = 10
CbmMenuItems    !byte 4
;CbmMenuStrLen   !byte 8
;CbmMenuWidth    !byte 0 ;= CbmMenuStrLen + 2
CbmMenuHeight   !byte 0 ;= 2*CbmMenuItems+1
;---- Dragging -----------------
;MayDrag         !byte 0 ZP
IsDragging      !byte 0
DragType        !byte 0; 0: drag object, 1: resize object
DragAnchorX     !byte 0
DragAnchorY     !byte 0
DragOldPosX     !byte 0
DragOldPosY     !byte 0
;DragNewPosX     !byte 0 ZP
;DragNewPosY     !byte 0 ZP
DragObjWidthMinus1  !byte 0
DragObjHeightMinus1 !byte 0
DragObjType     !byte 0; DOT_WINDOW: 0, DOT_ICON: 1
;---- Icons -------------------
; All icons have a width of 4 and a height of 3
IconAvailable   !byte 1,1,0
CurrentIcon     !byte 0; only used for dragging
DrvSymLeft      !byte 41,2; depend on CSTM_DeskPattern
DrvSymRight     !byte 37,3
DrvSymTop       !byte 39,117
;---- Settings values ---------
SIZE_OF_SETTINGS = 15
CSTM_Icons      !byte 0,0; Drive A
                !byte 0,3; Drive B
                !byte 0,6; Ultimate
CSTM_ActiveClr  !byte CL_ORANGE
CSTM_DeactiveClr!byte CL_MIDGRAY
CSTM_SelectClr  !byte CL_LIGHTGREEN
CSTM_MenuSelClr !byte CL_MIDGRAY
CSTM_WindowClr  !byte CL_LIGHTGRAY
CSTM_DesktopClr !byte CL_LIGHTBLUE
CSTM_DeskPattern!byte 0; 0 for solid, 1 for dotted
CSTM_DevNumbers !byte 8,9
;------------------------------
;; Complete window struct for current window
;CurrentWindow   !byte 255 ; Index/handle of current window (0-15)
;WindowType      !byte 0
;WindowBitsEx    !byte 0; see constants for documentation
;WindowBits      !byte 0; see constants for documentation
;WindowPosX      !byte 0
;WindowPosY      !byte 0
;WindowWidth     !byte 0
;WindowHeight    !byte 0
;WindowTitleStr  !byte 0,0
;WindowCtrlPtr   !byte 0,0
;WindowNumCtrls  !byte 0
;WindowFocCtrl   !byte 0
;WindowProc      !byte 0,0
;------------------------------
;; Complete control struct for current control
;ControlIndex    !byte 0
;ControlType     !byte 0
;ControlColor    !byte 0
;ControlPosX     !byte 0
;ControlPosY     !byte 0
;ControlWidth    !byte 0
;ControlHeight   !byte 0
;ControlBits     !byte 0; see constants for documentation
;ControlHilIndex !byte 0
;ControlTopIndex !byte 0
;ControlNumStr   !byte 0
;ControlStrings  !byte 0,0
;ControlID       !byte 0
;ControlBitsEx   !byte 0; see constants for documentation
;------------------------------
;; Window and control data
;WindowOnHeap    !byte 0,0 ZP
;ControlOnHeap   !byte 0,0 ZP
;window_counter  !byte 0 ZP
control_counter !byte 0
;AllocedWindows  !byte 0 ZP
;MinableWindows  !byte 0 ZP
;VisibleWindows  !byte 0 ZP
; Menu info
CurMenuType     !byte 0
CurMenuID       !byte 0
;CurMenuPosX     !byte 0 ZP
;CurMenuPosY     !byte 0 ZP
CurMenuWidth    !byte 0
;CurMenuHeight   !byte 0 ZP
;CurMenuItem     !byte $ff
;------------------------------
;EofWndHeap      !word WND_HEAP ; next free address on window heap
EofCtrlsHeap    !word CONTROL_HEAP ; next free address on controls heap
;------------------------------
; Tables
WndPriorityList !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
TaskBtnWidths   !byte 0,11,11,10,7,6,5,4
WndDefWidth     !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
WndDefHeight    !byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
TaskBtnHandles  !byte 0,0,0,0,0,0,0

ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte >(SCRMEM+0*40),>(SCRMEM+1*40),>(SCRMEM+2*40),>(SCRMEM+3*40),>(SCRMEM+4*40)
                !byte >(SCRMEM+5*40),>(SCRMEM+6*40),>(SCRMEM+7*40),>(SCRMEM+8*40),>(SCRMEM+9*40)
                !byte >(SCRMEM+10*40),>(SCRMEM+11*40),>(SCRMEM+12*40),>(SCRMEM+13*40),>(SCRMEM+14*40)
                !byte >(SCRMEM+15*40),>(SCRMEM+16*40),>(SCRMEM+17*40),>(SCRMEM+18*40),>(SCRMEM+19*40)
                !byte >(SCRMEM+20*40),>(SCRMEM+21*40),>(SCRMEM+22*40),>(SCRMEM+23*40),>(SCRMEM+24*40)
;ClrTabHi        !byte $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d9,$d9,$d9,$d9,$d9
;                !byte $d9,$da,$da,$da,$da,$da,$da,$da,$db,$db,$db,$db,$db
;BufScrTabHi     !byte >(DESKTOP_BUF+0*40),>(DESKTOP_BUF+1*40),>(DESKTOP_BUF+2*40),>(DESKTOP_BUF+3*40),>(DESKTOP_BUF+4*40)
;                !byte >(DESKTOP_BUF+5*40),>(DESKTOP_BUF+6*40),>(DESKTOP_BUF+7*40),>(DESKTOP_BUF+8*40),>(DESKTOP_BUF+9*40)
;                !byte >(DESKTOP_BUF+10*40),>(DESKTOP_BUF+11*40),>(DESKTOP_BUF+12*40),>(DESKTOP_BUF+13*40),>(DESKTOP_BUF+14*40)
;                !byte >(DESKTOP_BUF+15*40),>(DESKTOP_BUF+16*40),>(DESKTOP_BUF+17*40),>(DESKTOP_BUF+18*40),>(DESKTOP_BUF+19*40)
;                !byte >(DESKTOP_BUF+20*40),>(DESKTOP_BUF+21*40),>(DESKTOP_BUF+22*40),>(DESKTOP_BUF+23*40),>(DESKTOP_BUF+24*40)

; Defines 4 hourglass states (2 chars per state)
HourglassCharTab!byte $ff,$ff,$8f,$ff,$7e,$7e,$3c,$18
                !byte $18,$24,$42,$42,$81,$81,$ff,$ff
                !byte $ff,$ff,$81,$87,$7e,$7e,$3c,$18
                !byte $18,$24,$42,$42,$81,$bd,$ff,$ff
                !byte $ff,$ff,$81,$81,$42,$7e,$3c,$18
                !byte $18,$24,$42,$42,$bd,$ff,$ff,$ff
                !byte $ff,$ff,$81,$81,$42,$42,$24,$18
                !byte $18,$24,$42,$7e,$ff,$ff,$ff,$ff

; For drives A and B
;StringListDrvLo !byte <STRING_LIST_DRIVEA, <STRING_LIST_DRIVEB
StringListDrvHi !byte >STRING_LIST_DRIVEA, >STRING_LIST_DRIVEB
Str_Title_DrvLo !byte <Str_Title_DrvA, <Str_Title_DrvB
Str_Title_DrvHi !byte >Str_Title_DrvA, >Str_Title_DrvB
BlocksFreeHexLo !byte 0,0
BlocksFreeHexHi !byte 0,0
DiskSizeHexLo   !byte 0,0
DiskSizeHexHi   !byte 0,0
DriveType       !byte 0,0 ; 0: n.a., 1: foreign, 2: 1541, 3: 1571, 4: 1581, 5: FD, 6: HD, 7: RD, 8: RAMLink
WriteProtected  !byte 0,0
ShowFileSizes   !byte 0,0; BIT_EX_CTRL_SHOWSIZES, BIT_EX_CTRL_SHOWSIZES
ShowLowerCase   !byte 0,0,1
Max_Fn_Len_Plus2!byte 0,0,0; (maximal filename length) + 2
FileListBoxesLo !byte 0,0
FileListBoxesHi !byte 0,0
IsDiskDrive     !byte 1,1; 0 for non-disk drives (SD2IEC etc.)
IsDiskImage     !byte 0,0,0
DnpEndPosInPath !byte 0,0; if =0, then there is no dnp file in path
;PathLength      !byte 0,0 ZP
NumStrings      !byte 0,0
DiskHasError    !byte 0,0
; DiskInfo (keep order!!!)
Str_DriveType   !pet "0000","0000"
Str_DiskSize    !pet "0000","0000"
Str_Occupied    !pet "0000","0000"
Str_BlocksFree  !pet "1111","1111"
Str_NumFiles    !pet "2222","2222"
;------------------------------
; Per device no (0 - 29)
bMayRoot        !byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
;------------------------------
; Variable strings
Str_FileName    !pet "0123456789abcdef",0
Str_FileType    !pet "x"
Str_DialogEdit  !pet "0123456789abcdef",0