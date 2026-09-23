;------------------------------------------
; File gui64.inc
; Routines and constants for apps in GUI64
; WebFritzi, 22/09/2026
;------------------------------------------

; GUI64 Jump table
GUI_CreateWindow                = $5f00
GUI_CreateWindowEx              = $5f03
GUI_KillCurWindow               = $5f06
GUI_StdWndProc                  = $5f09
GUI_IsInCurControl              = $5f0c
GUI_Repaint                     = $5f0f
GUI_GetDesign                   = $5f12
GUI_UpdateControl               = $5f15
GUI_SelectCtrl_AddBits          = $5f18
GUI_SetCtrlStringList           = $5f1b
GUI_SetCtrlString               = $5f1e
GUI_SelectControl               = $5f21
GUI_SelectControl0              = $5f24
GUI_SelectControl1              = $5f27
GUI_SelectControl2              = $5f2a
GUI_SelectControl3              = $5f2d
GUI_SelectControl4              = $5f30
GUI_UpdateWindow                = $5f33
GUI_SetEditSLInfo               = $5f36
GUI_RegisterChars               = $5f39
GUI_SetCtrlActionsRoutine       = $5f3c
GUI_SetPaintCtrlsRoutine        = $5f3f
GUI_AddBufWidthToFD             = $5f42
GUI_AddBufWidthTo02             = $5f45
GUI_IsInCurMenu                 = $5f48
GUI_GetCurMenuID                = $5f4b
GUI_ShowMessage                 = $5f4e
GUI_GetMousePosInWnd            = $5f51
GUI_GetCSTMWindowColor          = $5f54
GUI_GetCSTMDesktopColor         = $5f57
GUI_StartTimer                  = $5f5a
GUI_StopTimer                   = $5f5d
GUI_InitTimer                   = $5f60
GUI_RepaintCurWindow            = $5f63
GUI_FindWndByType               = $5f66
GUI_SelectTopWindow             = $5f69
GUI_AddControl                  = $5f6c
;----------------------------------------------------------------
; Zero page
; Complete window structure for current window-------------------
CurrentWindow       = $10; Index/handle of current window (0-15)
WindowType          = $11
WindowBitsEx        = $12; do not use
WindowBits          = $13; see below for documentation
WindowPosX          = $14
WindowPosY          = $15
WindowWidth         = $16
WindowHeight        = $17
WindowTitleStr      = $18; and $19
WindowCtrlPtr       = $1a; and $1b
WindowNumCtrls      = $1c
WindowFocCtrl       = $1d
WindowProc          = $1e; and $1f
; Complete control structure for current control-----------------
ControlIndex        = $20
ControlType         = $21
ControlColor        = $22
ControlPosX         = $23
ControlPosY         = $24
ControlWidth        = $25
ControlHeight       = $26
ControlBits         = $27; see below for documentation
ControlHilIndex     = $28
ControlTopIndex     = $29
ControlNumStr       = $2a
ControlStrings      = $2b; and $2c
                     ;$2d depends on control
ControlBitsEx       = $2e; see below for documentation
                     ;$2d depends on control

; Add to ControlIndex for some controls
; For UpDown control (decimal digits)
UPDOWN_LOWERLIMIT   = 10
UPDOWN_UPPERLIMIT   = 11
UPDOWN_DIGIT_LO     = 12
UPDOWN_DIGIT_HI     = 13
; For Progressbar
PROGBAR_MAX_LO      = 10
PROGBAR_MAX_HI      = 11
PROGBAR_VAL_LO      = 12
PROGBAR_VAL_HI      = 13
; For Edit_SL control
EDITSL_CARRETPOS    = 9
EDITSL_MAX_STRLEN   = 10
EDITSL_FORBIDDEN    = 14; ptr to forbidden chars

; Zero page -----------------------------------------------------
ProgramMode         = $40; 0: normal, 1: menu, 255: dialog
MousePosInWndX      = $51
MousePosInWndY      = $52
actkey              = $53
wndParam0           = $5b; former "wndParam"
wndParam1           = $5c; former "wndParam+1"
Param0              = $5d
Param1              = $5e
; Frequently used variables -------------------------------------
key_shifted         = $6b
CurMenuItem         = $87
;----------------------------------------------------------------
; Free zero page addresses
ZP_0E               = $0e
ZP_0F               = $0f
ZP_5F               = $5f
ZP_60               = $60
ZP_FB               = $fb
ZP_FC               = $fc
ZP_FD               = $fd
ZP_FE               = $fe
;----------------------------------------------------------------

; Window Bits
BIT_WND_HASMENU         = %00000001
BIT_WND_RESIZABLE       = %00000010
BIT_WND_FIXEDWIDTH      = %00000100
BIT_WND_FIXEDHEIGHT     = %00001000
BIT_WND_CANMAXIMIZE     = %00010000; WIN only
BIT_WND_CANMINIMIZE     = %00100000; WIN only
BIT_WND_ISMINIMIZED     = %01000000
BIT_WND_ISMAXIMIZED     = %10000000; WIN only

; Control Bits
BIT_CTRL_ISMAXIMIZED    = %00000001
BIT_CTRL_ISPRESSED      = %00000010
BIT_CTRL_UPPERCASE      = %00000100
BIT_CTRL_DBLFRAME_TOP   = %00001000
BIT_CTRL_DBLFRAME_BTM   = %00010000
BIT_CTRL_DBLFRAME_RGT   = %00100000
BIT_CTRL_DBLFRAME_LFT   = %01000000
; Extended control bits
BIT_EX_CTRL_NOFRAME_TOP = %00000001
BIT_EX_CTRL_NOFRAME_BTM = %00000010

; Control Types
; Controls with action
CT_MENUBAR           = 1
CT_BUTTON            = 2
CT_LISTBOX           = 3
CT_FILELISTSCROLLBOX = 4
CT_COLORPICKER       = 5
CT_RADIOBUTTONGROUP  = 6
CT_UPDOWN            = 7
CT_EDIT_SL           = 8
CT_TEXTVIEWBOX       = 9
CT_CHECKBOX          = 10
; No action
CT_LABEL             = 31
CT_LABEL_ML          = 32
CT_FRAME             = 33
CT_PROGRESSBAR       = 34
CT_COLBOXLABEL       = 35

; Event codes
EC_RBTNPRESS         = 1
EC_RBTNRELEASE       = 2
EC_LBTNPRESS         = 3
EC_LBTNRELEASE       = 4
EC_MOUSEMOVE         = 5
EC_DBLCLICK          = 7
EC_SCROLLWHEELDOWN   = 8
EC_SCROLLWHEELUP     = 9
EC_KEYPRESS          = 10

; Program Modes
PM_NORMAL            = 0
PM_MENU              = 1
PM_DIALOG            = $ff

; App char indices
APP_CHAR_0           = 224
APP_CHAR_1           = 225
APP_CHAR_2           = 226
APP_CHAR_3           = 227
APP_CHAR_4           = 228
APP_CHAR_5           = 229
APP_CHAR_6           = 230
APP_CHAR_7           = 231
APP_CHAR_8           = 232
APP_CHAR_9           = 233
APP_CHAR_10          = 234
APP_CHAR_11          = 235
APP_CHAR_12          = 236
APP_CHAR_13          = 237
APP_CHAR_14          = 238
APP_CHAR_15          = 239

; All C64 colors
CL_BLACK             = 0
CL_WHITE             = 1
CL_RED               = 2
CL_CYAN              = 3
CL_MAGENTA           = 4
CL_DARKGREEN         = 5
CL_DARKBLUE          = 6
CL_YELLOW            = 7
CL_ORANGE            = 8
CL_BROWN             = 9
CL_ROSE              = 10
CL_DARKGRAY          = 11
CL_MIDGRAY           = 12
CL_LIGHTGREEN        = 13
CL_LIGHTBLUE         = 14
CL_LIGHTGRAY         = 15