VERSION = "2.1w"

;================================================================
; Addresses
;================================================================
; Standard addresses
VICBANK             = $c000
CLRMEM              = $d800
std_irq             = $ea31
VIC                 = $d000
SID                 = $d400
BASIC_ERR_START     = $a19e
;================================================================
; Zero page:
;----------------------------------------------------------------
; $02 - $0D: Frequently used pointers to buffer addresses
;            and local variables
; $0E - $0F: Used in IRQ handler
; $10 - $1F: Static window struct (see below)
; $20 - $2F: Static control struct (see below)
; $30 - $5A: Frequently used variables (see below)
; $5B - $60: Really frequently used variables (see below)
; $61 - $68: Used in Div_3216
; $69 - $8F: Frequently used variables (see below)
;----------------------------------------------------------------
; $96      : res
; $AB      : WiC64
; $FB - $FE: Really frequently used local variables
;================================================================
; Local variables------------------------------------------------
disk_size           = $04; 2 bytes (04/05)
BoxPosX             = $04
BoxPosY             = $05
BoxColor            = $06
cmd_len             = $06
channel             = $06
oldy                = $06
U_NumDirStrings     = $07
length_files        = $07
U_len               = $08; 2 bytes (08/09)
file_size           = $08; 2 bytes (08/09) (4 digit hexadecimal number)
BoxWidth            = $08
BoxHeight           = $09
file_size_dec       = $0a; 3 bytes (0a - 0c)
; Complete window structure for current window-------------------
CurrentWindow       = $10; Index/handle of current window (0-15)
WindowType          = $11
WindowBitsEx        = $12; see below for documentation
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
                     ;$2f depends on control
; Frequently used variables -------------------------------------
MouseInfo           = $30;5 bytes: xScr,yScr,x,y,xHiByte
VisibleWindows      = $35
error_code          = $36
CurDeviceInd        = $37; 0 for drive A, 1 for drive B
CurDeviceNo         = $38
exit_code           = $39
ProgramMode         = $40; 0: normal, 1: menu, 255: dialog
AllocedWindows      = $41
WndAddressInBuf     = $42; 2 bytes (42/43)
ViewerEOF           = $44; 2 bytes (44/45)
PathLength          = $46; 2 bytes (46/47)
MenuItem            = $48
WindowOnHeap        = $49; 2 bytes (49/4a)
ControlOnHeap       = $4b; 2 bytes (4b/4c)
ClipEnd             = $4d; 2 bytes (4d/4e)
TotalStart          = $4f; 2 bytes (4f/50)
MousePosInWndX      = $51
MousePosInWndY      = $52
actkey              = $53
BufWidth            = $54
EndReached          = $55
CurMenuPosX         = $56
CurMenuPosY         = $57
num_files           = $58
HeightMinus2        = $59
BytesPerLine        = $5a
; Really frequently used variables ------------------------------
wndParam0           = $5b; former "wndParam"
wndParam1           = $5c; former "wndParam+1"
Param0              = $5d; former "Param"
Param1              = $5e; former "Param+1"
ZP_5F               = $5f; former "dummy"
ZP_60               = $60; former "dummy+1"
; Frequently used variables -------------------------------------
window_counter      = $69
bTooManyFiles       = $6a
;keycode             = $6b
key_shifted         = $6b
StringWidth         = $6c
WidthMinus3         = $6d
offsetL             = $6e
ClipStart           = $6f; 2 bytes (6f/70)
Clock               = $71; 4 bytes (71 - 74)
TotalLength         = $75; 2 bytes (75/76)
musposx             = $77; 2 bytes (77/78)
musposy             = $79; 2 bytes (79/7a)
DiskToCopyFrom      = $7b; 2 bytes (7b/7c)
IsLBtnPressed       = $7d; documents left press at any time
bFirePressed        = $7e
FileSizeHex         = $7f; 2 bytes (7f/80)
DragNewPosX         = $81
DragNewPosY         = $82
BufHeight           = $83
ControlPressed      = $84
MayDrag             = $85
ScrollCaretPos      = $86
CurMenuItem         = $87
ScrollCaretHeight   = $88
CanCopy             = $89
dc_counter          = $8a
dc_counter_joy      = $8b
CurMenuHeight       = $8c
U_CurTarget         = $8d
MinableWindows      = $8e
TaskBtnWidth        = $8f
;----------------------------------------------------------------
res                 = $96
;================================================================

; Further GUI64 addresses =======================================
PATH_A_EX           = $6000 ;"A:"
PATH_A              = $6002
PATH_B_EX           = $6100 ;"B:"
PATH_B              = $6102
PATH_U_EX           = $6200 ;"U:"
PATH_U              = $6202
FREEMEM             = $6300
STRING_LIST_ULT     = $6500
;-- Internal GUI64 addresses ------------------------------------
WND_HEAP            = $7900; 16 wnd structs, must be $xx00 !!!
CONTROL_HEAP        = $7a00; 6 * 16 = 96 control structs
DESKTOP_BUF         = $8000
TASKBAR_BUF         = $8370
CLR_BUF             = DESKTOP_BUF + $0400
STRING_LIST_DRIVEA  = $8800
STRING_LIST_DRIVEB  = $9c00
APP_START           = $b000
APP_END             = $cfff
;-- Graphics data -----------------------------------------------
CHARBASE            = $d000
SPRITEBASE          = $d800
TASKCHARBASE        = $e000
SCRMEM              = $e400
SCRMEM_MINUS_CLRMEM = SCRMEM - CLRMEM
;-- File Viewer -------------------------------------------------
FILEVIEWERBUF_START = $e800
FILEVIEWERBUF_END   = $fffe
FILEVIEWERBUF_BLOCKS= (FILEVIEWERBUF_END - FILEVIEWERBUF_START + 2)/256
;================================================================

DESKBUF_XOR = >SCRMEM EOR >DESKTOP_BUF
CLRMEM_XOR  = >SCRMEM EOR >CLRMEM

; For quickly changing the char set
MAINCHARSHI = (>(CHARBASE - VICBANK))/4
TASKCHARSHI = (>(TASKCHARBASE - VICBANK))/4
; Application
MAX_WND_NUMBER = 16
MAXED = 255
DRVWND_HEIGHT = 13
FILE_RECORD_LENGTH = 20
;
; Cursors
CUR_DEFAULT    = 0
CUR_RESIZENWSE = 1
CUR_RESIZENS   = 2
CUR_RESIZEWE   = 3
CUR_CARRET     = 4
CUR_HOURGLASS  = 5
CUR_HAND       = 6
;----------------------------------
; Window struct members
;
WNDSTRUCT_HANDLE       = 0
WNDSTRUCT_TYPE         = 1
WNDSTRUCT_BTS_EX       = 2
WNDSTRUCT_BITS         = 3
WNDSTRUCT_POSX         = 4
WNDSTRUCT_POSY         = 5
WNDSTRUCT_WIDTH        = 6
WNDSTRUCT_HEIGHT       = 7
WNDSTRUCT_TITLESTRING  = 8  ; ptr
WNDSTRUCT_FIRSTCONTROL = 10 ; ptr
WNDSTRUCT_NUMCONTROLS  = 12
WNDSTRUCT_FOCUSED_CTRL = 13
WNDSTRUCT_WNDPROC      = 14
; Window Bits
BIT_WND_HASMENU     = %00000001
BIT_WND_RESIZABLE   = %00000010
BIT_WND_FIXEDWIDTH  = %00000100
BIT_WND_FIXEDHEIGHT = %00001000
BIT_WND_CANMAXIMIZE = %00010000
BIT_WND_CANMINIMIZE = %00100000
BIT_WND_ISMINIMIZED = %01000000
BIT_WND_ISMAXIMIZED = %10000000
; Window Bits Ex
; Specific last ex bit, overridden
; by the following ones
BIT_EX_WND_SPECIFIC = %10000000
BIT_EX_WND_ISDISK   = %10000000
BIT_EX_WND_ISERRMSG = %10000000
;----------------------------------
; Control struct members
;
CTRLSTRUCT_INDEX = 0
CTRLSTRUCT_TYPE = 1
CTRLSTRUCT_COLOR = 2
CTRLSTRUCT_POSX = 3
CTRLSTRUCT_POSY = 4
CTRLSTRUCT_WIDTH = 5
CTRLSTRUCT_HEIGHT = 6
CTRLSTRUCT_BITS = 7
CTRLSTRUCT_HIGHLIGHTED_INDEX = 8
CTRLSTRUCT_TOP_INDEX = 9
CTRLSTRUCT_NUMSTRINGS = 10
CTRLSTRUCT_STRINGS = 11
;CTRLSTRUCT_ID = 13
CTRLSTRUCT_BITS_EX = 14
;
; For UpDown control
UPDOWN_LOWERLIMIT = 10
UPDOWN_UPPERLIMIT = 11
UPDOWN_DIGIT_LO = 12
UPDOWN_DIGIT_HI = 13
; For Progressbar control
PROGBAR_MAX_LO = 10
PROGBAR_MAX_HI = 11
PROGBAR_VAL_LO = 12
PROGBAR_VAL_HI = 13
; For Edit_SL control
EDITSL_CARETPOS = 9
EDITSL_MAX_STRLEN = 10
EDITSL_FORBIDDEN = 14; ptr to forbidden chars
; For TextViewBox control
TEXTVIEWBOX_ISTEXT = 8
TEXTVIEWBOX_TOPLO = 9
TEXTVIEWBOX_TOPHI = 10
TEXTVIEWBOX_FILEADDRLO = 14
TEXTVIEWBOX_FILEADDRHI = 15
;----------------------------------
; Control Bits
BIT_CTRL_ISMAXIMIZED  = %00000001
BIT_CTRL_ISPRESSED    = %00000010
BIT_CTRL_UPPERCASE    = %00000100
BIT_CTRL_DBLFRAME_TOP = %00001000
BIT_CTRL_DBLFRAME_BTM = %00010000
BIT_CTRL_DBLFRAME_RGT = %00100000
BIT_CTRL_DBLFRAME_LFT = %01000000
; Extended control bits
BIT_EX_CTRL_NOFRAME_TOP = %00000001
BIT_EX_CTRL_NOFRAME_BTM = %00000010
BIT_EX_CTRL_LOWERCASE   = %01000000
BIT_EX_CTRL_SHOWSIZES   = %10000000

; Window Types (do not start with 0!!!)
WT_DRIVE_A = 1
WT_DRIVE_B = 2
WT_ULTIMATE = 3
WT_SETTINGS = 4
WT_FILEVIEW = 5
WT_DLG = 32 ; dummy - must be overwritten
WT_DLG_INFO = 33
WT_DLG_CLOCK = 34
WT_DLG_YESNO = 35
WT_DLG_RENAME = 36
WT_DLG_FORMAT = 37
WT_DLG_DISKINFO = 38
WT_DLG_COPYFILE = 39
WT_DLG_DEVNO = 40
WT_DLG_NEWFILE = 41
WT_DLG_LOAD = 42
WT_DLG_SORT = 43

; Control Types (must not be zero!!!)
CT_MENUBAR = 1
CT_BUTTON = 2
CT_LISTBOX = 3
CT_FILELISTSCROLLBOX = 4
CT_COLORPICKER = 5
CT_RADIOBUTTONGROUP = 6
CT_UPDOWN = 7
CT_EDIT_SL = 8
CT_TEXTVIEWBOX = 9
CT_CHECKBOX = 10
MAX_CT_ACTION = 10
; No action
MIN_CT_NOACTION = 31
CT_LABEL = 31
CT_LABEL_ML = 32
CT_FRAME = 33
CT_PROGRESSBAR = 34
CT_COLBOXLABEL = 35
; App
MIN_CT_APP = 50

; Menu (and MenuItem) IDs
ID_MENU_START = 0
ID_MENU_COLORPICKER = 1
ID_MENU_DISK = 2
ID_MENU_FILE = 3
ID_MENU_OPTS = 4
ID_MENU_IMAGE = 5
;
ID_MI_DISKREFRESH = 0
ID_MI_DEVICENO = 1
ID_MI_DISKINFO = 2
ID_MI_DISKFORMAT = 3
ID_MI_DISKRENAME = 4
ID_MI_DISKCLOSE = 5
;
ID_MI_FILENEW = 0
ID_MI_FILECUT = 1
ID_MI_FILECOPY = 2
ID_MI_FILEPASTE = 3
ID_MI_FILEDELETE = 4
ID_MI_FILERENAME = 5
ID_MI_FILEVIEW = 6
ID_MI_FILEBOOT = 7
;
ID_MI_SHOWSIZES = 0
ID_MI_LOWERCASE = 1
ID_MI_SORTBYNAME = 2
ID_MI_SORTBYTYPE = 3
ID_MI_SORTBYSIZE = 4
;
ID_MI_VIEWTEXT_UC = 0
ID_MI_VIEWTEXT_LC = 1
ID_MI_VIEWHEX = 2
ID_MI_VIEWCLOSE = 3
;
ID_MI_IMAGEMOUNT_A = 0
ID_MI_IMAGEMOUNT_B = 1
ID_MI_IMAGECLOSE = 2

; Menu types
MT_NORMAL = 0
MT_COLORPICKER = 1

; Game Modes
PM_NORMAL = 0
PM_MENU = 1
PM_DIALOG = 255

; Exit Codes
EC_RBTNPRESS = 1
EC_RBTNRELEASE = 2
EC_LBTNPRESS = 3
EC_LBTNRELEASE = 4
EC_MOUSEMOVE = 5
EC_GAMEEXIT = 6
EC_DBLCLICK = 7
EC_SCROLLWHEELDOWN = 8
EC_SCROLLWHEELUP = 9
EC_KEYPRESS = 10
EC_RUNFILE = 11
EC_BOOTFILE = 12
EC_TOOLTIP = 13
EC_APP = 14
EC_TIMER = 15
; Actually no real exit codes (rather control msges)
EC_SCROLL_UP = 15
EC_SCROLL_DOWN = 16
EC_SCROLL_PG_UP = 17
EC_SCROLL_PG_DOWN = 18

; File extensons
EXT_D64 = 1
EXT_D71 = 2
EXT_D81 = 3
EXT_DNP = 4

; Char set values
DT_Reserved_Char = 240
DT_Reserved = CHARBASE+DT_Reserved_Char*8
DT_App_Char = 224
DT_App_Chars = CHARBASE+DT_App_Char*8
TB_Reserved_Char = 118
TB_Reserved = TASKCHARBASE+TB_Reserved_Char*8

;Sprite Blocks
SP_Mouse0            = (Mousepointer0-SpriteData+SPRITEBASE)/64
SP_Mouse1            = (Mousepointer1-SpriteData+SPRITEBASE)/64
SP_Commodore1        = (Commodore1-SpriteData+SPRITEBASE)/64
SP_Commodore2        = (Commodore2-SpriteData+SPRITEBASE)/64
SP_StartBtnUL        = (StartBtnUL-SpriteData+SPRITEBASE)/64
SP_StartBtnLR        = (StartBtnLR-SpriteData+SPRITEBASE)/64
SP_Balken            = (Balken-SpriteData+SPRITEBASE)/64
SP_BalkenSchmal      = (BalkenSchmal-SpriteData+SPRITEBASE)/64
SP_ResizeCursorNWSE0 = (ResizeCursorNWSE0-SpriteData+SPRITEBASE)/64
SP_ResizeCursorNWSE1 = (ResizeCursorNWSE1-SpriteData+SPRITEBASE)/64
SP_ResizeCursorNS0   = (ResizeCursorNS0-SpriteData+SPRITEBASE)/64
SP_ResizeCursorNS1   = (ResizeCursorNS1-SpriteData+SPRITEBASE)/64
SP_ResizeCursorWE0   = (ResizeCursorWE0-SpriteData+SPRITEBASE)/64
SP_ResizeCursorWE1   = (ResizeCursorWE1-SpriteData+SPRITEBASE)/64
SP_CarretCursor      = (CarretCursor-SpriteData+SPRITEBASE)/64
SP_Hourglass         = (Hourglass-SpriteData+SPRITEBASE)/64
SP_HourglassComp     = (HourglassCompanion-SpriteData+SPRITEBASE)/64
;SP_HandOuter         = (HandOuter-SpriteData+SPRITEBASE)/64
;SP_HandInner         = (HandInner-SpriteData+SPRITEBASE)/64

;----------------------------------
; VIC Addresses
;
xPos0 = VIC
yPos0 = VIC+1
xPos1 = VIC+2
yPos1 = VIC+3
xPos2 = VIC+4
yPos2 = VIC+5
xPos3 = VIC+6
yPos3 = VIC+7
xPos4 = VIC+8
yPos4 = VIC+9
xPos5 = VIC+10
yPos5 = VIC+11
xPos6 = VIC+12
yPos6 = VIC+13
xPos7 = VIC+14
yPos7 = VIC+15
xposmsb  = VIC+16
SPR_VIS = VIC+21
SPR_STRETCH_VERT = VIC+23
SPR_PRIORITY = VIC+27
SPR_STRETCH_HORZ = VIC+29
FRAMECOLOR = VIC+32
BKGCOLOR = VIC+33
MULTICOLOR1 = VIC+34
MULTICOLOR2 = VIC+35
col0  = VIC+39
col1  = VIC+40
col2  = VIC+41
col3  = VIC+42
col4  = VIC+43
col5  = VIC+44
col6  = VIC+45
col7  = VIC+46
; Colors
CL_BLACK = 0
CL_WHITE = 1
CL_RED = 2
CL_CYAN = 3
CL_MAGENTA = 4
CL_DARKGREEN = 5
CL_DARKBLUE = 6
CL_YELLOW = 7
CL_ORANGE = 8
CL_BROWN = 9
CL_ROSE = 10
CL_DARKGRAY = 11
CL_MIDGRAY = 12
CL_LIGHTGREEN = 13
CL_LIGHTBLUE = 14
CL_LIGHTGRAY = 15
; Sprite pointers
SPRPTR_0 = SCRMEM+1016
SPRPTR_1 = SCRMEM+1017
SPRPTR_2 = SCRMEM+1018
SPRPTR_3 = SCRMEM+1019
SPRPTR_4 = SCRMEM+1020
SPRPTR_5 = SCRMEM+1021
SPRPTR_6 = SCRMEM+1022
SPRPTR_7 = SCRMEM+1023

;-------------------------------------
; Routines in Kernal ROM and BASIC ROM
;
; Status register
STATUS = $90
;; Prints string in A (lo) and Y (hi) to output file defined by CHKOUT
;STROUT = $AB1E
; Clears the screen
CLRSCR = $E544
; Restores the standard kernal vectors in the extended zero page
RESTOR = $FF8A
; Send LISTEN secondary address to serial bus. (Must call LISTEN beforehands.)
; Input: A = Secondary address.; Output: –; Used registers: A.
LSTNSA = $FF93
; Send TALK secondary address to serial bus. (Must call TALK beforehands.)
; Input: A = Secondary address; Output: –; Used registers: A.
TALKSA = $FF96
; Read byte from serial bus. (Must call TALK and TALKSA beforehands.)
; Input: –; Output: A = Byte read; Used registers: A.
IECIN  = $FFA5
; Write byte to serial bus. (Must call LISTEN and LSTNSA beforehands.)
; Input: A = Byte to write.; Output: –; Used registers: –
IECOUT = $FFA8
; Send UNTALK command to serial bus. Input: –; Output: –; Used registers: A.
UNTALK = $FFAB
; Send UNLISTEN command to serial bus. Input: –; Output: –; Used registers: A.
UNLSTN = $FFAE
; Sends LISTEN command to serial bus. Input: A = Device number.
; Output: –; Used registers: A.
LISTEN = $FFB1
; Send TALK command to serial bus. Input: A = Device number; Output: –; Used registers: A.
TALK   = $FFB4
; Fetch status of current input/output device, value of ST variable. (For RS232, status is cleared.)
; Input: –; Output: A = Device status; Used registers: A.
READST = $FFB7
; Set file parameters. Input: A = Logical number; X = Device number; Y = Secondary address.
; Output: –; Used registers: –
SETLFS = $FFBA
; Set file name parameters. Input: A = File name length; X/Y = Pointer to file name.
; Output: –; Used registers: –
SETNAM = $FFBD
; Open file. (Must call SETLFS and SETNAM beforehands.); Input: –
; Output: –; Used registers: A, X, Y.
OPEN   = $FFC0
; Close file. Input: A = Logical number.
; Output: –; Used registers: A, X, Y.
CLOSE  = $FFC3
; Define file as default input. (Must call OPEN beforehands.)
; Input: X = Logical number; Output: –; Used registers: A, X.
CHKIN  = $FFC6
; Define file as default output. (Must call OPEN beforehands.)
; Input: X = Logical number; Output: –; Used registers: A, X.
CHKOUT = $FFC9
; Close default input/output files (for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen.
; Input: –; Output: –; Used registers: A, X.
CLRCHN = $FFCC
; Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
; Input: –; Output: A = Byte read; Used registers: A, Y.
CHRIN  = $FFCF
; Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
; Input: A = Byte to write; Output: –; Used registers: –
CHROUT = $FFD2
; Load or verify file. (Must call SETLFS and SETNAM beforehands.);
; Input: A: 0 = Load, 1-255 = Verify; X/Y = Load address (if secondary address = 0).
; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1); X/Y = Address of last byte loaded/verified (if Carry = 0)
; Used registers: A, X, Y.
LOAD   = $FFD5
; Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehands.)
; Input: –; Output: A = Byte read; Used registers: A, X, Y.
GETIN  = $FFE4