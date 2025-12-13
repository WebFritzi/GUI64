VERSION = "1.95W"

;=============================================================
; Addresses
;=============================================================
; Standard addresses
VICBANK             = $c000
CLRMEM              = $d800
std_irq             = $ea31
VIC                 = $d000
SID                 = $d400
BASIC_ERR_START     = $a19e
; GUI64 addresses ============================================
PATH_A_EX           = $7000 ;"A:"
PATH_A              = $7002
PATH_B_EX           = $7100 ;"B:"
PATH_B              = $7102
FREEMEM             = $7200
APP_START           = $7300 ; Apps to be loaded in here
;-- Internal GUI64 addresses ---------------------------------
WND_HEAP            = $9800; 16 wnd structs, must be $xx00 !!!
CONTROL_HEAP        = $9900; 7 * 16 = 112 control structs
SCR_BUF             = $a000
TASKBAR_BUF         = $a370
CLR_BUF             = SCR_BUF + $0400
STRING_LIST_DRIVEA  = $a800
STRING_LIST_DRIVEB  = $bc00
;-- Graphics data --------------------------------------------
CHARBASE            = $d000
SPRITEBASE          = $d800
TASKCHARBASE        = $e000
SCRMEM              = $e400
SCRMEM_MINUS_CLRMEM = SCRMEM - CLRMEM
;-- File Viewer ----------------------------------------------
FILEVIEWERBUF_START = $e800
FILEVIEWERBUF_END   = $ff00
FILEVIEWERBUF_BLOCKS= (FILEVIEWERBUF_END - FILEVIEWERBUF_START)/256
;=============================================================
;
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
BIT_EX_LOWERCASE    = %00000001
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
CTRLSTRUCT_ID = 13
CTRLSTRUCT_BITS_EX = 14
;
; For Menubar control
CTRLSTRUCT_MENULIST = 4
; For UpDown control
CTRLSTRUCT_LOWERLIMIT = 10
CTRLSTRUCT_UPPERLIMIT = 11
CTRLSTRUCT_DIGIT_LO = 12
CTRLSTRUCT_DIGIT_HI = 13
; For Progressbar control
CTRLSTRUCT_MAX_LO = 10
CTRLSTRUCT_MAX_HI = 11
CTRLSTRUCT_VAL_LO = 12
CTRLSTRUCT_VAL_HI = 13
; For Edit_SL control
CTRLSTRUCT_CARRETPOS = 9
CTRLSTRUCT_MAX_STRLEN = 10
CTRLSTRUCT_FORBIDDEN = 14; ptr to forbidden chars
; For TextViewBox control
CTRLSTRUCT_ISTEXT = 8
CTRLSTRUCT_TOPLO = 9
CTRLSTRUCT_TOPHI = 10
CTRLSTRUCT_FILEADDRLO = 14
CTRLSTRUCT_FILEADDRHI = 15
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
BIT_EX_CTRL_SHOWSIZES   = %10000000
BIT_EX_CTRL_LOWERCASE   = %01000000
; Window Types (do not start with 0!!!)
WT_DRIVE_A = 1
WT_DRIVE_B = 2
WT_SETTINGS = 3
WT_FILEVIEW = 4
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
CT_LABEL = 5
CT_LABEL_ML = 6
CT_FRAME = 7
CT_COLORPICKER = 8
CT_RADIOBUTTONGROUP = 9
CT_UPDOWN = 10
CT_EDIT_SL = 11
CT_PROGRESSBAR = 12
CT_COLBOXLABEL = 13
CT_TEXTVIEWBOX = 14
; Control IDs
ID_BTN_CANCEL = 1
ID_BTN_APPLY = 2
ID_BTN_OK = 3
ID_BTN_SET = 4
ID_BTN_YES = 5
ID_BTN_NO = 6
ID_LB_IMAGES = 7
; Menu (and MenuItem) IDs
ID_MENU_START = 0
ID_MENU_COLORPICKER = 1
ID_MENU_DISK = 2
ID_MENU_FILE = 3
ID_MENU_OPTS = 4
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
; Splitter = 5
ID_MI_GUI64INFO = 6
;
ID_MI_VIEWTEXT_UC = 0
ID_MI_VIEWTEXT_LC = 1
ID_MI_VIEWHEX = 2
ID_MI_VIEWCLOSE = 3
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
EC_LLBTNPRESS = 11
EC_RUNFILE = 12
EC_BOOTFILE = 13
EC_TOOLTIP = 14
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
; CBM Menu
CbmMenuItems = 3
CbmMenuStrLen = 8
CbmMenuWidth = CbmMenuStrLen + 2
CbmMenuHeight = 2*CbmMenuItems+1
;
TB_Reserved_Char = 118
TB_Reserved = TASKCHARBASE+TB_Reserved_Char*8
DT_Reserved_Char = 240
DT_Reserved = CHARBASE+DT_Reserved_Char*8
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
; status register
STATUS = $90
; Prints string in A (lo) and Y (hi) to output file defined by CHKOUT
STROUT = $AB1E
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