;---------------------------------------------------------------
; GUI64 App: Sweepy
;
; Code: WebFritzi, 2026
;---------------------------------------------------------------

!to "Sweepy.d64",d64,"sweepy.gui","sweepy disk"

!source "gui64.inc.asm"

!zone Constants
; Constants
WT_SWEEPY        = 50 ; app window types start at 50
CT_SIMPLEFRAME   = 50 ; app control types start at 50
CT_ONECHAR_LABEL = 51
CT_MS_BOARD      = 52
ID_MENU_GAME     = 10 ; menu IDs start at 10
ID_MENU_HELP     = 11

BIT_REVEALED     = %01000000
BIT_FLAGGED      = %00100000

BIT_CTRL_DBLFRAME_LFTRGT = BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_RGT

!zone Graphics
*=$b000
                lda #WT_SWEEPY                  ; look for window with type "WT_SWEEPY"
                sta Param0                      ; 
                jsr GUI_FindWndByType           ;
                bcc +                           ; if not found, start app
                stx Param0                      ; otherwise, copy return value in X (window handle)
                jmp GUI_SelectTopWindow         ; to Param0, make this window top window, and leave
                ; Start app
+               ldx #<MSTimerHandler            ; 
                ldy #>MSTimerHandler            ; Initialize timer by providing address of routine
                jsr GUI_InitTimer               ; that reacts to timer events
                ;
                ldx #<CharList                  ; Register
                ldy #>CharList                  ; the
                lda #15                         ; 15 (up to 16 possible)
                jsr GUI_RegisterChars           ; chars defined in CharList (see zone "Data" below)
                ;
                ldx #<CtrlAction                ; Provide address of routine that defines the standard
                ldy #>CtrlAction                ; behavior of newly defined controls.
                jsr GUI_SetCtrlActionsRoutine   ; Here, the routine is void (see CtrlAction below)
                ;
                ldx #<PaintCtrls                ; Provide address of routine that
                ldy #>PaintCtrls                ; defines the look of newly defined
                jsr GUI_SetPaintCtrlsRoutine    ; controls
                ;
                ldx #<Wnd_Sweepy                ; Create a window with controls.
                ldy #>Wnd_Sweepy                ; The window data is in Wnd_Sweepy 
                jsr GUI_CreateWindowEx          ; (see zone "Data" below)
                ;
                ldx #<Ctrl_Board                ; Add control with data in Ctrl_Board (see zone "Data" below)
                ldy #>Ctrl_Board                ; separately. Just for demonstration purposes.
                jsr GUI_AddControl              ; Could have been in list with other controls.
                ;
                jsr GUI_GetDesign               ; Get GUI64 design (WIN/MAC)
                beq +                           ; returns Z=1 for WIN and Z=0 for MAC
                dec WindowHeight                ; For MAC design, decrement WindowHeight
                jsr GUI_UpdateWindow            ; (menu bar not in window) and confirm changes
+               ; Provide control info
                ; Menu
                jsr GUI_SelectControl0          ; Copy data of control 0 (menu bar) into
                ldx #<Str_MSMenubar             ; control struct in $20 - $2f and associate string list
                ldy #>Str_MSMenubar             ; at Str_MSMenubar
                lda #2                          ; with 2 strings
                jsr GUI_SetCtrlStringList       ; to control
                ; Simple frame #1
                jsr GUI_SelectControl1          ; Copy data of control 1 (1st SimpleFrame) into
                lda #BIT_CTRL_DBLFRAME_LFT      ; control struct in $20 - $2f, set
                sta ControlBits                 ; the control's bits (left frame is || instead of |)
                jsr GUI_UpdateControl           ; and confirm changes
                ; Simple frame #2
                jsr GUI_SelectControl2          ; Copy data of control 2 (2nd SimpleFrame) into control
                lda #BIT_CTRL_DBLFRAME_RGT      ; struct in $20 - $2f, set
                sta ControlBits                 ; the control's bits (right frame is || instead of |)
                jsr GUI_UpdateControl           ; and confirm changes
                ; Simple frame #3
                jsr GUI_SelectControl3          ; Copy data of control 2 (2nd SimpleFrame) into control
                lda #BIT_CTRL_DBLFRAME_LFTRGT   ; struct in $20 - $2f, set
                sta ControlBits                 ; the control's bits (right frame is || instead of |)
                jsr GUI_UpdateControl           ; and confirm changes
                ; Label 1
                lda #5
                jsr GUI_SelectControl           ; Copy data of control 5 (Label) into control struct in
                lda #CL_WHITE                   ; $20 - $2f
                sta ControlColor                ; choose white as control color
                ldx #<Str_FlagsLabel            ; and set
                ldy #>Str_FlagsLabel            ; Str_FlagsLabel 
                jsr GUI_SetCtrlString           ; as control string
                ; Label 2
                lda #6                          ; Copy data of control 6 (Label) into control struct in
                jsr GUI_SelectControl           ; $20 - $2f
                lda #CL_WHITE                   ; choose white as
                sta ControlColor                ; control color and
                jsr GUI_UpdateControl           ; confirm changes
                ;
                jmp NewGame                     ; Initialize game

; Window Proc (event handler for window)
MS_WndProc      jsr GUI_StdWndProc              ; Call standard window proc (MUST always be called)
                lda wndParam1                   ; contains ProgramMode (0: normal, 1: menu, 2: dialog)
                bmi +++; Dialog mode            ; leave if in dialog mode (never happens)
                beq +                           ; branch down if program mode is normal (PM_NORMAL)
                ; menu mode                     ; Menu mode:
                lda wndParam0                   ; contains event code
                cmp #EC_LBTNPRESS               ; check for left mouse button press
                bne +++                         ; if not, leave
                jsr GUI_IsInCurMenu             ; check if mouse is in current menu
                bcc +++                         ; if not, leave
                jsr GUI_GetCurMenuID            ; retrieves ID of current menu
                cmp #ID_MENU_GAME               ; distinguish between game menu and help menu
                beq GameMenuClicked             ; jump down for Game menu
                jmp HelpMenuClicked             ; jump down for Help menu
+               ; action in normal mode         ; Normal mode:
                lda wndParam0                   ; contains event code
                cmp #EC_LBTNPRESS               ; check for left mouse button press
                bne ++                          ; branch away for other events
                ; left button pressed           
                ldx ControlType                 ; distinguish between control types
                cpx #CT_ONECHAR_LABEL           ; start new game
                beq NewGame                     ; if smiley was pressed
                cpx #CT_MS_BOARD                ; jump to Tirade if grid was pressed
                bne +                           ; otherwise...
                jmp Tirade                      ; ...
+               rts                             ; leave
++              cmp #EC_RBTNPRESS               ; check for right mouse button press
                bne +++                         ; if not, leave
                ; right button pressed
                ldx ControlType                 ; distinguish between control types
                cpx #CT_MS_BOARD                ; if it wasn't the grid,
                bne +++                         ; leave
                jsr GetEntryIndex               ; otherwise, 
                lda MineField,x                 ; set a flag
                and #BIT_REVEALED               ; if clicked field
                bne +++                         ; wasn't revealed
                lda MineField,x                 ; 
                eor #BIT_FLAGGED                ; 
                sta MineField,x                 ; 
                jmp UpdatFlagsLabel             ; 
+++             rts

UpdatFlagsLabel and #BIT_FLAGGED
                beq +
                dec FlagsLeft
                jsr CheckWon
                bcc ++
                jsr Won
                jmp ++
+               inc FlagsLeft
                ;
++              lda FlagsLeft
                bmi +
                jsr ByteToDeciChar
                lda #$30
                sta Str_FlagsLabel
                jmp GUI_RepaintCurWindow        ; repaint current window
+               ; FlagsLeft is negative
                eor #%11111111
                tax
                inx
                txa
                jsr ByteToDeciChar
                lda #"-"
                sta Str_FlagsLabel
                jmp GUI_RepaintCurWindow        ; repaint current window

; Invoked when an item in the game menu was clicked
GameMenuClicked lda CurMenuItem                 ; menu items are sorted starting with 0
                beq NewGame                     ; branch to NewGame if menu item 0 ("New") was clicked
                ; Clicked on "Quit"
                jsr GUI_KillCurWindow           ; kill the app window
                jmp GUI_Repaint                 ; repaint entire GUI

HelpMenuClicked lda CurMenuItem
                bne +
                ldx #<Str_Mess_MS_Inf           ; Shows a message dialog with string
                ldy #>Str_Mess_MS_Inf           ; in Str_Mess_MS_Inf
                jsr GUI_ShowMessage             ; 
+               rts

; index of char in X
SetSmiley       jsr GUI_SelectControl4          ; Copy data of control 4 (OneCharLabel) into control
                stx ControlTopIndex             ; struct in $20 - $2f. Misuse ControlTopIndex as index
                lda #CL_YELLOW                  ; of char in char set.
                sta ControlColor                ; choose yellow as control color
                jmp GUI_UpdateControl           ; confirm changes

NewGame         ldx #APP_CHAR_NORMAL_SMILEY
                jsr SetSmiley
                ; reset flags label
                ldx #2
-               lda Str_FlagsStd,x
                sta Str_FlagsLabel,x
                dex
                bpl -
                ; reset timer label
                jsr GUI_StopTimer
                lda #$30
                sta Str_Timer
                sta Str_Timer+1
                sta Str_Timer+2
                lda #0
                sta TimerVals
                sta TimerVals+1
                sta TimerVals+2
                ;
                lda #10
                sta FlagsLeft
                sta TenthSec
                ;
                jsr ClearMineField
                jsr DistributeMines
                jmp GUI_RepaintCurWindow

GetEntryIndex   ; Get mouse pos in control
                jsr GUI_GetMousePosInWnd        ; Get mouse coords relative to window
                lda MousePosInWndX
                sec
                sbc ControlPosX
                sta ZP_60
                lda MousePosInWndY
                sec
                sbc ControlPosY
                sta ZP_5F
                asl
                asl
                asl
                clc
                adc ZP_5F
                adc ZP_60
                tax
                rts

!zone Control_Action_Paint
; X is ControlType
CtrlAction    rts                               ; Must be provided if new controls are registered

; X is ControlType
PaintCtrls      cpx #CT_SIMPLEFRAME             ; Depending on the controls' type, this routine
                beq PaintSimpleFram             ; defines how the newly registered controls are
                cpx #CT_ONECHAR_LABEL           ; displayed
                beq PaintOneCharLbl             ; FDFE points to position of control in paint buffer
                cpx #CT_MS_BOARD                ; 0203 points to position of control in color buffer
                beq PaintMSBoard
                rts

; Paints the control "SimpleFrame"
PaintSimpleFram ; First row
                jsr GUI_GetDesign
                beq +
                ; MAC
                lda #11
                bne ++
+               ; WIN
                lda #39
++              jsr PSF_13
                ; Second row
                jsr GUI_AddBufWidthToFD         ; goes one char down in paint buffer
                ldy ControlWidth
                dey
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_RGT      
                beq +
                lda #34                         ; ||
                bne ++
+               lda #37                         ; |
++              sta ($fd),y
                ldy #0
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_LFT
                beq +
                lda #34
                bne ++
+               lda #41
++              sta ($fd),y
                ; Third row
                jsr GUI_AddBufWidthToFD
                lda #36
PSF_13          ldy ControlWidth
                dey
                dey
-               sta ($fd),y
                dey
                bne -
                rts

PaintOneCharLbl ldy #0
                lda ControlTopIndex             ; char index in this case
                sta ($fd),y
                lda ControlColor                ; paints background
                sta ($02),y                     ; with ControlColor
                rts

PaintMSBoard    lda #<MineField
                sta $fb
                lda #>MineField
                sta $fc
                ;
                ldx ControlHeight
--              ldy ControlWidth
                lda #34; dblframe_horizontal
                sta ($fd),y
                dey
-               lda #APP_CHAR_BOARD_MAIN
                sta ($fd),y
                lda ($fb),y
                and #BIT_REVEALED
                bne +
                ; not revealed
                jsr GUI_GetCSTMWindowColor
                sta ($02),y
                lda ($fb),y
                and #BIT_FLAGGED
                beq +++
                ; -- flagged ----
                lda #APP_CHAR_FLAG
                sta ($fd),y
                bne +++; jmp +++
+               ; revealed
                lda ($fb),y
                bpl +
                ; -- mine -------
                lda #CL_RED
                sta ($02),y
                lda #APP_CHAR_MINE
                sta ($fd),y
                bne +++; jmp +++
+               ; -- no mine ----
                lda #CL_LIGHTBLUE
                sta ($02),y
                lda ($fb),y
                and #%00001111
                clc
                adc #APP_CHAR_BOARD_MAIN
                sta ($fd),y
+++             dey
                bpl -
                jsr GUI_AddBufWidthToFD         ; goes one char down in paint buffer
                jsr GUI_AddBufWidthTo02         ; goes one char down in color buffer
                jsr AddCtrlWidthToFB
                dex
                bne --
                ;
                ldy ControlWidth
                lda #APP_CHAR_BOARD_LR
                sta ($fd),y
                dey
                jsr GUI_GetDesign
                beq +
                lda #11; MAC
                bne ++
+               lda #53; WIN
++              ;
-               sta ($fd),y
                dey
                bpl -
                ;
                rts

AddCtrlWidthToFB
                lda $fb
                clc
                adc ControlWidth
                sta $fb
                bcc +
                inc $fc
+               rts

!zone Data
;----------------------------------------------------------------------
; Data
;----------------------------------------------------------------------
; Game related variables
MineField       !fill 81,0
FlagsLeft       !byte 10
TimerVals       !byte 0,0,0
TenthSec        !byte 10

Str_Title_App   !pet "Sweepy",0

; Definition of app window
; type, bits, xpos, ypos, width, height, address of string in title bar, address of wnd proc
Wnd_Sweepy      !byte WT_SWEEPY, %00100001, 11, 2, 11, 15, <Str_Title_App, >Str_Title_App
                !byte <MS_WndProc, >MS_WndProc
; Followed by control definitions (necessary for call CreateWindowEx)
; type, xpos, ypos, width, height, control string (null terminated)
                ;0
                !byte CT_MENUBAR, <MSMenubar, >MSMenubar, 0, 0
                !pet 0
                ;1
                !byte CT_SIMPLEFRAME, 0, 0, 5, 3
                !pet 0
                ;2
                !byte CT_SIMPLEFRAME, 6, 0, 5, 3
                !pet 0
                ;3
                !byte CT_SIMPLEFRAME, 4, 0, 3, 3
                !pet 0
                ;4
                !byte CT_ONECHAR_LABEL, 5, 1, 1, 1
                !pet 0
                ;5
                !byte CT_LABEL, 1,1,3,1
Str_FlagsStd    !pet "010",0
                ;6
                !byte CT_LABEL, 7,1,3,1
Str_Timer       !pet "000",0
                ; closing zero byte
                !byte 0

; This control is added dynamically - just for demonstraction purposes
                ;7
Ctrl_Board      !byte CT_MS_BOARD, 1,3,9,9
                !pet 0

; Strings
Str_Mess_MS_Inf !pet "Sweepy\By WebFritzi\",96," 2026",0
Str_FlagsLabel  !pet "0"
        DigitHi !pet "1"
        DigitLo !pet "0"
                !pet 0

; Chars
; Definition of new chars (8 bytes each, as usual)
CharList        ; 0: Smiley face
                APP_CHAR_HAPPY_SMILEY = APP_CHAR_0
                ;!byte 0,%01111110,%01011010,%01111110,%01011010,%01100110,%01111110,0
                !byte 255,%11111111,%11011011,%11111111,%11011011,%11100111,%11111111,255
                ; 1: Sad face
                APP_CHAR_SAD_SMILEY = APP_CHAR_1
                ;!byte 0,%01111110,%01011010,%01111110,%01100110,%01011010,%01111110,0
                !byte 255,%11111111,%11011011,%11111111,%11100111,%11011011,%11111111,255
                ; 2: MS board lower right
                APP_CHAR_BOARD_LR = APP_CHAR_2
                !byte %01111110,%11111110,%11111110,%11111110,%11111110,%11111110,%11111110,0
                ; 3: MS board main char
                APP_CHAR_BOARD_MAIN = APP_CHAR_3
                !byte 0,127,127,127,127,127,127,127
                ; 4: "1"
                !byte 0,127,%01110011,%01100011,%01110011,%01110011,%01100001,127
                ; 5: "2"
                !byte 0,127,%01100011,%01001001,%01110011,%01100111,%01000001,127
                ; 6: "3"
                !byte 0,127,%01100011,%01001001,%01110011,%01001001,%01100011,127
                ; 7: "4"
                !byte 0,127,%01110011,%01100011,%01010011,%01000001,%01110011,127
                ; 8: "5"
                !byte 0,127,%01000011,%01001111,%01000011,%01111001,%01000011,127
                ; 9: "6"
                !byte 0,127,%01100011,%01001111,%01000011,%01001001,%01100011,127
                ; 10: "7"
                !byte 0,127,%01000011,%01110011,%01100111,%01100111,%01100111,127
                ; 11: "8"
                !byte 0,127,%01100011,%01001001,%01100011,%01001001,%01100011,127
                ; 12: bomb/mine
                APP_CHAR_MINE = APP_CHAR_12
                ;!byte 0,127,%01111001,%01100111,%01000011,%01000011,%01100111,127
                !byte 0,127,%01010101,%01100011,%01000001,%01100011,%01010101,127
                ; 13: flag
                APP_CHAR_FLAG = APP_CHAR_13
                !byte 0,127,%01100111,%01100001,%01100111,%01101111,%01101111,127
                ; 14: Normal face
                APP_CHAR_NORMAL_SMILEY = APP_CHAR_14
                !byte 255,255,%11011011,255,255,%11000011,255,255

; Definition of menu bar
MSMenubar       !word Menu_MS_Game, Menu_MS_Help
Str_MSMenubar   !pet "Game",0,"?",0
; Definition of menus
; Format: ID, max_str_len, item_count, StringList
Menu_MS_Game    !pet ID_MENU_GAME,4,2,"New",0,"Quit",0
Menu_MS_Help    !pet ID_MENU_HELP,5,1,"About",0

!zone Game
MSTimerHandler  dec TenthSec
                bne ++
                lda #10
                sta TenthSec
                inc TimerVals+2
                lda TimerVals+2
                cmp #10
                bne ++
                lda #0
                sta TimerVals+2
                inc TimerVals+1
                lda TimerVals+1
                cmp #10
                bne ++
                lda #0
                sta TimerVals+1
                lda TimerVals
                cmp #9
                beq ++
                inc TimerVals
++              ldx #2
-               lda TimerVals,x
                ora #$30
                sta Str_Timer,x
                dex
                bpl -
                jmp GUI_RepaintCurWindow

CheckWon        ldx #80
-               lda MineField,x
                bpl +
                and #BIT_FLAGGED
                beq .notWon
                bne ++
+               and #BIT_REVEALED
                beq .notWon         ; safe field still covered
++              dex
                bpl -
                ; all safe fields are revealed
                sec
                rts
.notWon         clc
                rts

; Entry:
; Bit 7     = Mine
; Bit 6     = Revealed
; Bit 5     = Flagged
; Bit 4     = free
; Bits 0-3  = number of adjacent mines: 0..8
ClearMineField  lda #0
                ldx #80
-               sta MineField,x
                dex
                bpl -
                rts

DistributeMines ldx #10                ; 10 mines
                lda #81
                sta modulo
-               jsr Random
                cmp #243
                bcs -
                jsr Mod
                tay
                lda MineField,y
                bmi -                  ; already occupied
                lda #%10000000
                sta MineField,y
                jsr IncAdjacent        ; Y = position of mine
                dex
                bne -
                rts

MineCol         !byte 0
; Input: Y = position of mine
IncAdjacent     txa
                pha
                ; Determine column = Y mod 9
                tya
.mod9           cmp #9
                bcc .colReady
                sbc #9              ; C is set by CMP
                jmp .mod9
.colReady       sta MineCol         ; 0..8
                ;---------------- Above row
                tya
                cmp #9
                bcc .sameRow        ; first row => nothing above
                sec
                sbc #9
                tax                 ; X = field directly above
                inc MineField,x
                ; upper left
                lda MineCol
                beq .upperRight
                dex
                inc MineField,x
                inx                 ; back to upper center
.upperRight     lda MineCol
                cmp #8
                beq .sameRow
                inx
                inc MineField,x
                ;---------------- Same row
.sameRow        tya
                tax
                ; left
                lda MineCol
                beq .right
                dex
                inc MineField,x
                inx
.right          lda MineCol
                cmp #8
                beq .below
                inx
                inc MineField,x
                ;---------------- Row below
.below          tya
                cmp #72             ; indices 72..80 = last row
                bcs .done
                clc
                adc #9
                tax                 ; X = field directly below
                inc MineField,x
                ; lower left
                lda MineCol
                beq .lowerRight
                dex
                inc MineField,x
                inx
.lowerRight     lda MineCol
                cmp #8
                beq .done
                inx
                inc MineField,x
.done           pla
                tax
                rts

Won             jsr GUI_StopTimer
                ldx #APP_CHAR_HAPPY_SMILEY
                jmp SetSmiley

!zone Tirade
;======================================================================
; Reveal clicked field
; - Flagged field: do nothing
; - Mine: reveal complete board
; - Number 1..8: reveal only this field
; - Zero: reveal connected zero area plus bordering numbers
Tirade          
                jsr GetEntryIndex
                ; Flagged?
                lda MineField,x
                and #BIT_FLAGGED
                beq +
                rts
+               ; Already revealed?
                lda MineField,x
                and #BIT_REVEALED
                beq +
                rts
+               ; Reveal clicked field
                jsr GUI_StartTimer
                lda MineField,x
                ora #BIT_REVEALED
                sta MineField,x
                ; Mine?
                bpl +
                jsr Lost
                jmp GUI_RepaintCurWindow
+               ; Number 1..8?
                and #%00001111
                beq +
                jmp .finished
+               ; Zero field -> start flood fill
                stx TiradeQueue
                lda #0
                sta TiradeHead
                lda #1
                sta TiradeTail
;----------------------------------------------------------------------
; Get next zero field from queue
;
.next           lda TiradeHead
                cmp TiradeTail
                bne +
                jmp .finished
+               tax
                lda TiradeQueue,x
                sta TiradePos
                inc TiradeHead
;----------------------------------------------------------------------
; Convert field index 0..80 into X/Y coordinates 0..8
;
                lda TiradePos
                ldx #0              ; row
.div9           cmp #9
                bcc .gotXY
                sbc #9              ; C set by CMP
                inx
                bne .div9
.gotXY          sta TiradeX         ; column 0..8
                stx TiradeY         ; row    0..8
                lda #0
                sta TiradeDir
;----------------------------------------------------------------------
; Check all 8 surrounding fields
;
.neighbor       ldy TiradeDir
                ; Neighbor X
                lda TiradeX
                clc
                adc TiradeDX,y
                ; $ff from X-1 also fails this test
                cmp #9
                bcs .skip
                sta TiradeNX
                ; Neighbor Y
                lda TiradeY
                clc
                adc TiradeDY,y
                ; $ff from Y-1 also fails this test
                cmp #9
                bcs .skip
                sta TiradeNY
                ; index = Y * 9 + X
                asl
                asl
                asl
                clc
                adc TiradeNY
                adc TiradeNX
                tax
                jsr TiradeRevealNeighbor
.skip           inc TiradeDir
                lda TiradeDir
                cmp #8
                beq .next
                jmp .neighbor
.finished       jsr CheckWon
                bcc .repaint
                ; Won
                jsr Won
.repaint        jmp GUI_RepaintCurWindow
;----------------------------------------------------------------------
; X = field index
;
; Reveals a neighboring field.
; Zero fields are added to the flood-fill queue.
;
TiradeRevealNeighbor
                lda MineField,x
                ; Mine?
                ; A zero area can never expand through a mine.
                bmi .done
                ; Flagged or already revealed?
                and #(BIT_FLAGGED+BIT_REVEALED)
                bne .done
                ; Reveal field
                lda MineField,x
                ora #BIT_REVEALED
                sta MineField,x
                ; Number 1..8?
                and #%00001111
                bne .done
                ; Zero -> add to queue
                ldy TiradeTail
                txa
                sta TiradeQueue,y
                inc TiradeTail
.done           rts
;----------------------------------------------------------------------
; Lost -> reveal complete board
;
Lost            jsr GUI_StopTimer
                ldx #80
-               lda MineField,x
                ora #BIT_REVEALED
                sta MineField,x
                dex
                bpl -
                jsr GUI_SelectControl4
                lda #APP_CHAR_SAD_SMILEY
                sta ControlTopIndex
                lda #CL_RED
                sta ControlColor
                jmp GUI_UpdateControl
;----------------------------------------------------------------------
; Neighbor directions
;
; Order:
; upper left, upper, upper right,
; left,                 right,
; lower left, lower, lower right
;
TiradeDX        !byte $ff,  0,  1, $ff, 1, $ff, 0, 1
TiradeDY        !byte $ff,$ff,$ff,   0, 0,   1, 1, 1
;----------------------------------------------------------------------
; Flood-fill workspace
;
TiradeHead      !byte 0
TiradeTail      !byte 0
TiradePos       !byte 0
TiradeX         !byte 0
TiradeY         !byte 0
TiradeNX        !byte 0
TiradeNY        !byte 0
TiradeDir       !byte 0
TiradeQueue     !fill 81,0
;======================================================================


!zone Math
modulo          !byte 0
;a <- a mod [modulo], y <- a div [modulo]
Mod             sec
                ldy #$ff
mod_loop        iny
                sbc modulo
                bcs mod_loop
                adc modulo
                rts

Random          lda $dc04  ;Low-Byte  of Timer A in CIA-1
                eor $dc05  ;High-Byte of Timer A in CIA-1
                rts

; Expects byte in A
; Converts a byte to a displayable number
; in terms of two digits, DigitLo and DigitHi,
; which are part of Str_FlagsLabel
; Ex.: A = $13
; DigitLo = "9", DigitHi = "1" (both as chars)
ByteToDeciChar  ldy #10
                sty modulo
                jsr Mod
                clc
                adc #$30
                sta DigitLo
                tya
                clc
                adc #$30
                sta DigitHi
                rts