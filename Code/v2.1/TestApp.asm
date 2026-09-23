!zone Constants
; Constants
WT_MINESWEEPER   = 50 ; app window types start at 50
CT_SIMPLEFRAME   = 50 ; app control types start at 50
CT_ONECHAR_LABEL = 51
CT_MS_BOARD      = 52
ID_MENU_GAME     = 10
ID_MENU_HELP     = 11

BIT_REVEALED     = %01000000
BIT_FLAGGED      = %00100000

APP_CHAR_0       = 224
APP_CHAR_1       = 225
APP_CHAR_2       = 226
APP_CHAR_3       = 227
APP_CHAR_4       = 228
APP_CHAR_5       = 229
APP_CHAR_6       = 230
APP_CHAR_7       = 231
APP_CHAR_8       = 232
APP_CHAR_9       = 233
APP_CHAR_10      = 234
APP_CHAR_11      = 235
APP_CHAR_12      = 236
APP_CHAR_13      = 237
APP_CHAR_14      = 238
APP_CHAR_15      = 239

*=$6000
!zone Graphics
                ldx #<MSTimerHandler
                ldy #>MSTimerHandler
                jsr GUI_InitTimer
                ;
                ldx #<CharList
                ldy #>CharList
                lda #14
                jsr GUI_RegisterChars
                ;
                ldx #<MyCtrlAction
                ldy #>MyCtrlAction
                jsr GUI_SetCtrlActionsRoutine
                ;
                ldx #<MyPaintCtrls
                ldy #>MyPaintCtrls
                jsr GUI_SetPaintCtrlsRoutine
                ;
                ldx #<Wnd_MineSweep
                ldy #>Wnd_MineSweep
                jsr GUI_CreateWindowEx
                ;
                jsr GUI_GetDesign
                beq +
                dec WindowHeight
                jsr GUI_UpdateWindow
+               ; Provide control info
                ; Menu
                jsr GUI_SelectControl0
                ldx #<Str_MSMenubar
                ldy #>Str_MSMenubar
                lda #2
                jsr GUI_SetCtrlStringList
                ; Simple frame #1
                jsr GUI_SelectControl1
                lda #BIT_CTRL_DBLFRAME_LFT
                sta ControlBits
                jsr GUI_UpdateControl
                ; Simple frame #2
                jsr GUI_SelectControl2
                lda #BIT_CTRL_DBLFRAME_RGT
                sta ControlBits
                jsr GUI_UpdateControl
                ; Smiley char
                jsr GUI_SelectControl3
                lda #APP_CHAR_HAPPY_SMILEY
                sta ControlTopIndex
                lda #CL_YELLOW
                sta ControlColor
                jsr GUI_UpdateControl
                ; Label 1
                jsr GUI_SelectControl4
                lda #CL_WHITE
                sta ControlColor
                ldx #<Str_FlagsLabel
                ldy #>Str_FlagsLabel
                jsr GUI_SetCtrlString
                ; Label 2
                lda #5
                jsr GUI_SelectControl
                lda #CL_WHITE
                sta ControlColor
                jsr GUI_UpdateControl
                ;
                jmp NewGame

MS_WndProc      jsr GUI_StdWndProc
                lda wndParam1
                bmi +++; Dialog mode
                beq +
                ; action in menu mode
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne +++
                jsr GUI_IsInCurMenu
                bcc +++
                jsr GUI_GetCurMenuID
                cmp #ID_MENU_GAME
                beq GameMenuClicked
                jmp HelpMenuClicked
+               ; action in normal mode
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne ++
                ; left button pressed
                ldx ControlType
                cpx #CT_ONECHAR_LABEL
                beq NewGame
                cpx #CT_MS_BOARD
                bne +
                jmp Tirade
+               rts
++              cmp #EC_RBTNPRESS
                bne +++
                ; right button pressed
                ldx ControlType
                cpx #CT_MS_BOARD
                bne +++
                jsr GetEntryIndex
                lda MineField,x
                and #BIT_REVEALED
                bne +++
                lda MineField,x
                eor #BIT_FLAGGED
                sta MineField,x
                jmp UpdatFlagsLabel
+++             rts

UpdatFlagsLabel and #BIT_FLAGGED
                beq +
                dec FlagsLeft
                jmp ++
+               inc FlagsLeft
                ;
++              lda FlagsLeft
                bmi +
                jsr ByteToDeciChar
                lda #$30
                sta Str_FlagsLabel
                jmp GUI_RepaintCurWindow
+               ; Flags no is negative
                eor #%11111111
                tax
                inx
                txa
                jsr ByteToDeciChar
                lda #"-"
                sta Str_FlagsLabel
                jmp GUI_RepaintCurWindow

GameMenuClicked lda CurMenuItem
                beq NewGame
                ; Clicked on "Quit"
                jsr GUI_KillCurWindow
                jmp GUI_Repaint

HelpMenuClicked lda CurMenuItem
                bne +
                ldx #<Str_Mess_MS_Inf
                ldy #>Str_Mess_MS_Inf
                jsr GUI_ShowMessage
+               rts

NewGame         ; reset smiley char
                jsr GUI_SelectControl3
                lda #APP_CHAR_HAPPY_SMILEY
                sta ControlTopIndex
                lda #CL_YELLOW
                sta ControlColor
                jsr GUI_UpdateControl
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
                jsr GUI_GetMousePosInWnd
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
MyCtrlAction    rts

; X is ControlType
MyPaintCtrls    cpx #CT_SIMPLEFRAME
                beq PaintSimpleFram
                cpx #CT_ONECHAR_LABEL
                beq PaintOneCharLbl
                cpx #CT_MS_BOARD
                beq PaintMSBoard
                rts

PSF_13          ldy ControlWidth
                dey
                dey
-               sta ($fd),y
                dey
                bne -
                rts

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
                jsr GUI_AddBufWidthToFD
                ldy ControlWidth
                dey
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_RGT
                beq +
                lda #34
                bne ++
+               lda #37
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
                jsr PSF_13
                rts

PaintOneCharLbl ldy #0
                lda ControlTopIndex
                sta ($fd),y
                lda ControlColor
                sta ($02),y
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
                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                jsr AddCtrlWidthToFB
                dex
                bne --
                ;
                ldy ControlWidth
                lda #APP_CHAR_BOARD_LR
                sta ($fd),y
                dey
                +LDA_WM 53,11
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
MineField       !fill 81,0
FlagsLeft       !byte 10
TimerVals       !byte 0,0,0
TenthSec        !byte 10

Str_Title_App   !pet "Sweepy",0
Wnd_MineSweep   !byte WT_MINESWEEPER, %00100001, 11, 2, 11, 15, <Str_Title_App, >Str_Title_App
                !byte <MS_WndProc, >MS_WndProc
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
                !byte CT_ONECHAR_LABEL, 5, 1, 1, 1
                !pet 0
                ;4
                !byte CT_LABEL, 1,1,3,1
Str_FlagsStd    !pet "010",0
                ;5
                !byte CT_LABEL, 7,1,3,1
Str_Timer       !pet "000",0
                ;6
                !byte CT_MS_BOARD, 1,3,9,9
                !pet 0
                !byte 0

; Strings
Str_Mess_MS_Inf !pet "Sweepy\By WebFritzi\",96," 2026",0
Str_FlagsLabel  !pet "0"
        DigitHi !pet "1"
        DigitLo !pet "0"
                !pet 0

; Chars         
CharList        ; 0: Smiley face
                APP_CHAR_HAPPY_SMILEY = APP_CHAR_0
                !byte 0,%01111110,%01011010,%01111110,%01011010,%01100110,%01111110,0
                ; 1: Sad face
                APP_CHAR_SAD_SMILEY = APP_CHAR_1
                !byte 0,%01111110,%01011010,%01111110,%01100110,%01011010,%01111110,0
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

; Menubar
MSMenubar       !word Menu_MS_Game, Menu_MS_Help
Str_MSMenubar   !pet "Game",0,"?",0
; Menus
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
                bmi +               ; mine -> irrelevant
                and #BIT_REVEALED
                beq .notWon         ; safe field still covered
+               dex
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
                jsr GUI_StopTimer
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
                jsr GUI_SelectControl3
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