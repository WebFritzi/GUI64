InitHourglass   sta MHourglassCount
                jsr DefHourglassChars
                ldx #1
                stx HourglassCount
                dex
                stx ExHourglassCount
                jsr SetHourglassPos
                jmp PaintHourglass

;MHGC_Tab        !byte 1,5

; Defines the chars in the char set
; used for painting the hourglass
DefHourglassChars
                jsr MapOutIO
                ldx #63
-               lda HourglassCharTab,x
                eor #%11111111
                sta DT_Reserved,x
                dex
                bpl -
                jmp MapInIO

; Sets hourglass scr pos in ZP_58/ZP_59
SetHourglassPos lda WindowHeight
                lsr
                sbc #0
                clc
                adc WindowPosY
                tax
                dex
                lda ScrTabLo,x
                sta ZP_5F
                lda ScrTabHi,x
                sta ZP_60
                lda WindowWidth
                lsr
                sbc #0
                clc
                adc WindowPosX
                clc
                adc ZP_5F
                sta ZP_5F
                bcc +
                inc ZP_60
+               rts

HourglassCount  !byte 0
ExHourglassCount!byte 0
MHourglassCount !byte 0
; Expects hourglass screen(!) position in ZP_5F/ZP_60
PaintHourglass  dec HourglassCount
                bne ++
                lda MHourglassCount
                sta HourglassCount
                ldx ExHourglassCount
                inx
                cpx #6
                bcc +
                ldx #0
+               stx ExHourglassCount
                ldy #40
                lda HourglassChars0,x
                sta (ZP_5F),y
                ldy #80
                ;lda HourglassChars1,x
                clc
                adc #1
                sta (ZP_5F),y
++              rts

HourglassChars0 !byte 240,242,244,246,246,246; 246,248,250,252,252,252
;HourglassChars1 !byte 241,243,245,247,247,247; 247,249,251,253,253,253

char_offset     !byte 39,15
SetBkgPattern   ; Copy char #1 or char #4 to char #0 in char sets
                ldx CSTM_DeskPattern
                ldy char_offset,x
                jsr MapOutIO
                ldx #7
-               lda CHARBASE,y
                sta CHARBASE,x
                sta TASKCHARBASE,x
                dey
                dex
                bpl -
                jmp MapInIO

; Expects: cursor index in Y
; Cursors:
; CUR_DEFAULT    = 0
; CUR_RESIZENWSE = 1
; CUR_RESIZENS   = 2
; CUR_RESIZEWE   = 3
; CUR_CARRET     = 4
; CUR_HOURGLASS  = 5
; CUR_HAND       = 6
SetCursor       cpy CurrentCursor
                beq +
                sty CurrentCursor
                lda CursorSprites0,y
                sta SPRPTR_0
                lda CursorSprites1,y
                sta SPRPTR_1
+               rts

CursorSprites0  !byte <SP_Mouse0,<SP_ResizeCursorNWSE0,<SP_ResizeCursorNS0,<SP_ResizeCursorWE0,<SP_CarretCursor,<SP_Hourglass
CursorSprites1  !byte <SP_Mouse1,<SP_ResizeCursorNWSE1,<SP_ResizeCursorNS1,<SP_ResizeCursorWE1,<SP_CarretCursor,<SP_HourglassComp

PaintDesktop    ; Clear screen
                ldx #0
-               lda #0
                sta DESKTOP_BUF,x
                sta DESKTOP_BUF+$100,x
                sta DESKTOP_BUF+$200,x
                sta DESKTOP_BUF+$270,x ; until Taskbar
                lda CSTM_DesktopClr ; color
                sta CLR_BUF,x
                sta CLR_BUF+$100,x
                sta CLR_BUF+$200,x
                sta CLR_BUF+$270,x ; until Taskbar
                inx
                bne -
                jmp PaintIcons

GetYCoordCmdMenu; Get y-coord of upper beam
                lda #22
                sec
                sbc CbmMenuHeight
                ; Get y-coord of StartMenu (Scr coords)
                asl
                asl
                asl
                clc
                adc #54
                rts

; Highlights entries in Commodore menu
; by using sprites
Highlight       lda MayHighlight
                bne +
-               rts
+               lda MenuItem
                bmi -
                jsr GetYCoordCmdMenu
                sta dummy_irq
                ; Sprites
                ;
                lda #%00111111
                sta VIC+21
                lda #<SP_BalkenSchmal
                sta SPRPTR_2
                sta SPRPTR_3
                lda #<SP_Balken
                sta SPRPTR_4
                sta SPRPTR_5
                lda #CL_WHITE
                sta col2
                sta col3
                lda CSTM_ActiveClr
                sta col4
                sta col5
                ; X positions
                lda #32
                sta xPos2
                lda #72
                sta xPos3
                lda #28
                sta xPos4
                lda #76
                sta xPos5
                ; Y positions
                lda MenuItem
                asl
                asl
                asl
                asl
                clc
                adc dummy_irq
                sec
                sbc #5
                sta yPos2
                sta yPos3
                sta yPos4
                sta yPos5
                ; stretching and priority
                lda #%00010100
                sta SPR_STRETCH_HORZ ; wide sprites
                lda #%00001100
                sta SPR_PRIORITY
                rts

; Result in carry
IsInStartMenu   jsr MouseToScr
                lda MouseInfo
                cmp #CbmMenuWidth
                bcs +
                lda MouseInfo+1
                cmp #22
                bcs +
                lda MouseInfo+1
                ;clc
                adc CbmMenuHeight
                cmp #22
                bcs ++
+               clc
++              rts

;DrawSpritesDown lda #%00111111
;                sta VIC+21
;                lda #0; no stretch
;                sta VIC+29
;                lda #<SP_StartBtnUL
;                sta SPRPTR_2
;                lda #<SP_StartBtnLR
;                sta SPRPTR_3
;                lda #CL_BLACK
;                sta col2
;                lda #CL_WHITE
;                sta col3
;                lda #229
;                sta yPos2
;                sta yPos3
;                lda #25
;                sta xPos2
;                sta xPos3
;                ; Draw Commodore sprites
;                lda #<SP_Commodore1
;                sta SPRPTR_4
;                lda #<SP_Commodore2
;                sta SPRPTR_5
;                lda #CL_DARKBLUE
;                sta col4
;                lda #CL_RED
;                sta col5
;                lda #29
;                sta xPos4
;                sta xPos5
;                lda #233
;                sta yPos4
;                sta yPos5
;                rts

;DrawSpritesUp   ; Draw Commodore sprites
;                lda #<SP_Commodore1
;                sta SPRPTR_2
;                lda #<SP_Commodore2
;                sta SPRPTR_3
;                lda #CL_DARKBLUE
;                sta col2
;                lda #CL_RED
;                sta col3
;                lda #28
;                sta xPos2
;                sta xPos3
;                lda #232
;                sta yPos2
;                sta yPos3
;                ; Draw button frame
;                lda #<SP_StartBtnUL
;                sta SPRPTR_4
;                lda #<SP_StartBtnLR
;                sta SPRPTR_5
;                lda #CL_WHITE
;                sta col4
;                lda #CL_BLACK
;                sta col5
;                lda #25
;                sta xPos4
;                sta xPos5
;                ldx #229
;                stx yPos4
;                stx yPos5
;                ;
;                lda #0; no stretch
;                sta VIC+29
;                lda #%00111111
;                sta VIC+21
;                lda VIC+16
;                and #%11000011
;                sta VIC+16
;                rts

DrawSpritesDown lda #%00111111
                sta VIC+21
                lda #0; no stretch
                sta VIC+29
                ;
                ; A = col2
                ldx #29
                ldy #233
DrawStBnSprites ; Draw button frame
                sta col2
                eor #1
                sta col3
                sty yPos4
                sty yPos5
                ;
                lda #<SP_StartBtnUL
                sta SPRPTR_2
                lda #<SP_StartBtnLR
                sta SPRPTR_3
                lda #25
                sta xPos2
                sta xPos3
                ; Draw Commodore sprites
                lda #<SP_Commodore1
                sta SPRPTR_4
                lda #<SP_Commodore2
                sta SPRPTR_5
                lda #CL_DARKBLUE
                sta col4
                lda #CL_RED
                sta col5
                stx xPos4
                stx xPos5
                rts

DrawSpritesUp   ; Draw button frame
                ;lda #<SP_StartBtnUL
                ;sta SPRPTR_2
                ;lda #<SP_StartBtnLR
                ;sta SPRPTR_3
                ;lda #CL_WHITE
                ;sta col2
                ;lda #CL_BLACK
                ;sta col3
                ;lda #25
                ;sta xPos2
                ;sta xPos3
                ;ldx #229
                ;stx yPos2
                ;stx yPos3
                ; Draw Commodore sprites
                ;lda #<SP_Commodore1
                ;sta SPRPTR_4
                ;lda #<SP_Commodore2
                ;sta SPRPTR_5
                ;lda #CL_DARKBLUE
                ;sta col4
                ;lda #CL_RED
                ;sta col5
                ;lda #28
                ;sta xPos4
                ;sta xPos5
                ;lda #232
                ;sta yPos4
                ;sta yPos5

                ;jsr DrawCmdSprites
                lda #CL_WHITE
                ldx #28
                ldy #232
                jsr DrawStBnSprites
                ;
                ;lda #0; no stretch
                ;sta VIC+29
                lda #%00111111
                sta VIC+21
                ;lda VIC+16
                ;and #%11000011
                ;sta VIC+16
                rts

Drag            lda IsDragging
                bne +
                rts
+               jsr GetMouseInfo
                lda DragType
                beq drag_type_0
                ;---------------------------
                ; Resizing...
                ;---------------------------
                lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne FE
                ; Find left bound
                lda #7
                sta ZP_5F
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +

                ; Get menu bar width
                lda #0
                sta $02
                ;lda WindowCtrlPtr
                ;sta $fb
                ;lda WindowCtrlPtr+1
                ;sta $fc
                ;ldy #CTRLSTRUCT_NUMSTRINGS
                ;lda ($fb),y
                jsr SelectControl0
                ldx ControlNumStr
                ; Write string address to FDFE
                ;ldy #CTRLSTRUCT_STRINGS
                ;jsr AddrInFBtoFD
                jsr CtrlStringsToFD
-               jsr GetStrLen
                lda $02
                clc
                adc res
                clc
                adc #2; 2 for every menu item
                sta $02
                inc res
                lda res
                jsr AddToFD
                dex
                bne -
                ;
                ;lda $02
                ;sta res
                ;

                ;ldx res
                ldx $02
                dex
                stx ZP_5F
+               lda WindowPosX
                clc
                adc ZP_5F
                cmp MouseInfo
                bcs +
                lda MouseInfo
+               sec
                sbc WindowPosX
                tax
                inx
                stx WindowWidth
                ; FE
FE              lda WindowBits
                and #BIT_WND_FIXEDHEIGHT
                beq +
                lda WindowHeight
                clc
                adc WindowPosY
                jmp ++
+               lda WindowPosY
                clc
                adc #6
                cmp MouseInfo+1
                bcs +
                lda MouseInfo+1
+               cmp #22
                bcc ++
                lda #21 ; if wnd out of bounds
++              sec
                sbc WindowPosY
                tax
                inx
                stx WindowHeight
                jmp UpdWnd_RepAll
drag_type_0     ;---------------------------
                ; Repositioning...
                ;---------------------------
                ; Get Y position
                lda MouseInfo+1
                sec
                sbc DragAnchorY
                clc
                adc DragOldPosY
                bpl +
                lda #0
+               ;sta ZP_5F
                sta DragNewPosY
                clc
                adc DragObjHeightMinus1
                cmp #22
                bcc +
                lda #21
                ;sec
                sbc DragObjHeightMinus1
                ;sta ZP_5F
                sta DragNewPosY
+               ;lda ZP_5F
                ;sta DragNewPosY
                ; Get X position
                lda MouseInfo
                sec
                sbc DragAnchorX
                clc
                adc DragOldPosX
                bpl +
                lda #0 ; if wnd too far left
+               ;sta ZP_5F
                sta DragNewPosX
                clc
                adc DragObjWidthMinus1
                cmp #40
                bcc +
                lda #39
                ;sec
                sbc DragObjWidthMinus1
                ;sta ZP_5F
                sta DragNewPosX
+               ;lda ZP_5F
                ;sta DragNewPosX
                ; Show on screen
                lda DragObjType
                beq ++
                ; Drag object is an icon
                lda CurrentIcon
                asl
                tax
                tay
                lda DragNewPosX
                cmp CSTM_Icons,y
                bne +
                iny
                lda DragNewPosY
                cmp CSTM_Icons,y
                bne +
                rts
+               lda DragNewPosX
                sta CSTM_Icons,x
                inx
                lda DragNewPosY
                sta CSTM_Icons,x
                jmp RepaintAll
++              ; Drag object is a window
                lda DragNewPosX
                cmp WindowPosX
                bne +
                lda DragNewPosY
                cmp WindowPosY
                bne +
                rts
+               lda DragNewPosX
                sta WindowPosX
                lda DragNewPosY
                sta WindowPosY
UpdWnd_RepAll   jsr UpdateWindow
                jmp RepaintAll

;; Result in carry
;IsInStartBtn    lda MouseInfo+4
;                bne +
;                lda MouseInfo+2
;                cmp #47
;                bcs +
;                cmp #26
;                bcc +
;                lda MouseInfo+3
;                cmp #248
;                bcs +
;                cmp #230
;                bcs ++ ; yes
;+               clc    ; no
;++              rts

; Result in carry
IsInTaskbar     lda MouseInfo+1
                cmp #22
                rts

; Expects mouse in task bar
; Result in carry
IsInTaskBtns    lda MouseInfo
                cmp #33
                bcs +
                cmp #3
                bcs ++ ; yes
+               clc    ; no
++              rts

; Paints CBM menu to screen (no highlight)
PaintCbmMenu    lda #<Menu_Start
                sta $fb
                lda #>Menu_Start
                sta $fc
                jsr PaintMenuToBuf
                lda #22
                sec
                sbc CbmMenuHeight
                tax
                lda ScrTabLo,x
                sta $fb
                lda ScrTabHi,x
                sta $fc
                jsr BufToScreen
                lda #$ff
                sta CurMenuItem
                lda #0
                sta CurMenuPosX
                lda #17
                sta CurMenuPosY
                rts

; Stores mouse info in MouseInfo: xScr,yScr,x,y,xHiByte
GetMouseInfo    jsr MouseToScr
                lda VIC
                sta MouseInfo+2
                lda VIC+1
                sta MouseInfo+3
                lda VIC+16
                sta MouseInfo+4
                rts

;Returns Mouse pos in scr coords in MouseInfo, MouseInfo+1
MouseToScr      lda xPos0
                sec
                sbc #24
                sta MouseInfo
                lda $d010
                sbc #0
                lsr
                lda MouseInfo
                ror
                lsr
                lsr
                sta MouseInfo
                ;
                lda yPos0
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta MouseInfo+1
                rts

PosTo_Helper    sty ZP_5F
                lda ScrTabLo,x
                clc
                adc ZP_5F
                rts

;Expects scr pos in Y,X
;Output: scr mem adr in FBFC
PosToScrMemFB   jsr PosTo_Helper
                sta $fb
                lda ScrTabHi,x
                adc #0
                sta $fc
                rts

PosToDeskBufFB  jsr PosTo_Helper
                sta $fb
                ;lda BufScrTabHi,x
                lda ScrTabHi,x
                eor #DESKBUF_XOR;#$64;#$44
                adc #0
                sta $fc
                rts

;Expects scr pos in Y,X
;Output: clr mem adr in 0203
PosToClrMem02   jsr PosTo_Helper
                sta $02
                ;lda ClrTabHi,x
                lda ScrTabHi,x
                eor #CLRMEM_XOR;#$3c
                adc #0
                sta $03
                rts

PosToDeskColBuf02
                jsr PosTo_Helper
                sta $02
                ;lda BufScrTabHi,x
                lda ScrTabHi,x
                eor #DESKBUF_XOR;#$64;#$44
                adc #4
                sta $03
                rts

SMC_ScrFrom = copy_scrclr+1
SMC_ScrTo   = copy_scrclr+4
SMC_ClrFrom = copy_scrclr+7
SMC_ClrTo   = copy_scrclr+10
color           !byte 0
; Expects:
;  MapWidth, MapHeight, GapFrom, GapTo,
;  SMC_ScrFrom, SMC_ScrTo, SMC_ClrFrom, SMC_ClrTo
; If A>127 then chooses colors from map
; otherwise, chooses fixed color in A
CpyScrClrInfo   sta color
                bmi +
                ; Fixed color in A
                lda #$AD; opcode for LDA $hhll
                sta copy_scrclr+6
                lda #<color
                sta copy_scrclr+7
                lda #>color
                sta copy_scrclr+8
                jmp ++
+               ; Color from map
                lda #$B9; opcode for LDA $hhll,Y
                sta copy_scrclr+6
                ;
++              ldx MapHeight
                dex
-               ldy MapWidth
                dey
copy_scrclr     lda $FFFF,y; fill with scr from
                sta $FFFF,y; fill with scr to
                lda $FFFF,y; fill with clr from or with color
                sta $FFFF,y; fill with clr to
                ;----------------------
                dey
                bpl copy_scrclr
                ; Update SMC_ScrFrom
                lda SMC_ScrFrom
                clc
                adc GapFrom
                sta SMC_ScrFrom
                bcc +
                inc SMC_ScrFrom+1
                ; Update SMC_ScrTo
+               lda SMC_ScrTo
                clc
                adc GapTo
                sta SMC_ScrTo
                bcc +
                inc SMC_ScrTo+1
+               ; Update SMC_ClrTo
                lda SMC_ClrTo
                clc
                adc GapTo
                sta SMC_ClrTo
                bcc +
                inc SMC_ClrTo+1
+               lda color
                bpl ++
                ; Update SMC_ClrFrom
                lda SMC_ClrFrom
                clc
                adc GapFrom
                sta SMC_ClrFrom
                bcc ++
                inc SMC_ClrFrom+1
++              dex
                bpl -
                rts
;
;; Chooses VIC bank at VIC_BANK
;; Sets screen memory address at SCRMEM
;; Chooses char set at CHAR_BASE
;SetGraphicsEnvironment
;                ; Choose VIC bank at VIC_BANK
;                ; Tell CIA that data comes in at bits 0,1
;                lda $dd02
;                ora #%00000011
;                sta $dd02

;                ;; Note that bits 0-5 in >VICBANK are 0
;                ;lda #>VICBANK  ; 76543210    c = carry
;                ;rol            ; 6543210c    7
;                ;rol            ; 543210c7    6
;                ;rol            ; 43210c76    5
;                ;eor #%00000011
;                ;sta $fc
;                ;lda $dd00
;                ;and #%11111100
;                ;ora $fc
;                ;sta $dd00

;                lda $dd00
;                and #%11111100
;                ora #(3-((>VICBANK)/64))
;                sta $dd00

;                ; Choose char set at CHAR_BASE and screen ram address
;                ;lda $d018
;                ;and #%11110001
;                ;ora #((>(CHARBASE-VICBANK))/4);#MAINCHARSHI
;                ;sta $d018
;                ;; Choose screen ram at SCRMEM
;                ;lda #>(SCRMEM - VICBANK) ; hibyte of $1400 = 5400 - 4000
;                ;asl
;                ;asl
;                ;sta $fc
;                ;lda $d018
;                ;and #%00001111
;                ;ora $fc
;                ;sta $d018

;                lda $d018
;                and #%00000001
;                ora #(((>(SCRMEM-VICBANK))*4)+((>(CHARBASE-VICBANK))/4))
;                sta $d018
;                ;
;                ;lda #>SCRMEM
;                ;sta 648
;                rts