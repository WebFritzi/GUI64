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
                jsr MapInIO
                rts

; Sets hourglass scr pos in ZP_58/ZP_59
SetHourglassPos lda WindowHeight
                lsr
                bcs +
                tax
                dex
                txa
+               clc
                adc WindowPosY
                tax
                dex
                lda ScrTabLo,x
                sta $58
                lda ScrTabHi,x
                sta $59
                lda WindowWidth
                lsr
                bcs +
                tax
                dex
                txa
+               clc
                adc WindowPosX
                clc
                adc $58
                sta $58
                bcc +
                inc $59
+               rts

HourglassCount  !byte 0
ExHourglassCount!byte 0
MHourglassCount !byte 0
; Expects hourglass screen(!) position in ZP_58/ZP_59
PaintHourglass  dec HourglassCount
                beq +
                rts
+               lda MHourglassCount
                sta HourglassCount
                inc ExHourglassCount
                ldx ExHourglassCount
                cpx #6
                bcc +
                ldx #0
                stx ExHourglassCount
+               ldy #40
                lda HourglassChars0,x
                sta ($58),y
                ldy #80
                lda HourglassChars1,x
                sta ($58),y
                rts

HourglassChars0 !byte 240,242,244,246,246,246; 246,248,250,252,252,252
HourglassChars1 !byte 241,243,245,247,247,247; 247,249,251,253,253,253

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
                jsr MapInIO
                rts

; Expects: cursor index in Y
; Cursors:
; CUR_DEFAULT    = 0
; CUR_RESIZENWSE = 1
; CUR_RESIZENS   = 2
; CUR_RESIZEWE   = 3
; CUR_CARRET     = 4
SetCursor       cpy CurrentCursor
                beq +++
                sty CurrentCursor
                lda CursorSprites0,y
                sta SPRPTR_0
                lda CursorSprites1,y
                sta SPRPTR_1
+++             rts

CursorSprites0  !byte <SP_Mouse0,<SP_ResizeCursorNWSE0,<SP_ResizeCursorNS0,<SP_ResizeCursorWE0,<SP_CarretCursor
CursorSprites1  !byte <SP_Mouse1,<SP_ResizeCursorNWSE1,<SP_ResizeCursorNS1,<SP_ResizeCursorWE1,<SP_CarretCursor


PrepareScrBuf   ; Clear screen
                ldx #0
-               lda #0
                sta SCR_BUF,x
                sta SCR_BUF+$100,x
                sta SCR_BUF+$200,x
                sta SCR_BUF+$270,x ; until Taskbar
                lda CSTM_DesktopClr ; color
                sta CLR_BUF,x
                sta CLR_BUF+$100,x
                sta CLR_BUF+$200,x
                sta CLR_BUF+$270,x ; until Taskbar
                inx
                bne -
                jsr PaintIcons
                rts

GetYCoordCmdMenu; Get y-coord of upper beam
                lda #22
                sec
                sbc #CbmMenuHeight
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

IsInStartMenu   jsr MouseToScr
                lda MouseInfo
                cmp #CbmMenuWidth
                bcs +
                lda MouseInfo+1
                cmp #(22-CbmMenuHeight)
                bcc +
                cmp #22
                bcs +
                lda #1
                rts
+               lda #0
                rts

DrawSpritesDown lda #%00111111
                sta VIC+21
                lda #0; no stretch
                sta VIC+29
                lda #<SP_StartBtnUL
                sta SPRPTR_2
                lda #<SP_StartBtnLR
                sta SPRPTR_3
                lda #CL_BLACK
                sta col2
                lda #CL_WHITE
                sta col3
                lda #229
                sta yPos2
                sta yPos3
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
                lda #29
                sta xPos4
                sta xPos5
                lda #233
                sta yPos4
                sta yPos5
                rts

DrawSpritesUp   ; Draw Commodore sprites
                lda #<SP_Commodore1
                sta SPRPTR_2
                lda #<SP_Commodore2
                sta SPRPTR_3
                lda #CL_DARKBLUE
                sta col2
                lda #CL_RED
                sta col3
                lda #28
                sta xPos2
                sta xPos3
                lda #232
                sta yPos2
                sta yPos3
                ; Draw button frame
                lda #<SP_StartBtnUL
                sta SPRPTR_4
                lda #<SP_StartBtnLR
                sta SPRPTR_5
                lda #CL_WHITE
                sta col4
                lda #CL_BLACK
                sta col5
                lda #25
                sta xPos4
                sta xPos5
                ldx #229
                stx yPos4
                stx yPos5
                ;
                lda #0; no stretch
                sta VIC+29
                lda #%00111111
                sta VIC+21
                lda VIC+16
                and #%11000011
                sta VIC+16
                rts

; Checks if Point is in current window
IsInWndRect     lda Point
                cmp WindowPosX
                bcc +
                sec
                sbc WindowPosX
                cmp WindowWidth
                bcs +
                ;
                lda Point+1
                cmp WindowPosY
                bcc +
                sec
                sbc WindowPosY
                cmp WindowHeight
                bcs +
                ;
                lda #1
                rts
+               lda #0
                rts

ReducedWidth    !byte 0
ReducedHeight   !byte 0
X2              !byte 0
Y2              !byte 0

Drag            lda IsDragging
                bne +
                rts
+               jsr GetMouseInfo
                lda DragType
                bne +
                jmp drag_type_0
+               ;---------------------------
                ; Resizing...
                ;---------------------------
                lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne FE
                ; Find left bound for X2
                lda #7
                sta dummy
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                jsr GetMenubarWidth
                ldx res
                dex
                stx dummy
+               lda WindowPosX
                clc
                adc dummy
                cmp MouseInfo
                bcs +
                lda MouseInfo
+               sta X2
                sec
                sbc WindowPosX
                tax
                inx
                stx WindowWidth
                ; FE
FE              lda WindowBits
                and #BIT_WND_FIXEDHEIGHT
                beq +
                lda WindowHeight
                sta DragNewHeight
                clc
                adc WindowPosY
                sta Y2
                dec Y2
                jmp ++
+               lda WindowPosY
                clc
                adc #6
                cmp MouseInfo+1
                bcs +
                lda MouseInfo+1
+               cmp #22
                bcc +
                lda #21 ; if wnd out of bounds
+               sta Y2
++              sec
                sbc WindowPosY
                tax
                inx
                stx WindowHeight
                jsr UpdateWindow
                jmp RepaintAll
drag_type_0     ;---------------------------
                ; Repositioning...
                ;---------------------------
                ldx DragObjectWidth
                dex
                stx ReducedWidth; Icon: 3
                ldx DragObjectHeight
                dex
                stx ReducedHeight; Icon: 2
                ; Get Y position
                lda MouseInfo+1
                sec
                sbc DragAnchorY
                clc
                adc DragOldPosY
                bpl +
                lda #0
+               sta dummy
                clc
                adc ReducedHeight
                cmp #22
                bcc +
                lda #21
                sec
                sbc ReducedHeight
                sta dummy
+               lda dummy
                sta DragNewPosY
                ; Get X position
                lda MouseInfo
                sec
                sbc DragAnchorX
                clc
                adc DragOldPosX
                bpl +
                lda #0 ; if wnd too far left
+               sta dummy
                clc
                adc ReducedWidth
                cmp #40
                bcc +
                lda #39
                sec
                sbc ReducedWidth
                sta dummy
+               lda dummy
                sta DragNewPosX
                ; Show on screen
                lda DragObjectType
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
                jsr UpdateWindow
                jmp RepaintAll

IsInStartBtn    lda MouseInfo+4
                bne +
                lda MouseInfo+2
                cmp #50
                bcs +
                cmp #26
                bcc +
                lda MouseInfo+3
                cmp #230
                bcc +
                cmp #248
                bcs +
                lda #1
                rts
+               lda #0
                rts

IsInTaskbar     lda MouseInfo+1
                cmp #22
                bcc+
                lda #1
                rts
+               lda #0
                rts

; Expects mouse in task bar
IsInTaskBtns    lda MouseInfo
                cmp #3
                bcc +
                cmp #33
                bcs +
                lda #1
                rts
+               lda #0
                rts

; Paints CBM menu to screen (no highlight)
PaintCbmMenu    lda #<Menu_Start
                sta $fb
                lda #>Menu_Start
                sta $fc
                jsr PaintMenuToBuf
                lda #22
                sec
                sbc #CbmMenuHeight
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

MultiColorOff   lda $d016
                and #%11101111
                sta $d016
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
MouseToScr      lda $d000
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

                lda $d001
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta MouseInfo+1
                rts

;Expects scr pos in Y,X
;Output: scr mem adr in FBFC
PosToScrMemFB   sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $fb
                lda ScrTabHi,x
                adc #0
                sta $fc
                rts

PosToBufFB      sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $fb
                lda BufScrTabHi,x
                adc #0
                sta $fc
                rts

;Expects scr pos in Y,X
;Output: clr mem adr in 0203
PosToClrMem02   sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $02
                lda ClrTabHi,x
                adc #0
                sta $03
                rts

PosToColBuf02   sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $02
                lda BufClrTabHi,x
                adc #0
                sta $03
                rts

; Constants:
SMC_ScrFrom = copy_scrclr+1
SMC_ScrTo   = copy_scrclr+4
SMC_ClrFrom = copy_scrclr+7
SMC_ClrTo   = copy_scrclr+10
; Expects:
;  MapWidth, MapHeight, GapFrom, GapTo,
;  SMC_ScrFrom, SMC_ScrTo, SMC_ClrFrom, SMC_ClrTo
CpyScrClrInfo   ldx MapHeight
                dex
-               ldy MapWidth
                dey
copy_scrclr     lda $FFFF,y; fill with scr from
                sta $FFFF,y; fill with scr to
                lda $FFFF,y; fill with clr from
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
+               ; Update SMC_ClrFrom
                lda SMC_ClrFrom
                clc
                adc GapFrom
                sta SMC_ClrFrom
                bcc +
                inc SMC_ClrFrom+1
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
+               dex
                bpl -
                rts

; Chooses VIC bank at VIC_BANK
; Sets screen memory address at SCRMEM
; Chooses char set at CHAR_BASE
SetGraphicsEnvironment
                ; Choose VIC bank at VIC_BANK
                ; Tell CIA that data comes in at bits 0,1
                lda $dd02
                ora #%00000011
                sta $dd02
                ; Note that bits 0-5 in >VICBANK are 0
                lda #>VICBANK  ; 76543210    c = carry
                rol            ; 6543210c    7
                rol            ; 543210c7    6
                rol            ; 43210c76    5
                eor #%00000011
                sta $fc
                lda $dd00
                and #%11111100
                ora $fc
                sta $dd00
                ; Choose char set at CHAR_BASE
                lda $d018
                and #%11110001
                ora #((>(CHARBASE - VICBANK))/4);#MAINCHARSHI
                sta $d018
                ; Choose screen ram at SCRMEM
                lda #>(SCRMEM - VICBANK) ; hibyte of $1400 = 5400 - 4000
                asl
                asl
                sta $fc
                lda $d018
                and #%00001111
                ora $fc
                sta $d018
                ;
                lda #>SCRMEM
                sta 648
                rts