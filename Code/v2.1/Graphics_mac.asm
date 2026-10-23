InitHourglass   sta+2 MHourglassCount
                jsr DefHourglassChars
                ldx #1
                stx+2 HourglassCount
                dex
                stx+2 ExHourglassCount
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

; Sets hourglass screen pos in ZP_5F/ZP_60
SetHourglassPos ;lda WindowHeight
;                lsr
;                sbc #0
;                clc
;                adc WindowPosY
;                tax
;                lda ScrTabLo,x
;                sta ZP_5F
;                lda ScrTabHi,x
;                sta ZP_60
;                lda WindowWidth
;                lsr
;                sbc #0
;                clc
;                adc WindowPosX
;                clc
;                adc ZP_5F
;                sta ZP_5F
;                bcc +
;                inc ZP_60
;+               rts

                ; y position
                lda WindowHeight
                lsr
                sbc #0
                clc
                adc WindowPosY
                tax
                ;
                lda WindowWidth
                lsr
                sbc #0
                clc
                adc WindowPosX
                tay
                ;
                jsr PosToOffset
                ;
                sta ZP_5F
                lda ZP_60
                adc #>SCRMEM
                sta ZP_60
                rts

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
                lda+2 HourglassChars0,x
                sta (ZP_5F),y
                ldy #80
                ;lda HourglassChars1,x
                clc
                adc #1
                sta (ZP_5F),y
++              rts

HourglassChars0 !byte 248,250,252,254,254,254
;HourglassChars1 !byte 249,251,253,255,255,255

; Result in carry
IsInClock       lda MouseInfo+1
                bne +
                lda MouseInfo
                cmp #34
                rts

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
PatternChar     !byte 160,1

PaintDesktop    ldx CSTM_DeskPattern
                lda PatternChar,x
                sta ZP_5F
                ; Clear screen
                ldx #0
-               lda ZP_5F
                sta DESKTOP_BUF,x
                sta DESKTOP_BUF+$100,x
                sta DESKTOP_BUF+$200,x
                sta DESKTOP_BUF+$2d8,x
                lda CSTM_DesktopClr ; color
                sta DESKTOP_CLR_BUF,x
                sta DESKTOP_CLR_BUF+$100,x
                sta DESKTOP_CLR_BUF+$200,x
                sta DESKTOP_CLR_BUF+$2d8,x
                inx
                bne -
                ; Rounding bottom
                ldx CSTM_DeskPattern
                bne +
                lda #219
                sta DESKTOP_BUF+23*40
                lda #223
                sta DESKTOP_BUF+23*40+39
+               jmp PaintIcons

Drag            lda IsDragging
                bne +
                rts
+               ;
                ;jsr GetMouseInfo
                lda DragType
                beq drag_type_0
                ;---------------------------
                ; Resizing...
                ;---------------------------
                lda MouseInfo+5
                bpl +
                rts
+               lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne FE
                ; Find left bound
                lda #7
                sta ZP_5F
                lda WindowPosX
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
                cmp MouseInfo+5
                bcs +
                lda MouseInfo+5
+               cmp #25;22
                bcc ++
                lda #24;21 ; if wnd out of bounds
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
                ; Get Y position
                lda MouseInfo+5
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
                cmp #24
                bcc +
                lda #23
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
                jsr UpdateWindow
                jmp RepaintAll

; Stores mouse info in MouseInfo: xScr,yScr,x,y,xHiByte,yScr-1
GetMouseInfo    jsr MouseToScr; fills MouseInfo, MouseInfo+1, MouseInfo+5
                lda VIC
                sta MouseInfo+2
                lda VIC+1
                sta MouseInfo+3
                lda VIC+16
                sta MouseInfo+4
                rts

;Returns Mouse pos in scr coords in MouseInfo, MouseInfo+1, MouseInfo+5
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
                ;
                lda $d001
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta MouseInfo+1
                sec
                sbc #1
                sta MouseInfo+5
                rts

; X = y position
; Y = x position
; Result:
; A     = low byte of 40*y+x
; ZP_60 = high byte of 40*y (without carry from x)
; C     = carry from adding x
; Destroys X, ZP_5F, ZP_60
PosToOffset
                stx ZP_60
                sty ZP_5F

                txa
                asl
                asl
                adc ZP_60          ; A = 5*y, C safely 0

                ldx #0
                stx ZP_60

                asl
                rol ZP_60
                asl
                rol ZP_60
                asl
                rol ZP_60          ; ZP_60:A = 40*y, C safely 0

                adc ZP_5F          ; + x, carry belongs to high byte
                rts

; X = y position
; Y = x position
; Result:
; $fb/$fc = Desktop screen buffer
; $02/$03 = Desktop color buffer
PosToDeskBufFB02
!ifdef MAC {
                inx
}
PosToDeskBufFB02_NoINX
                jsr PosToOffset
                sta $fb
                sta $02
                lda ZP_60
                adc #>DESKTOP_BUF
                sta $fc
                adc #4             ; Carry is safely 0 here
                sta $03
                rts

;PosTo_Helper    sty ZP_5F
;                lda ScrTabLo,x
;                clc
;                adc ZP_5F
;                rts

;Expects scr pos in Y,X
;Output: scr mem adr in FBFC
PosToScrMemFB   ;jsr PosTo_Helper
;                sta $fb
;                lda ScrTabHi,x
;                adc #0
;                sta $fc
;                rts
                jsr PosToDeskBufFB02_NoINX
                lda $fc
                eor #DESKBUF_XOR
                sta $fc
                rts

;Expects scr pos in Y,X
;Output: clr mem adr in 0203
PosToClrMem02   ;jsr PosTo_Helper
;                sta $02
;                ;lda ClrTabHi,x
;                lda ScrTabHi,x
;                eor #$38
;                adc #0
;                sta $03
;                rts
                jsr PosToDeskBufFB02_NoINX
                lda $fc
                eor #(DESKBUF_XOR XOR CLRMEM_XOR)
                sta $03
                rts

;PosToDeskBufFB  inx
;                jsr PosTo_Helper
;                sta $fb
;                ;lda BufScrTabHi,x
;                lda ScrTabHi,x
;                eor #DESKBUF_XOR;#$40
;                adc #0
;                sta $fc
;                dex
;                rts

;PosToDeskColBuf02
;                inx
;                jsr PosTo_Helper
;                sta $02
;                ;lda BufScrTabHi,x
;                lda ScrTabHi,x
;                eor #DESKBUF_XOR;#$40
;                adc #4
;                sta $03
;                dex
;                rts