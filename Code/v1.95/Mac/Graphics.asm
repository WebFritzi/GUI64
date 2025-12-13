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
; Expects hourglass screen(!)  position in FBFC
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

HourglassChars0 !byte 246,248,250,252,252,252
HourglassChars1 !byte 247,249,251,253,253,253

; Assumes that mouse is in menu bar
IsInClock       lda MouseInfo+1
                bne +
                lda MouseInfo
                cmp #34
                bcc +
                cmp #40
                bcs +
                lda #1
                rts
+               lda #0
                rts

; Expects: cursor index in Y
; Cursors:
; CUR_DEFAULT    = 0
; CUR_RESIZENWSE = 1
; CUR_RESIZENS   = 2
; CUR_RESIZEWE   = 3
; CUR_CARRET     = 4
; CUR_HOURGLASS  = 5
SetCursor       cpy CurrentCursor
                beq +++
                sty CurrentCursor
                lda CursorSprites0,y
                sta SPRPTR_0
                lda CursorSprites1,y
                sta SPRPTR_1
+++             rts

CursorSprites0  !byte <SP_Mouse0,<SP_ResizeCursorNWSE0,<SP_ResizeCursorNS0,<SP_ResizeCursorWE0,<SP_CarretCursor,<SP_Hourglass
CursorSprites1  !byte <SP_Mouse1,<SP_ResizeCursorNWSE1,<SP_ResizeCursorNS1,<SP_ResizeCursorWE1,<SP_CarretCursor,<SP_HourglassComp

DevCharIndices  !byte 0,0
PaintDesktop    ldx CSTM_DeskPattern
                lda PatternChar,x
                sta dummy
                ; Clear screen
                ldx #0
-               lda dummy
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
                lda #240
                sta DESKTOP_BUF+23*40
                lda #241
                sta DESKTOP_BUF+23*40+39
+               ; Paint drive icons
                ; Only drives
                lda DrvSymTop,x
                sta DESKTOP_BUF+37
                sta DESKTOP_BUF+38
                sta DESKTOP_BUF+120+37
                sta DESKTOP_BUF+120+38
                lda DrvSymLeft,x
                sta DESKTOP_BUF+40+36
                sta DESKTOP_BUF+160+36
                lda DrvSymRight,x
                sta DESKTOP_BUF+40+39
                sta DESKTOP_BUF+160+39
                ldx #18
                stx DESKTOP_BUF+40+37
                stx DESKTOP_BUF+160+37
                inx
                stx DESKTOP_BUF+40+38
                stx DESKTOP_BUF+160+38
                ; Device numbers
                ; A
                lda DeviceNumbers
                jsr GetDrvCharInds
                sta DESKTOP_BUF+80+37
                lda DevCharIndices+1
                sta DESKTOP_BUF+80+38
                ; B
                lda DeviceNumbers+1
                jsr GetDrvCharInds
                sta DESKTOP_BUF+200+37
                lda DevCharIndices+1
                sta DESKTOP_BUF+200+38
                ; Colors
                lda #CL_LIGHTGRAY
                sta DESKTOP_CLR_BUF+40+37
                sta DESKTOP_CLR_BUF+40+38
                sta DESKTOP_CLR_BUF+160+37
                sta DESKTOP_CLR_BUF+160+38
                rts

; Depending on the device number in A, finds the right
; char indices in the char set for the drive icons
; and copies them into DevCharIndices
GetDrvCharInds  sta file_size; for possible later conversion
                ldx #0
                stx file_size+1
                cmp #8
                bne +
                ; It's #8
                ldx #20
                stx DevCharIndices
                inx
                stx DevCharIndices+1
                jmp ++
+               cmp #9
                bne +
                ; It's #9
                ldx #16
                stx DevCharIndices
                inx
                stx DevCharIndices+1
                jmp ++
+               ; It's between 10 and 29
                jsr ConvertToDec
                lda file_size_dec
                lsr
                lsr
                lsr
                lsr
                clc
                adc #223
                sta DevCharIndices
                lda file_size_dec
                and #%00001111
                clc
                adc #226
                sta DevCharIndices+1
++              lda DevCharIndices
                rts

; Checks if cursor is in drive icons
; Result in A
IsInDriveIcon_B lda MouseInfo+5
                cmp #6
                bcs nope
                cmp #4
                bcc nope
                jmp IsIn_X_range
IsInDriveIcon_A lda MouseInfo+5
                beq nope
                cmp #3
                bcs nope
IsIn_X_range    lda MouseInfo
                cmp #39
                bcs nope
                cmp #37
                bcc nope
                lda #1
                rts
nope            lda #0
                rts

; Checks if Point is in current window
; Result in A
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

ReducedWndWidth !byte 0
ReducedWndHeight!byte 0
X2              !byte 0
Y2              !byte 0

Drag            lda IsWndDragging
                bne +
                rts
+               ;
                ldx WindowWidth
                dex
                stx ReducedWndWidth
                ldx WindowHeight
                dex
                stx ReducedWndHeight
                ;
                jsr GetMouseInfo
                lda DragType
                bne +
                jmp drag_type_0
+               ;---------------------------
                ; Resizing...
                ;---------------------------
                lda MouseInfo+5
                bpl +
                rts
+               lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne FE
                ; Find left bound for X2
                lda #7
                sta dummy
                lda WindowPosX
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
                sta NewHeight
                clc
                adc WindowPosY
                sta Y2
                dec Y2
                jmp ++
+               lda WindowPosY
                clc
                adc #6
                cmp MouseInfo+5
                bcs +
                lda MouseInfo+5
+               cmp #25;22
                bcc +
                lda #24;21 ; if wnd out of bounds
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
                ; Get Y position
                lda MouseInfo+5
                sta dummy
                bpl +
                lda #0
                sta dummy
                jmp ++
+               clc
                adc ReducedWndHeight
                cmp #24;22
                bcc ++
                lda #23;21
                sec
                sbc ReducedWndHeight
                sta dummy
++              lda dummy
                sta NewPosY
                ; Get X position
                lda MouseInfo
                sec
                sbc DragWndAnchorX
                clc
                adc OldPosX
                bpl +
                lda #0 ; if wnd too far left
+               sta dummy
                clc
                adc ReducedWndWidth
                cmp #40
                bcc +
                lda #39
                sec
                sbc ReducedWndWidth
                sta dummy
+               lda dummy
                sta NewPosX
                ; Show on screen if necessary
                lda NewPosX
                cmp WindowPosX
                bne +
                lda NewPosY
                cmp WindowPosY
                bne +
                rts
+               lda NewPosX
                sta WindowPosX
                lda NewPosY
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