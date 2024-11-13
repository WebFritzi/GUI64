;; Checks if system is PAL or NTSC and respectively
;; If carry is clear then it's NTSC
;PALNTSC         lda $d012
;-               cmp $d012
;                beq -
;                bmi PALNTSC
;                cmp #$20
;                rts

char_offset     !byte 39,15
SetBkgPattern   lda CSTM_DeskPattern
                pha
                tax
                lda char_offset,x
                tax
                ldy #7
-               lda Chars,x
                sta Chars,y
                sta TaskbarChars,y
                dex
                dey
                bpl -
                pla
                beq +
                ; Dotted
                lda #<(DriveBkgDottedFull/64)
                sta SP_DriveBkgFull
                lda #<(DriveBkgDottedLeft/64)
                sta SP_DriveBkgLeft
                rts
+               ; Solid
                lda #<(DriveBkgSolidFull/64)
                sta SP_DriveBkgFull
                lda #<(DriveBkgSolidLeft/64)
                sta SP_DriveBkgLeft
                rts

IsAtPressedPoint
                lda #0
                sta res
                lda MouseInfo
                cmp PressedPoint
                bne +
                lda MouseInfo+1
                cmp PressedPoint+1
                bne +
                lda #1
                sta res
+               rts

; Expects: cursor index in Param
SetCursor       lda Param
                cmp CurrentCursor
                beq +++
                sta CurrentCursor
                lda CurrentCursor
                beq ++
                cmp #CUR_RESIZENWSE
                bne +
                ; Set NWSE cursor
                lda #<SP_ResizeCursorNWSE0
                sta SPRPTR_0
                lda #<SP_ResizeCursorNWSE1
                sta SPRPTR_1
                rts
+               cmp #CUR_CARRET
                bne +
                ; Set carret cursor
                lda #<SP_CarretCursor
                sta SPRPTR_0
                lda #<SP_CarretCursor
                sta SPRPTR_1
                rts
+               cmp #CUR_RESIZENS
                bne +
                ; Set NS cursor
                lda #<SP_ResizeCursorNS0
                sta SPRPTR_0
                lda #<SP_ResizeCursorNS1
                sta SPRPTR_1
                rts
+               cmp #CUR_RESIZEWE
                bne +
                ; Set WE cursor
                lda #<SP_ResizeCursorWE0
                sta SPRPTR_0
                lda #<SP_ResizeCursorWE1
                sta SPRPTR_1
+               rts
++              ; Set default cursor
                lda #<SP_Mouse0
                sta SPRPTR_0
                lda #<SP_Mouse1
                sta SPRPTR_1
+++             rts

;PaintDesktop    jsr PrepareScrBuf
;                lda #40
;                sta BufWidth
;                lda #22
;                sta BufHeight
;                lda #<SCRMEM
;                sta $fb
;                lda #>SCRMEM
;                sta $fc
;                jsr BufToScreen
;                rts

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
                ; Paint drives
                lda #58
                sta SCR_BUF+201
                lda #59
                sta SCR_BUF+202
                lda #62
                sta SCR_BUF+41
                lda #63
                sta SCR_BUF+42
                ldx CSTM_DeskPattern
                lda DrvSymLeft,x
                sta SCR_BUF+80
                sta SCR_BUF+240
                lda #60
                sta SCR_BUF+81
                sta SCR_BUF+241
                lda #61
                sta SCR_BUF+82
                sta SCR_BUF+242
                lda DrvSymRight,x
                sta SCR_BUF+83
                sta SCR_BUF+243
                lda #15
                sta CLR_BUF+41
                sta CLR_BUF+42
                sta CLR_BUF+81
                sta CLR_BUF+82
                sta CLR_BUF+201
                sta CLR_BUF+202
                sta CLR_BUF+241
                sta CLR_BUF+242
                rts

IsInDrive9Icon  lda #0
                sta res
                lda MouseInfo+1
                cmp #7
                bcs +
                cmp #5
                bcc +
                lda MouseInfo
                cmp #3
                bcs +
                cmp #1
                bcc +
                lda #1
                sta res
+               rts

IsInDrive8Icon  lda #0
                sta res
                lda MouseInfo+1
                cmp #3
                bcs +
                cmp #1
                bcc +
                lda MouseInfo
                cmp #3
                bcs +
                cmp #1
                bcc +
                lda #1
                sta res
+               rts

Highlight       lda MayHighlight
                bne +
                rts
+               lda MenuItem
                bpl +
                rts
+               ; Get y-coord of upper beam
                lda #22
                sec
                sbc #StartMenuHeight
                ; Get y-coord of StartMenu (Scr coords)
                asl
                asl
                asl
                clc
                adc #54
                sta dummy_irq
                ;
                lda #%00111111
                sta VIC+21
                lda #<SP_BalkenSchmal
                sta SPRPTR_2
                sta SPRPTR_3
                lda #<SP_Balken
                sta SPRPTR_4
                sta SPRPTR_5
                lda CSTM_ActiveClr
                sta col2
                sta col3
                sta col4
                sta col5
                ;
                lda #32
                sta xPos2
                lda #72
                sta xPos3
                lda #28
                sta xPos4
                lda #76
                sta xPos5
                ; dummy + MenuItem*16
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
                ;
                lda #%00010100
                sta SPR_STRETCH_HORZ ; wide sprites
                lda #%00001100
                sta SPR_PRIORITY
                rts

IsInStartMenu   lda #0
                sta res
                jsr MouseToScr
                lda MouseInfo
                cmp #StartMenuWidth
                bcs +
                lda MouseInfo+1
                cmp #(22-StartMenuHeight)
                bcc +
                cmp #22
                bcs +
                lda #1
                sta res
+               rts

DrawSpritesDown lda #%01111111
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
                lda #%11111111
                sta VIC+21
                lda VIC+16
                and #%11000011
                sta VIC+16
                rts

GetNewDrvSprites
                lda #1
                sta Point
                lda #1
                sta Point+1
                jsr IsInWndRect
                lda res
                bne ++++
                lda #2
                sta Point
                jsr IsInWndRect
                lda res
                bne +++
                lda #1
                sta Point
                lda #5
                sta Point+1
                jsr IsInWndRect
                lda res
                bne ++
                lda #2
                sta Point
                jsr IsInWndRect
                lda res
                bne +
                lda #%00001111
                sta NewDriveSprites
                rts
+               lda #%00001110
                sta NewDriveSprites
                rts
++              lda #%00001100
                sta NewDriveSprites
                rts
+++             lda #%00001010
                sta NewDriveSprites
                rts
++++            ; All sprites covered -> delete 'em all
                lda #0
                sta NewDriveSprites
                rts

UpdateDrvSprites
                jsr GetNewDrvSprites
                lda DriveSprites
                and NewDriveSprites
                sta DriveSprites
                rts

; Checks if Point is in current window
IsInWndRect     lda #0
                sta res
                ; X
                lda Point
                cmp WindowPosX
                bcc +
                lda WindowPosX
                clc
                adc WindowWidth
                tax
                dex
                cpx Point
                bcc +
                ; Y
                lda Point+1
                cmp WindowPosY
                bcc +
                lda WindowPosY
                clc
                adc WindowHeight
                tax
                dex
                cpx Point+1
                bcc +
                lda #1
                sta res
+               rts

DrawDriveSprites
                lda DriveSprites
                lsr
                lsr
                bne +
                ; No sprite to show
                jmp Drive9
+               and #%00000001
                bne +
                lda SP_DriveBkgLeft
                jmp ++
+               lda SP_DriveBkgFull
++              sta SPRPTR_2
                lda CSTM_DesktopClr
                sta col2
                lda #32
                sta xPos2
                lda #50
                sta yPos2
Drive9          lda DriveSprites
                and #%00000011
                bne +
                ; No sprite to show
                rts
+               and #%00000001
                bne +
                lda SP_DriveBkgLeft
                jmp ++
+               lda SP_DriveBkgFull
++              sta SPRPTR_3
                lda CSTM_DesktopClr
                sta col3
                lda #32
                sta xPos3
                lda #82
                sta yPos3
                ;
                lda #0
                sta SPR_STRETCH_HORZ
                lda #%00111111
                sta VIC+21
                lda SPR_PRIORITY
                ora #%00001100
                sta SPR_PRIORITY
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
                stx dummy_irq
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
                sta NewHeight
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
                jsr RepaintAll
                rts
drag_type_0     ;---------------------------
                ; Repositioning...
                ;---------------------------
                ; Get Y position
                lda MouseInfo+1
                sta dummy
                clc
                adc ReducedWndHeight
                cmp #22
                bcc +
                lda #21
                sec
                sbc ReducedWndHeight
                sta dummy
+               lda dummy
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
                jsr RepaintAll
                rts

IsInStartBtn    lda #0
                sta res
                lda MouseInfo+4
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
                sta res
+               rts

IsInTaskbar     lda #0
                sta res
                lda MouseInfo+1
                cmp #22
                bcc+
                lda #1
                sta res
+               rts

; Expects mouse in task bar
IsInTaskBtns    lda #0
                sta res
                lda MouseInfo
                cmp #3
                bcc +
                cmp #33
                bcs +
                lda #1
                sta res
+               rts

PaintStartMenu  lda #<Menu_Start
                sta $fb
                lda #>Menu_Start
                sta $fc
                jsr PaintMenuToBuf
                ldx #17
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

; Stores mouse info in MouseInfo
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

;Expects scr pos in Y,X
;Output: scr mem adr in FDFE
PosToClrMem     sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $fd
                lda ClrTabHi,x
                adc #0
                sta $fe
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
copy_scrclr     ;sei
;                lda #52
;                sta $01
;                lda $d020
;                cmp #33
;                beq +
;                inc SCRMEM
;+               lda #54
;                sta $01
;                cli

                lda $FFFF,y; fill with scr from
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

;SMC_ScrFrom_BS = copy_scrclr_bs+1
;SMC_ScrTo_BS   = copy_scrclr_bs+4
;SMC_ClrFrom_BS = copy_scrclr_bs+7
;SMC_ClrTo_BS   = copy_scrclr_bs+10
;; Expects:
;;  MapWidth, MapHeight, BufWidth,
;;  SMC_ScrFrom, SMC_ScrTo
;CpyBufToScreen  ; Prepare color addresses
;                lda SMC_ScrFrom_BS
;                sta SMC_ClrFrom_BS
;                lda SMC_ScrFrom_BS+1
;                clc
;                adc #$04
;                sta SMC_ClrFrom_BS+1
;                lda SMC_ScrTo_BS
;                sta SMC_ClrTo_BS
;                lda SMC_ScrTo_BS+1
;                clc
;                adc #$d4;#>CLRMEM_MINUS_SCRMEM
;                sta SMC_ClrTo_BS+1
;                ; Start loop
;                ldx MapHeight
;                dex
;-               ldy MapWidth
;                dey
;copy_scrclr_bs  lda $FFFF,y; fill with scr from
;                sta $FFFF,y; fill with scr to
;                lda $FFFF,y; fill with clr from
;                sta $FFFF,y; fill with clr to
;                ;----------------------
;                dey
;                bpl copy_scrclr_bs
;                ; Update SMC_ScrFrom and SMC_ClrFrom
;                lda SMC_ScrFrom_BS
;                clc
;                adc BufWidth
;                sta SMC_ScrFrom_BS
;                sta SMC_ClrFrom_BS
;                bcc +
;                inc SMC_ScrFrom_BS+1
;                inc SMC_ClrFrom_BS+1
;+               ; Update SMC_ScrTo and SMC_ClrTo
;                lda SMC_ScrTo_BS
;                clc
;                adc #40
;                sta SMC_ScrTo_BS
;                sta SMC_ClrTo_BS
;                bcc +
;                inc SMC_ScrTo_BS+1
;                inc SMC_ClrTo_BS+1
;+               dex
;                bpl -
;                rts
;
;SMC_ScrFrom_SB = copy_scrclr_sb+1
;SMC_ScrTo_SB   = copy_scrclr_sb+4
;SMC_ClrFrom_SB = copy_scrclr_sb+7
;SMC_ClrTo_SB   = copy_scrclr_sb+10
;; Expects:
;;  MapWidth, MapHeight, GapFrom, GapTo,
;;  SMC_ScrFrom, SMC_ScrTo
;CpyScrToBuf     ; Prepare color addresses
;                lda SMC_ScrFrom_SB
;                sta SMC_ClrFrom_SB
;                lda SMC_ScrFrom_SB+1
;                clc
;                adc #>CLRMEM_MINUS_SCRMEM
;                sta SMC_ClrFrom_SB+1
;                lda SMC_ScrTo_SB
;                sta SMC_ClrTo_SB
;                lda SMC_ScrTo_SB+1
;                clc
;                adc #$04
;                sta SMC_ClrTo_SB+1
;                ; Start loop
;                ldx MapHeight
;                dex
;-               ldy MapWidth
;                dey
;copy_scrclr_sb  lda $FFFF,y; fill with scr from
;                sta $FFFF,y; fill with scr to
;                lda $FFFF,y; fill with clr from
;                sta $FFFF,y; fill with clr to
;                ;----------------------
;                dey
;                bpl copy_scrclr_sb
;                ; Update SMC_ScrFrom and SMC_ClrFrom
;                lda SMC_ScrFrom_SB
;                clc
;                adc #40
;                sta SMC_ScrFrom_SB
;                sta SMC_ClrFrom_SB
;                bcc +
;                inc SMC_ScrFrom_SB+1
;                inc SMC_ClrFrom_SB+1
;+               ; Update SMC_ScrTo and SMC_ClrTo
;                lda SMC_ScrTo_SB
;                clc
;                adc BufWidth
;                sta SMC_ScrTo_SB
;                sta SMC_ClrTo_SB
;                bcc +
;                inc SMC_ScrTo_SB+1
;                inc SMC_ClrTo_SB+1
;+               dex
;                bpl -
;                rts

; Chooses VIC bank at VIC_BANK
; Sets screen memory address at SCRMEM
; Chooses char set at CHAR_BASE
SetGraphicsEnvironment
                ; Choose VIC bank at VIC_BANK
                ; Tell CIA that data comes in at bits 0,1
                lda $dd02
                ora #%00000011
                sta $dd02
                lda #>VICBANK
                lsr
                lsr
                lsr
                lsr
                lsr
                lsr
                eor #%00000011
                sta $fc
                lda $dd00
                and #%11111100
                ora $fc
                sta $dd00
                ; Choose default char set at CHAR_BASE
                lda #>(CHARBASE - VICBANK) ; hibyte of $1000 = $9000 - $8000
                lsr
                lsr
                sta $fc
                lda $d018
                and #%11110001
                ora $fc
                sta $d018
                ; Choose screen ram at SCRMEM
                lda #>(SCRMEM - VICBANK) ; hibyte of $1400 = 5400 - 4000
                asl
                asl
                sta $fc
                lda $d018
                and #$0f
                ora $fc
                sta $d018
                ;
                lda #>SCRMEM
                sta 648
                rts