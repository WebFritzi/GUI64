; Removes tooltip if tooltip is on
RemoveTooltip   lda bTooltipOn
                beq +
                lda #0
                sta bTooltipOn
                jsr RepaintAll
+               rts

; Shows tooltip with string at screen pos (Y/X)
; Expects: 
; string address in FDFE
; Param=0: Lower case, Param=1: Upper case
ShowTooltip     lda #1
                sta bTooltipOn
                stx Point
                sty Point+1
                jsr GetStrLen; res = strlen
                lda $fd
                pha
                lda $fe
                pha
                ; Prepare for using PaintBoxToFD02
                ldx res; strlen
                inx
                inx
                stx BoxWidth
                lda #3
                sta BoxHeight
                lda CSTM_WindowClr
                sta BoxColor
                lda #40
                sta BufWidth
                ; Correct x pos in Y
                ldy Point+1
                tya
                clc
                adc res
                cmp #39
                bcc +
                sec
                sbc #38
                sta res
                ldy Point+1
                tya
                sec
                sbc res
                tay
                dey ; added in Mac version
                dey ; added in Mac version
                dey ; added in Mac version
+               ldx Point
                inx ; added in Mac version
                jsr PosToClrMem02
                lda $02
                sta $fd
                pha
                lda $03
                clc
                adc #>SCRMEM_MINUS_CLRMEM
                sta $fe
                pha
                jsr PaintBoxToFD02; changes FD
                ;
                pla
                sta $fe
                pla
                sta $fd
                clc
                adc #41
                sta $fd
                bcc +
                inc $fe
+               pla
                sta $fc
                pla
                sta $fb
                lda Param; upper case?
                beq +
                jsr PrintStringUC
                rts
+               jsr PrintStringLC
                rts

strlen          !byte 0
active          !byte 0
; Draws a title bar for current window
; Expects 0 or 1 in active
PaintTitleBar   ; Print (poss. shortened) title string to FREEMEM
                lda #<FREEMEM
                sta $fd
                lda #>FREEMEM
                sta $fe
                lda WindowTitleStr
                sta $fb
                lda WindowTitleStr+1
                bne + ; if no title string is specified
                rts
+               sta $fc
                lda WindowWidth
                sec
                sbc #4
                sta Param
                jsr PrintStrMaxLen
                sty strlen
                ;
                lda WndAddressInBuf
                sta $fd
                lda WndAddressInBuf+1
                sta $fe
                ;
                lda active
                bne +
                ; Draw deactivated wnd title bar
                ldy WindowWidth
                dey
                ;lda #40
                +LDA_CHAR 40
                sta ($fd),y
                dey
                +LDA_CHAR 36
-               sta ($fd),y
                dey
                bpl -
                iny
                +LDA_CHAR 35
                sta ($fd),y
                jmp ++
+               ; Draw activated wnd title bar
                ldy WindowWidth
                dey
                +LDA_CHAR 44
                sta ($fd),y
                dey
                +LDA_CHAR 237
-               sta ($fd),y
                dey
                bne -
                +LDA_CHAR 46
                sta ($fd),y
                iny
                +LDA_CHAR 47
                sta ($fd),y
++              ; Paint title string from FREEMEM
                lda WindowWidth
                sec
                sbc strlen
                lsr
                sta dummy
                lda WindowWidth
                sec
                sbc dummy
                tay
                lda active
                beq +
                +LDA_CHAR 236
                sta ($fd),y
+               dey
                ldx strlen
                dex
-               lda FREEMEM,x
                jsr PetUCtoTitlebar
                sta ($fd),y
                dey
                dex
                bpl -
                lda active
                beq +++
                +LDA_CHAR 238
                cpy #1
                bne +
                +LDA_CHAR 239
+               sta ($fd),y
+++             ; Fill Color
                lda $fe
                clc
                adc #$04
                sta $fe
                ldy WindowWidth
                dey
                lda CSTM_TitleClr
-               sta ($fd),y
                dey
                bpl -
                rts

; Paints cur wnd to screen
; Expects window in buffer SCR_BUF/CLR_BUF
WindowToScreen  lda CurrentWindow
                bpl +
                rts
+               lda WindowWidth
                sta BufWidth
                lda WindowHeight
                sta BufHeight
                ldx WindowPosY
                inx
                lda ScrTabLo,x
                sta $fb
                sta $fd
                lda ScrTabHi,x
                sta $fc
                lda ClrTabHi,x
                sta $fe
                lda WindowPosX
                jsr AddToFB
                lda WindowPosX
                jsr AddToFD
                jmp BufToScreen

; Paints buffer to screen
; Expects:
; SCR dest coords in $FBFC
; BufWidth and BufHeight filled
BufToScreen     lda #40
                sta GapTo
                lda BufWidth
                sta GapFrom
                sta MapWidth
                lda BufHeight
                sta MapHeight
                lda $fb
                sta SMC_ScrTo
                sta SMC_ClrTo
                lda $fc
                sta SMC_ScrTo+1
                sec
                sbc #>SCRMEM_MINUS_CLRMEM
                sta SMC_ClrTo+1
                lda #<DESKTOP_BUF
                sta SMC_ScrFrom
                sta SMC_ClrFrom
                lda #>DESKTOP_BUF
                sta SMC_ScrFrom+1
                clc
                adc #$04
                sta SMC_ClrFrom+1
                jmp CpyScrClrInfo

; Positions in buffer
BoxPosX         !byte 0
BoxPosY         !byte 0
BoxWidth        !byte 0
BoxHeight       !byte 0
BoxColor        !byte 0
; Paints box to buffer
; Expects BufWidth and BoxPosX,...,BoxHeight and BoxColor filled
PaintBoxToBuf   ; Find pos in buffers
                lda WndAddressInBuf
                sta $fd
                sta $02
                lda WndAddressInBuf+1
                sta $fe
                clc
                adc #$04
                sta $03
                ldx BoxPosY
                beq +
                dex
-               jsr AddBufWidthToFD
                jsr AddBufWidthTo02
                dex
                bpl -
+               lda BoxPosX
                jsr AddToFD
                lda BoxPosX
                jsr AddTo02
PaintBoxToFD02  ; First line
                ldy BoxWidth
                cpy #2
                bcs +
                rts
+               dey
                lda #40
                sta ($fd),y
                dey
                lda #36
-               sta ($fd),y
                dey
                bne -
                lda #35
                sta ($fd),y
                ; Intermediate lines
                ldx BoxHeight
                cpx #2
                beq +
                dex
                dex
--              jsr AddBufWidthToFD
                ldy BoxWidth
                dey
                lda #41
                sta ($fd),y
                dey
                lda #160
-               sta ($fd),y
                dey
                bne -
                lda #37
                sta ($fd),y
                dex
                bne --
+               ; Last line
                jsr AddBufWidthToFD
                ldy BoxWidth
                dey
                lda #42
                sta ($fd),y
                dey
                lda #39
-               sta ($fd),y
                dey
                bne -
                lda #38
                sta ($fd),y
                ; Fill box with color
                ldx BoxHeight
                dex
--              ldy BoxWidth
                dey
                lda BoxColor
-               sta ($02),y
                dey
                bpl -
                jsr AddBufWidthTo02
                dex
                bpl --
                rts

; Paints cur wnd (active) to SCR/CLR_BUF
PaintCurWindow  lda #1
                sta active
                jmp +
PaintCurWndDeac lda #0
                sta active
+               lda CurrentWindow
                bpl +
                rts
+               lda #<DESKTOP_BUF
                sta WndAddressInBuf
                lda #>DESKTOP_BUF
                sta WndAddressInBuf+1
                lda WindowWidth
                sta BufWidth
; Paints cur wnd into buffers
; Expects: active = 0 or active = 1
;          BufWidth and BufHeight filled
PaintWndToBuf   jsr PaintTitleBar
                lda #0
                sta BoxPosX
                ldx #1
                stx BoxPosY
                lda WindowWidth
                sta BoxWidth
                ldx WindowHeight
                dex
                beq +
                stx BoxHeight
                lda CSTM_WindowClr
                sta BoxColor
                jsr PaintBoxToBuf
                jsr PaintControls
                ; Resize symbol in lower right corner
                lda WindowBits
                and #BIT_WND_RESIZABLE
                beq +
                lda WndAddressInBuf
                sta $fb
                lda WndAddressInBuf+1
                sta $fc
                ldx WindowWidth
                dex
                stx dummy
                txa
                jsr AddToFB
                ldx WindowHeight
                dex
-               lda BufWidth
                jsr AddToFB
                dex
                bne -
                lda #43
                ldy #0
                sta ($fb),y
+               rts
;                ; Paint shadow
;                lda CurrentWindow
;                cmp WndPriorityList
;                bne +++
;                lda WindowBits
;                and #BIT_WND_IS_ICONIZED
;                bne +++
                
;                lda BufWidth
;                cmp #40
;                bne +++
;                lda BufHeight
;                cmp #24
;                bne +++
;                lda WndAddressInBuf
;                sta $fb
;                lda WndAddressInBuf+1
;                clc
;                adc #$04
;                sta $fc
;                lda BufWidth
;                jsr AddToFB
                
;                ldy WindowWidth
;                lda WindowHeight
;                sta HeightCount
                
;                lda WindowPosX
;                clc
;                adc WindowWidth
;                cmp #40
;                bcc +
;                ldx WindowHeight
;                dex
;-               lda BufWidth
;                jsr AddToFB
;                dex
;                bne -
;                beq ++
                
;+               dec HeightCount
                
;-               lda ($fb),y
;                tax
;                lda ComplementCols,x
;                sta ($fb),y
;                lda BufWidth
;                jsr AddToFB
;                dec HeightCount
;                bne -
                
;++              lda WindowPosY
;                clc
;                adc WindowHeight
;                cmp #24
;                bcs +++
                
;                lda HeightCount
;                beq +
;                dey
;+
;-               lda ($fb),y
;                tax
;                lda ComplementCols,x
;                sta ($fb),y
;                dey
;                bne -
;+++             rts

;ComplementCols  !byte 0,11,11,12,11,12,11,12,11,11,12,11,11,12,11,12
;HeightCount     !byte 0