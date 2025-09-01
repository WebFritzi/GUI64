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
+               ldx Point
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

; Draws a title bar for current window
; Expects 0 or 1 in A (deactivated/activated)
PaintTitleBar   pha
                lda WndAddressInBuf
                sta $fd
                lda WndAddressInBuf+1
                sta $fe
                ; Draw logo, Maximize, Minimize, and Close symbols
                ldy #0
                sty Param
                lda #47
                sta ($fd),y
                ldy WindowWidth
                dey
                lda #44
                sta ($fd),y
                ldx WindowBits
                txa
                and #BIT_WND_CANMAXIMIZE
                beq ++
                inc Param
                dey
                txa ; window bits
                and #BIT_WND_ISMAXIMIZED
                beq +
                lda #48
                jmp store
+               lda #45
store           sta ($fd),y
++              ; No maximize/restore button
                lda WindowBits
                and #BIT_WND_CANMINIMIZE
                beq +
                inc Param
                dey
                lda #46
                sta ($fd),y
+               dey
                lda #4
-               sta ($fd),y
                dey
                bne -
                ; Draw Title string
                lda WindowTitleStr
                sta $fb
                lda WindowTitleStr+1
                beq +++ ; if no title string is specified
                sta $fc
                lda #2
                jsr AddToFD
                lda WindowWidth
                sec
                sbc #4
                sbc Param
                sta Param
                lda #1
                sta Param+1
                lda WindowType
                cmp #WT_DRIVE_A
                beq +
                cmp #WT_DRIVE_B
                beq +
                lda #0
                sta Param+1
+               jsr PrintStrMaxLen
                lda #2
                sta Val
                jsr SubValFromFD
+++             ; Fill Color
                lda $fe
                clc
                adc #$04
                sta $fe
                ldy WindowWidth
                dey
                pla ; previous Param (act/deact)
                bne +
                lda CSTM_DeactiveClr
                jmp loop
+               lda CSTM_ActiveClr
loop            sta ($fd),y
                dey
                bpl loop
                rts

; Deactivate curr wnd (changes titlebar color)
DeactivateWnd   lda VisibleWindows
                beq +
                ldx WindowPosY
                lda ScrTabLo,x
                sta $04
                lda ClrTabHi,x
                sta $05
                +AddByteTo04 WindowPosX
                ; Fill Color
                ldy WindowWidth
                dey
                lda CSTM_DeactiveClr
-               sta ($04),y
                dey
                bpl -
+               rts

; Paints cur wnd to screen
; Expects window in buffer SCR_BUF/CLR_BUF
WindowToScreen  lda WindowWidth
                sta BufWidth
                lda WindowHeight
                sta BufHeight
                clc
                adc WindowPosY
                cmp #23
                bcc +
                sec
                lda #22
                sec
                sbc WindowPosY
                sta BufHeight
+               ldx WindowPosY
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
                ; Only paint when wnd is not minimized
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                beq +
                rts
+               jmp BufToScreen

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
                lda #<SCR_BUF
                sta SMC_ScrFrom
                sta SMC_ClrFrom
                lda #>SCR_BUF
                sta SMC_ScrFrom+1
                clc
                adc #$04
                sta SMC_ClrFrom+1
-               lda $d012
                cmp #100
                bne -
                lda $d011
                and #%10000000
                bne-
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
                lda #4
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
PaintCurWindow  lda CurrentWindow
                bpl +
                rts
+               lda #<SCR_BUF
                sta WndAddressInBuf
                lda #>SCR_BUF
                sta WndAddressInBuf+1
                lda WindowWidth
                sta BufWidth
                lda #1
                sta Param
                ;jsr UpdateDrvSprites
; Paints cur wnd into buffers
; Expects: A = 0 (inactive) or A = 1 (active)
;          BufWidth and BufHeight filled
PaintWndToBuf   jsr PaintTitleBar
                lda #0
                sta BoxPosX
                ldx #1
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                ldx #2
+               stx BoxPosY
                lda WindowWidth
                sta BoxWidth
                ldx WindowHeight
                dex
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                dex
+               stx BoxHeight
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