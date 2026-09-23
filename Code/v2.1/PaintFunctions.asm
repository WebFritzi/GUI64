; Removes tooltip if tooltip is on
RemoveTooltip   lda bTooltipOn
                beq +
                lda #0
                sta bTooltipOn
                jmp RepaintAll
+               rts

; Shows tooltip with string at screen pos (Y/X)
; Expects: 
; string address in FDFE
; Param0=0: Lower case, Param0=1: Upper case
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
                ;sec
                sbc #38
                sta res
                ldy Point+1
                tya
                sec
                sbc res
                tay
!ifdef MAC{
                ;dey ; added in Mac version
                ;dey ; added in Mac version
                ;dey ; added in Mac version
}
+               ldx Point
!ifdef MAC{
                inx ; added in Mac version
}
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
                clc
                adc #41
                sta $fd
                bcc +
                inc $fe
+               pla
                sta $fc
                pla
                sta $fb
                lda Param0; upper case?
                jmp PrintStringCase

WndAddrInBufToFD
                lda WndAddressInBuf
                sta $fd
                lda WndAddressInBuf+1
                sta $fe
                rts

!ifdef WIN{
; Draws a title bar for current window
; Expects 0 or 1 in A (deactivated/activated)
PaintTitleBar   pha
                jsr WndAddrInBufToFD
                ; Draw logo, Maximize, Minimize, and Close symbols
                ldy #0
                sty Param0
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
                inc Param0
                dey
                txa ; window bits
                and #BIT_WND_ISMAXIMIZED
                beq +
                lda #48
                bne store; jmp store
+               lda #45
store           sta ($fd),y
++              ; No maximize/restore button
                lda WindowBits
                and #BIT_WND_CANMINIMIZE
                beq +
                inc Param0
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
                sbc Param0
                sta Param0
                lda #1
                sta Param1
                lda WindowType
                cmp #WT_DRIVE_A
                beq +
                cmp #WT_DRIVE_B
                beq +
                cmp #WT_ULTIMATE
                beq +
                lda #0
                sta Param1
+               jsr PrintStrMaxLen
                lda #2
                ;sta Val
                jsr SubAFromFD
+++             ; Fill Color
                lda $fe
                clc
                adc #$04
                sta $fe
                ldy WindowWidth
                dey
                pla ; previous Param0 (act/deact)
                bne +
                lda CSTM_DeactiveClr
                jmp loop
+               lda CSTM_ActiveClr
loop            sta ($fd),y
                dey
                bpl loop
                rts
} else ifdef MAC{
strlen          !byte 0
;active          !byte 0 ZP
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
                sta Param0
                jsr PrintStrMaxLen
                sty strlen
                ;
                jsr WndAddrInBufToFD
                ;
                lda active
                bne +
                ; Draw deactivated wnd title bar
                ldy WindowWidth
                dey
                lda #40
                sta ($fd),y
                dey
                lda #36
-               sta ($fd),y
                dey
                bpl -
                iny
                lda #35
                sta ($fd),y
                jmp ++
+               ; Draw activated wnd title bar
                ldy WindowWidth
                dey
                lda #44
                sta ($fd),y
                dey
                lda #50
-               sta ($fd),y
                dey
                bne -
                lda #46
                sta ($fd),y
                iny
                lda #47
                sta ($fd),y
++              ; Paint title string from FREEMEM
                lda WindowWidth
                sec
                adc strlen
                lsr
                tay
                lda active
                beq +
                lda #49
                sta ($fd),y
+               dey
                ldx strlen
                dex
-               lda FREEMEM,x
                ;jsr PetUCtoTitlebar
                sta ($fd),y
                dey
                dex
                bpl -
                lda active
                beq +++
                lda #51
                cpy #1
                bne +
                lda #241
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
}

!ifdef WIN{
; Deactivate cur wnd (changes titlebar color)
DeactivateWnd   lda VisibleWindows
                beq ++

                ldx WindowPosY
                lda ScrTabLo,x
                clc
                adc WindowPosX
                sta $04
                lda ScrTabHi,x
                ;+EOR_WM $3c,$38
                eor #CLRMEM_XOR
                adc #0
                sta $05

                ; Fill Color
                ldy WindowWidth
                dey
                lda CSTM_DeactiveClr
-               sta ($04),y
                dey
                bpl -
++              rts
}

WindowToScreen  lda CurrentWindow
                bpl +
                rts
+               lda WindowWidth
                sta BufWidth
                lda WindowHeight
                sta BufHeight
!ifdef WIN{
; Paints cur wnd to screen
; Expects window in buffer SCR_BUF/CLR_BUF
                clc
                adc WindowPosY
                cmp #23
                bcc +
                lda #22
                ;sec
                sbc WindowPosY
                sta BufHeight
+               ldx WindowPosY
                ;lda ScrTabLo,x
                ;sta $fb
                ;sta $fd
                ;lda ScrTabHi,x
                ;sta $fc
                ;;lda ClrTabHi,x
                ;+EOR_WM $3c,$38
                ;sta $fe
                ;lda WindowPosX
                ;jsr AddToFB
                ;lda WindowPosX
                ;jsr AddToFD
                ;; Only paint when wnd is not minimized
                ;lda WindowBits
                ;and #BIT_WND_ISMINIMIZED
                ;beq BufToScreen
                ;rts
} else ifdef MAC{
; Paints cur wnd to screen
; Expects window in buffer SCR_BUF/CLR_BUF
                ldx WindowPosY
                inx
}
                lda ScrTabLo,x
                sta $fb
                sta $fd
                lda ScrTabHi,x
                sta $fc
                ;lda ClrTabHi,x
                ;lda ScrTabHi,x
                ;+EOR_WM $3c,$38
                eor #CLRMEM_XOR
                sta $fe
                lda WindowPosX
                jsr AddToFB
                lda WindowPosX
                jsr AddToFD
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
                jsr WaitRaster_100
                lda #$ff
                jmp CpyScrClrInfo

WaitRaster_100  lda $d012
                cmp #100
                bne WaitRaster_100
                bit $d011
                bmi WaitRaster_100
                rts

; Computes col buf pos from scr buf pos
SetColBufPos    lda $fd
                sta $02
                lda $fe
                clc
                adc #$04
                sta $03
                rts

; Paints box to buffer
; Expects BufWidth and BoxPosX,...,BoxHeight and BoxColor filled
PaintBoxToBuf   ; Find pos in buffers
                jsr WndAddrInBufToFD
                ;
                ldx BoxPosY
                beq +
                dex
-               jsr AddBufWidthToFD
                dex
                bpl -
+               lda BoxPosX
                jsr AddToFD
                jsr SetColBufPos
PaintBoxToFD02  ; First line
                ldy BoxWidth
                dey
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
                dex
                dex
--              jsr AddBufWidthToFD
                ldy BoxWidth
                dey
                lda #41
                sta ($fd),y
                dey
                +LDA_WM 4,160
-               sta ($fd),y
                dey
                bne -
                lda #37
                sta ($fd),y
                dex
                bne --
                ; Last line
                jsr AddBufWidthToFD
                ldy BoxWidth
                jsr PaintFramBottom
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

PaintCurWndViaBufToScreen
                jsr PaintCurWindow
                jmp WindowToScreen

!ifdef WIN{
; Paints cur wnd (active) to DESKTOP_BUF
PaintCurWindow  lda CurrentWindow
                bpl +
                rts
+               lda #<DESKTOP_BUF
                sta WndAddressInBuf
                lda #>DESKTOP_BUF
                sta WndAddressInBuf+1
                lda WindowWidth
                sta BufWidth
                lda #1
                sta Param0
; Paints cur wnd into buffers
; Expects: 
; * A = 0 (inactive) or A = 1 (active)
; * BufWidth and BufHeight filled
PaintWndToBuf   jsr PaintTitleBar
                lda #0
                sta BoxPosX
                lda WindowWidth
                sta BoxWidth
                ldx #1
                ldy WindowHeight
                dey ; because of title bar
                ;lda WindowBits
                ;and #BIT_WND_HASMENU
                ;beq +
                ; has menu bar
                ;inx
                ;dey
;+               ; no menu bar
                stx BoxPosY
                sty BoxHeight
                ;
                lda CSTM_WindowClr
                sta BoxColor
                jsr PaintBoxToBuf
                jsr PaintControls
                ; Resize symbol in lower right corner
                lda WindowBits
                and #BIT_WND_RESIZABLE
                beq +
                jsr WndAddrInBufToFD
                ldx WindowWidth
                dex
                ;stx ZP_5F
                txa
                jsr AddToFD
                ldx WindowHeight
                dex
-               ;lda BufWidth
                jsr AddBufWidthToFD
                dex
                bne -
                lda #43
                ldy #0
                sta ($fd),y
+               rts
} else ifdef MAC{
; Paints cur wnd (active) to DESKTOP_BUF
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
; Expects:
; * active = 0 or active = 1
; * BufWidth and BufHeight filled
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
                jsr WndAddrInBufToFD
                ldx WindowWidth
                dex
                ;stx ZP_5F
                txa
                jsr AddToFD
                ldx WindowHeight
                dex
-               ;lda BufWidth
                jsr AddBufWidthToFD
                dex
                bne -
                lda #43
                ldy #0
                sta ($fd),y
+               rts
}
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