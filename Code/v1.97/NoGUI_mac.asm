error_codeTo0   lda #0
                sta error_code
                rts

CopyBlockFBtoFD ldy #0
-               lda ($fb),y
                sta ($fd),y
                dey
                bne -
                inc $fc
                inc $fe
                dex
                bne CopyBlockFBtoFD
                rts

; Puts -A into X
minus           eor #%11111111
                tax
                inx
                rts

; Adds value in A to FBFC
AddToFB         clc
                adc $fb
                sta $fb
                bcc +
                inc $fc
+               rts

; Adds value in A to FDFE
AddToFD         clc
                adc $fd
                sta $fd
                bcc +
                inc $fe
+               rts

Val             !byte 0
SubValFromFD    lda $fd
                sec
                sbc Val
                sta $fd
                bcs +
                dec $fe
+               rts

SubValFromFB    lda $fb
                sec
                sbc Val
                sta $fb
                bcs +
                dec $fc
+               rts

; Adds value in A to 0203
AddTo02         clc
                adc $02
                sta $02
                bcc +
                inc $03
+               rts

; Adds BufWidth to FDFE
AddBufWidthToFD lda BufWidth
                clc
                adc $fd
                sta $fd
                bcc +
                inc $fe
+               rts

; Adds BufWidth to 0203
AddBufWidthTo02 lda BufWidth
                clc
                adc $02
                sta $02
                bcc +
                inc $03
+               rts

;Once called, never changes
SetGlobals      ; Install mouse pointer sprites
                lda #<SP_Mouse0
                sta SPRPTR_0
                lda #<SP_Mouse1
                sta SPRPTR_1
                lda #CL_BLACK
                sta col0
                lda #CL_WHITE
                sta col1

                ; Turn on sprites
                lda #%00111111
                sta VIC+21

                ; Colors:
                ; bkg, frame, and multicolor
                lda #CL_BLACK
                sta FRAMECOLOR
                sta BKGCOLOR
                lda #CL_WHITE
                sta MULTICOLOR1
                lda #CL_BLACK
                sta MULTICOLOR2

                ; Prepare ScrTabLo/Hi, BufScrTabLo/Hi, BufClrTabLo/Hi
                ldx #0
                lda #<SCRMEM
                sta ScrTabLo,x
                lda #>SCRMEM
                sta ScrTabHi,x
                lda #>MENUBAR_BUF
                sta BufScrTabHi,x
                lda #>MENUBAR_CLR_BUF
                sta BufClrTabHi,x

-               lda ScrTabLo,x
                clc
                adc #40
                inx
                sta ScrTabLo,x
                ;
                dex
                lda ScrTabHi,x
                inx
                sta ScrTabHi,x
                dex
                lda BufScrTabHi,x
                inx
                sta BufScrTabHi,x
                dex
                lda BufClrTabHi,x
                inx
                sta BufClrTabHi,x
                bcc +
                inc ScrTabHi,x
                inc BufScrTabHi,x
                inc BufClrTabHi,x
+               cpx #24
                bcc -
                
                ; Set initial values
                lda #0
                sta PATH_A
                sta PATH_A+1
                sta PATH_B
                sta PATH_B+1
                sta ProgramMode
                sta AllocedWindows
                lda #"a"
                sta PATH_A_EX
                lda #"b"
                sta PATH_B_EX
                lda #":"
                sta PATH_A_EX+1
                sta PATH_B_EX+1
                ;
                lda #<WND_HEAP
                sta EofWndHeap
                lda #>WND_HEAP
                sta EofWndHeap+1
                ;
                lda #<CONTROL_HEAP
                sta EofCtrlsHeap
                lda #>CONTROL_HEAP
                sta EofCtrlsHeap+1
                rts