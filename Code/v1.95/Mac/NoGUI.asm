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

;---------------------------------------------------------------
; TIME FUNCTIONS
;---------------------------------------------------------------

TODInit         lda $01          ;Save $01
                pha              ; on stack
                lda #<INT_NMI    ;Setup NMI vector
                sta $fffa        ; to catch unwanted NMIs
                lda #>INT_NMI    ;
                sta $fffb        ;
                lda #$35         ;Bank out KERNAL
                sta $01          ; so new NMI vector is active

                lda #0
                sta $d011        ;Turn off display to disable badlines
                sta $dc0e        ;Set TOD Clock Frequency to 60Hz
                sta $dc0f        ;Enable Set-TOD-Clock
                sta $dc0b        ;Set TOD-Clock to 0 (hours)
                sta $dc0a        ;- (minutes)
                sta $dc09        ;- (seconds)
                sta $dc08        ;- (deciseconds)

                lda $dc08        ;
-               cmp $dc08        ;Sync raster to TOD Clock Frequency
                beq -                              
                
                ldx #0           ;Prep X and Y for 16 bit
                ldy #0           ; counter operation
                lda $dc08        ;Read deciseconds
-               inx              ;2   -+
                bne +            ;2/3  | Do 16 bit count up on
                iny              ;2    | X(lo) and Y(hi) regs in a 
                jmp ++           ;3    | fixed cycle manner
+               nop              ;2    |
                nop              ;2   -+
++              cmp $dc08        ;4 - Did 1 decisecond pass?
                beq -            ;3 - If not, loop-di-doop
                                 ;Each loop = 16 cycles
                                 ;If less than 118230 cycles passed, TOD is 
                                 ;clocked at 60Hz. If 118230 or more cycles
                                 ;passed, TOD is clocked at 50Hz.
                                 ;It might be a good idea to account for a bit
                                 ;of slack and since every loop is 16 cycles,
                                 ;28*256 loops = 114688 cycles, which seems to be
                                 ;acceptable. That means we need to check for
                                 ;a Y value of 28.

                cpy #28          ;Did 114688 cycles or less go by?
                bcc +            ;- Then we already have correct 60Hz $dc0e value
                lda #$80         ;Otherwise, we need to set it to 50Hz
                sta $dc0e
+               lda #$1b         ;Enable the display again
                sta $d011

                pla              ;Restore old $01 value
                sta $01          ; and potentially old NMI vector
                rts                             
INT_NMI         rti

SetTOD          lda $dc0f
                and #%01111111
                sta $dc0f
                ;
                lda Clock+2
                asl
                asl
                asl
                asl
                ora Clock+3
                sta $dc0a
                ;
                lda Clock
                asl
                asl
                asl
                asl
                ora Clock+1
                ;
                bne +
                lda #$92; actually $12
                sta $dc0b
                jmp ++
+               cmp #$12
                bcs +
                sta $dc0b
                jmp ++
+               bne +
                sta $dc0b
                jmp ++
+               sed
                sec
                sbc #$12
                cld
                ora #%10000000
                sta $dc0b
++              lda #0
                sta $dc08
                sta $dc09
                rts

DisplayClock    lda MayShowClock
                beq ++
                lda #':'
                jsr PetUCtoDesktop
                sta SCRMEM+36

                lda $dc0a
                tax
                lsr
                lsr
                lsr
                lsr
                sta Clock+2
                ora #$30
                jsr PetUCtoDesktop
                sta SCRMEM+37
                txa
                and #%00001111
                sta Clock+3
                ora #$30
                jsr PetUCtoDesktop
                sta SCRMEM+38
                
                lda $dc0b
                ldx $dc08
                tax
                and #%01111111
                cmp #$12
                bne +
                
                ; 12 pm = noon or 12 am = midnight
                txa
                and #%10000000
                tax
                
+               txa
                and #%10000000
                beq am
                ; pm
                txa
                and #%01111111
                sed
                clc
                adc #$12
                cld
                tax
                jmp +
am              txa
                and #%01111111
+               lsr
                lsr
                lsr
                lsr
                sta Clock
                ora #$30
                jsr PetUCtoDesktop
                sta SCRMEM+34
                txa
                and #%00001111
                sta Clock+1
                ora #$30
                jsr PetUCtoDesktop
                sta SCRMEM+35
++              rts