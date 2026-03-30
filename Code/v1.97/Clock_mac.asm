; Initialize the C64's TOD (time of day) clock
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

; Sets the TOD to the values in Clock
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