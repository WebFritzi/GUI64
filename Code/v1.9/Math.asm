; Divides FD by FC
; Result: FD with fractional part in FE
DivideFDbyFC    ASL $FD
                LDA #$00
                ROL

                LDX #$08
-               CMP $FC
                BCC +
                SBC $FC
+               ROL $FD
                ROL
                DEX
                BNE -

                LDX #$08
-               BCS +
                CMP $FC
                BCC ++
+               SBC $FC
                SEC
++              ROL $FE
                ASL
                DEX
                BNE -

                rts

; Multiplies FD with FE
; Result in A (hi byte) and X (lo byte)
MultiplyFDbyFE  lda #$00
                ldx #$08
                clc
-               bcc +
                clc
                adc $fe
+               ror
                ror $fd
                dex
                bpl -
                ldx $fd
                rts

multiplier      = $f7 ; $f8
multiplicand    = $f9 ; $fa
product         = $fb ; $fc, $fd, $fe
 
Mult16          lda #$00
                sta product+2       ; clear upper bits of product
                sta product+3
                ldx #$10            ; set binary count to 16
shift_r         lsr multiplier+1    ; divide multiplier by 2
                ror multiplier
                bcc rotate_r
                lda product+2       ; get upper half of product and add multiplicand
                clc
                adc multiplicand
                sta product+2
                lda product+3
                adc multiplicand+1
rotate_r        ror                 ; rotate partial product
                sta product+3
                ror product+2
                ror product+1
                ror product
                dex
                bne shift_r
                rts

; divident / divisor
divisor = $58     ;$59 used for hi-byte
dividend = $fb    ;$fc used for hi-byte
remainder = $fd   ;$fe used for hi-byte
result = dividend ;save memory by reusing divident to store the result

; Divides $FB/FC by $58/59
; Result in divident = $FBFC
Divide16Bit     lda #0          ;preset remainder to 0
                sta remainder
                sta remainder+1
                ldx #16         ;repeat for each bit: ...

divloop         asl dividend    ;dividend lb & hb*2, msb -> Carry
                rol dividend+1  
                rol remainder   ;remainder lb & hb * 2 + msb from carry
                rol remainder+1
                lda remainder
                sec
                sbc divisor     ;subtract divisor to see if it fits in
                tay             ;lb result -> Y, for we may need it later
                lda remainder+1
                sbc divisor+1
                bcc skip        ;if carry=0 then divisor didn't fit in yet

                sta remainder+1 ;else save substraction result as new remainder,
                sty remainder   
                inc result      ;and INCrement result cause divisor fit in 1 times

skip            dex
                bne divloop     
                rts