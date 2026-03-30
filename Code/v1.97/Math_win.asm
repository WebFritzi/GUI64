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

; Multiplies $F7/F8 by $F9/FA
; Result in product = $FB/FC/FD/FE
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

; Dividend / Divisor
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

N = $61
; Divide 32 bit dividend by 16 bit divisor
; (by By Garth Wilson (wilsonmines@dslextreme.com))
; Example for dividend ($12345678)
;--------------------------------
;     Hi Word   !     Lo Word   !
; HiByte LoByte ! HiByte LoByte !
;   12     34   !   56     78   !
;   N+3    N+2  !   N+5    N+4  !
;--------------------------------
; Divisor in N, N+1
; Result:
; 16 bit Quotient in N+4, N+5
; 16 bit Remainder in N+2, N+3
Div_3216        SEC             ; Detect overflow or /0 condition.
                LDA     N+2     ; Divisor must be more than high cell of dividend.  To
                SBC     N       ; find out, subtract divisor from high cell of dividend;
                LDA     N+3     ; if carry flag is still set at the end, the divisor was
                SBC     N+1     ; not big enough to avoid overflow. This also takes care
                BCS     ovflo   ; of any /0 condition.  Branch if overflow or /0 error.
                                ; We will loop 16 times; but since we shift the dividend
                LDX     #17     ; over at the same time as shifting the answer in, the
                                ; operation must start AND finish with a shift of the
                                ; low cell of the dividend (which ends up holding the
                                ; quotient), so we start with 17 in X.
-               ROL     N+4     ; Move low cell of dividend left one bit, also shifting
                ROL     N+5     ; answer in. The 1st rotation brings in a 0, which later
                                ; gets pushed off the other end in the last rotation.
                DEX
                BEQ     end     ; Branch to the end if finished.

                ROL     N+2     ; Shift high cell of dividend left one bit, also
                ROL     N+3     ; shifting next bit in from high bit of low cell.
                LDA     #0
                STA     N+7     ; Zero old bits of N+7 so subtraction works right.
                ROL     N+7     ; Store old high bit of dividend in N+7
                SEC             ; See if divisor will fit into high 17 bits of dividend
                LDA     N+2     ; by subtracting and then looking at carry flag.
                SBC     N       ; First do low byte.
                STA     N+6     ; Save difference low byte until we know if we need it.
                LDA     N+3     ;
                SBC     N+1     ; Then do high byte.
                TAY             ; Save difference high byte until we know if we need it.
                LDA     N+7     ; Bit 0 of N+7 serves as 17th bit.
                SBC     #0      ; Complete the subtraction by doing the 17th bit before
                BCC     -       ; determining if the divisor fit into the high 17 bits
                                ; of the dividend.  If so, the carry flag remains set.
                LDA     N+6     ; If divisor fit into dividend high 17 bits, update
                STA     N+2     ; dividend high cell to what it would be after
                STY     N+3     ; subtraction.
                BCS     -       ; Always branch

ovflo           LDA     #$FF    ; If overflow occurred, put FF
                STA     N+2     ; in remainder low byte
                STA     N+3     ; and high byte,
                STA     N+4     ; and in quotient low byte
                STA     N+5     ; and high byte.
end             RTS


; From http://forum.6502.org/viewtopic.php?p=102377
;\ ------------------------------------------------------------------------------
;\ ---  uint32_div16
;\ ---  Divide a 32-bit unsigned integer by a 16-bit uint.
;\ ---  Returns: 32-bit uint with quotient
;\ ---               32-bit uint with remainder (only bottom 16 bits significant)
;\ ------------------------------------------------------------------------------
;\ ON ENTRY: - UINT32_A/+3   - number to be divided
;\           - UINT16_B/+1   - divisor
;\ ON EXIT : - UINT32_RES/+3 - remainder
;\           - UINT32_A/+3   - quotient
;.uint32_div16
;  ldx #3
;.clr_loop
;  stz UINT32_RES,X
;  dex
;  bpl clr_loop

;  ldx #32                               ; There are 32 bits in dividend
;.uint32_div16_loop
;  ; ----- SHIFT LEFT -----
;  asl UINT32_A
;  rol UINT32_A + 1
;  rol UINT32_A + 2
;  rol UINT32_A + 3

;  rol UINT32_RES          ; Top-most bit of dividend now in remainder as LSB
;  rol UINT32_RES + 1      ; Over the course of 32 iterations, we gradually
;  rol UINT32_RES + 2      ; Move the dividend into the remainder
;  rol UINT32_RES + 3

;  ; ----- TEST SUBTRACTIONS -----
;  lda UINT32_RES                ; Try the low byte
;  sec
;  sbc UINT16_B
;  tay                             ; Store the result for later

;  lda UINT32_RES + 1            ; Now the high byte
;  sbc UINT16_B + 1
;  sta MATH_TMP_A                  ; Store for later

;  lda UINT32_RES + 2            ; Now third byte of remainder
;  sbc #0                          ; Divisor is always 0. Leave the result in A

;.test_result
;  bcc uint32_div16_nosub          ; Did subtraction succeed? If not, branch.
;  sta UINT32_RES + 2            ; Subtraction worked. Store third byte result
;  lda MATH_TMP_A                  ; Reload second byte result
;  sta UINT32_RES + 1
;  sty UINT32_RES                ; And save result of low-byte sub
;  inc UINT32_A
;.uint32_div16_nosub
;  dex
;  bne uint32_div16_loop
;  rts