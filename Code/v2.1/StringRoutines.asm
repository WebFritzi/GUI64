; Prints multi-line string from FBFC to FDFE
; with lower case conversion
; FDFE needs to be in Screen Buffer
PrintStringLC_ML
--              ldy #$ff
-               iny
                lda ($fb),y
                beq ++
                cmp #28; carriage return
                bne +
                iny
                tya
                jsr AddToFB
                dey
                jsr AddBufWidthToFD
                jmp --
+               jsr PetLCtoDesktop
                sta ($fd),y
                bne -;jmp -
++              rts

; For multi-line strings:
; Finds StringWidth and StringHeight of string in FBFC
; Result:
; StringHeight, StringWidth
; X: StringHeight
; Y: StringWidth
GetStringInfo   ldx #0; X counts string lines
                stx StringWidth
                stx StringHeight
--              ldy #$ff
-               iny
                lda ($fb),y
                beq +
                cmp #28; carriage return
                bne -
                ; char is carriage return
                iny
                tya
                jsr AddToFB
                dey
                inx
                cpy StringWidth
                bcc --
                sty StringWidth
                bcs --; jmp --
+               inx
                stx StringHeight
                cpy StringWidth
                bcc +
                sty StringWidth
+               ldy StringWidth
                rts

; Prints string from FBFC to FDFE
; BEQ (Z=1): Lower case
; BNE (Z=0): Upper case 
PrintStringCase bne PrintStringUC
; Prints string from FBFC to FDFE
; with lower case conversion
; and returns string len in Y
PrintStringLC   ldy #$ff
-               iny
                lda ($fb),y
                beq +
                jsr PetLCtoDesktop
                sta ($fd),y
                bne -;jmp -
+               rts

; Prints string from FBFC to FDFE
; with upper case conversion
; and returns string len in Y
PrintStringUC   ldy #$ff
-               iny
                lda ($fb),y
                beq +
                jsr PetUCtoDesktop
                sta ($fd),y
                bne -; jmp -
+               rts

; Prints string from FBFC to FDFE with max length in Param0
; Win version only: Param1=0: Lower case, Param1=1: Upper case
; If string is too long, it terminates with "..."
PrintStrMaxLen  ldy #0
                dec Param0
                bmi +++
                ;lda $fc
                ;cmp #$ff
                ;beq +++
                ldy #$ff
-               iny
                lda ($fb),y
                beq +++
!ifdef WIN{
                ldx Param1
                beq +
                jsr PetUCtoDesktop
                jmp ++
+               jsr PetLCtoDesktop
++
} else ifdef MAC{
                jsr PetUCtoTitlebar
}
                sta ($fd),y
                cpy Param0
                bcc -
                iny
                lda ($fb),y
                beq +++
                dey
                +LDA_WM 219,222
                sta ($fd),y
                iny
+++             rts

;Old01           !byte 0
; Only uses X
MapOutIO        ldx $01
                stx MapInIO+1;Old01
                ldx #$34
                stx $01
                rts

; Only uses X
MapInIO         ldx #0; overwritten by MapOutIO
                stx $01
                rts

; Copies char in A to y-th position in Reserved
; Expects A, smc1+1, smc2+1, smc3+1, smc4+1 filled
; smc5+1 is optional and should be filled with 
; DT_Reserved_Char or TB_Reserved_Char
CopyCharToReserved
                ; Find address in char set
                ;sta $02
                ;lda #0
                ;sta $03
                ;asl $02
                ;rol $03
                ;asl $02
                ;rol $03
                ;asl $02
                ;rol $03
                ;lda $02
                ;clc
                ldx #0
                stx $03
                asl
                rol $03
                asl
                rol $03
                asl
                rol $03
                clc
smc1            adc #0;Lobyte of char set
                sta $02
                lda $03
smc2            adc #0;Hibyte of char set
                sta $03
                ; Find y pos in Reserved
                tya
                asl
                asl
                asl
                clc
smc3            adc #0;Lobyte of Reserved
                sta $04
smc4            lda #0;Hibyte of Reserved
                adc #0
                sta $05
                ; Copy
                sty ZP_5F
                ldy #7
-               lda ($02),y
                sta ($04),y
                dey
                bpl -
                ldy ZP_5F
                tya
                clc
!ifdef WIN{
smc5            adc #0
} else ifdef MAC{
                adc #0
}
                rts

!ifdef MAC{
InvertReserved  lda #(256 - DT_Reserved_Char)
                ; Get number of bytes to invert
                asl
                asl
                asl
                tay
                dey
                ; Invert
                lda #<DT_Reserved
                sta $fb
                lda #>DT_Reserved
                sta $fc
-               lda ($fb),y
                eor #%11111111
                sta ($fb),y
                dey
                bne -
                lda ($fb),y
                eor #%11111111
                sta ($fb),y
                rts
}

!ifdef WIN{
PressReserved_TB
                lda #<TB_Reserved
                sta $fb
                lda #>TB_Reserved
                sta $fc
                ldx #10
                
;--              ldy #7
;-               lda ($fb),y
;                lsr
;                sta ($fb),y
;                dey
;                bpl -
;                ldy #7
;-               dey
;                lda ($fb),y
;                iny
;                sta ($fb),y
;                dey
;                bne -

--              ldy #7
-               dey
                lda ($fb),y
                lsr
                iny
                sta ($fb),y
                dey
                bne -

                ;ldy #0
                tya
                sta ($fb),y
                lda #8
                jsr AddToFB
                dex
                bne --
                rts

PressReserved_DT
                lda #<DT_Reserved
                sta $fb
                lda #>DT_Reserved
                sta $fc
                ldx ControlWidth
                dex
                dex

;--              ; Shift right
;                ldy #7
;-               lda ($fb),y
;                lsr
;                ora #%10000000
;                sta ($fb),y
;                dey
;                bpl -
;                ; Push down
;                ldy #7
;                lda ($fb),y
;                ldy #0
;                sta ($0c),y                
;                ldy #7
;-               dey
;                lda ($fb),y
;                iny
;                sta ($fb),y
;                dey
;                bne -
;                lda #$ff
;                sta ($fb),y

--              ldy #7
                ; Bottom row to second char
                lda ($fb),y
                lsr
                ora #%10000000
                ldy #0
                sta ($0c),y
                ; Edit rows 0-6 and push down
                ldy #7
-               dey
                lda ($fb),y
                lsr
                ora #%10000000
                iny
                sta ($fb),y
                dey
                bne -
                ; New upper row
                lda #$ff
                sta ($fb),y

                lda #8
                jsr AddToFB
                lda $0c
                clc
                adc #8
                sta $0c
                bcc +
                inc $0d
+               dex
                bne --
                rts
}

; Sets FDFE to next string and returns
; strlen of previous string in res and Y
NextString      jsr GetStrLen
                iny
                tya
                dey
                jmp AddToFD

; Gets length of string in FDFE
; Output in res (or Y)
GetStrLen       ldy #$ff
-               iny
                lda ($fd),y
                bne -
                sty res
                rts

;; Prints string from FBFC to FDFE
;; with lower case conversion
;; and writes strlen to res
;PrintIntString  ldy #0
;-               lda ($fb),y
;                beq +
;                jsr PetLCtoDesktop
;                sta ($fd),y
;                iny
;                bne-; jmp -
;+               sty res
;                rts

PetUCtoDesktop  cmp #32
                bcc InvalidChar; a < 32
                cmp #64
                bcs +
                ; 32 <= a < 64
                ora #%10000000
                rts
+               bne +
                ; a = 64
                lda #192
                rts
+               cmp #96
                bcs +
                ; 65 <= a < 96
                ;clc
                adc #64
                rts
+               cmp #123
                bcs +
                ; 96 <= a < 123
                ;clc
                adc #96
                rts
+               cmp #193
                bcc InvalidChar; 123 <= a < 193
                cmp #219
                bcc +; 193 <= a < 219
                ; 219 <= a < 256
InvalidChar     lda #191
+               rts

PetLCtoDesktop  cmp #32
                bcc InvalidChar
                cmp #91
                bcs +
                ; 32 <= a < 91
                ora #%10000000
                rts 
+               cmp #96
                bcs +
                ; 91 <= a < 96
                ;clc
                adc #64
                rts
+               cmp #123
                bcs +
                ; 96 <= a < 123
                ;clc
                adc #32
                rts
+               cmp #192
                bcc InvalidChar; 123 <= a < 192
                cmp #219
                bcs +
                ; 192 <= a < 218
                sec
                sbc #64
                rts
+               ; 219 <= a
                ;sec
                sbc #172
                rts

!ifdef MAC{
PetUCtoTitlebar cmp #255
                bne +
                lda #222
                rts
+               jsr PetUCtoDesktop
                sec
                sbc #64
                cmp #128
                bcc +
                ;sec
                sbc #64
+               rts
}

!ifdef WIN{
PetUCtoTaskbar  cmp #32
                bcc InvalidTBChar; a < 32 
                cmp #65
                bcc ++; 32 <= a < 65
                cmp #96
                bcs +
                ; 65 <= a < 96
                and #%00111111
                rts
+               cmp #123
                bcs +
                ; 96 <= a < 123
                sec
                sbc #32
                rts
+               cmp #192
                bcc InvalidTBChar; 123 <= a < 192
                cmp #219
                bcs +
                ; 192 <= a < 218
                and #%01111111
                rts
+               ; 218 <= a
InvalidTBChar   lda #63
++              rts

PetLCtoTaskbar  cmp #32
                bcc InvalidTBChar; a < 32
                cmp #91
                bcc ++; 32 <= a < 91
                cmp #96
                bcs +
                ; 91 <= a < 96
                and #%10111111
                rts
+               bne +
                ; a = 96
                lda #45
                rts
+               cmp #123
                bcs +
                ; 97 <= a < 123
                sec
                sbc #96
                rts
+               cmp #193
                bcc InvalidTBChar; 123 <= a < 193
                cmp #219
                bcs InvalidTBChar; 219 <= a < 256
                ; 193 <= a < 218
                sec
                sbc #192
++              rts
}