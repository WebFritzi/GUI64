; Prints multi-line string from FBFC to FDFE
; with lower case conversion
; FDFE needs to be in Screen Buffer
PrintStringLC_ML
                lda $fe
                cmp #$ff
                beq ++
--              ldy #$ff
-               iny
                lda ($fb),y
                beq ++
                cmp #28; carriage return
                bne +
                iny
                sty string_width_plus_1
                tya
                jsr AddToFB
                dey
                jsr AddBufWidthToFD
                jmp --
+               jsr PetLCtoDesktop
                sta ($fd),y
                jmp -
++              rts

; For multi-line strings:
; Finds StringWidth and StringHeight
; of string in FBFC
GetStringInfo   lda #0
                sta StringWidth
                sta StringHeight
                ldx #0; counts string lines
--              ldy #$ff
-               iny
                lda ($fb),y
                beq +
                cmp #28; carriage return
                bne -
                ; char is carriage return
                iny
                sty string_width_plus_1
                tya
                jsr AddToFB
                ;+AddByteToFB string_width_plus_1
                dey
                inx
                cpy StringWidth
                bcc --
                sty StringWidth
                jmp --
+               inx
                stx StringHeight
                cpy StringWidth
                bcc +
                sty StringWidth
+               rts
string_width_plus_1    !byte 0

; Prints string from FBFC to FDFE
; with lower case conversion
; and returns string len in res
PrintStringLC   ldy #0
                lda $fe
                cmp #$ff
                beq +
                ldy #$ff
-               iny
                lda ($fb),y
                beq +
                jsr PetLCtoDesktop
                sta ($fd),y
                jmp -
+               sty res
                rts

; Prints string from FBFC to FDFE
; with upper case conversion
; and returns string len in res
PrintStringUC   ldy #0
                lda $fe
                cmp #$ff
                beq +
                ldy #$ff
-               iny
                lda ($fb),y
                beq +
                jsr PetUCtoDesktop
                sta ($fd),y
                jmp -
+               sty res
                rts

; Prints string from FBFC to FDFE with max length in Param
; If string is too long, it terminates with "..."
; Length in Y
PrintStrMaxLen  ldy #0
                dec Param
                bmi +++
                lda $fc
                cmp #$ff
                beq +++
                ldy #$ff
-               iny
                lda ($fb),y
                beq +++
                sta ($fd),y
                cpy Param
                bcc -
                iny
                lda ($fb),y
                beq +++
                dey
                lda #255
                sta ($fd),y
                iny
+++             rts


Old01           !byte 0
MapOutIO        ldx $01
                stx Old01
                ldx #$34
                stx $01
                rts

MapInIO         ldx Old01
                stx $01
                rts

; Copies char in A to y-th position in Reserved
; Expects A, smc1+1, smc2+1, smc3+1, smc4+1 filled
; smc5+1 is optional and should be filled with 
; DT_Reserved_Char or TB_Reserved_Char
CopyCharToReserved
                ; Find address in char set
                sta $02
                lda #0
                sta $03
                asl $02
                rol $03
                asl $02
                rol $03
                asl $02
                rol $03
                lda $02
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
                sty dummy
                ldy #7
-               lda ($02),y
                sta ($04),y
                dey
                bpl -
                ldy dummy
                tya
                clc
                adc #0
                rts

InvertReserved_DT
                lda #10
                ; Get number of bytes to invert
                asl
                asl
                asl
                tay
                dey
                ; Invert
                ;jsr MapOutIO
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
                ;jsr MapInIO
                rts

; Sets FDFE to next string and returns
; strlen of previous string in res
NextString      ldy #$ff
-               iny
                lda ($fd),y
                bne -
                iny
                sty dummy
                tya
                jsr AddToFD
                ;+AddByteToFD dummy
                dey
                sty res
                rts

; Gets length of string in FDFE
; Output in res (or Y)
GetStrLen       ldy #$ff
-               iny
                lda ($fd),y
                bne -
                sty res
                rts

; Prints string from FDFE to FBFC
; with lower case conversion
; and writes strlen to res
PrintIntString  ldy #0
-               lda ($fd),y
                beq +
                jsr PetLCtoDesktop
                sta ($fb),y
                iny
                jmp -
+               sty res
                rts

PetUCtoDesktop  cmp #32
                bcs +
                lda #191
                rts
+               cmp #64
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
                clc
                adc #64
                rts
+               cmp #123
                bcs +
                ; 96 <= a < 123
                clc
                adc #96
                rts
+               cmp #193
                bcs +
                ; 123 <= a < 193
                lda #191
                rts
+               cmp #219
                bcs +
                ; 193 <= a < 219
                rts
+               ; 219 <= a
                lda #191
                rts

PetLCtoDesktop  cmp #32
                bcs +
                lda #191
                rts
+               cmp #91
                bcs +
                ; 32 <= a < 91
                ora #%10000000
                rts 
+               cmp #96
                bcs +
                ; 91 <= a < 96
                clc
                adc #64
                rts
+               cmp #123
                bcs +
                ; 96 <= a < 123
                clc
                adc #32
                rts
+               cmp #192
                bcs +
                ; 123 <= a < 192
                lda #191
                rts
+               cmp #219
                bcs +
                ; 192 <= a < 218
                sec
                sbc #64
                rts
+               ; 219 <= a
                ;lda #191
                sec
                sbc #172
                rts

PetUCtoTitlebar cmp #255
                bne +
                lda #222
                rts
+               jsr PetUCtoDesktop
                sec
                sbc #64
                cmp #128
                bcc +
                sec
                sbc #64
+               rts