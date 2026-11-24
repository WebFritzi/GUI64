
; the following depack code is
; written by Marek Matula (MMS/Taboo)
; in its original form and has been
; slightly modified

; compress using taboo levelcrush or
; crush.exe by taboo,
; don't forget to convert the
; compressed files (not needed for mem-decompressing)


; Level-crusher v1.0/v1.1 depacker
; (c)1998 Taboo Productions!
; All rights reserved

LC_SPEED   = 6

byte       = DECOMPVARS+$00
hi         = DECOMPVARS+$01
dest       = decdestlo
desth      = decdesthi

decompress:
            jsr getbyte
.if LOADCOMPD_TO
            clc
            adc loadaddroffslo
            php
.endif
storedadrl: sta dest
            jsr getbyte
.if LOADCOMPD_TO
            plp
            adc loadaddroffshi
.endif
storedadrh: sta desth

            ldx #$00
            stx byte
jp18:       stx hi
            lda #$01
            asl byte
            bne *+$05
            jsr getbit
            bcs jp2
jp4:	       asl byte
            bne *+$05
            jsr getbit
            bcs jp3
            asl byte
            bne *+$05
            jsr getbit
            rol a
            rol hi
            bpl jp4
jp3:        tax
            beq jp5

            ; literal run
            ldy #$00
literalrun: jsr getbyte
            sta (dest),y
            inc dest
            bne :+
            inc desth
:           dex
            bne literalrun
jp5:        cpx hi
            dec hi
            bcc literalrun
            stx hi

jp2:        lda #$01
            asl byte
            bne *+$05
            jsr getbit
            bcc jp9
jp8:        asl byte
            bne *+$05
            jsr getbit
            bcs jp10
            asl byte
            bne *+$05
            jsr getbit
            rol a
            bcc jp8

            ; decompression finished
            rts

jp9:        inx
jp10:       adc #$01
            sta depseqle
            txa
            asl byte
            bne *+$05
            jsr getbit
            rol a
            asl byte
            bne *+$05
            jsr getbit
            rol a
            tay
            lda #$00
jp12:       ldx tab,y
jp11:       asl byte
            bne *+$05
            jsr getbit
            rol a
            rol hi
            dex
            bne jp11
            dey
            bmi jp14
            cpy #$03
            clc
            beq jp14
            adc #$01
            bcc jp12
            inc hi
            bcs jp12
jp14:       adc depseqle
            bcc jp15
            inc hi

            ; copy sequence
jp15:       clc
            sbc dest
            eor #$ff
            sta depseqcp+$01
            lda hi
            sbc desth
            eor #$ff
            sta depseqcp+$02
            ldy #$00
depseqcp:   lda $00,y
            sta (dest),y
            iny
depseqle = *+$01
            cpy #$00
            bne depseqcp
            tya
            clc
            adc dest
            sta dest
            bcc jp17
            inc desth
jp17:       jmp jp18

getbit:     pha
            jsr getbyte
            sec
            rol a
            sta byte
            pla
jp19:       rts

tab:
    .if LC_SPEED = 6
            .byte 4,3,3,3,4,2,2,2
    .endif
    .if LC_SPEED = 5
            .byte 4,2,3,3,4,2,2,2
    .endif
    .if LC_SPEED = 4
            .byte 4,2,2,3,4,2,2,2
    .endif
    .if LC_SPEED = 3
            .byte 4,2,2,2,4,2,2,2
    .endif
    .if LC_SPEED = 2
            .byte 3,2,2,2,3,2,2,2
    .endif
    .if LC_SPEED = 1
            .byte 3,1,2,2,3,1,2,2
    .endif
    .if LC_SPEED = 0
            .byte 2,2,1,1,2,2,1,1
    .endif
