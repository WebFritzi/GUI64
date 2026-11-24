;; Detects whether it's an NTSC or PAL machine
;; Result in A:
;; 0: NTSC, 1: PAL
;DetectNTSC_PAL  lda $d012
;-               cmp $d012
;                beq -
;                bmi DetectNTSC_PAL
;                cmp #$20
;                ; NTSC if carry is clear
;                lda #0
;                rol
;                rts

; Retrieves whether design is WIN or MAC
; Return val in A (A=0: WIN, A=1: MAC)
; and zero flag (BEQ: WIN, BNE: MAC)
GetDesign
!ifdef WIN{
                lda #0
}
!ifdef MAC{
                lda #1
}
                rts

!ifdef WIN{
; Puts address in (FB),y and (FB),y+1
; to FDFE
AddrInFBtoFD    lda ($fb),y
                sta $fd
                iny
                lda ($fb),y
                sta $fe
                rts
}

FBFC_To_FDFE    lda $fb
                sta $fd
                lda $fc
                sta $fe
                rts

C_0203_To_0405  lda $02
                sta $04
                lda $03
                sta $05
                rts

error_codeTo0   lda #0
                sta error_code
                rts

; Copies number of blocks (256 bytes) in X from FBFC to FDFE
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

;; Puts -A into X
;minus           eor #%11111111
;                tax
;                inx
;                rts

; Adds value in A to FBFC
AddToFB         clc
                adc $fb
                sta $fb
                bcc +
                inc $fc
+               rts

; Adds BufWidth to FDFE
AddBufWidthToFD lda BufWidth
; Adds value in A to FDFE
AddToFD         clc
                adc $fd
                sta $fd
                bcc +
                inc $fe
+               rts

SubAFromFD      eor #$ff
                sec
                adc $fd
                sta $fd
                bcs +
                dec $fe
+               rts

SubAFromFB      eor #$ff
                sec
                adc $fb
                sta $fb
                bcs +
                dec $fc
+               rts

; Adds BufWidth to 0203
AddBufWidthTo02 lda BufWidth
; Adds value in A to 0203
AddTo02         clc
                adc $02
                sta $02
                bcc +
                inc $03
+               rts

