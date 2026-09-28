DevCharIndex2   !byte 0
!ifdef WIN{
IconUltChars    !byte 30, 39, 63,41,37, 32; solid
                !byte  1,117,118, 2, 3,119; dither
}
!ifdef MAC{
IconUltChars    !byte 30,39,31,41,32; solid
                !byte  1, 4, 1, 2, 1; dither
}

PaintIcons      ; FBFC = IconA_DeskPos/IconU_DeskPos
                ; FDFE = IconB_DeskPos
                ; 0203 = IconA_DeskColPos/IconU_DeskColPos
                ; 0405 = IconB_DeskColPos
                ;
                ; Get drive B screen/color addresses
                ldy CSTM_Icons+2
                ldx CSTM_Icons+3
                jsr PosToDeskBufFB
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                jsr PosToDeskColBuf02
                lda $02
                sta $04
                lda $03
                sta $05
                ; Get drive A screen/color addresses
                ldy CSTM_Icons
                ldx CSTM_Icons+1
                jsr PosToDeskBufFB
                jsr PosToDeskColBuf02
                ;---------------------------------------
                ; Icons A and B
                ;---------------------------------------
                ldx CSTM_DeskPattern
                lda DrvSymTop,x
                ldy #1
                sta ($fb),y
                sta ($fd),y
                iny
                sta ($fb),y
                sta ($fd),y
                ;
                lda DrvSymLeft,x
                ldy #40
                sta ($fb),y
                sta ($fd),y
                ;
                lda DrvSymRight,x
                ldy #43
                sta ($fb),y
                sta ($fd),y
                ; Disk lever and slot
                +LDA_WM 124,18
                ldy #41
                sta ($fb),y
                sta ($fd),y
                ;
                +LDA_WM 125,19
                iny
                sta ($fb),y
                sta ($fd),y
                ; Device numbers below
                ; A
                lda CSTM_DevNumbers
                jsr GetDrvCharInds
                ldy #81
                sta ($fb),y
                lda DevCharIndex2
                iny
                sta ($fb),y
                ; B
                lda CSTM_DevNumbers+1
                jsr GetDrvCharInds
                ldy #81
                sta ($fd),y
                lda DevCharIndex2
                iny
                sta ($fd),y
                ; Colors of drive lever/slot
                lda #CL_LIGHTGRAY
                ldy #41
                sta ($02),y
                sta ($04),y
                iny
                sta ($02),y
                sta ($04),y
                ;---------------------------------------
                ; Icon Ultimate
                ;---------------------------------------
                lda bIsUltimate
                beq ++
                ;
                ldy CSTM_Icons+4
                ldx CSTM_Icons+5
                jsr PosToDeskBufFB
                jsr PosToDeskColBuf02
                ;
                ldx #0
                lda CSTM_DeskPattern
                beq +
                +LDX_WM 6,5
                ; First row
+               ldy #0
-               lda IconUltChars,x
                sta ($fb),y
                inx
                iny
                cpy #3
                bcc -
                ; Second row
                ldy #40
                lda IconUltChars,x
                sta ($fb),y
                ;
                iny
                +LDA_WM 11,106
                sta ($fb),y
                ;
                iny
                +LDA_WM 62,60
                sta ($fb),y
!ifdef WIN{
                iny
                inx
                lda IconUltChars,x
                sta ($fb),y
}
                ; Third row
                ldy #80
                inx
                lda IconUltChars,x
                sta ($fb),y
                ;
                iny
                +LDA_WM 120,61
                sta ($fb),y
                ;
                iny
                +LDA_WM 121,62
                sta ($fb),y
                ;---------------------------------------
                ; Ultimate icon color
                ;---------------------------------------
                ldy #42
                lda #CL_LIGHTGRAY
                sta ($02),y
                ;
++              rts

; Depending on the device number in A, finds the right
; char indices in the char set for the drive icons
; and copies them into A and DevCharIndex2
GetDrvCharInds  sta file_size
                ldx #0
                stx file_size+1
                cmp #8
                bne +
                ; dev no 8
                +LDX_WM 126,20
                bne SetDevCharInd89; jmp SetDevCharInds
+               cmp #9
                bne +
                ; dev no 9
                +LDX_WM 122,16
SetDevCharInd89 txa
                inx
                stx DevCharIndex2
                rts
+               ; dev no 10 to 29
                jsr ConvertToDec
                lda file_size_dec
                lsr
                lsr
                lsr
                lsr
                clc
                ;adc #223
                +adc_WM 95,239
                pha
                lda file_size_dec
                and #%00001111
                clc
                ;adc #226
                +adc_WM 98,242
                sta DevCharIndex2
                pla
                rts

;; Depending on the device number in A, finds the right
;; char indices in the char set for the drive icons
;; and copies them into DevCharIndices
;GetDrvCharInds  sta file_size; for possible later conversion
;                ldx #0
;                stx file_size+1
;                cmp #8
;                bne +
;                ; It's #8
;                +LDX_WM 126,20
;-               stx DevCharIndices
;                inx
;                stx DevCharIndices+1
;                bne ++; jmp ++
;+               cmp #9
;                bne +
;                ; It's #9
;                +LDX_WM 122,16
;                bne -; jmp -
;+               ; It's between 10 and 29
;                jsr ConvertToDec
;                lda file_size_dec
;                lsr
;                lsr
;                lsr
;                lsr
;                clc
;                adc #223
;                sta DevCharIndices
;                lda file_size_dec
;                and #%00001111
;                clc
;                adc #226
;                sta DevCharIndices+1
;++              lda DevCharIndices
;                rts

; Checks if CurrentIcon overlaps with another icon
; Result in carry
; If yes (carry set), overlapping icon in Y
DoesIconOverlap ldy #2; icon counter
-               cpy CurrentIcon
                beq +
                lda IconAvailable,y
                beq +
                jsr DoesIconOverlapWithIconY
                bcc +
                rts
+               dey
                bpl -
                clc
                rts

; Checks if CurrentIcon overlaps with icon Y
; Result in carry
DoesIconOverlapWithIconY
                tya
                asl
                tax
                ; X in [ix-3,ix+3]
                lda DragNewPosX
                sec
                sbc CSTM_Icons,x
                clc
                adc #3
                cmp #7
                bcs +
                ; Y in [iy-2,iy+2]
                inx
                lda DragNewPosY
                sec
                sbc CSTM_Icons,x
                clc
                adc #2
                cmp #5
                bcs +
                sec
                rts
+               clc
                rts

; Checks if cursor is in any icon
; Return values:
; Carry: Yes/No
; res: index of icon
IsInAnyIcon     lda #$ff
                sta res
                ldy #2; counter for icons
                clc
                ;
-               ldx IconAvailable,y
                beq +
                jsr IsInIconY
                bcs ++
+               dey
                bpl -
++              sty res
                rts

; Checks if cursor is in icon Y
; Result in carry
IsInIconY       tya
                pha
                asl
                tay
                ;
                ldx MouseInfo
                dex
                txa
                cmp CSTM_Icons,y
                beq +
                dex
                txa
                cmp CSTM_Icons,y
                bne ++
+               iny
                +LDXV_WM MouseInfo+1,MouseInfo+5
                dex
                txa
                cmp CSTM_Icons,y
                beq +
                dex
                txa
                cmp CSTM_Icons,y
                bne ++
+               pla
                tay
                sec; yes
                rts
++              pla
                tay
                clc; no
                rts

; Shorter alternative:
;                tya
;                pha
;                asl
;                tay
;                ;
;                ldx MouseInfo
;                dex
;                txa
;                cmp CSTM_Icons,y
;                bcc +
;                sbc CSTM_Icons,y
;                eor #255
;                cmp #254
;                bcc +
;                ;
;                iny
;                +LDXV_WM MouseInfo+1,MouseInfo+5
;                dex
;                txa
;                cmp CSTM_Icons,y
;                bcc +
;                sbc CSTM_Icons,y
;                eor #255
;                cmp #254
;+               pla
;                tay
;                rts