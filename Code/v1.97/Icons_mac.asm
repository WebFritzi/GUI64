DevCharIndices  !byte 0,0
IconUltChars    !byte 30,39,31,41,32; solid
                !byte  1, 4, 1, 2, 1; dither
PaintIcons      ; Only drives
                ; FBFC = IconA_DeskPos
                ; FDFE = IconB_DeskPos
                ; 0203 = IconA_DeskColPos
                ; 0405 = IconB_DeskColPos
                ; 0607 = IconU_DeskPos
                ; 0809 = IconU_DeskColPos
                lda bIsUltimate
                beq +
                ldy CSTM_Icons+4
                ldx CSTM_Icons+5
                jsr PosToDeskBufFB
                lda $fb
                sta $06
                lda $fc
                sta $07
                jsr PosToDeskColBuf02
                lda $02
                sta $08
                lda $03
                sta $09
+               ldy CSTM_Icons+2
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
                ldy CSTM_Icons
                ldx CSTM_Icons+1
                jsr PosToDeskBufFB
                jsr PosToDeskColBuf02
                ; Icons A and B
                ldx CSTM_DeskPattern
                lda DrvSymTop,x
                ldy #1
                sta ($fb),y
                sta ($fd),y
                iny
                sta ($fb),y
                sta ($fd),y
                lda DrvSymLeft,x
                ldy #40
                sta ($fb),y
                sta ($fd),y
                lda DrvSymRight,x
                ldy #43
                sta ($fb),y
                sta ($fd),y
                ; Disk lever and slot
                lda #18
                ldy #41 
                sta ($fb),y
                sta ($fd),y
                lda #19
                iny
                sta ($fb),y
                sta ($fd),y
                ; Device numbers below
                ; A
                lda DeviceNumbers
                jsr GetDrvCharInds
                ldy #81
                sta ($fb),y
                lda DevCharIndices+1
                iny
                sta ($fb),y
                ; B
                lda DeviceNumbers+1
                jsr GetDrvCharInds
                ldy #81
                sta ($fd),y
                lda DevCharIndices+1
                iny
                sta ($fd),y
                ; Colors
                lda #CL_LIGHTGRAY
                ldy #41
                sta ($02),y
                sta ($04),y
                iny
                sta ($02),y
                sta ($04),y
                ; Icon Ultimate
                lda bIsUltimate
                beq ++
                ldx #0
                lda CSTM_DeskPattern
                beq +
                ldx #5
+               ldy #0
                lda IconUltChars,x
                sta ($06),y
                iny
                inx
                lda IconUltChars,x 
                sta ($06),y
                iny
                inx
                lda IconUltChars,x
                sta ($06),y
                ldy #40
                inx
                lda IconUltChars,x
                sta ($06),y
                iny
                lda #11
                sta ($06),y
                iny
                lda #60
                sta ($06),y
                lda #CL_LIGHTGRAY
                sta ($08),y
                ldy #80
                inx
                lda IconUltChars,x
                sta ($06),y
                iny
                lda #61
                sta ($06),y
                iny
                lda #62
                sta ($06),y
++              rts

; Depending on the device number in A, finds the right
; char indices in the char set for the drive icons
; and copies them into DevCharIndices
GetDrvCharInds  sta file_size; for possible later conversion
                ldx #0
                stx file_size+1
                cmp #8
                bne +
                ; It's #8
                ldx #20
                stx DevCharIndices
                inx
                stx DevCharIndices+1
                jmp ++
+               cmp #9
                bne +
                ; It's #9
                ldx #16
                stx DevCharIndices
                inx
                stx DevCharIndices+1
                jmp ++
+               ; It's between 10 and 29
                jsr ConvertToDec
                lda file_size_dec
                lsr
                lsr
                lsr
                lsr
                clc
                adc #223
                sta DevCharIndices
                lda file_size_dec
                and #%00001111
                clc
                adc #226
                sta DevCharIndices+1
++              lda DevCharIndices
                rts

; Checks if CurrentIcon overlaps with another icon
; If yes, overlapping icon in Y
DoesIconOverlap ldy #2; icon counter
-               cpy CurrentIcon
                beq +
                lda IconAvailable,y
                beq +
                jsr DoesIconOverlapWithIconY
                beq +
                rts
+               dey
                bpl -
                lda #0
                rts

; Checks if CurrentIcon overlaps with icon Y
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
                lda #1
                rts
+               lda #0
                rts

; Checks if cursor is in any icon
; Return values:
; A: Yes/No
; res: index of icon
IsInAnyIcon     lda #$ff
                sta res
                ldy #2; counter for icons
                ;
-               jsr IsInIconY
                bne +
                dey
                bpl -
-               lda #0
                rts
+               ; It's in an icon
                ; Check if icon is available
                ldx IconAvailable,y
                beq -
                sty res
                lda #1
                rts

; Checks if cursor is in icon Y
IsInIconY       tya
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
                ldx MouseInfo+5
                dex
                txa
                cmp CSTM_Icons,y
                beq +
                dex
                txa
                cmp CSTM_Icons,y
                bne ++
+               tya
                lsr
                tay
                lda #1
                rts
++              tya
                lsr
                tay
                lda #0
                rts