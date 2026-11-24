
; The following depack code is
; written by Pasi Ojala (Albert/PU239)
; in its original form and has been
; slightly modified.

; The original routine was located at
; http://www.cs.tut.fi/~albert/Dev/pucrunch/sa_uncrunch.asm
; at the time of writing.

; crunch using the -c0 switch

.define OLD_VERSION 0; pre 2004/3/24, this includes http://www.cs.tut.fi/~albert/Dev/pucrunch/pucrunch_x86.zip as of 2013/4/9

LZPOS  = DECOMPVARS + $00
bitstr = DECOMPVARS + $02

decompress:   ldx #5
@222:         jsr getbyt   ; skip 'p', 'u', endAddr HI&LO, leave starting escape in A
              dex
              bne @222
              sta esc+1    ; starting escape

              jsr getbyt   ; read startAddr
.if MEM_DECOMP_TO_API
              ldx storedadrl
              cpx #OPC_LDA_ZP
              bne :+
storedadrl:   lda decdestlo
:
.endif
.if LOADCOMPD_TO
              clc
              adc loadaddroffslo
              php
.endif
              sta OUTPOS
              jsr getbyt
.if MEM_DECOMP_TO_API
              ldx storedadrh
              cpx #OPC_LDA_ZP
              bne :+
storedadrh:   lda decdesthi
:
.endif
.if LOADCOMPD_TO
              plp
              adc loadaddroffshi
.endif
              sta OUTPOS+1

              jsr getbyt   ; read # of escape bits
              sta escB0+1
              sta escB1+1
              lda #8
              sec
              sbc escB1+1
              sta noesc+1  ; 8-escBits

              jsr getbyt
              sta mg+1     ; maxGamma + 1
              lda #9
              sec
              sbc mg+1     ; 8 - maxGamma == (8 + 1) - (maxGamma + 1)
              sta longrle+1
              jsr getbyt
              sta mg1+1    ; (1<<maxGamma)
              asl a
              clc
              sbc #0
              sta mg21+1   ; (2<<maxGamma) - 1
              jsr getbyt
              sta elzpb+1

              jsr getbyt   ; exec address
              sta lo+1     ; lo
              jsr getbyt
              sta hi+1     ; hi

              jsr getbyt   ; rleUsed
              ldx #0
              tay
              beq @1       ; Y == 0 ?
@0:           jsr getbyt
              sta table,x
              inx
              dey
              bne @0
@1:            ; setup bit store - $80 means empty
              lda #$80
              sta bitstr
              bne main

getbyt:       jsr getnew
              lda bitstr
              ror
              rts

newesc:       ldy esc+1    ; remember the old code (top bits for escaped byte)
escB0:        ldx #2       ; ** PARAMETER  0..8
              jsr getchkf  ; get & save the new escape code
              sta esc+1
              tya          ; pre-set the bits
              ; Fall through and get the rest of the bits.
noesc:        ldx #6       ; ** PARAMETER  8..0
              jsr getchkf
              jsr putch    ; output the escaped/normal byte

              ; Fall through and check the escape bits again
main:         ldy #0       ; Reset to a defined state
              tya          ; A = 0
escB1:        ldx #2       ; ** PARAMETER  0..8
              jsr getchkf  ; X = 0
esc:          cmp #0
              bne noesc
              ; Fall through to packed code

              jsr getval   ; X = 0
              sta LZPOS    ; xstore - save the length for a later time
              lsr          ; cmp #1  ; LEN == 2 ? (A is never 0)
              bne lz77     ; LEN != 2  -> LZ77
              ;tya         ; A = 0
              jsr get1bit  ; X = 0
              lsr          ; bit -> C, A = 0
              bcc lz77_2   ; A=0 -> LZPOS+1
              ;***FALL THRU***

              ; e..e01
              jsr get1bit  ; X = 0
              lsr          ; bit -> C, A = 0
              bcc newesc   ; e..e010
              ;***FALL THRU***

              ; e..e011
srle:         iny          ; Y is 1 bigger than MSB loops
              jsr getval   ; Y is 1, get len, X = 0
              sta LZPOS    ; xstore - Save length LSB
mg1:          cmp #64      ; ** PARAMETER 63-64 -> C clear, 64-64 -> C set..
              bcc chrcode  ; short RLE, get bytecode

longrle:      ldx #2       ; ** PARAMETER  111111xxxxxx
              jsr getbits  ; get 3/2/1 more bits to get a full byte, X = 0
              sta LZPOS    ; xstore - Save length LSB

              jsr getval   ; length MSB, X = 0
              tay          ; Y is 1 bigger than MSB loops

chrcode:      jsr getval   ; Byte Code, X = 0
              tax          ; this is executed most of the time anyway
              lda table-1,x; Saves one jump if done here (loses one txa)

.if OLD_VERSION
              cpx #32      ; 31-32 -> C clear, 32-32 -> C set..
.else
              cpx #16      ; 15-16 -> C clear, 16-16 -> C set..
.endif
              bcc @1       ; 1..31, we got the right byte from the table

              ; Ranks 32..64 (11111°xxxxx), get byte..
              txa          ; get back the value (5 valid bits)
.if OLD_VERSION
              ldx #3
.else
              ldx #4
.endif
              jsr getbits  ; get 3 more bits to get a full byte, X = 0

@1:           ldx LZPOS    ; xstore - get length LSB
              inx          ; adjust for cpx#$ff;bne -> bne
dorle:        jsr putch
              dex
              bne dorle    ; xstore 0..255 -> 1..256
              dey
              bne dorle    ; Y was 1 bigger than wanted originally
mainbeq:      beq main     ; reverse condition -> jump always


lz77:         jsr getval   ; X = 0
mg21:         cmp #127     ; ** PARAMETER  Clears carry (is maximum value)
              bne noeof
              ; EOF
eof:
hi:           ldx #0
lo:           ldy #0
              rts

noeof:        sbc #0       ; C is clear -> subtract 1  (1..126 -> 0..125)
elzpb:        ldx #0       ; ** PARAMETER (more bits to get)
              jsr getchkf  ; clears Carry, X = 0
lz77_2:       sta LZPOS+1  ; offset MSB
              jsr get8bit  ; clears Carry, X = 0
              ; Note: Already eored in the compressor..
              ;eor #255    ; offset LSB 2's complement -1 (i.e., -X = ~X+1)
              adc OUTPOS   ; -offset -1 + curpos (C is clear)
              ldx LZPOS    ; xstore = LZLEN (read before it's overwritten)
              sta LZPOS

              lda OUTPOS+1
              sbc LZPOS+1  ; takes C into account
              sta LZPOS+1  ; copy X+1 number of chars from LZPOS to OUTPOS
              ;ldy #0      ; Y was 0 originally, we don't change it

              inx          ; adjust for cpx#$ff;bne -> bne
lzloop:       lda (LZPOS),y; using abs,y is 3 bytes longer, only 1 cycle/byte faster
              jsr putch    ; Note: must be copied forwards!
              iny          ; Y does not wrap because X=0..255 and Y initially 0
              dex
              bne lzloop   ; X loops, (256,1..255)
              beq mainbeq  ; jump through another beq (-1 byte, +3 cycles)


getnew:       pha          ; 1 Byte/3 cycles
              jsr getbyte
              sec
              rol          ; Shift out the next bit and
                           ;  shift in C=1 (last bit marker)
              sta bitstr   ; bitstr initial value = $80 == empty
              pla          ; 1 Byte/4 cycles
              rts
              ; 25+12 = 37

              ; getval : Gets a 'static huffman coded' value
              ; ** Scratches X, returns the value in A **
getval:       inx          ; X <- 1
              txa          ; set the top bit (value is 1..255)
gv0:          asl bitstr
              bne @1
              jsr getnew
@1:           bcc getchk   ; got 0-bit
              inx
mg:           cpx #7       ; ** PARAMETER unary code maximum length + 1
              bne gv0
              beq getchk   ; inverse condition -> jump always
              ; getval: 18 bytes
              ; 15 + 17*n + 6+15*n+12 + 36*n/8 = 33 + 32*n + 36*n/8 cycles

              ; getbits: Gets X bits from the stream
              ; ** Scratches X, returns the value in A **
get8bit:      ldx #7
get1bit:      inx          ;2
getbits:      asl bitstr
              bne @1
              jsr getnew
@1:           rol          ;2
getchk:       dex          ;2    more bits to get ?
getchkf:      bne getbits  ;2/3
              clc          ;2    return carry cleared
              rts          ;6+6

OUTPOS   = *+$01
putch:        sta $aaaa    ; ** parameter
              inc OUTPOS   ; ZP
              bne @0
              inc OUTPOS+1 ; ZP
@0:           rts

.if OLD_VERSION
table:        .res 31,0
.else
table:        .res 15,0
.endif
