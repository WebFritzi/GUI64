; ByteBoozer Decruncher    /HCL May.2003
; B2 Decruncher            December 2014
; with slight modifications by Krill

decompress = Decrunch
decompsrc  = Get1+1

.FEATURE labels_without_colons, leading_dot_in_identifiers

;Variables..        #Bytes
zp_base	= DECOMPVARS ; -
bits		= zp_base   ;1

put		= decdestlo


.macro	.GetNextBit
.local DgEnd
	asl bits
	bne DgEnd
	jsr GetNewBits
DgEnd
.endmacro

.macro	.GetLen
.local GlEnd
.local GlLoop
	lda #1
GlLoop
	.GetNextBit
	bcc GlEnd
	.GetNextBit
	rol
	bpl GlLoop
GlEnd
.endmacro

Decrunch
	jsr Gnb
	lda loadaddrhi
	sta Get2+2
	sta Get3+2
	ldx loadaddrlo
	jsr GetNewBits
	tya
.if LOADCOMPD_TO
	clc
	adc loadaddroffslo
	php
.endif
storedadrl:
	sta put
	jsr GetNewBits
	tya
.if LOADCOMPD_TO
	plp
	adc loadaddroffshi
.endif
storedadrh:
	sta put + 1
	lda #$80
	sta bits

DLoop
	POLLBLOCK

	.GetNextBit
	bcs Match
Literal
	; Literal run.. get length.
	.GetLen
	sta LLen+1

	ldy #0
LLoop
Get3	lda $ff00,x
	inx
	bne *+5
	jsr GnbInc
	sta (put),y
	iny
LLen	cpy #0
	bne LLoop

	clc
	tya
	adc put
	sta put
	bcc *+4
	inc put+1

	iny
	beq DLoop

	; Has to continue with a match..

Match
	; Match.. get length.
	.GetLen
	sta MLen+1

	; Length 255 -> EOF
	cmp #$ff
	beq End

	; Get num bits
	cmp #2
	lda #0
	rol
	.GetNextBit
	rol
	.GetNextBit
	rol
	tay
	lda Tab,y
	beq MByte

	; Get bits < 8
MLoop1	.GetNextBit
	rol
	bcs MLoop1
	bmi MShort
MByte
	; Get byte
	eor #$ff
	tay
Get2	lda $ff00,x
	inx
	bne MLong
	jsr GnbInc
	jmp MLong
MShort
	ldy #$ff
MLong
	;clc
	adc put
	sta MLda+1
	tya
	adc put+1
	sta MLda+2

	ldy #$ff
MLoop2	iny
MLda	lda $b00b,y
	sta (put),y
MLen	cpy #0
	bne MLoop2

	;sec
	tya
	adc put
	sta put
	bcc *+4
	inc put+1

	jmp DLoop

GetNewBits
Get1	ldy $ff00,x
	sty bits
	rol bits
	inx
	beq GnbInc
End	rts

GnbInc
	inc Get1+2
	inc Get2+2
	inc Get3+2
Gnb
	php
	pha
	tya
	pha
	GETBLOCK Get1+2
	pla
	tay
	pla
	ldx #0
	plp
	rts

Tab
	; Short offsets
	.byte %11011111 ; 3
	.byte %11111011 ; 6
	.byte %00000000 ; 8
	.byte %10000000 ; 10
	; Long offsets
	.byte %11101111 ; 4
	.byte %11111101 ; 7
	.byte %10000000 ; 10
	.byte %11110000 ; 13
