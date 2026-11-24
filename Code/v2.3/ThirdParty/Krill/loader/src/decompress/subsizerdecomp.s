
;**************************************************************************
;*
;* Copyright (c) 2015, 2017 Daniel Kahlin <daniel@kahlin.net>
;* Written by Daniel Kahlin <daniel@kahlin.net>
;* Slightly modified by Gunnar Ruthenberg <krill@plush.de>
;*
;* DESCRIPTION
;*   subsizer 0.6 decruncher - stand alone version
;*
;*   usage:
;*     You need to provide a function to get a byte from the input
;*     stream. (must preserve X,Y and C)
;*
;******

decompress = decrunch

.FEATURE leading_dot_in_identifiers

;**************************************************************************
;*
;* Configuration options
;*
;******
FORWARD_DECRUNCHING	=	1
HAVE_LONG_PARTS		=	1


	.if HAVE_LONG_PARTS
PART_MASK	 =	%00001111
N_PARTS		=	16
	.else
PART_MASK	 =	%00000111
N_PARTS		=	8
	.endif


len_zp = DECOMPVARS
copy_zp = DECOMPVARS + 1
hibits_zp = DECOMPVARS + 3
buf_zp = DECOMPVARS + 4
.if MEM_DECOMP_TO_API
dest_zp = decdestlo
.else
dest_zp = DECOMPVARS + 5
.endif
endm_zp = DECOMPVARS + 7


;**************************************************************************
;*
;* NAME  fast macros
;*
;******

;******
;* get bit macro
	.macro	get_bit
	.local .gb_skp1
	asl	buf_zp
	bne	.gb_skp1
; C=1 (because the marker bit was just shifted out)
	jsr	dc_get_byte
	rol
	sta	buf_zp
.gb_skp1:
	.endmacro


;******
;* get bits max8 macro
	.macro	get_bits_max8
	.local .gb_lp1
	.local .gb_skp1
.gb_lp1:
	asl	buf_zp
	bne	.gb_skp1
; C=1 (because the marker bit was just shifted out)
	pha
	jsr	dc_get_byte
	rol
	sta	buf_zp
	pla
.gb_skp1:
	rol
	dey
	bne	.gb_lp1
	.endmacro


;******
;* get bits max8 masked macro
	.macro	get_bits_max8_masked
	.local .gb_lp1
	.local .gb_skp1
.gb_lp1:
	asl	buf_zp
	bne	.gb_skp1
; C=1 (because the marker bit was just shifted out)
	tay
	jsr	dc_get_byte
	rol
	sta	buf_zp
	tya
.gb_skp1:
	rol
	bcs	.gb_lp1
	.endmacro


;******
;* get bits max16 macro
	.macro	get_bits_max16
	.local .gb_lp1
	.local .gb_skp1
.gb_lp1:
	asl	buf_zp
	bne	.gb_skp1
; C=1 (because the marker bit was just shifted out)
	pha
	jsr	dc_get_byte
	rol
	sta	buf_zp
	pla
.gb_skp1:
	rol
	rol	hibits_zp
	dey
	bne	.gb_lp1		; C=0 for all Y!=0
	.endmacro


;**************************************************************************
;*
;* NAME  decrunch
;*
;******
decrunch:
.if MEM_DECOMP_TO_API
; Get endm_zp, dest_zp, and buf_zp
dc_lp00:
	jsr	dc_get_byte
	sta	endm_zp
	jsr	dc_get_byte
storedadrh:
	sta	dest_zp+1
	jsr	dc_get_byte
storedadrl:
	sta	dest_zp+0
	jsr	dc_get_byte
	sta	buf_zp
	ldx	#0
.else
	ldx	#4
dc_lp00:
	jsr	dc_get_byte
	sta	buf_zp-1,x
	dex
	bne	dc_lp00
.endif
; X = 0

.if LOADCOMPD_TO
	clc
	lda loadaddroffslo
	adc dest_zp+0
	sta dest_zp+0
	lda loadaddroffshi
	adc dest_zp+1
	sta dest_zp+1
.endif

;	ldx	#0
dc_lp01:

;******
;* get 4 bits
	lda	#%11100000
dcg_lp1:
	asl	buf_zp
	bne	dcg_skp1
; C=1 (because the marker bit was just shifted out)
	tay
	jsr	dc_get_byte
	rol
	sta	buf_zp
	tya
dcg_skp1:
	rol
	bcs	dcg_lp1
; Acc = 4 bits.

	sta	bits,x

	txa
	and	#PART_MASK
	tay
	beq	dc_skp01

	lda	#0
	sta	hibits_zp
	ldy	bits-1,x
	sec
dc_lp02:
	rol
	rol	hibits_zp
	dey
	bpl	dc_lp02
; C = 0
;	clc
	adc	base_l-1,x
	tay
	lda	hibits_zp
	adc	base_h-1,x

dc_skp01:
	sta	base_h,x
	tya
	sta	base_l,x
	inx
	cpx	#N_PARTS*4+4
	bne	dc_lp01

; perform decrunch

	ldy	#0
	; fall through

;**************************************************************************
;*
;* NAME  decruncher
;*
;* DESCRIPTION
;*   decruncher
;*
;******
decrunch_entry:

;******
;* single literal byte
;*
dc_literal:
.if FORWARD_DECRUNCHING
	jsr	dc_get_byte
;	ldy	#0
	sta	(dest_zp),y
	inc	dest_zp
	bne	dc_skp5
	inc	dest_zp+1
dc_skp5:
dc_common:
.else
	lda	dest_zp
	bne	dc_skp5
	dec	dest_zp+1
dc_skp5:
	dec	dest_zp
	jsr	dc_get_byte
;	ldy	#0
dc_common:
	sta	(dest_zp),y
.endif
	; fall through

decrunch_main:

;------
; perform actual decrunch
dc_lp1:
	get_bit
	bcs	dc_literal

; get length as bits/base.
	ldx	#$80-N_PARTS
dc_lp2:
	inx
	bmi	dc_skp0
	get_bit
	bcc	dc_lp2
	clc
dc_skp0:
; C = 0, Y = 0
;	lda	#0
	tya
	ldy	bits_len-$80+N_PARTS-1,x
	beq	dcb1_skp2
	get_bits_max8
dcb1_skp2:
; C = 0
	adc	base_len-$80+N_PARTS-1,x
	sta	len_zp
; C = 0


.if FORWARD_DECRUNCHING
	tax
.else
;******
;* IN: len = $01..$100 (Acc = $00..$ff)
;* OUT: dest_zp = dest_zp - len,  X = len-1
;*
	tax
;	clc
	eor	#$ff
	adc	dest_zp
	sta	dest_zp
	bcs	dc_skp22
	dec	dest_zp+1
dc_skp22:
.endif

; check end marker here to avoid thrashing carry earlier
	cpx	endm_zp
	beq	done

;******
;* Get selector bits depending on length.
;*
;* IN: len = $01..$100 (X = $00..$ff)
;* OUT:
;*
	cpx	#4
	bcc	dc_skp2
	ldx	#3
dc_skp2:

; get offset as bits/base.
	lda	tabb,x
	get_bits_max8_masked
	tax
; C = 0

	lda	#0
	sta	hibits_zp
	ldy	bits_offs,x
	beq	dcb3_skp2
	get_bits_max16
dcb3_skp2:
; C = 0,  Acc/hibits_zp + base_offs,x = offset - 1

.if FORWARD_DECRUNCHING
	adc	base_offs_l,x
	bcc	dcb3_skp3
	inc	hibits_zp
	clc
dcb3_skp3:
	eor	#$ff
	adc	dest_zp
	sta	copy_zp
	lda	dest_zp+1
	sbc	hibits_zp
	sbc	base_offs_h,x
	sta	copy_zp+1
.else
; perform: copy_zp = Acc/hibits_zp + base_offs,x + 1 + dest_zp
; result:  copy_zp = dest_zp + offset
	adc	base_offs_l,x
	bcc	dcb3_skp3
	inc	hibits_zp
dcb3_skp3:
	sec
	adc	dest_zp
	sta	copy_zp
	lda	hibits_zp
	adc	base_offs_h,x
; C = 0
	adc	dest_zp+1
	sta	copy_zp+1
.endif

.if FORWARD_DECRUNCHING
copy:
.if 1
; this is shorter
	ldx	len_zp
	inx
	ldy	#$ff
dc_lp4:
	iny
	lda	(copy_zp),y
	sta	(dest_zp),y
	dex
	bne	dc_lp4
.else
; and this might be faster on average, as there are many 1-byte sequences
	ldy	len_zp
	beq	dc_skp4
	ldx	len_zp
	ldy	#$00
dc_lp4:
	lda	(copy_zp),y
	sta	(dest_zp),y
	iny
	dex
	bne	dc_lp4
dc_skp4:
	lda	(copy_zp),y
	sta	(dest_zp),y
.endif
; C = 1
	tya
	adc	dest_zp
	sta	dest_zp
	bcc	dc_skp22
	inc	dest_zp+1
dc_skp22:
	ldy	#$00
.else
;******
;* Reverse fast copy
;*
;* IN: len = $01..$100 (len_zp = $00..$ff), C = 0
;*
copy:
	ldy	len_zp
	beq	dc_skp4
dc_lp4:
	lda	(copy_zp),y
	sta	(dest_zp),y
	dey
	bne	dc_lp4
dc_skp4:
	lda	(copy_zp),y
;	sta	(dest_zp),y
.endif

	jmp	dc_common
;	bcc	dc_common		; always taken

;******
;* exit out
done:
	rts

	.if	HAVE_LONG_PARTS
tabb:
	.byte	%10000000 | (48 >> 2)	; 2 bits
	.byte	%11100000 | (0  >> 4)	; 4 bits
	.byte	%11100000 | (16 >> 4)	; 4 bits
	.byte	%11100000 | (32 >> 4)	; 4 bits
	.else
tabb:
	.byte	%10000000 | (24 >> 2)	; 2 bits
	.byte	%11000000 | (0  >> 3)	; 3 bits
	.byte	%11000000 | (8  >> 3)	; 3 bits
	.byte	%11000000 | (16 >> 3)	; 3 bits
	.endif



;**************************************************************************
;*
;* NAME  dc_get_byte
;*
;* DESCRIPTION
;*   Get byte from the packed stream.
;*
;******
dc_get_byte = getbyte

end_decruncher:

begin_tables:
;**************************************************************************
;*
;* NAME  base_l, base_h, bits
;*
;* DESCRIPTION
;*   Data for bits/base decoding.
;*
;******
base_l:
base_len:
	.res	N_PARTS,0
base_offs_l:
	.res	N_PARTS*3+4,0
base_h	= * - N_PARTS
;	.res	N_PARTS,0
base_offs_h:
	.res	N_PARTS*3+4,0

bits:
bits_len:
	.res	N_PARTS,0
bits_offs:
	.res	N_PARTS*3+4,0

end_tables:

; eof
