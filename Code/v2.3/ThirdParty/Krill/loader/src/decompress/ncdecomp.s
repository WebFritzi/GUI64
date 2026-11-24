;
; NuCrunch 1.0
; Christopher Jam
; May 2018
; with slight modifications by Krill
;

decompress = decrunch
decompsrc  = read+1

NC_BLOCK_INTERFACE = 1

.macro getByte1
	jsr get_byte
.endmacro

.macro getBit1
.local nomore
	asl zbs1
	bne nomore
	getByte1
	sec
	rol
	sta zbs1
nomore:
.endmacro

; get head of a pair of bits from the bitpair stream
; (must getBit2t precisely once before invoking again)

.macro getBit2h
.local nomore
	asl zbs2
	bne nomore
	getByte1
	sec
	rol
	sta zbs2
nomore:
.endmacro

.if NC_BLOCK_INTERFACE
; same, but preserving A
.macro getBit2hpa
.local nomore
	asl zbs2
	bne nomore
	pha
	getByte1
	sec
	rol
	sta zbs2
	pla
nomore:
.endmacro
.else
; same, but preserving A/ trashing X.
.macro getBit2hpa
.local nomore
	asl zbs2
	bne nomore
	tax
	getByte1
	sec
	rol
	sta zbs2
	txa
nomore:
.endmacro
.endif

; get tail of a pair of bits from the bitpair stream
.macro getBit2t
	asl zbs2
.endmacro

; get head of a quad of bits from the quad stream
; (must getBit4t precisely three times before invoking again)

.macro getBit4h
.local nomore
	asl zbs4
	bne nomore
	getByte1
	sec
	rol
	sta zbs4
nomore:
.endmacro


; get tail of a quad of bits from the quad stream
.macro getBit4t
	asl zbs4
.endmacro

; note, trashes X.  Also, carry is clear when done
.macro getExpGoulombTail
.local ndone
ndone:
	getBit2hpa
	rol
	getBit2t
	bcs ndone
.endmacro

.macro getExpGoulombTail_odd_aligned
.local ndone
ndone:
	getBit2t
	rol
	getBit2hpa
	bcs ndone
.endmacro

.ifdef NUCRUNCH_ALIGN_FOR_SPEED
	.byte <-$64-*,0  ; place decode_copy on a page boundary
.endif

decrunch_zpa=DECOMPVARS   ;5 bytes required
zbs1 = decrunch_zpa+$00 ; 1 byte
zbs2 = decrunch_zpa+$01 ; 1 byte
zbs4 = decrunch_zpa+$02 ; 1 byte
zpc  = decrunch_zpa+$03 ; 2 bytes
zpd  = decdestlo

offsetm1 = zpc ; these are aliased, as never need both


decrunch:
;	ldy #0
	sty zbs1
	sty zbs2
	sty zbs4


decrunch_next_group:
.if NC_BLOCK_INTERFACE
	jsr read_init
	lda loadaddrhi
	sta literal_read+2
	ldx loadaddrlo
.endif
next_segment:
	jsr get_byte
.if LOADCOMPD_TO
	clc
	adc loadaddroffslo
	php
.endif
storedadrl:
	sta zpd+0
	jsr get_byte
.if LOADCOMPD_TO
	plp
	adc loadaddroffshi
.endif
storedadrh:
	sta zpd+1

decode_literal:

    ; get count [ExpGoulomb0+1] in x
.if NC_BLOCK_INTERFACE
	getBit1
	lda#1
	bcc ret1
	getExpGoulombTail
ret1:
	sta literal_len + 1
.else
	ldx#1
	getBit1
	bcc ret1
	lda#1
	getExpGoulombTail
	tax
ret1:
.endif

literal_loop:
.if NC_BLOCK_INTERFACE
literal_read:
	lda $ff00,x
	inx
	bne *+5
	jsr read_inc
.else
decompgetbyte:
	jsr getcmem
.endif
	sta (zpd),y
	iny
.if NC_BLOCK_INTERFACE
literal_len:
	cpy #0
.else
	dex
.endif
	bne literal_loop

	clc
	tya
	adc zpd
	sta zpd
	bcc *+4
	inc zpd+1

	ldy#0
	; literal is always followed by copy

decode_copy:
	getBit2h
	bcc short_offset
	lda#1
	getExpGoulombTail_odd_aligned
	adc#255
	sta offsetm1+1
	getByte1
	sta offsetm1
	jmp got_high

short_offset:
	lda#0
	sta offsetm1+1

	;ExpGoulomb k=3
	getBit4h
	lda#1
	bcc no_tail
	getExpGoulombTail_odd_aligned
no_tail:
	adc#255

	getBit4t
	rol
	getBit4t
	rol
	getBit4t
	rol
	sta offsetm1
got_high:

.if NC_BLOCK_INTERFACE
	lda#1
	getBit2t
	bcc length_two
	getExpGoulombTail
	cmp#255
	beq end_of_segment  ; copy length of 256 marks end of segment
length_two:
	sta copy_len+1
.else
	ldx#1
	getBit2t
	bcc length_two
	lda#1
	getExpGoulombTail
	tax
	cpx#255
	beq end_of_segment  ; copy length of 256 marks end of segment
length_two:
.endif

	; note carry is clear at this point; good as we want to subtract (offsetm1+1)
	lda zpd
	sbc offsetm1
	sta zpc

	lda zpd+1
	sbc offsetm1+1
	sta zpc+1

	lda (zpc),y
	sta (zpd),y
copy_loop:
	iny
	lda (zpc),y
	sta (zpd),y
.if NC_BLOCK_INTERFACE
copy_len:
	cpy #0
.else
	dex
.endif
	bne copy_loop
	tya

    ; carry will be set from SBC above
	adc zpd
	sta zpd
	bcc *+4
	inc zpd+1

.if NC_BLOCK_INTERFACE
	POLLBLOCK
.endif

	ldy#0
	getBit1
	bcs jmp_decode_copy
	jmp decode_literal
jmp_decode_copy:
	jmp decode_copy

get_byte:
.if NC_BLOCK_INTERFACE
read:
	lda $ff00,x
	inx
	beq read_inc
	rts
read_inc:
	inc literal_read+2
	inc read+2
read_init:
	pha
	tya
	pha
	GETBLOCK read+2
	pla
	tay
	pla
	ldx#0
end_of_segment:
	rts

.else
decompgetbyte1:
    jmp getcmem
.endif
.if NC_BLOCK_INTERFACE
.else
end_of_file:
	rts
end_of_segment:
	lda offsetm1
	cmp#0
	beq end_of_file
	jmp next_segment
.endif

decrunch_end:

