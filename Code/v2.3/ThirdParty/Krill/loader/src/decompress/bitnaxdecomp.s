
decompsrc = bitfire_lz_sector_ptr1

.if MEM_DECOMP_TO_API
		; cannot copy remaining uncompressed blob of unknown size
		.error "***** MEM_DECOMP_TO_API is not supported for BITNAX. Copy compressed data to original location, then use MEM_DECOMP_API to decompress in-place. *****"
.endif

BITFIRE_DECOMP_ZERO_OVERLAP = 1

.FEATURE labels_without_colons, leading_dot_in_identifiers

.lz_bits = DECOMPVARS + 0
.lz_dst  = DECOMPVARS + 1
.lz_end  = DECOMPVARS + 3
.lz_tmp  = DECOMPVARS + 5

decompress:
		jsr .lz_next_page_
		lda loadaddrhi
		sta bitfire_lz_sector_ptr2 + 1
		ldx loadaddrlo
.if BITFIRE_DECOMP_ZERO_OVERLAP
		ldy #$03; destination and end addresses
.else
		ldy #$01; destination address
.endif
:		lda (loadaddrlo),y
		sta .lz_dst,y
		inx
		bne :+
		jsr .lz_next_page
:		dey
		bpl :--

.if LOADCOMPD_TO
		clc
		lda loadaddroffslo
		adc .lz_dst
		sta .lz_dst
		lda loadaddroffshi
		adc .lz_dst+1
		sta .lz_dst+1
    .if BITFIRE_DECOMP_ZERO_OVERLAP
		clc
		lda loadaddroffslo
		adc .lz_end
		sta .lz_end
		lda loadaddroffshi
		adc .lz_end+1
		sta .lz_end+1
	.endif
.endif
		sec

.lz_type_refill
		jsr .lz_refill_bits		;refill bit buffer .lz_bits

		;******** Start the next match/literal run ********
.lz_type_check
		bcc .lz_do_match
		beq .lz_type_refill		;we will fall through on entry

		;******** Process literal run ********

		lda #$00
:
		rol				;-> a = $01 after first round
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits		;kills y
		bcc .lz_lrun_gotten

		asl .lz_bits
		bne :-
		jsr .lz_refill_bits
		bne :-

.lz_lrun_gotten
		sta .lz_lcopy_len		;Store LSB of run-length
		ldy #$00
.lz_lcopy
bitfire_lz_sector_ptr2	= * + 1			;Copy the literal data, forward or overlap is getting a pain in the ass.
		lda $ff00,x
		sta (.lz_dst),y
		inx
		bne :+
		clc
		jsr .lz_next_page
:
		iny
.lz_lcopy_len = * + 1
		cpy #$00
		bne .lz_lcopy

		tya
;.if LOAD_VIA_KERNAL_FALLBACK | (LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16))
		bne *+5
		jmp .lz_maximum			;maximum literal run, bump sector pointers and so on and force new type bit
;.else
;		beq .lz_maximum			;maximum literal run, bump sector pointers and so on and force new type bit
;.endif
						;XXX TODO can we reuse the same code? In one case continue with match, in other case redecide
		clc
		adc .lz_dst
		sta .lz_dst
		bcc *+4
		inc .lz_dst+1

		POLLBLOCK
						;no need for a type bit, after each literal a match follows, except for maximum runlength literals

		;******** Process match ********

.lz_do_match
		lda #$01			;this could be made shorter by using the last bitfetch of the upcoming loop and restoring the carry again by a cmp #$02. Saves bytes, but makes things slower, as eof check is also done with all short matches then

		asl .lz_bits			;first length bit (where a one identifies
		bne *+5				;a two-byte match)
		jsr .lz_refill_bits
		bcc .lz_get_offs		;all done, length is 2, skip further bitfetches (and eof check)
:
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol

		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		bcc :-
.lz_got_len
		tay				;XXX TODO could this be placed elsewhere to make the tay obsolete?
		beq .lz_end_of_file		;A 257-byte (=>$00) run serves as a sentinel, but not with zero-overlap, except when depacking from a non inplace address, then it is still appended
.lz_get_offs
		sta .lz_mcopy_len		;store length at final destination

		lda #%11000000			;fetch 2 more prefix bits
		rol				;previous bit is still in carry \o/
:
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs :-

		beq .lz_8_and_more		;0 + 8 bits to fetch, branch out before table lookup to save a few cycles and one byte in the table, also save complexity on the bitfetcher
		tay
		lda .lz_lentab,y
:						;same as above
		asl .lz_bits			;XXX same code as above, so annoying :-(
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs :-

		bmi .lz_less_than_8		;either 3,4,6 or 7 bits fetched -> highbyte will be $ff
.lz_8_and_more
		jsr .lz_refill_bits
		eor #$ff			;5 of 13, 2 of 10, 0 of 8 bits fetched as highbyte, lowbyte still to be fetched
		sta .lz_tmp			;XXX this is a pain in the arse that A and Y need to be swapped :-(
		tya
		ldy .lz_tmp
		SKIPWORD
.lz_less_than_8
		ldy #$ff			;XXX TODO silly, y is set twice in short case
		adc .lz_dst			;subtract offset from lz_dst
		sta .lz_m+1
		tya				;hibyte
		adc .lz_dst+1
		sta .lz_m+2

		ldy #$ff			;The copy loop. This needs to be run
						;forwards since RLE-style matches can overlap the destination
.lz_mcopy
		iny
.lz_m		lda $face,y			;copy one byte
		sta (.lz_dst),y
.lz_mcopy_len	= * + 1
		cpy #$ff
		bne .lz_mcopy

		tya				;advance destination pointer
;		sec				;XXX TODO carry set = type check needed, cleared (literal) = match follows anyway
		adc .lz_dst
		sta .lz_dst

.if BITFIRE_DECOMP_ZERO_OVERLAP = 0
.lz_skip_poll	bcc :+
.lz_maximum	inc .lz_dst+1			;this is also used by maximum length
		bcs .lz_skip_end
:
.else
		bcc :+				;proceed to check
.lz_maximum
		inc .lz_dst+1			;advance hi byte
;		lda .lz_dst			;if entering via .lz_maximum, a = 0, so we would pass the following check only if the endadress is @ $xx00
:						;if so, the endaddress can't be $xx00 and the highbyte check will fail, as we just successfully wrote a literal with type bit, so the end address must be greater then the current lz_dst, as either another literal or match must follow. Can you still follow me?! :-D
		eor .lz_end			;check end address
.lz_skip_poll	beq .lz_check_end			;all okay, poll for a new block

.endif ; BITFIRE_DECOMP_ZERO_OVERLAP

		POLLBLOCK

.lz_skip_end
						;literals needing an explicit type bit
		asl .lz_bits			;fetch next type bit
		jmp .lz_type_check

.if BITFIRE_DECOMP_ZERO_OVERLAP

.lz_check_end
		lda .lz_dst+1			;check highbyte
		eor .lz_end+1
		bne .lz_skip_end		;skip poll, so that only one branch needs to be manipulated
		;sta .barrier			;clear barrier and force to load until EOF, XXX does not work, but will at least force one additional block before leaving as barrier will be set again upon next block being fetched. Will overlap be > than 2 blocks? most likely not? CRAP, tony taught me that there is /o\
		;lda #$ff
		;sta bitfire_load_addr_hi	;needed if the barrier method will not work out, plain jump to poll loop will fail on stand alone depack?
		;jmp .lz_next_page		;load any remaining literal blob if there, or exit with rts in case of plain decomp (rts there instead of php). So we are forced until either the sector_ptr reaches $00xx or EOF happens, so nothing can go wrong

		; fetching any remaining final literals uncompressed blob is performed by the caller (loadcompd in resident.s) 
.endif ; BITFIRE_DECOMP_ZERO_OVERLAP

.lz_end_of_file
		rts

.lz_refill_bits
bitfire_lz_sector_ptr1	= * + 1
bitfire_load_addr_hi = * + 2
		ldy $ff00,x
		inx
		bne .lz_same_page

.lz_next_page
		inc bitfire_load_addr_hi
		inc bitfire_lz_sector_ptr2 + 1
.lz_next_page_
		php
		pha
		sty .lz_tmp
		GETBLOCK bitfire_load_addr_hi
		ldx #0
		ldy .lz_tmp
		pla
		plp

.lz_same_page
						;store bits? happens on all calls, except when a whole literal is fetched
		bcc :+				;only store lz_bits if carry is set (in all cases, except when literal is fetched for offset)
		sty .lz_bits
		rol .lz_bits
:		rts

.lz_lentab = * - 1
		;short offset init values
		;.byte %00000000			;2
		.byte %11011111			;0
		.byte %11111011			;1
		.byte %10000000			;3

		;long offset init values
		.byte %11101111			;offset 0
		.byte %11111101			;offset 1
		.byte %10000000			;offset 2
		.byte %11110000			;offset 3
