
decompsrc = lz_sector_ptr1

;-------------------------------------------------------------------------------
;Regular version of the Lempel-Ziv decompressor
;-------------------------------------------------------------------------------
lz_dst          = decdestlo             ;Decompression destination pointer.
                                        ;Initialize this to whatever address
                                        ;you want to decompress to

lz_bits         = DECOMPVARS + $00      ;Shift register. Initialized to $80
                                        ;for a new file

lz_scratch      = DECOMPVARS + $01      ;Temporary zeropage storage

lz_ybuffer      = DECOMPVARS + $02      ;Temporary register storage when fetching sector

lz_sector       = $ff00                 ;The one-page buffer from which the
                                        ;compressed data is actually read,
                                        ;and which gets refilled by
                                        ;lz_fetch_sector.

;-------------------------------------------------------------------------------
;This is the user's hook to replenish the sector buffer with some new bytes.
;
;A and Y are expected to be preserved while carry must remain set on exit.
;X should point to the first byte of the new data, e.g. zero for a full 256-byte
;page of data or two to skip past the sector and track links.
;
;When fetching from a larger in-memory array rather than a single sector buffer
;the lz_sector_ptr1..3 pointers will need to be patched up
;-------------------------------------------------------------------------------
lz_fetch_sector:
		inc lz_sector_ptr1 + 1
		inc lz_sector_ptr2 + 1
		inc lz_sector_ptr3 + 1
lz_fetch_sector_:
		pha
		sty lz_ybuffer
		GETBLOCK lz_sector_ptr1 + 1
		ldx #0
		ldy lz_ybuffer
		pla
		sec
		rts

decompress:
		jsr lz_fetch_sector_
		lda loadaddrhi
		sta lz_sector_ptr2 + 1
		sta lz_sector_ptr3 + 1
		ldx loadaddrlo

		jsr _lz_refill_bits
		tya
storedadrl:
		sta lz_dst + $00
		jsr _lz_refill_bits
		tya
storedadrh:
		sta lz_dst + $01

.if LOADCOMPD_TO
		clc
		lda loadaddroffslo
		adc lz_dst + $00
		sta lz_dst + $00
		lda loadaddroffshi
		adc lz_dst + $01
		sta lz_dst + $01
.endif

;-------------------------------------------------------------------------------
;This is the main lz_decrunch function which may be called to decompress an
;entire file.
;
;On entry and exit the X register points to the next available byte in the
;sector buffer, in ascending order from $00 to $ff.
;This implies that the initial sector must have already been fetched, and that a
;file ending with X wrapped to $00 will have needlessly fetched an extra sector
;(which may be taken advantage of when decoding a contiguous set of files.)
;-------------------------------------------------------------------------------

		;******** Start the next match/literal run ********

lz_decrunch:	sec			;This is the main entry point. Forcibly
_lz_type_refill:
		jsr _lz_refill_bits	;fill up the the bit buffer on entry
		bne _lz_type_cont	;(BRA)

		;Wrap the high-byte of the destination pointer.
_lz_mfinish:	bcc *+4
_lz_maximum:	inc lz_dst+1		;This is also used by maximum length
					;literals needing an explicit type bit

		POLLBLOCK

		;Literal or match to follow?
		asl lz_bits
_lz_type_cont:	bcc _lz_do_match
		beq lz_decrunch


		;******** Process literal run ********

		lda #%00000000		;Decode run length
_lz_lrun_loop:	rol
		asl lz_bits
		bcs _lz_lrun_test
_lz_lrun_back:	asl lz_bits
		bne _lz_lrun_loop

		jsr _lz_refill_bits
		bne _lz_lrun_loop	;(BRA)

_lz_lrun_test:	bne _lz_lrun_gotten
		jsr _lz_refill_bits
		bcc _lz_lrun_back

_lz_lrun_gotten:
		sta _lz_copy_cnt+1	;Store LSB of run-length
		ldy #$00
_lz_lcopy:
lz_sector_ptr2	= *+1			;Copy the literal data. Note the
		lda lz_sector,x
		inx
		bne *+5
		jsr lz_fetch_sector	;Grab a new sector for the literal loop
		sta (lz_dst),y
		iny
_lz_copy_cnt:	cpy #$00
		bne _lz_lcopy

		;Time to advance the destination pointer.
		;Maximum run length literals exit here as a type-bit needs
		;to be fetched afterwards
		tya
		beq _lz_maximum
		clc
		adc lz_dst+0
		sta lz_dst+0
		bcc *+4
		inc lz_dst+1

		POLLBLOCK

		;One literal run following another only makes sense if the
		;first run is of maximum length and had to be split. As that
		;case has been taken care of we can safely omit the type bit
		;here


		;******** Process match ********

_lz_do_match:	lda #%00100000		;Determine offset length by a two-bit
_lz_moff_range:	asl lz_bits		;prefix combined with the first run
		bne *+5			;length bit (where a one identifies
		jsr _lz_refill_bits	;a two-byte match).
		rol			;The rest of the length bits will
		bcc _lz_moff_range	;then follow *after* the offset data

		tay
		lda _lz_moff_length,y
		beq _lz_moff_far

_lz_moff_loop:	asl lz_bits		;Load partial offset byte
		bne *+9
		sty lz_scratch
		jsr _lz_refill_bits
		ldy lz_scratch

		rol
		bcc _lz_moff_loop

		bmi _lz_moff_near

_lz_moff_far:	sta lz_scratch		;Save the bits we just read as the
					;high-byte

lz_sector_ptr3	= *+1
		lda lz_sector,x		;For large offsets we can load the
		inx			;low-byte straight from the stream
		bne *+5			;without going throught the shift
		jsr lz_fetch_sector	;register

;		sec
		adc _lz_moff_adjust_lo,y
		bcs _lz_moff_pageok
		dec lz_scratch
		sec
_lz_moff_pageok:
		adc lz_dst+0
		sta _lz_match+0

		lda lz_scratch
		adc _lz_moff_adjust_hi,y
		sec
		bcs _lz_moff_join	;(BRA)

_lz_moff_near:
;		sec			;Special case handling of <8 bit offsets.
	 	adc _lz_moff_adjust_lo,y;We may can safely ignore the MSB from
;		sec			;the base adjustment table as the
		adc lz_dst+0		;maximum base (for a 4/5/6/7 bit
		sta _lz_match+0		;length sequence) is 113
		lda #$ff
_lz_moff_join:	adc lz_dst+1
		sta _lz_match+1

		cpy #$04		;Get any remaning run length bits
		lda #%00000001
		bcs _lz_mrun_gotten

_lz_mrun_loop:	asl lz_bits
		bne *+5
		jsr _lz_refill_bits
		rol
		asl lz_bits
		bcc _lz_mrun_loop
		bne _lz_mrun_gotten
		jsr _lz_refill_bits
		bcc _lz_mrun_loop

_lz_mrun_gotten:
		tay			;A 257-byte (=>$00) run serves as a
		beq _lz_end_of_file	;sentinel

		sta _lz_mcopy_len

		ldy #$ff		;The copy loop. This needs to be run
_lz_mcopy:	iny			;forwards since RLE-style matches can
_lz_match	= *+1			;overlap the destination
		lda $ffff,y
		sta (lz_dst),y
_lz_mcopy_len	= *+1
		cpy #$ff
		bne _lz_mcopy

		tya			;Advance destination pointer
;		sec
		adc lz_dst+0
		sta lz_dst+0
		jmp _lz_mfinish


		;******** Fetch some more bits to work with ********

lz_sector_ptr1	= *+1
_lz_refill_bits:
		ldy lz_sector,x
		sty lz_bits
		inx
		bne *+5
		jsr lz_fetch_sector
;		sec
		rol lz_bits
_lz_end_of_file:
		rts

		;******** Offset coding tables ********

		;This length table is a bit funky. The idea here is to use the
		;value as the initial value of the shift register instead of
		;keeping a separate counter.
		;In other words we iterate until the leading one is shifted out.
		;Then afterwards the bit just below it (our new sign bit) is set
		;if the offset is shorter than 8-bits, and conversely it's
		;cleared if we need to fetch a separate low-byte
		;as well.
		;The fact that the sign bit is cleared as a flag is compensated
		;for in the lz_moff_adjust_hi table

_lz_moff_length:
		;Long (>2 byte matches)
		.byte %00011111		;4 bits
		.byte %00000011		;7 bits
		.byte %01011111		;10 bits
		.byte %00001011		;13 bits
		;Short (2 byte matches)
		.byte %01011111		;10 bits
		.byte %00000000		;8 bits
		.byte %00000111		;6 bits
		.byte %00111111		;3 bits
_lz_moff_adjust_lo:
		;Long (>2 byte matches)
		.byte %11111110		;1-16
		.byte %11101110		;17-144
		.byte %01101110		;145-1168
		.byte %01101110		;1169-9360
		;Short (2 byte matches)
		.byte %10110110		;329-1352
		.byte %10110110		;73-328
		.byte %11110110		;9-72
		.byte %11111110		;1-8
_lz_moff_adjust_hi = *-2
		;Long (>2 byte matches)
;		.byte %11111111		;1-16 (unreferenced)
;		.byte %11111111		;17-144 (unreferenced)
		.byte %01111111		;145-1168
		.byte %01111011		;1169-9360
		;Short (2 byte matches)
		.byte %01111110		;329-1352
		.byte %11111110		;73-328
;		.byte %11111111		;9-72 (unreferenced)
;		.byte %11111111		;1-8 (unreferenced)
