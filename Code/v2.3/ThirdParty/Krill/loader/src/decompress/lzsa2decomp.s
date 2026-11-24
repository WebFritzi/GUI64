
; with slight modifications by Krill/Plush

.FEATURE labels_without_colons, leading_dot_in_identifiers

decompress = .loadcomp_entry
decompsrc  = lzsa_srcptr

.if MEM_DECOMP_TO_API
		; cannot copy remaining uncompressed blob of unknown size
		.error "***** MEM_DECOMP_TO_API is not supported for LZSA2. Copy compressed data to original location, then use MEM_DECOMP_API to decompress in-place. *****"
.endif

BITFIRE_ZP_ADDR = DECOMPVARS - 1

.define asr alr

lzsa2_get_byte:
		lda (lzsa_srcptr),y ;Subroutine version for when
		inc <lzsa_srcptr + 0    ;inlining isn't advantageous.
		beq .lz_next_page
		rts
.lz_next_page
		inc <lzsa_srcptr + 1    ;Inc & test for bank overflow.
.lz_next_page_
		pha
		txa
		pha
		GETBLOCK <lzsa_srcptr + 1
		pla
		tax
		pla
		rts

.loadcomp_entry
		jsr .lz_next_page_		;shuffle in data first

;		ldy #$00			;Initialize source index.
		ldx #$04
:
		jsr lzsa2_get_byte
		dex
		sta <lzsa_dstptr + 0,x
		bne :-

.if LOADCOMPD_TO
		ldx #2
:
		clc
		lda loadaddroffslo
		adc <lzsa_dstptr,x
		sta <lzsa_dstptr,x
		lda loadaddroffshi
		adc <lzsa_dstptr + 1,x
		sta <lzsa_dstptr + 1,x
		dex
		dex
		bpl :-
		ldx #0
.endif

; -----------------------------------------------------------------------------
;
;  Copyright (C) 2019 Emmanuel Marty
;
;  This software is provided 'as-is', without any express or implied
;  warranty.  In no event will the authors be held liable for any damages
;  arising from the use of this software.
;
;  Permission is granted to anyone to use this software for any purpose,
;  including commercial applications, and to alter it and redistribute it
;  freely, subject to the following restrictions:
;
;  1. The origin of this software must not be misrepresented; you must not
;     claim that you wrote the original software. If you use this software
;     in a product, an acknowledgment in the product documentation would be
;     appreciated but is not required.
;  2. Altered source versions must be plainly marked as such, and must not be
;     misrepresented as being the original software.
;  3. This notice may not be removed or altered from any source distribution.
;
;  Original software by Emmanuel Marty, altered by Tobias Bindhammer
;  to adopt it to bitfire, includes optimization in size and speed and
;  changes to the encoded format
; -----------------------------------------------------------------------------

lzsa_cmdbuf	=	BITFIRE_ZP_ADDR + 1	;1 byte.
lzsa_nibflg	=	BITFIRE_ZP_ADDR + 2	;1 byte.
lzsa_nibble	=	BITFIRE_ZP_ADDR + 3	;1 byte.
lzsa_offset	=	BITFIRE_ZP_ADDR + 4	;1 word.
lzsa_winptr	=	BITFIRE_ZP_ADDR + 6	;1 word.
lzsa_srcptr	=	BITFIRE_ZP_ADDR + 8	;1 word.
lzsa_dstptr	=	BITFIRE_ZP_ADDR + 10	;1 word.
lzsa_endptr	=	BITFIRE_ZP_ADDR + 12	;1 word.
lzsa_length	=	lzsa_winptr + 0
lzsa_tmp	=	lzsa_winptr + 1

		;command byte:
		;765 432 10
		;ttt mmm ll
		;this saves an lsr in .asm - with xyzllmmm we would need 3 lsr, now we need two lsr

		;ll   literals length
		;mmm  match length

.depacker_start
		;XXX TODO can be saved when used with filename? or if it exits cleared
lzsa2_depack
		sty <lzsa_nibflg		;Initialize nibble buffer.

		;
		;Copy bytes from compressed source data.
		;
						;Hi-byte of length or offset.
.cp_length
		jsr lzsa2_get_byte
		sta <lzsa_cmdbuf		;Preserve this for later.

		and #$03			;Extract literal length.
		beq .lz_offset			;size = 0 -> no literal, continue with match

		cmp #$03			;Extended length?
		bne .got_cp_len			;1..2
		jsr .get_length			;x is set by .get_length
		beq .put_cp_len
.got_cp_len
		inx				;Increment # of pages to copy.
.put_cp_len
		stx <lzsa_length
		tax				;low byte
.cp_page
;.lz_ptr1
;		lda $face,x
;		sta (dst),y
;		inx
;		bne +
;		jsr .lz_next_page
;+
;		iny
;		cpy #$00
;		bne .cp_page
;		tya
;		clc
;		adc <lzsa_dstptr + 0
;		sta <lzsa_dstptr + 0
;		bcc +
;		inc <Ã¶zsa_dstptr + 1
;+
;		dec <lzsa_length
;		bne .cp_page

		lda (lzsa_srcptr),y
		;saves another 6 bytes if used here, then single point of change
		;jsr lzsa2_get_byte
		sta (lzsa_dstptr),y

		inc <lzsa_srcptr + 0
		bne .skip1
		jsr .lz_next_page
.skip1
		inc <lzsa_dstptr + 0
		bne .skip2
		inc <lzsa_dstptr + 1
.skip2
		dex
		bne .cp_page
		dec <lzsa_length		;Any full pages left to copy?
		bne .cp_page

		;ttt  token for match offset len
		;codepath 1
		;0m0  5-bit offset  - m goes into first bit of offset + one nibble is read		;11111111 111mnnnn
		;0m1  13-bit offset - m goes into first bit of offset + one nibble and on byte is read	;111mnnnn bbbbbbbb

		;codepath 2
		;10m  9-bit offset  - m goes into first bit of offset + one byte is read		;1111111m bbbbbbbb
		;110  16-bit offset - two bytes are read						;bbbbbbbb bbbbbbbb
		;111  repeat offset

		;fetch a nibble
		;13,9,16 -> fetch a lowbyte
		;9 bit fetch a bit
		;16 -> fetch a highbyte
		;rep, skip all

.lz_offset
		lda <lzsa_cmdbuf
		asl
		bcc .get_5_13_bits		;prefer path with 3 options over path with 2 options
.get_9_16_rep
		asl
		bcc .get_9_bits
.get_16_rep
		bmi .lz_length			;Repeat previous offset.
.get_16_bits
		jsr lzsa2_get_byte  		;Get hi-byte of offset.
		bne .get_low8
.get_9_bits
		asl
		lda #$ff			;-> $ff/$fe
		rol
		bne .get_low8			;BRA
.get_5_13_bits
		sta <lzsa_tmp
		jsr lzsa2_get_nibble
		asl <lzsa_tmp			;shift in bit 5/13
		rol
		asl <lzsa_tmp			;shift in token bit to decide 5/13
		dex				;x = $ff
		bcc .get_low0			;all done for 5-bit offset
		sbc #2				;Subtract 512 because 13-bit - this extends the range for 13 bits offsets, as 0..511 is covered by 9 bit matches
						;offset starts at $FE00.
.get_low8
		tax
		jsr lzsa2_get_byte
.get_low0
		sta <lzsa_offset + 0
		stx <lzsa_offset + 1		;Save new offset.

		;
		;Copy bytes from decompressed window.
		;
		;N.B. X=0 is expected and guaranteed when we get here.
		;

.lz_length
		ldx #$00			;Hi-byte of length.
		lda <lzsa_cmdbuf
		lsr
		asr #$0e			;extract match len and clear C
		adc #$02			;correct length
		cmp #$09			;Extended length?
		bne .got_lz_len			;a = 2 .. 8, C = 0
		jsr .get_length			;x is set by .get_length, y is still 0
		clc
		beq .calc_lz_addr		;only need to check for zero here
.got_lz_len					;C = 0
		eor #$ff			;Negate the lo-byte of length
		tay
		iny
		eor #$ff			;restore A, a bit ugly
		inx				;Increment # of pages to copy.
		;clc				;Calc destination for partial page
		adc <lzsa_dstptr + 0
		sta <lzsa_dstptr + 0
		bcs .calc_lz_addr_
		dec <lzsa_dstptr + 1
.calc_lz_addr
		lda <lzsa_dstptr + 0		;N.B. Offset is negative!
.calc_lz_addr_
		clc				;Calc address of match.
		adc <lzsa_offset + 0
		sta <lzsa_winptr + 0
		lda <lzsa_dstptr + 1
		adc <lzsa_offset + 1
		sta <lzsa_winptr + 1
.lz_page
		lda (lzsa_winptr),y
		sta (lzsa_dstptr),y
		iny
		bne .lz_page
		inc <lzsa_winptr + 1
		inc <lzsa_dstptr + 1
		dex				;Any full pages left to copy?
		bne .lz_page

		lda <lzsa_srcptr + 0
		eor <lzsa_endptr + 0
		bne .lz_poll
		eor <lzsa_srcptr + 1
		eor <lzsa_endptr + 1
		beq .finished2
.lz_skip_end
		jmp .cp_length			;Loop around to the beginning.

.lz_poll
		POLLBLOCK
;		ldy #0
		ldx #$00			;clear x afterwards, yet a bit annoying, y stays 0
		beq .lz_skip_end

.get_length
		;entered with x = 0
		adc #$0e			;C = 1 -> adc #$0f
		sta <lzsa_tmp
		jsr lzsa2_get_nibble
		adc #$00			;C = 1
		beq .byte_length		;Extended length?
		adc <lzsa_tmp 			;C = 0 from addition above -> $10 + lzsa_tmp
.got_length					;Z-flag is set
		rts				;lengths.

.byte_length
		jsr lzsa2_get_byte  		;So rare, this can be slow!
		clc				;adc #$0f + 0
		adc <lzsa_tmp
		bcc .got_length
		beq .finished
.word_length
						;this is fetched big-endian, to avoid pha/pla, saves code
		jsr lzsa2_get_byte		;So rare, this can be slow!
		tax
		jsr lzsa2_get_byte		;So rare, this can be slow!
		and #$ff			;restore z-flag
		rts
.finished
		pla				;Decompression completed, pop
		pla				;return address.
.finished2
		rts

		;
		;Get a nibble value from compressed data in A.
		;

		;XXX TODO pack nibbles in another way? -> 10101010 -> first nibble = and #$55, second nibble = asr #$aa?
lzsa2_get_nibble
		lsr <lzsa_nibflg		;Is there a nibble waiting?
		lda <lzsa_nibble		;Extract the lo-nibble.
		bcs .got_nibble

		inc <lzsa_nibflg		;Reset the flag.

		jsr lzsa2_get_byte

		sta <lzsa_nibble		;Preserve for next time.
		lsr				;Extract the hi-nibble.
		lsr
		lsr
		lsr
		sec
.got_nibble
		ora #$f0
		rts
