
;
; (c) Copyright 2021 by Tobias Bindhammer. All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;     * The name of its author may not be used to endorse or promote products
;       derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

.FEATURE labels_without_colons, leading_dot_in_identifiers

.if MEM_DECOMP_TO_API
			; cannot perform src == dst check when not depacking in-place,
			; as offsets (arbitrary src - original src) or (arbitrary dst - original dst) are not known,
			; also cannot copy remaining uncompressed blob of unknown size
			.error "***** MEM_DECOMP_TO_API is not supported for ZX0. Copy compressed data to original location, then use MEM_DECOMP_API to decompress in-place. *****"
.endif

.if PREFER_SPEED_OVER_SIZE

; faster but bigger decruncher

decompsrc  = lz_src

lz_bits   = BLOCKINDEX + 1
lz_src    = DECOMPVARS + 0
lz_dst    = decdestlo
lz_len_hi = DECOMPVARS + 4

; USE_DALI is defined in resident.s
.if USE_DALI
.define set_lz_bit_marker ror
.define get_lz_bit        lsr <lz_bits
.else
.define set_lz_bit_marker rol
.define get_lz_bit        asl <lz_bits
.endif
.define inc_src_ptr       jsr lz_next_page		;sets X = 0, so all sane

			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax				;save bits fetched so far
			lda (lz_src),y			;fetch another lz_bits byte from stream
			set_lz_bit_marker
			sta <lz_bits
			inc <lz_src + 0 		;postponed pointer increment, so no need to save A on next_page call
			beq .lz_inc_src2
.lz_inc_src2_
			txa				;restore fetched bits, also postponed, so A can be trashed on lz_inc_src above
			bcs .lz_lend			;last bit fetched?
.lz_get_loop
			get_lz_bit			;fetch payload bit
.lz_length_16_
			rol				;shift in new payload bit
			bcs .lz_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
			get_lz_bit			;fetch control bit
			bcc .lz_get_loop		;need more bits
			beq .lz_refill_bits		;buffer is empty, fetch new bits
.lz_lend						;was the last bit
			rts
.lz_length_16						;this routine happens very rarely, so we can waste cycles
			pha				;save so far received lobyte
			tya				;was lda #$01, but A = 0 + upcoming rol makes this also start with A = 1
			jsr .lz_length_16_		;get up to 7 more bits
			sta <lz_len_hi			;and save hibyte
			pla				;restore lobyte
;			bne .lz_not_zero		;was the lobyte zero?
;			pla				;pull lowbyte of last jsr-call from stack
;			pha				;restore
;			;eor #<.lz_jsr_addr
;			bmi +				;eof and match case, no need to decrement lz_len_hi
;			dec <lz_len_hi			;decrement lz_len_hi
;+
;			tya

			bne .lz_not_zero		;was the lobyte zero?
			dec <lz_len_hi			;yes, so decrement hibyte beforehand to avoid expensive checks later on, except for one case
			tya				;keep Z = 0, (but not needed in case of eof, the dec also results in 0 there)

.lz_not_zero
			rts

			;------------------
			;DECOMP INIT
			;------------------
decompress
							;copy over end_pos and lz_dst from stream
;			ldy #$00			;needs to be set in any case, also plain decomp enters here
			ldx #$02
.if USE_DALI
			stx <lz_bits
.endif
:
			lda (lz_src),y
			sta <lz_dst,x
			inc <lz_src
			jsr .lz_init
			dex
			bpl :-
			sty .lz_offset_lo + 1		;initialize offset with $0000
			sty .lz_offset_hi + 1
			sty <lz_len_hi			;reset len - XXX TODO could also be cleared upon installer, as the depacker leaves that value clean again
.if LOADCOMPD_TO
			clc
			lda loadaddroffslo
			adc <lz_dst
			sta <lz_dst
			lda loadaddroffshi
			adc <lz_dst + 1
			sta <lz_dst + 1
			jmp .lz_poll
.else
			bmi .lz_poll; jmp
.endif

			;------------------
			;SELDOM STUFF
			;------------------
.lz_inc_src2
			inc_src_ptr
			bne .lz_inc_src2_
.lz_inc_src3
			inc_src_ptr
			bcs .lz_inc_src3_
.lz_dst_inc
			inc <lz_dst + 1
			bcs .lz_dst_inc_

			;------------------
			;SELDOM STUFF
			;------------------
.lz_clc
			clc
			bcc .lz_clc_back
.lz_cp_page
			dec <lz_len_hi
			txa				;much shorter this way. if we recalculate m_src and dst, endcheck also hits in if we end with an multipage match, else maybe buggy?
			beq .lz_l_page			;if entered from a literal, x == 0
			tya				;if entered from a match, x is anything between $01 and $ff due to inx stx <lz_dst + 1, except if we would depack to zp?
			bcs .lz_m_page			;as Y = 0, we can skip the part that does Y = A xor $ff

			;------------------
			;POLLING
			;------------------
.lz_poll
.lz_start_over
			POLLBLOCK

			;------------------
			;ENTRY POINT DEPACKER
			;------------------

			lda #$01			;we fall through this check on entry and start with literal
			get_lz_bit
			bcs .lz_match			;after each match check for another match or literal?

			;------------------
			;LITERAL
			;------------------
.lz_literal
			get_lz_bit
			bcs :++
:
			get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
			get_lz_bit			;fetch payload bit
			bcc :-
:
			bne :+
.lz_start_depack
			jsr .lz_refill_bits
:
			tax
.lz_l_page
.lz_cp_lit						;XXX TODO copy with Y += 1 but have lz_src + 0 eor #$ff in x and countdown x, so that lz_src + 1 can be incremented in time?
			lda (lz_src),y			;/!\ Need to copy this way, or we run into danger to copy from an area that is yet blocked by barrier, this totally sucks, loading in order reveals that
			sta (lz_dst),y

			inc <lz_src + 0
			beq .lz_inc_src3
.lz_inc_src3_
			inc <lz_dst + 0
			beq .lz_dst_inc
.lz_dst_inc_
			dex
			bne .lz_cp_lit
							;XXX TODO we could also just manipulate a branch and make it go to either page handling or fall through? enable branch if 16 bits are fetched and lz_len  > 0? dop or bcs to disable and enable, 80 or b0
			lda <lz_len_hi			;more pages to copy?
			bne .lz_cp_page			;happens very seldom

			;------------------
			;NEW OR OLD OFFSET
			;------------------
							;XXX TODO fetch length first and then decide if literal, match, repeat? But brings our checks for last bit to the end? need to check then on typebit? therefore entry for fetch is straight?
							;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			rol				;was A = 0, C = 1 -> A = 1 with rol, but not if we copy literal this way
			get_lz_bit
			bcs .lz_match			;either match with new offset or old offset

			;------------------
			;REPEAT LAST OFFSET
			;------------------
.lz_repeat
			get_lz_bit
			bcs :++
:
			get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
			get_lz_bit			;cheaper with 2 branches, as initial branch to .lz_literal therefore is removed
			bcc :-
:
			bne :+
			jsr .lz_refill_bits		;fetch more bits
			beq .lz_m_page			;XXX TODO sec after sbc #1 is also sufficient, but slower
:
			sbc #$01			;subtract 1, will be added again on adc as C = 1
.lz_match_big						;we enter with length - 1 here from normal match
			eor #$ff
			tay
.lz_m_page
			eor #$ff			;restore A

			adc <lz_dst + 0			;add length
			sta <lz_dst + 0
			bcs .lz_clc			;/!\ branch happens very seldom, if so, clear carry
			dec <lz_dst + 1			;subtract one more in this case
.lz_clc_back
.lz_offset_lo		sbc #$00			;carry is cleared, subtract (offset + 1)
			sta .lz_msrcr + 0
			lax <lz_dst + 1
.lz_offset_hi		sbc #$00
			sta .lz_msrcr + 1
.lz_cp_match						;XXX TODO if repeated offset: add literal size to .lz_msrcr and done?
.lz_msrcr = * + 1
			lda $beef,y
			sta (lz_dst),y
			iny
			bne .lz_cp_match
			inx
			stx <lz_dst + 1			;cheaper to get lz_dst + 1 into x than lz_dst + 0 for upcoming compare

			lda <lz_len_hi			;check for more loop runs
.if LOAD_TO_RAM_UNDER_IO | LOAD_VIA_KERNAL_FALLBACK
                        beq :+
			jmp .lz_cp_page			;do more page runs? Yes? Fall through
:
.else
			bne .lz_cp_page			;do more page runs? Yes? Fall through
.endif
.lz_check_poll
			cpx <lz_src + 1			;check for end condition when depacking inplace, lz_dst + 0 still in X
.if LOAD_TO_RAM_UNDER_IO | LOAD_VIA_KERNAL_FALLBACK
			bne :+

			lda <lz_dst + 0
			eor <lz_src + 0
			beq lz_next_page
:			jmp .lz_start_over
.else
			bne .lz_start_over

			lda <lz_dst + 0
			eor <lz_src + 0
			bne .lz_start_over
			beq lz_next_page
.endif
							;XXX TODO, save one byte above and the beq lz_next_page can be omitted and lz_next_page copied here again
			;jmp .ld_load_raw		;but should be able to skip fetch, so does not work this way
			;top				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway
							;as last block is forced, we would always wait for last block to be loaded if we enter this loop, no matter how :-)
			;------------------
			;MATCH
			;------------------
:
			get_lz_bit			;fetch payload bit
			rol				;add bit to number
.lz_match
			get_lz_bit			;fetch control bit
			bcc :-				;not yet done, fetch more bits

			bne :+				;last bit or bitbuffer empty? fetched 1 to 4 bits now
			jsr .lz_refill_bits		;refill bitbuffer
			beq .lz_eof			;so offset was $100 as lowbyte is $00, only here 4-8 bits are fetched
:
			sbc #$01			;subtract 1, elias numbers range from 1..256, we need 0..255
			lsr				;set bit 15 to 0 while shifting hibyte
			sta .lz_offset_hi + 1		;hibyte of offset

			lda (lz_src),y			;fetch another byte directly, same as refill_bits...
			ror				;and shift -> first bit for lenth is in carry, and we have %0xxxxxxx xxxxxxxx as offset
			sta .lz_offset_lo + 1		;lobyte of offset

			inc <lz_src + 0			;postponed, so no need to save A on next_page call
			beq .lz_inc_src1
.lz_inc_src1_
			lda #$01			;fetch new number, start with 1
			bcs .lz_match_big		;length = 1, do it the very short way
:
			get_lz_bit			;fetch more bits
			rol
			get_lz_bit
			bcc :-
			bne .lz_match_big
			jsr .lz_refill_bits		;fetch remaining bits
;.lz_jsr_addr = * - 1
;			bcs .lz_match_big		;lobyte != 0?
			bne .lz_match_big		;lobyte != 0?

			;------------------
			;SELDOM STUFF
			;------------------

			inc <lz_len_hi			;need to correct <lz_len_hi
			bcs .lz_match_big		;and enter match copy loop
.lz_inc_src1
			inc_src_ptr
			bne .lz_inc_src1_

			;------------------
			;NEXT PAGE IN STREAM
			;------------------
.lz_init
			bne .lz_next_page_
lz_next_page
			inc <lz_src + 1
.lz_next_page_
			php
			txa
			pha
			GETBLOCK <lz_src + 1
			pla
			tax
			plp
.lz_eof
			rts

;XXX TODO
;decide upon 2 bits with bit <lz_bits? bmi + bvs + bvc? bpl/bmi decides if repeat or not, bvs = length 2/check for new bits and redecide, other lengths do not need to check, this can alos be used on other occasions?
;do a jmp ($00xx) to determine branch?

.else

decompsrc  = .lz_src

OFFSET_OPT = 0; +8

.lz_bits   = BLOCKINDEX + 1
.lz_src    = DECOMPVARS + 0
.lz_dst    = decdestlo
.if OFFSET_OPT
.else
.lz_offset = DECOMPVARS + 2
.endif
.lz_len_hi = DECOMPVARS + 4

; USE_DALI is defined in resident.s
.if USE_DALI
.define rot ror
.define shf lsr
.else
.define rot rol
.define shf asl
.endif

			;------------------
			;SELDOM STUFF
			;------------------
.lz_l_page
.lz_dcp
			dec <.lz_len_hi
			bcs .lz_cp_lit
			sec
			bcs .lz_match_big

			;------------------
			;POINTER HANDLING LITERAL COPY
			;------------------
.lz_dst_inc
			inc <.lz_dst + 1
			bcs .lz_dst_inc_
.lz_src_inc
			jsr .lz_next_page		;sets X = 0, so all sane
			bcs .lz_src_inc_

decompress
							;copy over end_pos and lz_dst from stream
			ldx #2
.if USE_DALI
			stx <.lz_bits
.endif
:
			lda (.lz_src),y			;copy over first two bytes
			sta <.lz_dst,x
			inc <.lz_src + 0
			jsr .lz_init
.if OFFSET_OPT
.else
			sty .lz_offset,x; reset offset and length-hi
.endif
			dex
			bpl :-
.if LOADCOMPD_TO
			clc
			lda loadaddroffslo
			adc <.lz_dst
			sta <.lz_dst
			lda loadaddroffshi
			adc <.lz_dst + 1
			sta <.lz_dst + 1
.endif

.if OFFSET_OPT
			sty .lz_offset_lo + 1		;initialize offset with $0000
			sty .lz_offset_hi + 1
			sty <.lz_len_hi			;reset len - XXX TODO could also be cleared upon installer, as the depacker leaves that value clean again
.endif
							;start with an empty lz_bits, first shf <.lz_bits leads to literal this way and bits are refilled upon next shift
.if USE_DALI
.elseif LOAD_VIA_KERNAL_FALLBACK | MEM_DECOMP_API
			lda #$40
			sta <.lz_bits
.endif

			;------------------
			;POLLING
			;------------------
.lz_start_over		POLLBLOCK

			;------------------
			;LITERAL
			;------------------

			lda #$01			;we fall through this check on entry and start with literal
			shf <.lz_bits
			bcs .lz_match			;after each match check for another match or literal?
.lz_literal
			jsr .lz_length
			tax
			beq .lz_l_page			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it returns on c = 1, what is always true after jsr .lz_length
.lz_cp_lit
			lda (.lz_src),y			;Need to copy this way, or wie copy from area that is blocked by barrier
			sta (.lz_dst),y

			inc <.lz_src + 0
			beq .lz_src_inc
.lz_src_inc_
			inc <.lz_dst + 0
			beq .lz_dst_inc
.lz_dst_inc_
			dex
			bne .lz_cp_lit

			lda <.lz_len_hi			;more pages to copy?
			bne .lz_l_page			;happens very seldom

			;------------------
			;NEW OR OLD OFFSET
			;------------------
							;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			rol				;A = 0, C = 1 -> A = 1
			shf <.lz_bits
			;rol
			;bne .lz_match
			;else A = 0
			;but only for lowbyte?!
			bcs .lz_match			;either match with new offset or old offset

			;------------------
			;DO MATCH
			;------------------
.lz_repeat
			jsr .lz_length
			sbc #$01
			bcc .lz_dcp			;fix highbyte of length in case and set carry again (a = $ff -> compare delivers carry = 1)
			;sec				;XXX TODO in fact we could save on the sbc #$01 as the sec and adc later on corrects that again, but y would turn out one too less
.lz_match_big						;we enter with length - 1 here from normal match
			eor #$ff
			tay
							;XXX TODO save on eor #$ff and do sbc lz_dst + 0?
			eor #$ff			;restore A
.lz_match_len2						;entry from new_offset handling
			adc <.lz_dst + 0
			sta <.lz_dst + 0
			tax				;remember for later end check, cheaper this way
			bcs .lz_clc			;/!\ branch happens very seldom, if so, clear carry
			dec <.lz_dst + 1		;subtract one more in this case
.lz_clc_back
.if OFFSET_OPT
.lz_offset_lo		sbc #$00			;carry is cleared, subtract (offset + 1) in fact we could use sbx here, but would not respect carry, but a and x are same, but need x later anyway for other purpose
.else
			sbc <.lz_offset
.endif
			sta .lz_msrcr + 0
			lda <.lz_dst + 1
.if OFFSET_OPT
.lz_offset_hi		sbc #$00
.else
			sbc <.lz_offset + 1
.endif
			sta .lz_msrcr + 1
			;				;XXX TODO would have dst + 0 and + 1 in X and A here, of any use?
.lz_cp_match
			;XXX TODO if repeated offset: add literal size to .lz_msrcr and done?
.lz_msrcr = * + 1
			lda $beef,y
			sta (.lz_dst),y
			iny
			bne .lz_cp_match
			inc <.lz_dst + 1

			lda <.lz_len_hi			;check for more loop runs
			bne .lz_m_page			;do more page runs? Yes? Fall through
.lz_check_poll
			cpx <.lz_src + 0		;check for end condition when depacking inplace, .lz_dst + 0 still in X
.lz_skip_poll		bne .lz_start_over		;-> can be changed to .lz_poll, depending on decomp/loadcomp

			lda <.lz_dst + 1
			sbc <.lz_src + 1
			bne .lz_start_over

			;jmp .ld_load_raw		;but should be able to skip fetch, so does not work this way
			;top				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway
							;as last block is forced, we would always wait for last block to be loaded if we enter this loop, no matter how :-)

			;------------------
			;NEXT PAGE IN STREAM
			;------------------
.lz_init
			bne :+
.lz_next_page
			inc <.lz_src + 1
:
.if LOAD_TO_RAM_UNDER_IO
                        jmp getblock
.else
			php
			txa
			pha
			GETBLOCK <.lz_src + 1
			pla
			tax
			plp
			rts
.endif
			;------------------
			;FETCH A NEW OFFSET
			;------------------
:							;lz_length as inline
			shf <.lz_bits			;fetch payload bit
			rol				;can also moved to front and executed once on start
.lz_match
			shf <.lz_bits
			bcc :-

			bne :+
			jsr .lz_refill_bits
:
			sbc #$01			;XXX TODO can be omitted if just endposition is checked, but 0 does not exist as value?
			bcc .lz_eof			;underflow. must have been 0

			lsr
.if OFFSET_OPT
			sta .lz_offset_hi + 1		;hibyte of offset
.else
			sta .lz_offset + 1		;hibyte of offset
.endif
			lda (.lz_src),y			;fetch another byte directly
			ror
.if OFFSET_OPT
			sta .lz_offset_lo + 1
.else
			sta .lz_offset
.endif
			inc <.lz_src + 0		;postponed, so no need to save A on next_page call
			bne :+
			jsr .lz_next_page		;preserves carry, all sane
:
			lda #$01
			ldy #$fe
			bcs .lz_match_len2		;length = 1 ^ $ff, do it the very short way :-)
:
			shf <.lz_bits			;fetch first payload bit
							;XXX TODO we could check bit 7 before further shf?
			rol				;can also moved to front and executed once on start
			shf <.lz_bits
			bcc :-
			bne .lz_match_big
			ldy #$00			;only now y = 0 is needed
			jsr .lz_refill_bits		;fetch remaining bits
;			bcs .lz_match_big
			jmp .lz_match_big

			;------------------
			;SELDOM STUFF
			;------------------
.lz_clc
			clc
			bcc .lz_clc_back
.lz_m_page
			dec <.lz_len_hi
			inc .lz_msrcr + 1		;XXX TODO only needed if more pages follow
			bne .lz_cp_match

			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax
			lda (.lz_src),y
			rot
			sta <.lz_bits
			inc <.lz_src + 0 		;postponed, so no need to save A on next_page call
			bne :+				;XXX TODO if we would prefer beq, 0,2% saving
			jsr .lz_next_page		;preserves carry and A, clears X, Y, all sane
:
			txa
			bcs .lz_lend

			;lda #$00
			;slo <.lz_bits
.lz_get_loop
			shf <.lz_bits			;fetch payload bit
.lz_length_16_
			rol				;can also moved to front and executed once on start
			bcs .lz_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.lz_length
			shf <.lz_bits

			bcc .lz_get_loop
			beq .lz_refill_bits
.lz_lend
.lz_eof
			rts
.lz_length_16						;happens very rarely
			pha				;save LSB
			tya				;was lda #$01, but A = 0 + rol makes this also start with MSB = 1
			jsr .lz_length_16_		;get up to 7 more bits
			sta <.lz_len_hi			;save MSB
			pla				;restore LSB
			rts

.if LOAD_TO_RAM_UNDER_IO
getblock		php
			txa
			pha
			GETBLOCK <.lz_src + 1
			pla
			tax
			plp
			rts
.endif

.endif
