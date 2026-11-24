
decompress = decrunch
decompsrc  = sp

TC_BLOCK_INTERFACE = 1

.if (PLATFORM = diskio::platform::COMMODORE_64) | (PLATFORM = diskio::platform::COMMODORE_128)
USE_UNINTENDED_OPCODES = 1
.else
USE_UNINTENDED_OPCODES = 0
.endif

    ;.export decrunch
.if USE_UNINTENDED_OPCODES
    .define sbx axs
.endif

.if MEM_DECOMP_TO_API
dp=decdestlo
.else
dp=DECOMPVARS + 0 ;4
.endif
sp=DECOMPVARS + 2 ;6  ; sp must follow dp, cf init code
cs=DECOMPVARS + 4 ;8


decrunch:
.if TC_BLOCK_INTERFACE = 0
    stx sp+1
.endif

    ldy#2
init_loop:
.if USE_UNINTENDED_OPCODES
    stx sp-2,y   ;first iter stores sp-low :D
.else
    sta sp-2,y   ;first iter stores sp-low :D
.endif
.if TC_BLOCK_INTERFACE
    ; read three blocks ahead,
    ;  - one because literal strings read up to 128 bytes past sp
    ;  - two more to absorb up to 256 blocks worth of read 254 bytes/use 256 bytes
    tya
    pha
    jsr tc_getblock
    pla
    tay
.endif
.if USE_UNINTENDED_OPCODES
    lax(sp),y
.else
    lda(sp),y
.endif
    dey
    bpl init_loop
    pha

.if MEM_DECOMP_TO_API
storedadrl = * + 1
storedadrh = * + 1
    lda #0
    cmp #OPC_STA_ZP
    beq :+
    lda dp; override destination address
    bne *+4
    dec dp+1
    dec dp
    jmp :++
:   lda sp-2; destination address as stored in header
    sta dp
    lda sp-1
    sta dp+1
:
.endif

.if LOADCOMPD_TO
    clc
    lda loadaddroffslo
    adc dp
    sta dp
    lda loadaddroffshi
    adc dp+1
    sta dp+1
.endif

    lda#$02
    bne update_sp

literal_run:
literal_loop:
    iny
    lda(sp),y
    sta(dp),y
    dex
    bmi literal_loop

    tya
    pha
    clc
increase_dp_by_a_and_sp_by_tos_plus_one:
    adc dp
    sta dp
    bcc :+
    inc dp+1
:
    pla
update_sp:
    sec
    adc sp
    sta sp
    bcc :+
    inc sp+1
.if TC_BLOCK_INTERFACE
    jsr tc_getblock
.endif
:
next_command:
    POLLBLOCK

    ldy#0
.if USE_UNINTENDED_OPCODES
    lax(sp),y
.else
    lda(sp),y
    tax
.endif
    beq decrunch_done
    ; literal:   x = 128+length-1
    ; near copy: a = %11xxxxxx
    ; far copy:  a|0xf8 = >(~(offset-1)), x = 8*(length-2) | (some low bits)
    asl
    bcc far_copy
    bpl literal_run

near_copy:
    ldx#$07   ; clear high byte of -ve offset. Also ensures copy_loop doesn't loop.
    .byt $f0  ; beq (not taken) to skip over the iny
far_copy:
    iny
    ; carry is set for near_copy, clear for far_copy

    lda(sp),y ;fetch second byte (or for near copy, refetch first).  This is low 8 bits of offset.
    adc dp
    sta cs
    txa
    ora#$f8
    adc dp+1
    sta cs+1
.if USE_UNINTENDED_OPCODES = 0
    txa
    lsr
    lsr
    lsr
    tax
.endif
    tya
    pha  ; save opcode length to stack
    ldy#1
    lda(cs),y
    sta(dp),y

copy_loop:
    iny
    lda(cs),y
    sta(dp),y
.if USE_UNINTENDED_OPCODES
    txa   ; spend an extra 2 cycles per byte here to save 10 in the bitfield extraction. A win on average
    sbx#8
    bpl copy_loop
.else
    dex
    bpl copy_loop
    clc
.endif
    tya
    bcc increase_dp_by_a_and_sp_by_tos_plus_one ; always taken.

decrunch_done:
    pla
    iny
    sta(dp),y
    rts

.if TC_BLOCK_INTERFACE
tc_getblock:
    GETBLOCK sp+1
    rts
.endif

edecrunch:
