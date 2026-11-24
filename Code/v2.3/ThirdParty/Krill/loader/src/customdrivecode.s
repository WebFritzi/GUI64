
__NO_CUSTOMDRIVECODE_SYMBOLS_IMPORT = 1
.include "loader.inc"

.include "drives/drivecode-common.inc"
.include "hal/hal.inc"

.include "cpu.inc"
.scope drive
.include "via.inc"
.endscope
.include "cia.inc"

.segment "DISKIO_PLUGIN_ZP" : zeropage

CUSTOMPARAM: .res 2
DRVCODEOFS:  .res 1
DRVCODEPTR:  .res 2
BUFFERPTR:   .res 2
.if ONLY_1541_AND_COMPATIBLE = 0
DRIVETYPE:   .res 1
.endif; ONLY_1541_AND_COMPATIBLE

UPLOADENTR:  .res 2
UPLOADTO:    .res 2
UPLOADCNT:   .res 2
UPLOADPTR:   .res 2

plugin_zp_last = * - 1
.export plugin_zp_last

.segment "DISKIO_PLUGIN"

.ifdef TRNSADDR
            .org TRNSADDR - 2
            .word * + 2; load address
.endif

.export swapdrvcod

swapdrvcod: stx CUSTOMPARAM
            sty CUSTOMPARAM + 1

            stx DRVCODEPTR
            sty DRVCODEPTR + 1

.if ONLY_1541_AND_COMPATIBLE = 0

            lda #$80
            sta DRIVETYPE
            BRANCH_IF_INSTALLED :+
            lda #diskio::status::DEVICE_INCOMPATIBLE
            sec
            rts
:
.endif; !ONLY_1541_AND_COMPATIBLE

            ldy #swapparams::buffer
            lda (CUSTOMPARAM),y
            sta BUFFERPTR
            sta bufmain41
.if ONLY_1541_AND_COMPATIBLE = 0
            sta bufmain71
            sta bufmain81
.endif; !ONLY_1541_AND_COMPATIBLE
            iny
            lda (CUSTOMPARAM),y
            sta BUFFERPTR + 1
            sta bufmain41 + 1
.if ONLY_1541_AND_COMPATIBLE = 0
            sta bufmain71 + 1
            sta bufmain81 + 1
.endif; !ONLY_1541_AND_COMPATIBLE

            CLEAR

:           SET_FLAGS_N_DATA_V_CLK
            bvc :-

            ldy #$ff
:           iny
            lda trampoline,y
            jsr sendbyte
            bne :-

            ldy #swapparams::drivecode41
            jsr prepmove
            ldx #drivecode::from - 1
:           lda UPLOADENTR,x
            sta trampoln41 + payldprm41 - trmplrun41,x
            dex
            bpl :-

.if ONLY_1541_AND_COMPATIBLE = 0
            lda #diskio::drivetype::DRIVES_1541

            SET_FLAGS_N_DATA_V_CLK
            bvc is1581
            bmi setdrvtype

            IDLE

            ldy #swapparams::drivecode71
            jsr prepmove
            ldx #drivecode::from - 1
:           lda UPLOADENTR,x
            sta trampoln71 + payldprm71 - trmplrun71,x
            dex
            bpl :-

            lda #diskio::drivetype::DRIVES_157X
            bne setdrvtype; jmp

is1581:     IDLE

            ldy #swapparams::drivecode81
            jsr prepmove
            ldx #drivecode::from - 1
:           lda UPLOADENTR,x
            sta custom81 + payldprm81 - trampoln81,x
            dex
            bpl :-

            lda #diskio::drivetype::DRIVES_1581_CMD

setdrvtype: sta DRIVETYPE
.endif; !ONLY_1541_AND_COMPATIBLE

            jsr selectdcod

            pha
            ldy #drivecode::entry
            jsr prepmove
            pla

.if ONLY_1541_AND_COMPATIBLE = 0

            cmp #swapparams::drivecode81
            bne doupload

            ldy #drivecode::from
:           lda UPLOADENTR - 1,y
            jsr sendbyte
            dey
            bne :-

.endif; !ONLY_1541_AND_COMPATIBLE

doupload:   jsr uploadcode

            ldy #ldrmain41 - drvcode41 - drivecode::length
            jsr prepmove

            ; buffer loader drive code
            CLEAR

:           SET_FLAGS_N_DATA_V_CLK
            bvc :-

            ldy #0
getdrvcode: jsr receivbyte
            sta (BUFFERPTR),y
            iny
            bne :+
            inc BUFFERPTR + 1
:           inc UPLOADCNT
            bne getdrvcode
            inc UPLOADCNT + 1
            bmi getdrvcode

:           SET_FLAGS_N_DATA_V_CLK
            bpl :-

            ; upload custom drive code
            lda CUSTOMPARAM
            sta DRVCODEPTR
            lda CUSTOMPARAM + 1
            sta DRVCODEPTR + 1
            ldy DRVCODEOFS
            jsr uplodpayld

            CLEAR

            OK_CLC
            rts

.export restoreldr

restoreldr:
.if ONLY_1541_AND_COMPATIBLE = 0

            lda DRIVETYPE
            bpl :+
            lda #diskio::status::DEVICE_INCOMPATIBLE
            sec
            rts
:
.endif; !ONLY_1541_AND_COMPATIBLE

            INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT
            CLEAR

waitrestor: SET_FLAGS_N_DATA_V_CLK
            bpl waitrestor
            bvc waitrestor

.if ONLY_1541_AND_COMPATIBLE = 0

            lda DRIVETYPE

.endif; !ONLY_1541_AND_COMPATIBLE

            jsr selectdcod

            ldy #ldrmain41 - drvcode41 - drivecode::length
            jsr uplodpayld

            INSTALL_IDLE
            rts

.export sendbyte

sendbyte:   pha
            nop; delay
            SENDBYTE
            pla
            rts

.export receivbyte

receivbyte: RECEIVEBYTE
            rts

selectdcod:
.if ONLY_1541_AND_COMPATIBLE = 0

            cmp #diskio::drivetype::DRIVES_157X
            bne check1581
            lda #swapparams::drivecode71
            ldx #.lobyte(drvcode71)
            ldy #.hibyte(drvcode71)
            bne dcodselctd; jmp

check1581:  cmp #diskio::drivetype::DRIVES_1581_CMD
            bne upload1541
            lda #swapparams::drivecode81
            ldx #.lobyte(drvcode81)
            ldy #.hibyte(drvcode81)
            bne dcodselctd; jmp
upload1541:

.endif; ONLY_1541_AND_COMPATIBLE

            lda #swapparams::drivecode41
            ldx #.lobyte(drvcode41)
            ldy #.hibyte(drvcode41)

dcodselctd: sta DRVCODEOFS
            stx DRVCODEPTR
            sty DRVCODEPTR + 1
            rts

uplodpayld: jsr prepmove

uploadcode: ldy #0
:           lda (UPLOADPTR),y
            jsr sendbyte
            iny
            bne :+
            inc UPLOADPTR + 1
:           inc UPLOADCNT
            bne :--
            inc UPLOADCNT + 1
            bmi :--
            rts

prepmove:   ldx #0
:           lda (DRVCODEPTR),y
            iny
            sta UPLOADENTR,x
            inx
            cpx #8
            bne :-

           ;sec
            lda #0
            sbc UPLOADCNT
            sta UPLOADCNT
            lda #0
            sbc UPLOADCNT + 1
            sta UPLOADCNT + 1
            rts

trampoline:
            .scope upload41

.include "via.inc"

            .org $ef; FILENAME41

recvtrmpln: DRIVEGETBYTE 1541, trampolinecmp, nozeroes
            sta $0105,x; BLOCKBUFFER41 + 5
            inx
            cpx #custm41end - trampoln41 + 1
            bne recvtrmpln
            .byte $ff
            .word (entry - 1)
entry:      lda getbytecmp41 + 1
            sta .lobyte(trampolinecmp + 1)
            ldx #1
            ldy VIA1_PRB + 16
            bcs recvtrmpln; jmp

            .reloc

           ;.byte 0; end marker is 0 at drvcode41

            .endscope

drvcode41:  .word 0; entry, not used here
            .word trmplrun41; to
            .word custm41end - trampoln41; length
            .word trampoln41; from

ldrmain41:  .word $0700 + RETURNSIZE41; buffer length
bufmain41:  .word 0; buffer

trampoln41:
            .org $0106; = BLOCKBUFFER41 + 6

RESTORELOOP41 = $d9; with SENDVAL41
RESTOREMAIN41 = RESTORELOOP41 + 1

trmplrun41: .scope custom41

.include "drives/drivecode-common.inc"

.include "via.inc"

            lda #ATNA_OUT | CLK_OUT | DATA_OUT
            sta VIA1_PRB

            ; buffer end of read loop to make space for the receive routine
            ldx #21 - 1
:           lda RESTORELOOP41,x
            sta SECTORLINKTABLE41,x
            dex
            bpl :-

            lda getbytecmp41 + 1
            sta customloop + (getbytecmp41 + 1) - getbyte41

            ; put receive routine for buffered loader code to zeropage
            ldx #$0100 - (custmstore - customloop - 3)
:           lda custmstore - $0100,x
            sta .lobyte(restorloop - restrloope),x
            inx
            bne :-
            ldx #$0100 - (restrloope - restorloop)
:           lda restrloope - $0100,x
            sta $00,x
            inx
            bne :-

            lda #ATNA_OUT
            sta VIA1_PRB
            lda #CLK_IN | DATA_IN
:           bit VIA1_PRB
            bne :-

SENDVAL41 = RESTORELOOP41

sendzp:     lda $00,x
            DRIVESENDBYTE 1541, SENDVAL41
            inx
            bne sendzp

            ldx #.lobyte(returnrun)
sendmain:   lda returnrun & $ff00,x
            DRIVESENDBYTE 1541, SENDVAL41
            inx
            bne sendmain
            inc sendmain + 2
            dec numpages41
            bne sendmain

            lda #ATNA_OUT | CLK_OUT | DATA_OUT
            sta (V1B41,x); VIA1_PRB

            lda #OPC_RTS
            sta getbyterts41

            ldx #RESTOREMAIN41
:           lda .lobyte($0100 - RESTOREMAIN41),x
            ldy payldpre41 - $0100,x
            sty .lobyte($0100 - RESTOREMAIN41),x
            sta $00,x; buffer low end of zeropage at high end of zeropage
            inx
            bne :-

            lda #ATNA_OUT
            sta ($03,x); VIA1_PRB

            ldy VIA1_PRB
            .byte OPC_JMP_ABS, 0; jmp $0000

numpages41: .byte 7

CUSTOMPARAM41 = $fa

restorloop: sta (.lobyte(CUSTOMPARAM41 + 2),x); x = 0
            inc .lobyte(CUSTOMPARAM41 + 2)
            bne :+
            inc .lobyte(CUSTOMPARAM41 + 3)
:           inc .lobyte(CUSTOMPARAM41 + 4)
            bne restorloop + 3 - (custmstore - customloop)
            inc .lobyte(CUSTOMPARAM41 + 5)
            bne restorloop + 3 - (custmstore - customloop)
            .byte OPC_JMP_ABS
            .word $0200 - RETURNSIZE41; entry
            .word $0200 - RETURNSIZE41; to
            .word $10000 - ($0600 + RETURNSIZE41); length
restrloope:

UPLOADOFFS = custmstore - (customloop + 3)

            ; executed at $0000
customloop: DRIVEGETBYTE 1541, getbyte
custmstore: sta (.lobyte(UPLOADOFFS + 19),x); x = 0
            inc .lobyte(UPLOADOFFS + 19)
            bne :+
            inc .lobyte(UPLOADOFFS + 20)
:           inc .lobyte(UPLOADOFFS + 21)
            bne customloop + 3
            inc .lobyte(UPLOADOFFS + 22)
            bne customloop + 3
            .byte OPC_JMP_ABS

            .endscope

payldprm41: .word 0; entry
            .word 0; to
            .word 0; -length
payldpre41:

return:
            .org $01e0

returnrun:  ldx #<topofstack41
            lda #$18 ; ATNA_OUT | CLK_OUT
            sta (<(V1B41 - topofstack41),x); $1800
            txs
            ldx #21 - 1
:           lda SECTORLINKTABLE41,x
            sta RESTORELOOP41,x
            dex
            bpl :-
            lda (<(V2B41 - $ff),x); $1c00
            and #$08 ; BUSY_LED
            beq :+
            lda #$ff
:           sta LEDSTATE41
            jsr initlink41
            jmp idleloop41

            .assert * = $0200, error, "***** 1541 return code at wrong position. *****"

            .org * - (returnrun - return)
returnend:

            .assert * = $0200, error, "***** 1541 custom swap code at wrong position. *****"

RETURNSIZE41 = returnend - return

            .reloc
custm41end:

.if ONLY_1541_AND_COMPATIBLE = 0

drvcode71:  .word 0; entry, not used here
            .word trmplrun71; to
            .word custom71end - trampoln71; length
            .word trampoln71; from

            .word $0700 + RETURNSIZE71; buffer size
bufmain71:  .word 0; buffer

trampoln71:
            .org $0700; = BLOCKBUFFER71

RETURNSIZE71 = 34

trmplrun71: .scope custom71

.include "via.inc"

            lda #$18 ; ATNA_OUT | CLK_OUT
            sta $1800; VIA1_PRB
            ldx #$ff
            lda $1c00; VIA2_PRB
            and #$08 ; BUSY_LED
            beq :+
            txa
:           sta LEDSTATE71
            stx <CLEARSECTORLINKTABLE71
            ldx #<topofstack71
            txs
            jsr initlink71
:           lda $1800; VIA1_PRB
            lsr
            bcc :-
            jmp idleloop71

            sei
            lda #ATNA_OUT | CLK_OUT | DATA_OUT
            sta VIA1_PRB

            ; move receive routine for buffered loader code
            ldx #0
:           lda restorloop,x
            sta FILENAME71,x
            inx
            cpx #restrloope - restorloop
            bne :-

            lda #ATNA_OUT
            sta VIA1_PRB
            lda #$05; CLK_IN | DATA_IN
:           and VIA1_PRB
            bne :-

SENDVAL71 = SECTORLINKTABLE71

            ; send loader code at $0000-$0700 plus return-to-loader code
            ldx #256 - RETURNSIZE71
sendcode71: lda $ff00 + RETURNSIZE71,x
            DRIVESENDBYTE 1571, SENDVAL71
            inx
            bne :+
            inc sendcode71 + 2
:           inc count
            bne sendcode71
            inc count + 1
            bmi sendcode71

            lda #ATNA_OUT | CLK_OUT | DATA_OUT
            sta VIA1_PRB

CUSTOMZPBUFFSIZE71 = payldpre71 - (customloop + 3)
.export CUSTOMZPBUFFSIZE71
            ldx #CUSTOMZPBUFFSIZE71
:           lda customloop + 3 - 1,x
            ldy .lobyte($00 - 1),x
            sty .lobyte(CUSTOMZPBUFFER71 - 1),x
            sta .lobyte($00 - 1),x
            dex
            bne :-

            lda #ATNA_OUT
            sta VIA1_PRB

            ldy VIA1_PRB
            .byte OPC_JMP_ABS

count:      .word $10000 - ($0700 + RETURNSIZE71)

CUSTOMPARAM71 = $5a

restorloop: DRIVEGETBYTE 1571, getbyterestore
            sta (.lobyte(CUSTOMPARAM71 + 2),x); x = 0
            inc .lobyte(CUSTOMPARAM71 + 2)
            bne :+
            inc .lobyte(CUSTOMPARAM71 + 3)
:           inc .lobyte(CUSTOMPARAM71 + 4)
            bne restorloop + 3
            inc .lobyte(CUSTOMPARAM71 + 5)
            bne restorloop + 3
            .byte OPC_JMP_ABS
            .word $0700; entry
            .word $0100; to
            .word $10000 - ($0600 + RETURNSIZE71); length
restrloope:

UPLOADOFFS = getbytrt71 - (customloop + 3)

            ; executed at $0000
customloop: DRIVEGETBYTE 1571, getbytecmp
getbytrt71: sta (.lobyte(UPLOADOFFS + 19),x); x = 0
            inc .lobyte(UPLOADOFFS + 19)
            bne :+
            inc .lobyte(UPLOADOFFS + 20)
:           inc .lobyte(UPLOADOFFS + 21)
            bne customloop + 3
            inc .lobyte(UPLOADOFFS + 22)
            bne customloop + 3
            .byte OPC_JMP_ABS

            .endscope

payldprm71: .word 0; entry
            .word 0; to
            .word 0; -length
payldpre71:

            .assert (* - trmplrun71) <= CUSTOMUPLOADSIZE71, error, "***** 1571 custom upload code too large. *****"
            .assert (* - trmplrun71) >= CUSTOMUPLOADSIZE71, error, "***** 1571 custom upload code too small. *****"

            .reloc
custom71end:

drvcode81:  .word trampoln81; entry
            .word trampoln81; to
            .word custom81end - custom81; length
            .word custom81; from

            .word DRVCODEND81 - $0300; buffer size
bufmain81:  .word 0; buffer

custom81:
            .org $0b00; = BLOCKBUFFER81

trampoln81: lda #payldpre81 - payldprm81 - 1; = $05 = CLK_IN | DATA_IN
            tax
:           ldy payldprm81,x
            sty CUSTOMPARAM81,x
            dex
            bpl :-

           ;lda #CLK_IN | DATA_IN
:           bit CIA_PRB
            bne :-

SENDVAL81 = CUSTOMPARAM81 + 6

            ; send loader code at $0300
            ldx #0
sendcode81: lda a:$0300 & $ff00,x
            DRIVESENDBYTE 1581, SENDVAL81
            inx
            bne :+
            inc sendcode81 + 2
:           cpx #.lobyte(DRVCODEND81)
            bne sendcode81
            lda sendcode81 + 2
            cmp #.hibyte(DRVCODEND81)
            bne sendcode81

            ldx #0
            stx CIA_PRB

            jmp CUSTOMRECEIVE81

payldprm81: .word 0; entry
            .word 0; to
            .word 0; -length
payldpre81:
            .reloc
custom81end:

.endif; !ONLY_1541_AND_COMPATIBLE
