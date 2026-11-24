
.include "cpu.inc"
.include "cia.inc"
.include "via.inc"

.include "drives/drivecode-common.inc"

ZP_FIRST              = $00

BUFFER                = $00
SYS_SP                = $01
JOBCODESTABLE         = $02; fixed in ROM
JOBTRKSCTTABLE        = $0b; fixed in ROM - $0b..$1c
FILETRACK             = $0b
FILESECTOR            = $0c
FILENAMEHASHLO        = $0d
FILENAMEHASHHI        = $0e
NUMFILES              = $0f
CURRDIRBLOCKTRACK     = $10
CYCLESTARTENDTRACK    = $11
CYCLESTARTENDSECTOR   = $12
NEXTDIRBLOCKTRACK     = $13
NEXTDIRBLOCKSECTOR    = $14
FIRSTDIRSECTOR        = $15
NEXTCONTIGUOUSBLOCK   = $16
TEMP                  = $17

ZP_LAST               = $17

;BLOCKBUFFERJOBTRACK  = $1b; fixed in ROM - track for job at buffer 8 ($0b00)
;BLOCKBUFFERJOBSECTOR = $1c; fixed in ROM - sector for job at buffer 8 ($0b00)

DISKCHANGED           = $25; fixed in ROM
DRIVESTATE            = $26; fixed in ROM
DRIVEOFF              = $00; literal
OPEN_FILE_TRACK       = $4c; fixed in ROM

LEDSTATE              = $79; fixed in ROM

IRQVECTOR_LO          = $0192
IRQVECTOR_HI          = $0193
HDRS2                 = $01bc
ROMOS_DIRTRACK81      = $022b
OPEN_FILE_SECTOR      = $028b

STROBE_CONTROLLER     = $ff54

OK_DV                 = $00
READ_DV               = $80
MOTOFFI_DV            = $8a
SEEK_DV               = $8c

RESET_TIMERB          = $cb9f
CONTROLLERIRQPERIODFD = $4e20

BUFFER0               = $0300
BUFFERSIZE            = $0100

SENDTABLELO           = $0900
SENDTABLEHI           = $0a00

BLOCKBUFFER81         = $0b00
TRACKOFFSET           = 0
SECTOROFFSET          = 1

REQUESTEDTRACK        = JOBTRKSCTTABLE + (2 * BUFFERINDEX) + TRACKOFFSET
REQUESTEDSECTOR       = JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET
LOADEDSECTOR          = JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET

LINKTRACK             = BLOCKBUFFER81 + TRACKOFFSET
LINKSECTOR            = BLOCKBUFFER81 + SECTOROFFSET

BINARY_NIBBLE_MASK    = %00001111

ROMOS_MAXTRACK        = $8f; MAXTRACK81 - 1
ROMOS_MAXSECTOR       = $75; MAXSECTOR81 + 1
MAXTRACK81            = 80; literal
MAXSECTOR81           = 39; literal

BUFFERINDEX           = (BLOCKBUFFER81 - BUFFER0) / BUFFERSIZE

            .org $0300

.export cmdfdfix0 : absolute
.export cmdfdfix1 : absolute
.export cmdfdfix2 : absolute
.if (::PLATFORM <> diskio::platform::COMMODORE_128) & (!::DISABLE_WATCHDOG)
.export cmdfdfix3 : absolute
.export cmdfdfix4 : absolute
.endif

drvcodebeg81: .byte .hibyte(drvcodeend81 - * + $0100 - $01); init transfer count hi-byte

sysirqvbuf: .word 0

syszpbuf:   .res ZP_LAST - ZP_FIRST + 1

filename81: .res 16

            .res 24; overflow area for 1541 custom code upload trampoline

swapzp81:   ldx #ZP_LAST - ZP_FIRST
:           lda ZP_FIRST,x
            ldy syszpbuf,x
            sty ZP_FIRST,x
            sta syszpbuf,x
            dex
            bpl :-
            rts

            FNAMEHASH 1581

getdirtrk:
cmdfdfix1 = * + 1
cmdfdfix2 = * + 2
            lda ROMOS_DIRTRACK81
.if DIRTRACK81 = 40
            rts
.endif
            cmp #40
            bne :+
            lda #DIRTRACK81
:
.if DIRTRACK81 <> 40
            rts
.endif

trackseek:  sta REQUESTEDTRACK
            sty REQUESTEDSECTOR
            tax
            dex
            stx HDRS2 + (2 * BUFFERINDEX)
            jsr initcntr81
            lda #SEEK_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER

            lda DISKCHANGED
            ora diskchangd + 1
            sta diskchangd + 1

            ; fall through

initwdog81: ; the i-flag is set here
.if ::DISABLE_WATCHDOG
            rts
.endif
            lda #.lobyte(uninstall)
            sta IRQVECTOR_LO
            lda #.hibyte(uninstall)
            sta IRQVECTOR_HI
            lda cmdfdfix2; 0 for FD
            beq :+
            lda #$ff
            sta CIA_TB_LO
            sta CIA_TB_HI
:
.if !::DISABLE_WATCHDOG
            rts
.endif

enablwdg81: lda cmdfdfix2; 0 for FD
            beq :+
            ldy #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sty CIA_CRB
            bit CIA_ICR
            rts
:           lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS
            sta VIA_IER; no IRQs from VIA
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA_IER; timer 1 IRQs from VIA
            ldy #$ff
            sty VIA_T1C_H
            rts

getblock81: sta REQUESTEDTRACK
            sty REQUESTEDSECTOR
getblockag: jsr initcntr81
            lda #READ_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER

            lda DISKCHANGED
            ora diskchangd + 1
            sta diskchangd + 1

            jsr initwdog81

            lda JOBCODESTABLE + BUFFERINDEX; FD does not return the error status in the accu
            cmp #OK_DV + 1

            ; the link track is returned last so that the z-flag
            ; is set if this block is the file's last one (see FIND_FILE)
            ldy LINKSECTOR
            ldx LOADEDSECTOR
            lda LINKTRACK
            rts

initcntr81: lda sysirqvbuf
            sta IRQVECTOR_LO
            lda sysirqvbuf + 1
            sta IRQVECTOR_HI
            lda cmdfdfix2; 0 for FD
            beq :+
            jmp RESET_TIMERB
:           lda #.lobyte(CONTROLLERIRQPERIODFD)
            sta VIA_T1C_L
            lda #.hibyte(CONTROLLERIRQPERIODFD)
            sta VIA_T1C_H
            rts

fadeled:    txa
            tay
            beq motrledoff
:           nop
            bit OPC_BIT_ZP
            dey
            bne :-
            pha
            jsr busyledoff
            pla
            tay
:           nop
            bit OPC_BIT_ZP
            iny
            bne :-
            dex
bsyledon81: lda #DRIVE_LED
            ora CIA_PRA
            ldy #$ff
            bne store_cia; jmp

motrledoff: lda DRIVESTATE
            ora LEDSTATE
            beq :+

            txa
            pha

            ; fill track cache
            lda REQUESTEDTRACK
            ldy REQUESTEDSECTOR
            jsr getblock81

            ; turn off motor
            jsr initcntr81
            lda #MOTOFFI_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER
            lda #DRIVEOFF; $00
            sta DRIVESTATE

            pla
            tax

busyledoff: lda CIA_PRA
            and #.lobyte(~DRIVE_LED); turn off drive led
            ldy #$00
store_cia:  sta CIA_PRA
            sty LEDSTATE
:           rts

uninstall:  jsr getdirtrk
            jsr trackseek

            ldx LEDSTATE
:           jsr fadeled
            lda LEDSTATE
            bne :-

            ldx SYS_SP
            txs
            ldx #ZP_LAST - ZP_FIRST
:           lda syszpbuf,x
            sta ZP_FIRST,x
            dex
            bpl :-
            jsr initcntr81

            lda #1
            sta DISKCHANGED; re-read BAM etc.

            lda #CIA_SET_INTF | FLAG1_IRQ | SERIAL_IRQ | TIMERB_IRQ
            sta CIA_ICR
            bit CIA_ICR
            cli
            rts

idleloop:   ldx #$00; turn off motor and busy led
            lda #DRIVE_LED; check if busy led is lit
            and CIA_PRA
            beq driveidle
            dex; fade off the busy led, then turn off motor
driveidle:  jsr fadeled; fade off the busy led
            lda CIA_PRB
            and #ATN_IN | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            eor #ATN_IN | CLK_OUT | CLK_IN            | DATA_IN
            beq driveidle; wait until there is something to do
            lda CIA_PRB; to be safe, read a second time
            and #ATN_IN | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            eor #ATN_IN | CLK_OUT | CLK_IN            | DATA_IN
            lsr; check for reset, uninstallation or custom drive code upload
            beq :+
            jmp uninstall; check for reset or uninstallation

:           jsr enablwdg81; enable watchdog, the computer might be reset while sending over a
                          ; byte, leaving the drive waiting for handshake pulses
            ENABLE_WATCHDOG

            filename = filename81
            GET_FILENAME 1581

            ; matches against hash of filename in FILENAMEHASHLO/HI
            BLOCKBUFFER = BLOCKBUFFER81
            findfile81 = findfile
            FIND_FILE 1581
loadfile81:
            bcs filenfound

            lda FILENAMEHASHVALSLO + 1,x ; for load-next
            sta FILENAMEHASHLO           ; functionality,
            lda FILENAMEHASHVALSHI + 1,x ; store hash of
            sta FILENAMEHASHHI           ; next file's name
            lda DIRTRACKS,x
            tax

            ; check for illegal track or sector
            beq toillegal
            cpx ROMOS_MAXTRACK; #MAXTRACK81 - 1
            beq :+
            bcs toillegal + 1
:           cpy ROMOS_MAXSECTOR; #MAXSECTOR81 + 1
            bcc :+
toillegal:  sec
cmdfdfix0:  jmp illegalts; is changed to bit illegalts on FD2000/4000 to disable illegal track or sector error,
                         ; ROM variables for logical track/sector boundaries aren't known (probably around MAXTRACKFD = $54)

:           tya
            pha
            jsr bsyledon81
            pla
            tay; FILESECTOR
            txa; FILETRACK

            ldx #1
            stx NEXTCONTIGUOUSBLOCK

loadblock:  sta REQUESTEDTRACK
            sty REQUESTEDSECTOR
:           jsr getblockag
            bcs :-
           ;ldy LINKSECTOR
           ;lda LINKTRACK
            pha
            beq :+
            ldy #$ff
:           lda #1
            ldx #0
            cpx LINKTRACK
            rol; last block: set lsb, clear lsb otherwise
            tax
            lsr
            lda LINKSECTOR; the file's last block's length (last byte index)
            pha
            sty blocksize + 1
            dey
            tya
            eor #$ff
            bcs :+
            lda NEXTCONTIGUOUSBLOCK
:           stx BLOCKBUFFER81 + 1; block index and last block size/next contiguous block index flag
            ; accu: next contiguous block index/block size
            jsr sendblock; send the block over
            inc NEXTCONTIGUOUSBLOCK
            pla; LINKSECTOR
            tay
            pla; LINKTRACK
            bcs fileclosed
            bne loadblock

            ; loading is finished

            clc; all ok after loading

filenfound: ; branches here with carry set
illegalts:  ; or illegal t or s

            jsr sendstatus
            bcs fileclosed

:           lda CIA_PRB
            and #ATN_IN | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            cmp #ATN_IN | CLK_OUT | CLK_IN            | DATA_IN
            bne :-; wait until host is in idle mode

            lda BLOCKBUFFER81; offset 0: status byte
            bmi :+; only after successful load

            jmp prpnxtfile; jumps to prpdnxtfil
prpdnxtfil: ; always succeeds, trackseek to track in y is valid
trseekidle: jsr trackseek
:           jmp idleloop

fileclosed: inc BLOCKBUFFER81
            beq :+; branch if file not found: DATA_OUT remains set, clear it otherwise
            lda #0
            sta CIA_PRB
:           bit CIA_PRB; wait for ATN_IN set,
            bpl :-     ; acknowledgement of the block transfer
            lda #CLK_OUT; drive busy
            sta CIA_PRB
            jmp idleloop

inittimers: lda cmdfdfix2; 0 for FD
            beq :+
.if ::PLATFORM = diskio::platform::COMMODORE_128
            lda #5; set burst timer, anything below 5 yields transfer errors
            sta CIA_TA_LO
            lda #0
            sta CIA_TA_HI
            lda #IOMODE_OUTPUT | COUNT_PHI2 | FORCE_LOAD | CONTINUOUS | TIMER_START; enable burst mode
            sta CIA_CRA
.endif; ::PLATFORM = diskio::platform::COMMODORE_128
.if !::DISABLE_WATCHDOG
            ; watchdog initialisation
            jsr initwdog81
            lda #CIA_CLR_INTF | EVERY_IRQ
            sta CIA_ICR
            lda #CIA_SET_INTF | TIMERB_IRQ
            sta CIA_ICR
            bne :++; jmp
:           jsr initwdog81
.endif
:           rts

            ; get custom drive code
getcustom:  lda #CLK_OUT; 1581
            sta CIA_PRB
            lda #CLK_IN
:           bit CIA_PRB; wait for CLK_IN set
            beq :-

            ldx SYS_SP
            txs
            jsr swapzp81; restore system zeropage values
            jsr initcntr81
            lda #CIA_SET_INTF | FLAG1_IRQ | SERIAL_IRQ | TIMERB_IRQ
            sta CIA_ICR
            bit CIA_ICR

customparm = TEMP
uploadrout = $2000 - customend + getbyte; as high up in RAM as possible
CUSTOMPARAM81 = customparm
CUSTOMRECEIVE81 = uploadrout

            sei
            ldx #customend - getbyte
:           lda getbyte - 1,x
            sta uploadrout - 1,x
            dex
            bne :-
            lda #OPC_STA_ZPXI
            sta uploadrout + getbyterts - getbyte

           ;ldx #0
            stx CIA_PRB; clear DATA_OUT

            ldx #6
:           jsr getbyte
            sta customparm - 1,x
            dex
            bne :-
            jmp uploadrout

            ; must not clobber x
getbyte:    DRIVEGETBYTE 1581
getbyterts: rts; is changed to sta (zp,x) for custom drive code upload
            .byte customparm + 2
            inc customparm + 2
            bne :+
            inc customparm + 3
:           inc customparm + 4
            bne getbyte
            inc customparm + 5
            bmi getbyte
            jmp (customparm); execute custom drive code
customend:

            ; carry: clear = ok, set = load error
sendstatus: lda #0
            sta blocksize + 1

            lda #diskio::status::FILE_NOT_FOUND
            bcs sendblock
            lda #diskio::status::OK
sendblock:  sta BLOCKBUFFER81; next contiguous block index/block size

            jsr enablwdg81

            lda #CLK_OUT | DATA_OUT; set DATA_OUT as well so there is a flank only on CLK when signalling ready,
            sta CIA_PRB; such that just one serial bus read can safely determine block ready (CLK clear) and drive present (DATA set) vs device not present (both clear)
            lda #DATA_OUT; clear CLK_OUT, set DATA_OUT as signal of presence
            sta CIA_PRB; block ready signal

            ldx #$20; here, the watchdog timer is polled manually because
                    ; an extra-long time-out period is needed since the computer may
                    ; still be busy decompressing a large chunk of data,
                    ; this is the round counter: $20 * ($ff00 - $0100) = 2,080,768 cycles at 2 MHz is roughly 1 second
            lda cmdfdfix2; 0 for FD
            bne waitready

waitrdyfd:  lda VIA_T1C_H; see if the watchdog barked
            bne :+
            dex          ; if yes, decrease the round counter
.if ::DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
           ;ldy #$ff
            sty VIA_T1C_H; reset watchdog time-out
.endif
:           lda #CLK_IN
            bit CIA_PRB
            bmi waitrdyfd; wait for ATN_IN clear
.if !::DISABLE_WATCHDOG
            sty VIA_T1C_H; reset watchdog time-out
.endif
            jmp :++

closefile:  sec
            rts

waitready:  lda CIA_TB_HI; see if the watchdog barked
            bne :+
            dex          ; if yes, decrease the round counter
.if ::DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
           ;ldy #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sty CIA_CRB; reset watchdog time-out
.endif
:           lda #CLK_IN
            bit CIA_PRB
            bmi waitready; wait for ATN_IN clear
.if !::DISABLE_WATCHDOG
            sty CIA_CRB; reset watchdog time-out
.endif
:           bne closefile
timeout:    ENABLE_WATCHDOG

            ldy #$00

.if ::PLATFORM = diskio::platform::COMMODORE_128

            bit CIA_ICR
            lda cmdfdfix2; 0 for FD
            beq fdsendblk
            ldx #FSM_BUS_DRIVER_OUTPUT | CLK_OUT
            stx CIA_PRB

    .if ::USE_ASYNCHRONOUS_BURST_HANDSHAKE

            lda BLOCKBUFFER81,y
sendloop:
blocksize:  cpy #$00
            iny
            sta CIA_SDR; clock out data byte
            lda #SERIAL_IRQ
            ldx #FSM_BUS_DRIVER_OUTPUT; clear CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !::DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB; reset watchdog time-out
        .else
            nop; need some slack, as CIA sets the flag a little too early
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            bcs bcssendone

            cpy blocksize + 1
            iny
            sta CIA_SDR; clock out data byte
            lda #SERIAL_IRQ
            ldx #FSM_BUS_DRIVER_OUTPUT | CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !::DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB; reset watchdog time-out
        .else
            nop; need some slack, as CIA sets the flag a little too early
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            bcc sendloop
bcssendone: bcs senddone; jmp

fdsendblk:  lda #$38
            sta VIA_PRA; set to output
            lda VIA_ACR
            and #.lobyte(~SHIFT_REG_CONTROL)
            ora #SHIFT_OUT_T2
            sta VIA_ACR

            lda BLOCKBUFFER81,y
fdsendloop: sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
            cpy blocksize + 1
            iny
            lda #IRQ_SHIFT_REG
            ldx #FSM_BUS_DRIVER_OUTPUT; clear CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !::DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .else
            nop; need some slack
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            bcs senddone
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
            cpy blocksize + 1
            iny
            lda #IRQ_SHIFT_REG
            ldx #FSM_BUS_DRIVER_OUTPUT | CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !::DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .else
            nop; need some slack, as CIA sets the flag a little too early
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            bcc fdsendloop

    .else ; !::USE_ASYNCHRONOUS_BURST_HANDSHAKE

            lda BLOCKBUFFER81,y
sendloop:   sta CIA_SDR; clock out data byte
blocksize:  cpy #$00
            bcs bcswaittkn
            iny
        .if !::DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START 
            sta CIA_CRB; reset watchdog time-out
        .endif
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            cpy blocksize + 1
            lda BLOCKBUFFER81,y
            sta CIA_SDR; clock out data byte

            bcs waitdtaken
            iny
        .if !::DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB; reset watchdog time-out
        .endif
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            lda BLOCKBUFFER81,y
            bcc sendloop; jmp

fdsendblk:  lda #$38
            sta VIA_PRA; set to output
            lda VIA_ACR
            and #.lobyte(~SHIFT_REG_CONTROL)
            ora #SHIFT_OUT_T2
            sta VIA_ACR

            lda BLOCKBUFFER81,y
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
fdsendloop: cpy blocksize + 1
bcswaittkn: bcs waittaken
            iny
        .if !::DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .endif
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte

            cpy blocksize + 1
            bcs waitdtaken
            iny
        .if !::DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .endif
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
            bcc fdsendloop; jmp

waitdtaken: bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi waitdtaken
            bpl senddone; jmp
waittaken:  bit CIA_PRB; wait for ATN_IN set = data taken
            bpl waittaken

    .endif; !::USE_ASYNCHRONOUS_BURST_HANDSHAKE

senddone:   lda cmdfdfix2; 0 for FD
            bne :+
            lda #$18
            sta VIA_PRA
:
.else; ::PLATFORM <> diskio::platform::COMMODORE_128

sendloop:
        .if !::DISABLE_WATCHDOG
cmdfdfix3 = * + 1
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START ; is changed to VIA access for FD
cmdfdfix4 = * + 1
            sta CIA_CRB                                           ; 2 + 4 - reset watchdog time-out
        .endif
            ldx BLOCKBUFFER81,y                                   ; 4
            lda SENDTABLELO,x                                     ; 4
                                                                  ; = 22 (+6 with watchdog)

:           bit CIA_PRB                                           ; 4
            bmi :-                                                ; 3
            sta CIA_PRB                                           ; 4
            asl                                                   ; 2
            and #.lobyte(~ATNA_ENABLE_OUT)                        ; 2
                                                                  ; = 15

:           bit CIA_PRB                                           ; 4
            bpl :-                                                ; 3
            sta CIA_PRB                                           ; 4
            lda SENDTABLEHI,x                                     ; 4
                                                                  ; = 15

:           bit CIA_PRB                                           ; 4
            bmi :-                                                ; 3
            sta CIA_PRB                                           ; 4
            asl                                                   ; 2
            and #.lobyte(~ATNA_ENABLE_OUT)                        ; 2
blocksize:  cpy #0                                                ; 2
            iny                                                   ; 2
                                                                  ; = 19

:           bit CIA_PRB                                           ; 4
            bpl :-                                                ; 3
            sta CIA_PRB                                           ; 4
            bcc sendloop                                          ; 3
                                                                  ; = 75
:           bit CIA_PRB; wait for acknowledgement
            bmi :-     ; of the last data byte

.endif; ::PLATFORM <> diskio::platform::COMMODORE_128

            lda #CLK_OUT; drive busy
            sta CIA_PRB

:           bit CIA_PRB; wait for ATN_IN set,
            bpl :-     ; acknowledgement of the block transfer

            sei; disable watchdog
            clc
            rts

dcodinit81: lda #CLK_OUT
            sta CIA_PRB; signal idle to the host, note that ATNA response with ATN_IN low is not possible on 1581,
                       ; so a KERNAL LISTEN command cannot be detected for automatic uninstallation
            jsr swapzp81
            tsx
            stx SYS_SP

            lda IRQVECTOR_LO
            sta sysirqvbuf + 0
            lda IRQVECTOR_HI
            sta sysirqvbuf + 1

            ldx #$00
:           txa
            sta TEMP
            lsr
            lsr
            alr #%00100010
            eor TEMP      ; bit 3 ^ bit 0
            and #%00010001; if result is 0: both bits are equal (no swap required)
           ;clc           ; if result is 1: swap bits by inverting them
            adc #%01110111; 0 -> 7, 1 -> 8
            ora #%01100110; 0 -> 7, 1 -> e
            eor #%10001000; 0 -> f, 1 -> 6
            eor TEMP
            tay
            and #BINARY_NIBBLE_MASK
            sta SENDTABLELO,x
            tya
            lsr
            lsr
            lsr
            lsr
            tay
            sta SENDTABLEHI,x
            inx
            bne :-

            jsr inittimers

            lda NUMFILES
            bne :+

            jsr getdirtrk
            ldy #0
            jsr trackseek
            lda #$ff
            sta NUMFILES
:           jmp idleloop

drvcodeend81:
DRVCODEND81 = *
.export DRVCODEND81

DIRBUFFSIZE        = (SENDTABLELO - *) / 4
DIRTRACKS          = *
DIRSECTORS         = DIRTRACKS + DIRBUFFSIZE
FILENAMEHASHVALSLO = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVALSHI = FILENAMEHASHVALSLO + DIRBUFFSIZE

DIRTRACKS81       = DIRTRACKS

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

DIRBUFFSIZE81 = DIRBUFFSIZE
.export DIRBUFFSIZE81

            .assert * <= BLOCKBUFFER81, error, "***** 1581 drive code too large. *****"

dinstall:   sei
            lda #CIA_ATN_IN_INPUT | WRITE_PROTECT_OUTPUT | FSM_BUS_DRIVER_DIRECTION_OUTPUT | ATNA_ENABLE_OUT_OUTPUT | CLK_OUT_OUTPUT | CLK_IN_INPUT | DATA_OUT_OUTPUT | DATA_IN_INPUT
            sta CIA_DDRB
            lda #CLK_OUT
            sta CIA_PRB

            ldx #.lobyte(drvcodebeg81 - $01)

:           lda CIA_PRB; wait for ATN_IN set and DATA_IN clear
            and #ATN_IN | DATA_IN
            cmp #ATN_IN
            bne :-

dgetrout:   inx
            bne :+
            inc dgetputhi
:
            ; must not clobber x
            lda #%10000000
            sta BUFFER
            sta CIA_PRB
:           lda CIA_PRB
:           cmp CIA_PRB
            beq :-
            lda CIA_PRB
            and #CLK_IN
            cmp #CLK_IN
            ror BUFFER
            bcc :--
            lda BUFFER
dgetputhi = * + $02
            sta a:.hibyte(drvcodebeg81 - $01) << 8,x
            cpx #.lobyte(drvcodeend81 - $01)
            bne dgetrout
            dec drvcodebeg81
            bne dgetrout

            jmp dcodinit81

drvprgend81:
            .reloc
