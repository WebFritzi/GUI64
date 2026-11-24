
.include "cpu.inc"
.include "via.inc"
.include "cia.inc"

.include "drives/drivecode-common.inc"

CURRTRACK71            = $00
MAXTRACK_A             = $01
LEDSTATE71             = $02; fixed: TWO
NUMSECTORSONTRACK      = $03
MAXTRACK               = $04
DISKCHANGEBUFFER       = $05
FILENAMEHASHLO         = $06
FILENAMEHASHHI         = $07
CHECKSUM               = $08
LINKSECTOR71           = CHECKSUM
ERRORCOUNT             = $09
CLEARSECTORLINKTABLE71 = $0a
TEMP                   = $0b
CURRSTEPSPEEDLOW       = TEMP
GCRBUFFER              = TEMP
BLOCKSIZE              = TEMP
SPECSTARTSECTOR        = TEMP
TRACKINC               = $0c
HEADERTRACK            = TRACKINC
GENSPECLIMIT           = TRACKINC
LINKTRACK71            = $0d
FILESECTOR             = $0e
FILEDIRBUFFERINDEX     = $0f
BLOCKINDEXBASE         = $10
CURRBLOCKINDEX         = $11
ID0                    = $12; fixed: DSKID
ID1                    = $13; fixed: DSKID
PREVBLOCKINDEXPLUS1    = $14
MAXCONFIRMEDBLOCKINDEX = $15
MAXCONFIRMEDBLKIDXSEC  = $16
NUMCONTIGUOUSBLOCKS    = $17
NUMFILES71             = $18; fixed: OPC_CLC
NEXTCONTIGUOUSBLOCK    = $19
NEXTDIRBLOCKSECTOR     = $1a
DIRBLOCKPOS            = $1b
SPECULATIVEINTERLEAVE  = $1c
SPECWRAPSECTOR         = $1d; SPECWRAPSECTOR = CLEARINDEXMASK
CLEARINDEXMASK         = SPECWRAPSECTOR
SPECWRAPSECTORINDEX    = $1e
CLEARIDXMIN            = SPECWRAPSECTORINDEX

SECTORLINKTABLE71      = $1f; $15 = MAXNUMSECTORS bytes
LOADEDSECTOR           = SECTORLINKTABLE71 + MAXNUMSECTORS

FILETRACK              = LOADEDSECTOR + 1; must be INDEXTABLE - 2: see trackinit
REQUESTEDSECTOR        = FILETRACK + 1; must be INDEXTABLE - 1: see trackinit
INDEXTABLE             = REQUESTEDSECTOR + 1; $15 = MAXNUMSECTORS bytes
TRACKLINKTABLE         = INDEXTABLE + MAXNUMSECTORS; $15 = MAXNUMSECTORS bytes

DIRBUFFER              = TRACKLINKTABLE + MAXNUMSECTORS

CYCLESTARTENDSECTOR    = FILEDIRBUFFERINDEX

FILENAME71             = INDEXTABLE; max. $10 bytes

DIRBUFFSIZE            = (enddirbuffer - DIRBUFFER) / 5
DIRTRACKS              = DIRBUFFER
DIRSECTORS             = DIRTRACKS + DIRBUFFSIZE
NUMBLOCKS              = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVALSLO     = NUMBLOCKS + DIRBUFFSIZE
FILENAMEHASHVALSHI     = FILENAMEHASHVALSLO + DIRBUFFSIZE

DIRBUFFSIZE71          = DIRBUFFSIZE
.export DIRBUFFSIZE71

            .assert DIRBUFFSIZE >= 11, error, "***** Dir buffer too small. *****"

BLOCKBUFFER71          = $0700
TRACKOFFSET            = $00
SECTOROFFSET           = $01
TWOSIDEDOFFSET         = $03
BLOCKSOFFSET           = $1e

ROMOS_TRACK_DIFF       = $42

DECGCRTAB10ZZZ432LO    = $a00d; = $9f0d (GCRTBA)
;DECGCRTAB3210ZZZ4LO   = $9f0f; GCRTBD
;DECGCRTAB0ZZZ4321HI   = $9f1d; GCRTBE
DECGCRTAB210ZZZ43HI    = $9f2a; GCRTBG
DECGCRTAB43210ZZZHI    = $a00d; GCRTB1
DECGCRTABZZ43210ZHI    = $a10d; GCRTB2
DECGCRTABZ43210ZZLO    = $a20d; GCRTB3
DECGCRTABZZZ43210LO    = $a30d; GCRTB4

BINARY_NIBBLE_MASK     = %00001111

MAXINTERLEAVE          = 16
MAXNUMSECTORS          = 21
NUMTRACKS_SINGLESIDED  = 41
NUMTRACKS_A            = 35
NUMTRACKS_B            = NUMTRACKS_SINGLESIDED
MAXTRACK71             = NUMTRACKS_A + NUMTRACKS_B

INDEXSPECULATIVE       = %01000000
BLOCKPENDING           = %00100000

TIMER                  = VIA2_T1C_H

ERR                    = $80; ERRORCOUNT reset value, $40 is too little: would cause stepping when spinning up
ERT                    = $20; ERRORCOUNT retry value

BITRATECOUNT           = 4; number of read attempts before bit-rate cycling on initial current track retrieval
SUCCESSCOUNT           = 10; number of consecutive successful per-bit-rate attempts on initial current track retrieval

INITIALSPECINTERLEAVE  = 4

            .org $c3

drvcodebeg71:
            .byte .hibyte(drvcodeend71 - * + $0100 - $01); init transfer count hi-byte

FINDTRACKN = INDEXTABLE

bitratecnt = PREVBLOCKINDEXPLUS1
successcnt = MAXCONFIRMEDBLKIDXSEC

enddirbuffer:

V2A:        .word VIA2_PRA

getbytewdg: lda #$ff
TIM = * + 1
            sta TIMER; reset watchdog time-out
            ENABLE_WATCHDOG
V1B = getbyte + 1
getbyte:    DRIVEGETBYTE 1571, getbytecmp
            rts

            ; for normal busy LED fading speed and correct head stepping speed
onemhz71:   lda #.lobyte(~TWO_MHZ)
V1A = * + 1
            and VIA1_PRA
store_via1: sta VIA1_PRA
            rts

dofadeled:  dey
            bne dofadeled

            .assert .hibyte(*) = .hibyte(dofadeled), error, "***** Page boundary crossing in LED fade loop. *****"

BUSY_LED_MASK = ~BUSY_LED
            ldx #.lobyte(BUSY_LED_MASK)
            sax (.lobyte(V2B - BUSY_LED_MASK),x); VIA2_PRB
            ldy LEDSTATE71
:           iny
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"
TWO = * + 1
            dec LEDSTATE71
V2B = * + 1
bsetv2b71:  ora VIA2_PRB

            .assert * <= $0100, error, "***** V2B not in zeropage. *****"

            sta VIA2_PRB
            rts

fadeled:    jsr onemhz71; 1 MHz so the LED fades at the same speed as on 1541

MOTOR_LED_MASK = ~(BUSY_LED | MOTOR)
            ldx #.lobyte(MOTOR_LED_MASK)
            lda (.lobyte(V2B - MOTOR_LED_MASK),x); VIA2_PRB
            ldy LEDSTATE71
            bne dofadeled

            sax (.lobyte(V2B - MOTOR_LED_MASK),x); VIA2_PRB
notrkstep:  rts

trkseek71c: ldy CURRTRACK71
            stx CURRTRACK71
            lsr CLEARSECTORLINKTABLE71

trkseek71:  lda #MOTOR; turn on the motor
trackseekx: jsr bsetv2b71
            ldx #$80 | (MINSTEPSPEED + 1)
trackstep:  tya; destination track
            beq notrkstep; don't do anything if invalid track
            cmp MAXTRACK
            bcs notrkstep; don't do anything if invalid track
            cmp CURRTRACK71
            beq setbitrate; branch if on same track

            sec
            lda CURRTRACK71
            sbc MAXTRACK_A
            beq :+
            bcc :+
            sta CURRTRACK71; the current track is on the 2nd side,
                           ; temporarily store the 2nd side physical track number
:           sec
            tya; destination track
            sbc MAXTRACK_A
            beq :+
            bcs :++; branch if the destination track is on the 2nd side
:           clc
            tya; the destination track is on the 1st side
:           pha
            lda VIA1_PRA
            and #.lobyte(~SIDE_SELECT)
            bcc :+
            ora #SIDE_B
:
c1570fix1:  sta VIA1_PRA
            pla
            sec
            sbc CURRTRACK71
            sty CURRTRACK71
            beq initlinktb; branch if destination track reached

            ; do the track jump
            ldy #$01; move up (inwards)
            sty CURRSTEPSPEEDLOW
            bcs :+
            eor #$ff; invert track difference
            adc #$01
halftrkdwn: ldy #$03; move down (outwards)
:           sty TRACKINC
            asl; half-tracks
            tay

            jsr onemhz71

halftrack:  stx TIMER; reset track-step timer
            lda VIA2_PRB
            anc #.lobyte(~(SYNC_MARK | MOTOR)); clc
            adc TRACKINC
            ora #MOTOR
            sta VIA2_PRB
            cpx #($80 | SINGLESTEPSPEED) - 1
            beq initlinktb; stepping to adjacent track: branch if second half-track step has been issued

            txa
headaccl:   cmp #$80 | MAXSTEPSPEED
            beq noheadacc
            pha
           ;sec
            lda CURRSTEPSPEEDLOW
            sbc #STEPPERACC
            sta CURRSTEPSPEEDLOW
            pla
            sbc #$00

noheadacc:  cpx TIMER
            beq noheadacc; wait until the counter hi-byte has decreased by 1
            dex
            bmi headaccl
seekswitch: tax
            dey
            bne halftrack

initlinktb: lda CLEARSECTORLINKTABLE71
            bpl :++
initlink71: lda #$ff; sector link unknown
            ldx #MAXNUMSECTORS
:           sta SECTORLINKTABLE71,x; sector links are unknown
            dex
            bpl :-; sets LOADEDSECTOR = SECTORLINKTABLE71 + MAXNUMSECTORS with x = MAXNUMSECTORS

:           lda #ERR
            sta ERRORCOUNT

            ; bit-rates:
            ; tracks 31-35/66+   (17 blocks): %00 - sector interleave 3 (lowest density, slowest clock, innermost tracks)
            ; tracks 25-30/60-65 (18 blocks): %01 - sector interleave 3
            ; tracks 18-24/53-59 (19 blocks): %10 - sector interleave 3
            ; tracks  1-17/36-52 (21 blocks): %11 - sector interleave 4 (highest density, fastest clock, outermost tracks)
setbitrate: lda CURRTRACK71
            jsr getnumscts
            stx NUMSECTORSONTRACK
            ldy #0
            sta (V2B),y; VIA2_PRB
            ; fall through

twomhz:     lda #BYTE_READY | TWO_MHZ; the accu must contain a negative number upon return
            ora VIA1_PRA
            jmp store_via1

            BLOCKBUFFER = BLOCKBUFFER71
            NUMFILES = NUMFILES71
            FNAMEHASH 1571; must not be overwritten by watchdog IRQ/reset/custom code upload
readerrts = * - 1

            ; * >= $0100
stack:
            .assert stack >= $0100, error, "***** 1571 stack too low in memory. *****"

            .word 0, 0, 0
stackend:
topofstack71 = stackend - 1

            .assert stackend < $0200, error, "***** 1571 stack too high in memory. *****"

readheader: lda #%01000000
            ldy #$fc

readdata:   bit VIA1_PRA
            bmi readdata

            eor VIA2_PRA ; 11222223
            sta GCRBUFFER
            and #%11000000
            bne readerrts; z = 0

loaddata:   bit VIA1_PRA              ;  4
            bmi loaddata              ;  6
            sta CHECKSUM              ;  9
            lda GCRBUFFER             ; 12 ;   11222223
            lsr                       ; 14 ;   .1122222:3
            lda VIA2_PRA              ; 18 ;   33334444
            pha                       ; 21
            ror                       ; 23 ;   33333444
            lsr                       ; 25 ;   .3333344
            tax                       ; 27
            lda DECGCRTABZ43210ZZLO,x ; 31 ;   ....3333
            ldx GCRBUFFER             ; 34 ;   11222223
            ora DECGCRTABZZ43210ZHI,x ; 38 ;   22223333

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            sta BLOCKBUFFER71 + 0,y   ; 11
            lda VIA2_PRA              ; 15 ;   45555566
            sta GCRBUFFER             ; 18
            asl                       ; 20 ; 4:5555566.
            pla                       ; 24 ;   33334444
            rol                       ; 26 ;   33344444
            asl                       ; 28 ;   3344444.
            tax                       ; 30
            lda DECGCRTABZZ43210ZHI,x ; 34 ;   4444....
            ldx GCRBUFFER             ; 37 ;   45555566
            ora DECGCRTABZ43210ZZLO,x ; 41 ;   44445555

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            sta BLOCKBUFFER71 + 1,y   ; 11
            lda #%00000011            ; 13
            axs #%00100000            ; 15 ;   HHHLLL66
            lda VIA2_PRA              ; 19 ;   66677777
            sta GCRBUFFER             ; 22
            ora #%00011111            ; 24 ;   666HHHHH
            axs #0                    ; 26 ;   666LLL66
            lda DECGCRTAB210ZZZ43HI,x ; 30 ;   6666....
            ldx GCRBUFFER             ; 33 ;   66677777
            ora DECGCRTABZZZ43210LO,x ; 37 ;   66667777
            sta BLOCKBUFFER71 + 2,y   ; 42

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            ldx VIA2_PRA              ; 10 ;   00000111
            eor BLOCKBUFFER71 + 1,y   ; 14
            eor BLOCKBUFFER71 + 0,y   ; 18
            eor CHECKSUM              ; 21
            sta CHECKSUM              ; 24
            lda DECGCRTAB43210ZZZHI,x ; 28 ;   0000....
            pha                       ; 31
            lda #%00000111            ; 33
            axs #%01000000            ; 35 ;   HHLLLL111
            iny                       ; 37
            iny                       ; 39
            iny                       ; 41

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            lda VIA2_PRA              ; 10 ;   11222223
            sta GCRBUFFER             ; 13
            ora #%00111111            ; 15 ;   11HHHHHH
            axs #0                    ; 17 ;   11LLL111
            pla                       ; 21 ;   0000....
            ora DECGCRTAB10ZZZ432LO,x ; 25 ;   00001111
            sta BLOCKBUFFER71 + 0,y   ; 30
            eor CHECKSUM              ; 33
            iny                       ; 35
            beq loaddone              ; 37
            jmp loaddata              ; 40

getnumscts: tay
            sec
            sbc MAXTRACK_A
            beq :+
            bcs getnumscts
:           ldx #21; number of blocks
            lda #SYNC_MARK | BITRATE_MASK; $e0
            ora ((V2B - 21),x); VIA2_PRB
            cpy #18
            bcc :++; bit-rate $60
            ldx #19
            cpy #25
            bcc :+ ; bit-rate $40
            dex; 18
            and #.lobyte(~(%10 << BITRATE_SHIFT)); -$40
            cpy #31
            bcc :++; bit-rate $20
            dex; 17
:           and #.lobyte(~(%01 << BITRATE_SHIFT)); -$20
:           rts

loaddone:   sta CHECKSUM

:           lda (V1A),y; VIA1_PRA
            bmi :-

            lax GCRBUFFER         ; 11222223
            lsr                   ; .1122222:3
            lda (V2A),y; VIA2_PRA ; 3333LHLH
            ror                   ; 33333LHL
            lsr                   ; .33333LH:L
            tay
            lda DECGCRTABZZ43210ZHI,x; 2222....

            SKIPWORD

            .assert * >= $02a9, error, "***** 1571 watchdog IRQ vector located below $02a9. *****"
            .assert * <= $02a9, error, "***** 1571 watchdog IRQ vector located above $02a9. *****"

            .word uninstall

            ora DECGCRTABZ43210ZZLO,y; 22223333
            tay; ID0
            eor CHECKSUM
           ;clc
            rts; z = 1

chksumerr:  ; with repeated checksum errors, head might have landed between tracks or on the wrong track
            lda ERRORCOUNT; is changed to dec ERRORCOUNT after init
            bne :+

            ldx #ERT
            stx ERRORCOUNT
            lda (.lobyte(V2B - ERT),x); VIA2_PRB
            anc #BITRATE | BUSY_LED | TRACK_STEP
            sbc #(1 << BITRATE_SHIFT) - 1; cycle to the next bit-rate (denser until wrap)
            adc #$03; and step down half a track on bit-rate wrap
            ora #MOTOR
            sta (.lobyte(V2B - ERT),x); VIA2_PRB

:           sec; operation not successful
            rts; checksum mismatch: z = 0, c = 1

            ; in: y: track
            ;     a: sector or negative value for any sector
getblkchid: ldx #.lobyte(compareid - (idswitch + 2)); check against stored ID
getblkstid: ; store read ID, ldx #.lobyte(storeid - (idswitch + 2)) executed by caller
            ldy #DIRTRACK
            stx idswitch + 1
getblock71: sta REQUESTEDSECTOR
            jsr trkseek71; stores the number of blocks on the current track in NUMSECTORS
getblkscan:
findblkhdr: lda #$ff
            sta TIMER
:           jsr waitsync
            beq readerror; returns with carry set on time-out
            cmp #%01010010; $52, check if the sync is followed by a block header
            bne :-

            jsr readheader
bnecsumerr: bne chksumerr

            lda BLOCKBUFFER71 + $fd; sector
            cmp NUMSECTORSONTRACK
            bcs chksumerr
            sta LOADEDSECTOR

            ldx BLOCKBUFFER71 + $fe; track
            beq chksumerr
            cpx MAXTRACK
            bcs chksumerr
            stx HEADERTRACK

            ldx BLOCKBUFFER71 + $ff; ID1

            ; y = ID0
idswitch:   .byte OPC_RTS, 0; is changed to bcc storeid/compareid after init

storeid:    sty ID0
            stx ID1
compareid:  cpy ID0
            bne bnereaderr
            cpx ID1
            bne bnereaderr

           ;lda LOADEDSECTOR
            tax
            eor REQUESTEDSECTOR
            beq loadblock; branch if requested sector
            bmi :+

            ; specific sector requested but not reached yet
            lda REQUESTEDSECTOR
            cmp NUMSECTORSONTRACK
            bcc findblkhdr
            bcs readerror; jmp

:           ; negative value: no specific sector requested, out-of-order sector fetch
           ;ldx LOADEDSECTOR
            lda INDEXTABLE,x
            ldy SECTORLINKTABLE71,x
            ldx #255 - (BLOCKPENDING | INDEXSPECULATIVE)
            sax CURRBLOCKINDEX

            asl
            bcs :+; branch if block index not known nor speculated
            ora #255 - (BLOCKPENDING * 2); with positive INDEXTABLE value, branch if the block has already been loaded into the host's memory (BLOCKPENDING not set)
            tay
:           iny; with negative INDEXTABLE value, block index is unknown: branch to findblkhdr if linked sector is known (the block has been loaded before already)
            bne findblkhdr

            ; skip this block if the following block is next in the stream
            ldx NEXTCONTIGUOUSBLOCK
            bne :+
            ldx NUMSECTORSONTRACK
:           dex
            cpx LOADEDSECTOR
            beq findblkhdr

loadblock:  ; wait for data block sync
            jsr waitsync
            cmp #%01010101; $55, check if the sync is followed by a data block
            bne bnecsumerr

            lda #%11000000
           ;ldy #0
            jsr readdata
            bne bnecsumerr

            lax HEADERTRACK
            eor CURRTRACK71
            beq sanitychsw

            ; track error
            jsr trkseek71c
            asl CLEARSECTORLINKTABLE71
bnereaderr: bne readerror; jmp

linkerror:  asl INDEXTABLE,x
            bmi readerror; branch if INDEXSPECULATIVE is set: clear index, set MSB
            ror INDEXTABLE,x; restore non-speculative index
            SKIPWORD; skip to NUMFILES71 = clc
:           sec            ; refill directory buffer when disk has been changed
            ror NUMFILES71 ; NUMFILES71 = $18 = OPC_CLC
readerror:  ; ID mismatch or illegal track or sector (invalid track/sector link)
checkchg:   ; must not change y
            lax VIA2_PRB; check light sensor for disk removal
            eor DISKCHANGEBUFFER
            stx DISKCHANGEBUFFER
            and #WRITE_PROTECT
            bne :-
            ; read error: z = 1
            sec; operation not successful
            rts

            ; block link sanity check
sanitychsw: .byte OPC_RTS, LOADEDSECTOR; is changed to ldx LOADEDSECTOR after init

            lda #$fe
            sta SECTORLINKTABLE71,x; mark block as loaded (linked sector is known)

            lda BLOCKBUFFER71 + SECTOROFFSET
            sta LINKSECTOR71
.ifdef GUI64_DIRECTORY_COMPAT
            ldy CURRTRACK71
            cpy #DIRTRACK
            beq mdDirectoryEnd71; final directory link byte is not a payload size
.endif
.ifdef GUI64_DIRECTORY_COMPAT
            cmp #1; an empty file is legal; zero-filled speculative sectors remain invalid
.else
            cmp #2
.endif
mdDirectoryEnd71:
            lda BLOCKBUFFER71 + TRACKOFFSET
            sta LINKTRACK71
            sta TRACKLINKTABLE,x
            beq chklastbsz

            cmp MAXTRACK; check whether track link is within the valid range
            bcs linkerror; if not, return error

            jsr getnumscts; get number of sectors on linked track
            cpx LINKSECTOR71; check whether sector link is within the valid range
chklastbsz: ; branch if invalid block size (0..1 = 1..2 bytes)
            bcc linkerror; branch if sector number too large

            lda LINKSECTOR71 ; return the loaded block's sector link sector number
            ldx LOADEDSECTOR ; return the loaded block's sector number
            ldy LINKTRACK71  ; return the loader block's sector link track number

            cpy CURRTRACK71
            beq sametrack
            asl           ; must not be $ff
            ora #%10000000; link is not to same track
sametrack:
            cmp LOADEDSECTOR        ; block must not link to itself
            beq linkerror           ; (but larger cycles are not detected)
            sta SECTORLINKTABLE71,x ; set block link
            cpy CURRTRACK71         ; z flag must be set
            clc                     ; operation successful
            rts

waitsync:   ldy #0
:           lda (TIM),y; TIMER
            beq readerror; will return $00 in the accu
            lda (V2B),y; VIA2_PRB, check SYNC
            bmi :-; branch if no sync
            lda (V2A),y; VIA2_PRA; clear latch
:           lda (V1A),y; VIA1_PRA, check BYTEREADY
            bmi :-; branch if byte not ready
            lda (V2A),y; VIA2_PRA; is never $00 but usually $52 (header) or $55 (data)
            rts

            ; get custom drive code
getcustom:  ldx #0
            lda #ATNA_OUT | DATA_OUT; 1571
            sta (V1B,x); VIA1_PRB
:           lda #CLK_IN
            and (V1B,x); VIA1_PRB, wait for CLK_IN set
            beq :-
            lda #ATNA_OUT
            sta (V1B,x); VIA1_PRB

CUSTOMZPBUFFER71 = FILENAME71
CUSTOMUPLOADSIZE71 = $bb + RETURNSIZE71
RETURNSIZE71 = $22

:           jsr getbytewdg
            sta BLOCKBUFFER71,x
            inx
            cpx #CUSTOMUPLOADSIZE71
            bne :-
            jmp BLOCKBUFFER71 + RETURNSIZE71

:           inc CURRTRACK71
uninstall:  ldy #18; ROM dir track
            jsr trkseek71
           ;ldy #0
            lda (V2B),y; VIA2_PRB
            bit TWO; the stepper bits are set to %00 after reset: ensure full-track
            bne :- ; stepper alignment (may still be off by a half-track)

            dec reset; OPC_ADC_ABS -> OPC_JMP_ABSI

            and #BUSY_LED
            beq reset
            dey
            sty LEDSTATE71

statussent: bne fadeledidl

            ; prepare next file, only after successful load
            ldx FILEDIRBUFFERINDEX
            lda FILENAMEHASHVALSLO + 1,x
            ldy FILENAMEHASHVALSHI + 1,x
            jmp setnextfile

fadeledidl: jsr fadeled
            bne fadeledidl

reset:      adc RESET_VECTOR

prpdnxtfil: ; jump from prpnxtfjmp when drive has cached next file's first block
idleloop71: jsr fadeled
            jsr checkchg
           ;lda #0
            tax
            lda (V1B,x); VIA1_PRB
            bpl uninstall
            lsr
            bcs idleloop71; wait until there is something to do

            jsr twomhz

            FILENAME = FILENAME71
            GET_FILENAME 1571

            ; matches against hash of filename in FILENAMEHASHLO/HI
            findfile71 = findfile
            FIND_FILE 1571

           ;ldy #diskio::status::FILE_NOT_FOUND; $ff
loadfile71:
            bcs filenfound
DIRSECTORS71 = DIRSECTORS
           ;lda DIRSECTORS,x
            sta FILESECTOR

            lda #0
            sta PREVBLOCKINDEXPLUS1
            sta NUMCONTIGUOUSBLOCKS

trackloop:  sta BLOCKINDEXBASE
            lda #BUSY_LED | MOTOR
            jsr trackseekx
            ; x contains the number of sectors on this track
            inx
trackinit:  sta INDEXTABLE - 2,x; accu is negative with INDEXSPECULATIVE set, sector indices are unknown
            dex
            bpl trackinit; sets REQUESTEDSECTOR = INDEXTABLE - 1 with x = 1 and FILETRACK = INDEXTABLE - 2 with x = 0

            .assert .hibyte(*) = .hibyte(trackinit), error, "***** Page boundary crossing in trackinit loop. *****"

            stx LEDSTATE71
            ldx FILESECTOR
            stx NEXTCONTIGUOUSBLOCK
           ;ldy #0; initial block index
            jsr speculinit; set non-speculative block indices according to known links, build initial speculative block index table for this track

            lda LOADEDSECTOR
            cmp FILESECTOR
            beq servecache

blockloop:  jsr getblkscan
            bcs blockloop; branch if block fetch not successful

           ;ldx LOADEDSECTOR
           ;ldy LINKTRACK71
           ;cpy CURRTRACK71
            bne nosetspcil; branch if linked block not on current track
            sec
           ;lda LINKSECTOR71
            sbc LOADEDSECTOR; determine likely interleave
            tay
            bcs setspecilv
nosetspcil: ldy SPECULATIVEINTERLEAVE
setspecilv:;ldx LOADEDSECTOR
            lda INDEXTABLE,x
            anc #.lobyte(~(BLOCKPENDING | INDEXSPECULATIVE))
bmiblkloop: bmi blockloop; branch if block conceivably not belonging to the file

            sta CURRBLOCKINDEX
            beq refuspecil; branch if first file block on track: cannot determine likely interleave
            lda PREVBLOCKINDEXPLUS1; first file block must be loaded first in order to know the load address to be able to place successive
            beq blockloop          ; blocks in host memory, skip loaded blocks until the required one is read

            tya
            beq refuspecil
            cmp #MAXINTERLEAVE + 1; validate
            bcs refuspecil        ; speculative interleave
            adc LINKSECTOR71
            cmp NUMSECTORSONTRACK
            bcc :+
            sbc NUMSECTORSONTRACK
:           ldx LINKSECTOR71
            cmp SECTORLINKTABLE71,x
            bne refuspecil; only accept interleave if distances to this block and the next match
            sty SPECULATIVEINTERLEAVE
refuspecil:
            ldx MAXCONFIRMEDBLKIDXSEC
            ldy MAXCONFIRMEDBLOCKINDEX
            sec; clear mis-speculation detected flag
            jsr specmanage
servecache:
prpnxtfjmp: jmp prpdnxtfil; is changed to eor prpdnxtfil

            jsr sendblock
            bcs :+; branch if file closed

            bmi blockloop
            bne trackloop

            ; loading is finished

filenfound: tya; $00 = diskio::status::OK

            ldy #0; send over one byte
            jsr sendstatus
:           dec prpnxtfjmp; OPC_EOR_ABS -> OPC_JMP_ABS
            lax BLOCKBUFFER71
            bcs fileclosed

            ldx #ATN_IN | ATNA_OUT | CLK_OUT | CLK_IN
:           cpx VIA1_PRB; wait until host
            beq :-      ; is in idle mode

            tax
            jmp statussent

fileclosed:;ldy #$ff
            iny
           ;ldx BLOCKBUFFER71
            inx
            beq :+; branch if file not found: DATA_OUT remains set, clear it otherwise
            tya
            sta (V1B),y; VIA1_PRB
:           lda (V1B),y; VIA1_PRB, wait for ATN_IN set,
            bpl :-     ; acknowledgement of the block transfer
            lda #ATNA_OUT | CLK_OUT; drive busy
            sta (V1B),y; VIA1_PRB
            txa
            beq :+
            jmp idleloop71; ok
:           jmp fadeledidl; file not found

sendblock:  ldx LOADEDSECTOR
            lda INDEXTABLE,x
            anc #.lobyte(~BLOCKPENDING); clc
            sta INDEXTABLE,x

           ;clc
            sbc #INDEXSPECULATIVE - 1
            ; c = 1 if index speculative, 0 otherwise

            bcc :+; branch if block index is not speculative
            sta CURRBLOCKINDEX; speculative index might have changed
            lda MAXCONFIRMEDBLOCKINDEX
            cmp CURRBLOCKINDEX; do not transfer block if another block has been confirmed to have the same or a
            bcc :+            ; higher index (this block's index is wrong or wouldn't be speculative any more)

           ;sec
            ror INDEXTABLE,x; clear index, set MSB
            clc
            rts

:           lda NEXTCONTIGUOUSBLOCK
:           tax
            lda INDEXTABLE,x
            cmp #MAXNUMSECTORS
            bcs :++; branch if block has not been confirmed and transferred already
            inc NUMCONTIGUOUSBLOCKS
            lda SECTORLINKTABLE71,x
            bpl :-; branch if there is a linked sector on the same track

            alr #%00011111 * 2
            sta FILESECTOR; first file sector on the linked track
            ldy TRACKLINKTABLE,x
            sty FILETRACK
            beq nostep

            ; perform Shrydar Stepping (R)(TM) to minimise single-track stepping overhead:
            ; nudge the R/W head in the right direction, then wait a few bycles while it gains momentum,
            ; then enable the destination track's stepper magnet long before the head has reached the intermediate half-track's magnet,
            ; relying on the head's inertia, then send over the block while the head keeps moving beyond the intermediate half-track's stepper magnet
            ; and ultimately settles on the destination track.
            ; sending the block over takes at least 72 bycles
            ldy CURRTRACK71
            cpy FILETRACK
            bcs :+
            iny
            SKIPBYTE
:           dey
            ldx #$80 | SINGLESTEPSPEED
            jsr trackstep
nostep:
            ; calculate block index in host memory
            sec

:           stx NEXTCONTIGUOUSBLOCK

            lda BLOCKINDEXBASE
            adc CURRBLOCKINDEX
            tay
            sec
            sbc PREVBLOCKINDEXPLUS1
            sty PREVBLOCKINDEXPLUS1
            ldy #0
            cpy LINKTRACK71
            rol; last block: set lsb, clear lsb otherwise
            sta BLOCKBUFFER71 + $01; block index and last block size/next contiguous block index flag

            lsr
            lda NUMCONTIGUOUSBLOCKS
            dey
            bcc :+
            ; handle file's last block
           ;sec
            lda #0
            sbc LINKSECTOR71
            ldy LINKSECTOR71; the file's last block's length (last byte index)
:           ; accu: next contiguous block index/block size

sendstatus: sta BLOCKBUFFER71; next contiguous block index/block size or status byte
            sty BLOCKSIZE

            ldx #$20; here, the watchdog timer is polled manually because
                    ; an extra-long time-out period is needed since the computer may
                    ; still be busy decompressing a large chunk of data,
                    ; this is the round counter: $20 * ($ff00 - $0100) = 2,080,768 cycles at 2 MHz is roughly 1 second
            ldy #$ff
            sty TIMER; reset watchdog time-out

            lda #ATNA_OUT | CLK_OUT | DATA_OUT | DATA_IN; set DATA_OUT as well so there is a flank only on CLK when signalling ready,
            sta (V1B - $20,x); VIA1_PRB, such that just one serial bus read can safely determine block ready (CLK clear) and drive present (DATA set) vs device not present (both clear)
            lda #ATNA_OUT | DATA_OUT; clear CLK_OUT, set DATA_OUT as signal of presence
            sta (V1B - $20,x); VIA1_PRB; block ready signal

            sec; for closefile

waitready:  lda TIMER; see if the watchdog barked
            bne :+
            dex      ; if yes, decrease the round counter
.if ::DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
.endif
            sty TIMER; reset watchdog time-out

:           lda #ATN_IN | CLK_IN
            and VIA1_PRB
            bmi waitready; wait for ATN_IN clear

            bne closefile

            sty TIMER; reset watchdog time-out

timeout:    ENABLE_WATCHDOG
            iny; ldy #0

.if ::PLATFORM = diskio::platform::COMMODORE_128

            lda #TWO_MHZ | FAST_SERIAL_OUTPUT
            ora (V1A),y; VIA1_PRA
            sta (V1A),y; VIA1_PRA
            lda #IOMODE_OUTPUT | COUNT_PHI2 | CONTINUOUS | TIMER_START
            sta CIA_CRA
            lda #CLK_OUT; clear ATNA_OUT, $08 = SERIAL_IRQ
            sta (V1B),y; VIA1_PRB

            bit CIA_ICR

    .if ::USE_ASYNCHRONOUS_BURST_HANDSHAKE

            ldx BLOCKBUFFER71,y
sendloop:   cpy BLOCKSIZE
            iny
            stx CIA_SDR; clock out data byte
            ldx #ATN_IN | DEVICE_NUMBER | ATNA_OUT; set ATNA_OUT, clear CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
            stx TIMER; need some slack, as CIA sets the flag a little too early (MOS5710 doesn't)
            stx VIA1_PRB; toggle CLK_OUT: signal data sent, toggle ATNA_OUT
            ldx BLOCKBUFFER71,y
:           bit VIA1_PRB; wait for ATN_IN set = data taken
            bpl :-
            bcs senddone

            cpy BLOCKSIZE
            iny
            stx CIA_SDR; clock out data byte
            ldx #$ff
:           bit CIA_ICR; wait until data sent
            beq :-
            stx TIMER; need some slack, as CIA sets the flag a little too early (MOS5710 doesn't)
            sta VIA1_PRB; toggle CLK_OUT: signal data sent, toggle ATNA_OUT
            ldx BLOCKBUFFER71,y
:           bit VIA1_PRB; wait for ATN_IN clear = data taken
            bmi :-
            bcc sendloop
senddone:

    .else; !::USE_ASYNCHRONOUS_BURST_HANDSHAKE

            lda BLOCKBUFFER71,y
sendloop:   sta CIA_SDR; clock out data byte
            cpy BLOCKSIZE
            bcs waittaken
            iny
            lda BLOCKBUFFER71,y
            ldx #ATN_IN | DEVICE_NUMBER | ATNA_OUT; set ATNA_OUT
            stx TIMER
:           bit VIA1_PRB; wait for ATN_IN set = data taken
            bpl :-
            stx VIA1_PRB; toggle ATNA_OUT
            cpy BLOCKSIZE
            sta CIA_SDR; clock out data byte

            bcs waitdtaken
            iny
            lda BLOCKBUFFER71,y
            ldx #ATN_IN | DEVICE_NUMBER; clear ATNA_OUT
            stx TIMER
:           bit VIA1_PRB; wait for ATN_IN clear = data taken
            bmi :-
            stx VIA1_PRB; toggle ATNA_OUT
            bcc sendloop; jmp

waitdtaken: bit VIA1_PRB; wait for ATN_IN clear = data taken
            bmi waitdtaken
            bpl senddone; jmp
waittaken:  bit VIA1_PRB; wait for ATN_IN set = data taken
            bpl waittaken
senddone:
    .endif; !::USE_ASYNCHRONOUS_BURST_HANDSHAKE

BURST_OFF = IOMODE_INPUT | COUNT_PHI2 | CONTINUOUS | TIMER_START
            ldx #BURST_OFF
            stx CIA_CRA
            lda #.lobyte(~FAST_SERIAL_OUTPUT)
            and (V1A - BURST_OFF,x); VIA1_PRA
            sta (V1A - BURST_OFF,x); VIA1_PRA

            lda #ATNA_OUT | CLK_OUT; drive busy
            sta (V1B - BURST_OFF,x); VIA1_PRB

.else; ::PLATFORM <> diskio::platform::COMMODORE_128

            ldx #255 - DATA_OUT
sendloop:   lda BLOCKBUFFER71,y     ; 4
            eor #%01110111          ; 2
            asl                     ; 2
            rol                     ; 2
            rol                     ; 2
                                    ; = 26

:           bit VIA1_PRB            ; 4
            bmi :-                  ; 3
            sax VIA1_PRB            ; 4
            stx TIMER               ; 4 - reset watchdog time-out
            ror                     ; 2
            ror                     ; 2
                                    ; = 19

:           bit VIA1_PRB            ; 4
            bpl :-                  ; 3
            sax VIA1_PRB            ; 4
            ror                     ; 2
            lsr                     ; 2
                                    ; = 15

:           bit VIA1_PRB            ; 4
            bmi :-                  ; 3
            sax VIA1_PRB            ; 4
            lsr                     ; 2
            lsr                     ; 2
            cpy BLOCKSIZE           ; 3
            iny                     ; 2
                                    ; = 20

:           bit VIA1_PRB            ; 4
            bpl :-                  ; 3
            sax VIA1_PRB            ; 4
            bcc sendloop            ; 3
                                    ; = 80

            lda #ATNA_OUT | CLK_OUT; drive busy
:           bit VIA1_PRB; wait for ATN low,
            bmi :-      ; acknowledgement of the last data byte

            sta VIA1_PRB

.endif; ::PLATFORM <> diskio::platform::COMMODORE_128

:           bit VIA1_PRB; wait for ATN_IN set,
            bpl :-      ; acknowledgement of the block transfer

            sei; disable watchdog

            lda NUMCONTIGUOUSBLOCKS
            ldy FILETRACK
            clc
closefile:  rts

            ; validate block indices according to currently-known links
idxloop71:  tax; link sector
            iny; block index
            tya
            eor INDEXTABLE,x
            bmi confirmidx; branch if index was neither known nor speculated until now
            eor #BLOCKPENDING | INDEXSPECULATIVE
            beq confirmidx; branch if not-yet-loaded speculative index matches
            eor #BLOCKPENDING
            beq confirmspc; branch if already-loaded speculative index matches
speculinit: clc ; mis-speculated block index detected
confirmidx: tya
            ora #BLOCKPENDING
confirmspc: sta INDEXTABLE,x
specmanage: lda SECTORLINKTABLE71,x; linked sector
            bpl idxloop71; branch if there is a linked block on the same track

            .assert .hibyte(*) = .hibyte(idxloop71), error, "***** Page boundary crossing in idxloop71. *****"

            stx MAXCONFIRMEDBLKIDXSEC
            sty MAXCONFIRMEDBLOCKINDEX

            lda #255 - BLOCKPENDING
            SKIPWORD
rspeculate: ldy SPECWRAPSECTORINDEX
            stx SPECSTARTSECTOR
            bcc :+; branch if mis-speculated block index detected

            ; on first track, respeculate from last speculative index when it now has a known link
            lax SPECWRAPSECTOR
            ora SECTORLINKTABLE71,x
            bmi specabort
            lda #255 - (BLOCKPENDING | INDEXSPECULATIVE)
            cpy SPECWRAPSECTORINDEX
            bcc rspeculate
            clc
:
            sta CLEARINDEXMASK; = SPECWRAPSECTOR

            ; mis-speculated block indices detected: rebuild speculative block index table

           ;clc; minus one block: do not transfer alleged final block, as it may be mis-speculated
               ; and load beyond the actual file end address due to possibly being larger than the file's final block
            ldx FILEDIRBUFFERINDEX
            lda NUMBLOCKS,x
            sbc BLOCKINDEXBASE
            bcc specabort; limit to alleged number of file blocks
            cmp NUMSECTORSONTRACK
            ldx NUMSECTORSONTRACK
            bcc :+
            txa
:           sta GENSPECLIMIT

            sty CLEARIDXMIN; = SPECWRAPSECTORINDEX
clearidxtb: lda CLEARINDEXMASK
            and INDEXTABLE - 1,x
            cmp CLEARIDXMIN
            beq :+
            bcc :+
            ror INDEXTABLE - 1,x
:           dex
            bne clearidxtb

            .assert .hibyte(*) = .hibyte(clearidxtb), error, "***** Page boundary crossing in clear index table loop. *****"

            ldx SPECSTARTSECTOR
speculate:  iny
            cpy GENSPECLIMIT
            bcs specdone
            stx SPECWRAPSECTOR
            lda SECTORLINKTABLE71,x
            bpl havenewidx; branch if there is a linked sector on the same track
            txa
           ;clc
            adc SPECULATIVEINTERLEAVE
havenewidx: tax
            sec
            sbc NUMSECTORSONTRACK
            bcs specwrap
            lda INDEXTABLE,x
            bpl specsectkn
            tya
            ora #BLOCKPENDING | INDEXSPECULATIVE
            sta INDEXTABLE,x
            sty SPECWRAPSECTORINDEX
            bne speculate; jmp

            .assert .hibyte(*) = .hibyte(speculate), error, "***** Page boundary crossing in speculate loop. *****"

specsectkn: inx
            txa
specwrap:   ldx BLOCKINDEXBASE; for the first file track, only speculate until wrap and stop on already-taken speculative sectors,
            bne havenewidx    ; as going on would likely lead to much mis-speculation and ultimately lower speed
specabort:  rts

specdone:  ;sec
            ror SPECWRAPSECTOR
            rts

            .assert * <= BLOCKBUFFER71, error, "***** 1571 drive code too large. *****"

dcodinit71: lda #ATNA_OUT | CLK_OUT; signal idle to the host with ATN_IN clear
            sta VIA1_PRB

           ;ldx ROMOS_TRACK_DIFF
            txa
            ldx #.lobyte(topofstack71)
            txs
            pha

            lda #T1_FREE_RUNNING | PA_LATCHING_ENABLE; watchdog IRQ: count phi2 pulses, 16-bit free-running,
            sta VIA2_ACR                             ; port a latching should not be needed here
                                                     ; (IC rather than discrete logic),
                                                     ; but it is enabled just to be sure
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

            lda #MAXTRACK71 + 1
            sta MAXTRACK
            lda #NUMTRACKS_A
            sta MAXTRACK_A

.if ::PLATFORM = diskio::platform::COMMODORE_128
            lda #5       ; default burst timing is 6, anything below 5 yields transfer
            sta CIA_TA_LO; errors, hard-wired setting for 1571CR's MOS5710 is also 5
           ;lda #0
           ;sta CIA_TA_HI
.endif; ::PLATFORM = diskio::platform::COMMODORE_128

            ; watchdog initialisation
            lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS
            sta VIA1_IER; no IRQs from VIA 1
            sta VIA2_IER; no IRQs from VIA 2
            sta CIA_ICR; no IRQs from CIA
            bit CIA_ICR

TIMER_VAL = IRQ_SET_FLAGS | IRQ_TIMER_1; $c0
            ldx #TIMER_VAL
.if ::DISABLE_WATCHDOG
            bit VIA2_IER; timer 1 IRQs from VIA 2
.else
            stx VIA2_IER; timer 1 IRQs from VIA 2
.endif
           ;ldx #$c0
            stx CLEARSECTORLINKTABLE71
            stx NUMFILES71

            ; fade off the busy LED if lit
            lda #.lobyte(~MOTOR)
            and (.lobyte(VIA2_PRB - TIMER_VAL),x); VIA2_PRB
            sta (.lobyte(VIA2_PRB - TIMER_VAL),x); VIA2_PRB
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection
            and #BUSY_LED
            beq :+
            lda #$ff
:           sta LEDSTATE71

:           jsr fadeled
            bne :-

            jsr twomhz

            lda #MAXNUMSECTORS
            sta NUMSECTORSONTRACK
            lda #INITIALSPECINTERLEAVE
            sta SPECULATIVEINTERLEAVE

            ldy #$ff; invalid track number -> no step but turn on motor
            pla; ROMOS_TRACK_DIFF
            bne stepperok; branch if the drive had already seeked before the loader has been started

            ; the drive was reset immediately before running the loader -
            ; step down a track: this works normally if the stepping bits are congruent with the stepper motor;
            ; however, it may happen that the bits are misaligned (opposite to the actual stepper position, bit 1
            ; reversed), this alone does not move the head but stepping makes it go into the direction opposite to
            ; the one desired when moving; the stepping down two halftracks will actually step up and step down one
            ; halftrack each and thus will end up on the same track as before, but align the stepper bits to the motor.

            ldy #2
            sty CURRTRACK71
            dey
stepperok:  jsr trkseek71

            ldx #findtrkned - findtrkerr
:           lda findtrkerr - 1,x
            sta FINDTRACKN - 1,x
            dex
            bne :-

            .assert (FINDTRACKN + findtrkned - findtrkerr) <= enddirbuffer, error, "***** 1571 findtrackno too large. *****"

            stx REQUESTEDSECTOR
            jmp findtrckno - findtrkerr + FINDTRACKN

findtrkerr: dec bitratecnt
            bne findtrknum
            lax VIA2_PRB
            axs #$0100 - (1 << BITRATE_SHIFT)
            stx VIA2_PRB; cycle through the 4 bit-rates

            dec ERRORCOUNT
            bne :+
            lda #OPC_RTS
            sta seekswitch
            jsr halftrkdwn
            jsr twomhz
            lda #OPC_TAX
            sta seekswitch
findtrckno: lda #ERR
            sta ERRORCOUNT

:           lda #BITRATECOUNT
            sta bitratecnt

            ; find current track number
findtrknum: lda #SUCCESSCOUNT
            sta successcnt

:           jsr getblkscan; any sector, no block link sanity check, store ID
            bcs findtrkerr
            ldx HEADERTRACK
            cpx CURRTRACK71
            stx CURRTRACK71
            bne findtrknum; on different track number, retry with same bit-rate
            jsr loadblock
            bcs findtrkerr
            dec successcnt; accept current track number only after some consecutive successful attempts
            bne :-

            lda #OPC_DEC_ZP
            sta chksumerr
            lda #OPC_BCC
            sta idswitch
            lda #OPC_LDX_ZP
            sta sanitychsw

            jsr initlinktb

            ldy #DIRTRACK
            jsr trkseek71
            jmp idleloop71
findtrkned:
drvcodeend71:

TRAMPOLINEOFFSET = $24; dgetrout - dinstall + 1

            .org * - TRAMPOLINEOFFSET

dinstall:   sei
            ldx ROMOS_TRACK_DIFF; $42
            txs

            lda #.lobyte(~TWO_MHZ); transfer in slow mode in order to be
            and VIA1_PRA          ; less susceptible to interference by
            sta VIA1_PRA          ; any passive drives running at 1 MHz

            lda #ATNA_OUT | CLK_OUT
            sta VIA1_PRB
            lda #VIA_ATN_IN_INPUT | VIA_DEVICE_NUMBER_OUTPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT
            sta VIA1_DDRB
:           lda VIA1_PRB; wait for ATN_IN set and DATA_IN clear
            lsr
            bcs :-
            lda #ATNA_OUT
            sta VIA1_PRB

            ldx #.lobyte(drvcodebeg71 - 1)
dgetrout:   inx

            .assert * >= drvcodeend71, error, "***** 1571 trampoline too low in memory. *****"

            bne :+
            inc dgetputhi
:           DRIVEGETBYTE 1571, getbyteinstall; there is no watchdog while installing
dgetputhi = * + 2
            sta a:.hibyte(drvcodebeg71 - 1) << 8,x
            cpx #.lobyte(drvcodeend71 - 1)
            bne dgetrout
            dec drvcodebeg71
            bne dgetrout

            tsx; ROMOS_TRACK_DIFF
            jmp dcodinit71

drvprgend71:
            .assert * <= $0800, error, "***** 1571 drive code too large. *****"

            .reloc
