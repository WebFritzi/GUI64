
.include "loader.inc"

.include "cpu.inc"
.include "via.inc"

.include "drives/drivecode-common.inc"

HEADERTRACK            = $18; fixed: OPC_CLC, HEADERTRACK = BLOCKINDEX_STATUS = SPECSTARTSECTOR
BLOCKINDEX_STATUS      = HEADERTRACK
SPECSTARTSECTOR        = HEADERTRACK
NUMSECTORSONTRACK      = $19; fixed
STACKBUFFER            = $20; 4 bytes
PREVBLOCKINDEXPLUS1    = $29; fixed: BITRATECOUNT
SPECULATIVEINTERLEAVE  = $38; fixed: OPC_SEC
MAXCONFIRMEDBLKIDXSEC  = $39; fixed: SUCCESSCOUNT
CURRBLOCKINDEX         = $54
ERRORCOUNT             = $58; fixed
DISKCHANGEBUFFER       = $56
ID041                  = $59
ID1                    = $5a; must be ID041 + 1
LOADEDSECTOR           = $5c
BLOCKINDEXBASE         = $5e
NEXTDIRBLOCKSECTOR     = $66
SPECWRAPSECTOR         = $6c; SPECWRAPSECTOR = CLEARINDEXMASK
CLEARINDEXMASK         = SPECWRAPSECTOR
SPECWRAPSECTORINDEX    = $6e; SPECWRAPSECTORINDEX = CLEARIDXMIN
CLEARIDXMIN            = SPECWRAPSECTORINDEX
FILENAMEHASHLO         = $70
FILENAMEHASHHI         = $71
MAXCONFIRMEDBLOCKINDEX = $72
NUMCONTIGUOUSBLOCKS    = $74
FILESECTOR41           = $76; FILESECTOR41 = NEXTCONTIGUOUSBLOCK = CYCLESTARTENDSECTOR
NEXTCONTIGUOUSBLOCK    = FILESECTOR41
CYCLESTARTENDSECTOR    = FILESECTOR41
NUMFILES               = $78; fixed: OPC_SEI
CURRTRACK41            = $7a; fixed: OPC_NOP

SECTORLINKTABLE41      = $3e; INDEXTABLE + MAXNUMSECTORS; length is MAXNUMSECTORS = $15 bytes

DIRBLOCKPOS            = $e0
FILEDIRBUFFERINDEX     = $e1
FILENAMEHASHVALSHI     = $e2; length is DIRBUFFSIZE

LEDSTATE41             = FILENAMEHASHVALSHI + DIRBUFFSIZE; must be INDEXTABLE - 2: see trackinit
REQUESTEDSECTOR41      = LEDSTATE41 + 1; = FILETRACK, must be INDEXTABLE - 1: see trackinit
FILETRACK              = REQUESTEDSECTOR41
INDEXTABLE             = REQUESTEDSECTOR41 + 1; length is MAXNUMSECTORS = $15 bytes
FILENAME41             = INDEXTABLE; max. 16 bytes

DIRBUFFSIZE            = 11

LINKTRACK41            = .lobyte(decode2 + 1)
LINKSECTOR41           = .lobyte(decode6 + 1); LINKSECTOR41 = TEMP = BLOCKSIZE
TEMP                   = LINKSECTOR41
BLOCKSIZE              = TEMP
GENSPECLIMIT           = .lobyte(decode1 + 1)
CURRSTEPSPEEDLOW       = .lobyte(decode4 + 1)

BLOCKBUFFER41          = $0100

DECODETABLE            = $0700; effectively starts at $0722

DIRTRACKS              = $0703
DIRSECTORS             = DIRTRACKS + DIRBUFFSIZE

ROMOS_TRACK_DIFF       = $42

MAXINTERLEAVE          = 16
MAXNUMSECTORS          = 21
MAXTRACK41             = 41

TIMER                  = VIA2_T1C_H
TIMERLO                = VIA2_T1C_L
TIMER2                 = VIA1_T1C_H
TIMER2LO               = VIA1_T1C_L

INDEXSPECULATIVE       = %01000000
BLOCKPENDING           = %00100000

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

NMS = MAXNUMSECTORS; NUMSECTORSONTRACK initial value
NFL = $ff          ; NUMFILES initial value
SIL = 4            ; SPECULATIVEINTERLEAVE initial value

V1  = VIA1_PRB
V1B = $07
V1B41 = V1B
V2  = VIA2_PRB
V2B = $0f
V2B41 = V2B

_E0 = $0b; fixed
EF  = $2e; fixed, ~ATNA_OUT
FE  = $30; fixed

T1  = TIMER
TI1 = $11
T2  = TIMER2
TI2 = $27

BL  = STACK - $1a; offset for number of file blocks in directory sector
BLP = $68; fixed
ST  = STACK
STK = $69; fixed

CLEARSECTORLINKTABLE = STK; fixed
SF  = STACK + $fe
SFE = FE; fixed

ERR = $80; ERRORCOUNT initial value, $40 is too little: would cause stepping when spinning up
ERT = $20; ERRORCOUNT retry value

BTR = 4; number of read attempts before bit-rate cycling on initial current track retrieval
SCN = 10; number of consecutive successful per-bit-rate attempts on initial current track retrieval
BITRATECOUNT = PREVBLOCKINDEXPLUS1
SUCCESSCOUNT = MAXCONFIRMEDBLKIDXSEC

___ = REPOSITORY_VERSION; will be initialised dynamically

            .org 3

.export mdCopyScan = getblkscan
.export mdCopyEOF = DECODETABLE+.lobyte(finishload)
.export mdCopyLinkTrack = LINKTRACK41, mdCopyLinkSector = LINKSECTOR41
.export mdCopyImage = drivecode41, mdCopyBase = drvcodebeg41
.export mdCopyHash = gethashval, mdCopyHashEnd = wdogentr41-3
.export mdCopyOpen = findfile41, mdCopyUninstall = uninstall
.export mdCopyLoad = loadfile41, mdCopyWait = waitready, mdCopyWaitEnd = timeout
.export mdCopyBlockLoop = blockloop, mdCopyServe = servecache, mdCopyClose = closefile
.export mdCopyNext = toblksent, mdCopyCache = mdCopyCacheHook
.export mdCopySpec = mdCopySpecHook, mdCopyFind = mdCopyFindHook, mdCopyFindEnd = mdCopyFindEndHook
.export mdCopyFirst = mdCopyFirstHook, mdCopyInc = mdCopyIncHook, mdCopyControl = mdCopyControlHook

drvcodebeg41:
            .byte .hibyte(drvcodeend41 - * + $0100 - $01); init transfer count hi-byte

            ;                         $d0 doubles as JOBCODE_JUMP to $0700 to handle the watchdog IRQ
            .byte                     $d0, $40, $90, $00, >V1, $20, $60, $e0, $c0, $a0, $80, $60; ZP $00..$03: end of INDEXTABLE
            .byte >V2, <T1, $1f, $3e, $9f, $3f, $8f, $37, ___, NMS, $5f, $3a, $df, $3b, $cf, $33
            .byte ___, ___, ___, ___, $bf, $3d, $af, $35, >T2, BTR, $6f, $30, $ff, $39, $ef, $31
            .byte <SF, >SF, $0f, $36, $3f, $3c, $2f, $34, SIL, SCN, $4f, $32, $7f, $38

findtrkerr: dec BITRATECOUNT
            beq :+
            jmp findtrknum

:           lax VIA2_PRB
            axs #$0100 - (1 << BITRATE_SHIFT)
            stx VIA2_PRB; cycle through the 4 bit-rates
            lda #BTR
            jmp findtrkstp

            .byte           ___, $2e, ___, $2f, ___, $27, ERR, ___, ___, $2a, ___, $2b, ___, $23

todataread: bne readcontd                  ;  54
            jmp dataread                   ;  57  59  66  70

            .byte                          $2d, ___, $25, <BL, <ST, >ST, $20, ___, $29, ___, $21

tofndtrerr: bcc findtrkerr; jmp

            .byte           ___, $26, ___, $2c, ___, $24, NFL

            ;                      bit-rates %11 %10 %01 %00
readloop00: .byte OPC_NOP_ABS              ;               9 ;              ; nop $22xy
            .byte                                                   ___, $22
readloop01: php                            ;           8  12
            .byte                                                                  $28; plp
           ;plp                            ;          12  16
readloop10:
readloop11:
_02 = * + 2; .hibyte(DECODETAB6) = 2
decode6:    adc DECODETAB6,y               ;   7   9  16  20 ;   --6-5566 -> $x0 ; y = --6-----, lsb = ----5566
            pha                            ;  10
V2A = * + 1
            lax VIA2_PRA                   ;  14  16  23  27 ;   00000111   ; 0 - %11: [0..25], %10: [0..27], %01: [0..29], %00: [0..31]
            eor #%11111000                 ;  16
            sax .lobyte(decode1 + 1)       ;  19             ;   -----111
_09 = *; OPC_ORA_IMM = $09
            ora #%00000111                 ;  21             ;   00000HHH
            tay                            ;  23
            ldx #%00111110                 ;  25
            lda VIA2_PRA                   ;  29  31  38  42 ;   11222223   ; 1 - %11: [26..51], %10: [28..55], %01: [30..59], %00: [32..63]
            sax .lobyte(decode2 + 1)       ;  32             ;   --22222-
            alr #%11000001                 ;  34             ;   -11-----:3
            tax                            ;  36
            lda DECODETAB0 - %00000111,y   ;  40             ;   00000HHH -> $x0
decode1:    eor DECODETAB1,x               ;  44             ;   -11--111 -> $0x, x = -11-----, lsb = -----111
            tsx                            ;  46
            pha                            ;  49
densty11sw: beq todataread                 ;  51  57  64  68 ;              ; %11: beq todataread - %10, %01, %00: bvs todataread
readcontd:  eor STACK + 1,x                ;  55
            eor STACK + 2,x                ;  59
            eor STACK + 3,x                ;  63
checksum:   eor #0                         ;  65
readdata:   sta checksum + 1               ;  68             ;              ; entry
            lda VIA2_PRA                   ;  72  78  85  89 ; 3:33334444   ; 2 - %11: [52..77], %10: [56..83], %01: [60..89], %00: [64..95]
            ldx #%00001111                 ;  74
            sax .lobyte(decode4 + 1)       ;  77             ;   ----4444
            arr #%11110000                 ;  79             ;   33333---
            tay                            ;  81
decode2:    lda $00                        ;  84             ;   --22222- -> $x0
            eor DECODETAB3,y               ;  88             ;   33333--- -> $0x
            pha                            ;  91
            lda VIA2_PRA                   ;  95 101 108 112 ;   45555566   ; 3 - %11: [78..103], %10: [84..111], %01: [90..119], %00: [96..127]
            sax .lobyte(decode6 + 1)       ;  98             ;   ----5566
            alr #%11111100                 ; 100             ;   -455555-   ; clc
            tax                            ; 102
decode4:    lda $00                        ; 105             ;   ----4444 -> $x0, partial
            adc .lobyte(DECODETAB5),x      ; 109             ;   -455555- -> $xy, partial
            pha                            ; 112
            lax VIA2_PRA                   ; 116 121 128 132 ;   66677777   ; 4 - %11: [104..129], %10: [112..139], %01: [120..149], %00: [128..159]
            alr #%01000000                 ; 118             ;   --6-----   ; clc
            tay                            ; 120
            adc DECODETAB7,x               ; 124 130 137 141 ;   66677777 -> $yx, partial, clv
                                           ;[130 140 150 160]
densityswt: bvs readloop11                 ;   3   2   2   2 ; is changed to bvc * for bit-rates %10, %01, %00
            bvs readloop11                 ;   3   5   5   5 ; branch to readloop[00|01|10|11]
bitrateswt   = * - 1
READLOOPBASE = *
            bvs readloop11                 ;   3
            bvs readloop11                 ;   3
            bvs readloop11                 ;   3
            jmp readloop11                 ;   3

            .byte ___; padding

findtrknum: lda #SCN; number of attempts on initial current track retrieval
            sta SUCCESSCOUNT; reset attempt counter
:           jsr getblkscan; any sector, no sector link sanity check, store ID
bcctofndte: bcc tofndtrerr
            cmp CURRTRACK41
            sta CURRTRACK41
            bne findtrknum; on different track number, retry with same bit-rate
            lda #.lobyte(:+ - 1)
            sta STACKBUFFER + 2; return address lo
            jsr readblock
:           bcc bcctofndte
            dec SUCCESSCOUNT; accept current track number only after some consecutive successful attempts
            bne :--

            jmp toidleloop

            ; * = $0100

            .byte 0
            .byte 0; number of blocks (low-byte) of file #8 in loaded dir block
            .byte 0; header sector number
stackbottom:
            .assert stackbottom >= (STACK + 3), error, "***** 1541 stack below $0103. *****"
            .assert stackbottom <= (STACK + 3), error, "***** 1541 stack above $0103. *****"

            .word 0; rawtoser etc. return address
            .word 0; getblock etc. return address
stackend:
topofstack41 = stackend - 1

            .assert stackend >= (STACK + 7), error, "***** 1541 top of stack below $0107. *****"
            .assert stackend <= (STACK + 7), error, "***** 1541 top of stack above $0107. *****"

stepperfix: ; here, the drive was apparently reset immediately before running the loader -
            ; step down a track: this works normally if the stepping bits are congruent with the stepper motor.
            ; however, it may happen that the bits are misaligned (opposite to the actual stepper position, bit 1
            ; reversed), this alone does not move the head but stepping makes it go into the direction opposite to
            ; the one desired when moving. the stepping down two halftracks will actually step up and step down one
            ; halftrack each and thus will end up on the same track as before, but align the stepper bits to the motor.

            ldy #2
            sty CURRTRACK41
            dey
            jsr trkseek41

stepperok:  lda #OPC_STA_ZPX
            sta initlinklp
            ldy #DECTABOFFS
:           lda decodetab,y
            sta DECODETABLE,y
            iny
            bne :-
            beq findtrknum; jmp

            .byte 0; padding

DECTABOFFS = $22
decodetab  = * & $ff00

decoffs0   = $2d
decoffs1   = $00
decoffs2   = $00
decoffs3   = $01
decoffs5   = $01
decoffs7   = $01

DECODETAB0 = DECODETABLE + decoffs0
DECODETAB1 = DECODETABLE + decoffs1
DECODETAB2 = decoffs2
DECODETAB3 = DECODETABLE + decoffs3
DECODETAB5 = decoffs5
DECODETAB7 = DECODETABLE + decoffs7

BRANCHTAB  = DECODETABLE + $92

            .assert .lobyte(*) >= DECTABOFFS, error, "***** 'decodetab' data is below in-page offset $22. *****"
            .assert .lobyte(*) <= DECTABOFFS, error, "***** 'decodetab' data is above in-page offset $22. *****"

BR0 = .lobyte(readloop00 - READLOOPBASE)
BR1 = .lobyte(readloop01 - READLOOPBASE)
BR2 = .lobyte(readloop10 - READLOOPBASE)
BR3 = .lobyte(readloop11 - READLOOPBASE)

            .byte           $b4, $b0, $ba, $ba, $bc, $b8

senddone:   bcc donesend; jmp

            .byte                                                   $6e, $6f, $67, $d3, $6a, $6b
            .byte $63, $b0, $26, $6d, $65, $2a, $60, $69, $61, $6c, $66, $6c, $64, $1a, $62, $68
            .byte $9c, $99, $b5, $b1, $b7, $b3, $b6, $b2, $f3, $e1, $5e, $5f, $57, $7a, $5a, $5b
            .byte $53, $e0, $c4, $5d, $55, $6a, $50, $59, $51, $e8, $56, $5c, $54, $5a, $52, $58
            .byte $65, $bf, $bd, $b9, $bf, $bb, $be, $28, $e1, $e5, $4e, $4f, $47, $ba, $4a, $4b
            .byte $43, $e4, $ee, $4d, $45, $aa, $40, $49, $41, $ec, $46, $4c, $44, $3a, $42, $48

donesend:   lda #ATNA_OUT | CLK_OUT; drive busy, entry
            sta (.lobyte(V1B - $ef),x); VIA1_PRB
            bcs fileclosd2
fileclosed:
waitsent:   lda (.lobyte(V1B - $ef),x); VIA1_PRB
            bpl waitsent
            bcs donesend

            sei; disable watchdog

            .byte                                                                  $fa; nop

            lax NUMCONTIGUOUSBLOCKS; ok: $00, file not found: $ff
            .byte OPC_STA_ABS; sta $a2e2

            .byte      $e2, BR0; nop zp, but older 1541U firmwares do not emulate this correctly

            bne checkfnf

            .byte                          $ea; nop

            dec prpnxtfjmp; OPC_EOR_ABS -> OPC_JMP_ABS

            .byte                                              $ea; nop

            ldx FILEDIRBUFFERINDEX; prepare next file, only after successful load
            lda FILENAMEHASHVALSLO + 1,x
            ldy FILENAMEHASHVALSHI + 1,x
mdCopyCacheHook:
            jmp setnextfile; cache first block of next file

toblksent:  ldy FILETRACK
            jmp blocksent

            .byte                                              $ef, $2e, $2f, $27, $9a, $2a, $2b
            .byte $23, $e6, BR1, $2d, $25, $8a, $20, $29, $21, $ee, $26, $2c, $24, $0a, $22, $28

checkfnf:   inx; file not found: $ff -> $00
            bne toblksent

            SKIPBYTE
fileclosd2: tya; fileclosed: FILEDIRBUFFERINDEX, ok: >= 0, file not found: $ff
            tax; checkfnf: file not found: $00 -> $ff
            jmp mayfadeled; with N = 1, force fading off LED

            .byte                                              $e9, $1e, $1f, $17, $da, $1a, $1b
            .byte $13, $e3, BR2, $1d, $15, $ca, $10, $19, $11, $eb, $16, $1c, $14, $4a, $12, $18

finishload: sty NUMCONTIGUOUSBLOCKS; entry, ok: $00, file not found: $ff
            tya; status
            tax
            dex; stx BLOCKSIZE
            iny; y < BLOCKSIZE to send over one byte, ok: $01 < $ff, file not found: $00 < $fe
            jmp sendstatus

            .byte                                              $ed, $0e, $0f, $07, $94, $0a, $0b
            .byte $03, $e7, BR3, $0d, $05, $dc, $00, $09, $01, $29, $06, $0c, $04, $92, $02, $08

            ; * = $0200
DECODETAB6:
bitrates:   .byte .lobyte(~(%11 << BITRATE_SHIFT)); tracks 30+,   17 sectors
            .byte .lobyte(~(%10 << BITRATE_SHIFT)); tracks 25-30, 18 sectors
            .byte .lobyte(~(%01 << BITRATE_SHIFT)); tracks 18-24, 19 sectors
            .byte                $07
            .byte .lobyte(~(%00 << BITRATE_SHIFT)); tracks  1-17, 21 sectors

            .byte                         $90, $60, $80, $1f, $90, $60, $80, $69, $90, $60, $80

            ; the raw <-> serial mapping swaps bits 0 and 3 of the lower nibble and
            ; XORs the result with $7f, so it is the same for both encoding and decoding

sertorawdt: sta TRACKLINKTABLE,x
sertorawd:  dey
sertorawr:  lda (STK),y; STACK
sertoraw41:
rawtoser:   bit _09
            beq :++
:           eor #%00001001
            bit _09
            beq :-
:           eor #%01111111
            sta HEADERTRACK; = BLOCKINDEX_STATUS = SPECSTARTSECTOR
            rts

            .byte                          $00, $e0, $60, $fe, $00, $e0, $60

trkseek41:  .byte OPC_LDA_IMM; lda #0

            .byte                                                                  $00, $e0, $60; cpx #$60

trackseekx: ldx #$80 | (MINSTEPSPEED + 1)
            sax CURRSTEPSPEEDLOW; stores $0x
            jsr setbv2b41
trackstep:  ; when single-stepping, CURRSTEPSPEEDLOW = decode4 + 1 (----4444) is not reset: it's assumed to be $0x
            cpy CURRTRACK41
            beq setbitrate; branch if on same track

nexttrack:  bcc :+
            inc CURRTRACK41; move up (inwards)
            SKIPWORD
:           dec CURRTRACK41; move down (outwards)
halftrkdwn: lda #MOTOR / 4
            rol
            asl; $04: move down (outwards), $06: move up (inwards)
halftrack:  sta ERRORCOUNT; will be left at $84/$86, $40 is too little: would cause stepping when spinning up
            eor VIA2_PRB
           ;clc
            jsr exectrseek
            cpx #($80 | SINGLESTEPSPEED) - 1
            beq nostepwait; stepping to adjacent track: branch when second half-track step has been issued

            stx TIMER; reset track-step timer
            txa
headaccl:   cmp #$80 | MAXSTEPSPEED
            beq noheadacc
            pha
           ;sec
            lda CURRSTEPSPEEDLOW
            sbc #STEPPERACC
            sta CURRSTEPSPEEDLOW
            pla
            sbc #0

noheadacc:  cpx TIMER
            beq noheadacc; wait until the counter hi-byte has decreased by 1
            dex
            bmi headaccl
            tax

nostepwait:;sec
            lda #$80 - 1
            adc ERRORCOUNT
seekswitch: bmi halftrack; set to rts when stepping a single half-track during initialisation
            cpy CURRTRACK41
            bne nexttrack; branch if not on destination track

initlinktb: lsr CLEARSECTORLINKTABLE
            bcs setbitrate

initlink41: lda #$ff; sector link unknown
            ldx #MAXNUMSECTORS - 1
initlinklp: lda SECTORLINKTABLE41,x; is changed to sta SECTORLINKTABLE41,x after init, sector links are unknown
            dex
            bpl initlinklp
            sta LOADEDSECTOR

            ; bit-rates:
            ; tracks 31+   (17 blocks): %00 - sector interleave 3 (lowest density, slowest clock, innermost tracks)
            ; tracks 25-30 (18 blocks): %01 - sector interleave 3
            ; tracks 18-24 (19 blocks): %10 - sector interleave 3
            ; tracks  1-17 (21 blocks): %11 - sector interleave 4 (highest density, fastest clock, outermost tracks)

setbitrate: lax _E0; $e0 = SYNC_MARK | BITRATE_MASK, INDEXSPECULATIVE
            ora (.lobyte(V2B - $e0),x); VIA2_PRB
           ;ldy CURRTRACK41
            jsr getnumscts
            inx
            stx NUMSECTORSONTRACK

            SKIPWORD
motrledoff: and #255 - (BUSY_LED | MOTOR)
            ldy #0
            beq store_via2; jmp

.ifdef GUI64_DIRECTORY_COMPAT
            ; Stop the idle drive/LED immediately. Reuse the former fading
            ; delay for the directory check; keep every following address.
fadeled:    lda VIA2_PRB
            jmp motrledoff
mdLastBlockSize41:
            ldy CURRTRACK41
            cpy #DIRTRACK
            beq :+; the final directory link byte is not a payload size
            cmp #1; accept an empty file, keep rejecting zero-filled speculative sectors
:           ldy LINKTRACK41
            rts
            nop; packed drive-code layout
.else
fadeled:    ldx #255 - BUSY_LED
            lda (.lobyte(V2B - (255 - BUSY_LED)),x); VIA2_PRB
            ldy LEDSTATE41
            beq motrledoff
:           dey
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"

            sax (.lobyte(V2B - (255 - BUSY_LED)),x); VIA2_PRB
            ldy LEDSTATE41
:           iny
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"
.endif

            .assert * >= $02a9, error, "***** 1571 watchdog IRQ vector in 1541 drivecode located below $02a9. *****"
            .assert * <= $02a9, error, "***** 1571 watchdog IRQ vector in 1541 drivecode located above $02a9. *****"

            .word uninstall; uninstall = cmp $04, relevant for option ONLY_1541_AND_COMPATIBLE

            .assert .lobyte(uninstall) = OPC_CMP_ZP, error, "***** Invalid 1571 IRQ vector optimisation. *****"

            dec LEDSTATE41

setbv2b41:  ora VIA2_PRB; setbv2b41 must not clobber y, see trkseek41
turnmotron: ora #MOTOR
store_via2: sta VIA2_PRB; store_via2 must not clobber y, see motrledoff/exectrseek
            rts

dataread:   ; read and decode ID0 (header) or checksum (data)
           ;ldx #0
            lda (V2A,x) ; VIA2_PRA     ;  63  65  72  76 ; 3:33334444   ; 2 - %11: [52..77], %10: [56..83], %01: [60..89], %00: [64..95]
            ror                        ;                 ;   33333LHL:H
            tay
            lda DECODETAB3 - 2,y       ;                 ;   33333--- -> $0x
            ldy decode2 + 1            ;                 ;   --22222-
            eor DECODETAB2,y           ;                 ;   --22222- -> $x0
            tay                        ; header: ID041, data: checksum
:           eor STACK,x                ; header: ID1 track sector checksum
            inx
            cpx #4                     ; = .lobyte(stackend - 3)
            bne :-
            eor checksum + 1           ; is 0 with header
            eor #$80
            cmp #$ff                   ; c = 1: checksum matches
            txs

            bit LOADEDSECTOR
            bpl blockread

            ; process block header

            bcc chksumerr

            pla; STACK + 5, return address lo
            sta STACKBUFFER + 2
            pla; STACK + 6, return address hi
            sta STACKBUFFER + 3
            txs; $04

            ; header: checksum sector track ID1 ID0
            lda (.lobyte(STK - $04),x); STACK, ID1
            cpy ID041
            bne :+
            cmp ID1
            beq :++
:           ldx REQUESTEDSECTOR41; set ID only when reading the BAM sector

IDMM_ARG = idmismatch - (idmmbranch + 2)
idmmbranch: .byte OPC_BIT_ZP, .lobyte(IDMM_ARG); is changed to bne idmismatch after init

            .assert IDMM_ARG >= -128, error, "***** idmismatch branch too far. *****"
            .assert IDMM_ARG <=  127, error, "***** idmismatch branch too far. *****"

            sty ID041
            sta ID1

:           ldy #2
            jsr sertorawr; STACK + 2, sector
            cmp NUMSECTORSONTRACK; check if sector number is within range of
            bcs chksumerr        ; the allowed sector numbers for the current track
            sta LOADEDSECTOR

            jsr sertorawd; STACK + 1, track
           ;sta HEADERTRACK
            beq chksumerr
            cmp #MAXTRACK41 + 1
            bcs chksumerr

trkchkswtc: sec; is changed to lax LOADEDSECTOR after init
            rts

            eor REQUESTEDSECTOR41
            beq readblock; branch if requested sector
            bpl findblkhdr; specific sector requested but not reached yet

            ; negative value: no specific sector requested, out-of-order sector fetch

           ;ldx LOADEDSECTOR
            lda INDEXTABLE,x
            ldy SECTORLINKTABLE41,x
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

readblock:  ldy #%01010101;11 = $07, $55
            jsr waitsyncdb   ; 17 ; returns with x = $fe for inx : txs
            inx              ; 19
            bvc readblkdat   ; 22

chksumerr:  ldx #ERT
            lda (.lobyte(V2B - ERT),x); VIA2_PRB
            anc #BITRATE | BUSY_LED | TRACK_STEP; anc is okay, as this code is not reached with 1541U
chksumswtc: lda ERRORCOUNT; is changed to dec ERRORCOUNT after init
idmismatch: bne checkchg

            ; with repeated checksum errors, the r/w head might have landed between tracks or on the wrong track
            stx ERRORCOUNT
           ;clc
            sbc #(1 << BITRATE_SHIFT) - 1; cycle to the next bit-rate (denser until wrap)
exectrseek: adc #$03; and step down half a track on bit-rate wrap
            jmp turnmotron; checksum error: z = 0, c = 0

blockread:  ; LINKTRACK41 must not be 0, see nostep
           ;stx LINKTRACK41; = decode2 + 1, which is non-0 at this point

            ; swap stack with loaded data
           ;ldx #4
:           ldy stackbottom - 1,x
            lda STACKBUFFER - 1,x
            sta stackbottom - 1,x
            sty STACKBUFFER - 1,x
            dex
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in swap stack <-> data loop. *****"

            bcc chksumerr; branch on checksum mismatch

            lax HEADERTRACK
            eor CURRTRACK41
            tay
sanitychsw: beq sanitychck - 1; is changed to beq sanitychk after init

            ; track error
            ldy CURRTRACK41
            stx CURRTRACK41
            inc CLEARSECTORLINKTABLE; is reset in trkseek41

getblktrsk: jsr trkseek41; stores the number of blocks on the current track in NUMSECTORSONTRACK
getblkscan:
findblkhdr: lax FE; $fe
            sta LOADEDSECTOR; reset header/data flag to header
            sta (.lobyte(TI1 - $fe),x); TIMER, reset sync time-out

            ldy #%01010010;01 = $08, $52
:           jsr waitsynchb   ; 17
            bvs :-           ; 19
            ldx #3           ; 21

readblkdat: bvc *            ;  2

            .assert .hibyte(*) = .hibyte(* - 2), error, "***** Page boundary crossing in bytesync loop. *****"

            alr #%11000001   ;  4    ;   --------:3
            bne chksumerr    ;  6
denstyhalf: bvs :+           ;  9  8 ; bit-rate %11: bvs (branch taken), bit-rates %10, %01, %00: bvc (branch not taken)
            pha              ;    11
:           txs              ; 11 13
            jmp readdata     ; 14 16

           ;lda VIA2_PRA     ; 21 23 ; 3:33334444 ; 2 - cycle 21 is  -4 in  [0..25], cycle 23 is -4 in  [0..27]
           ;lda VIA2_PRA     ; 44 46 ;   45555566 ; 3 - cycle 44 is  -7 in [26..51], cycle 46 is -9 in [32..55]
           ;lax VIA2_PRA     ; 65 67 ;   66667777 ; 4 - cycle 65 is -12 in [52..77], cycle 67 is +3 in [64..83]
           ;adc DECODETAB7,x ; 73 75 ;   66667777 ;     cycle 73 is  -4 in [52..77], cycle 75 is -8 in [64..83]

getblkbam = * - 2; readdata = $00ba = lax #0: read sector 0 of dir track

            .assert .lobyte(readdata) = OPC_LAX_IMM, error, "***** Invalid getblkbam optimisation. *****"

getblkdir:  ldy #DIRTRACK
getblock41: sta REQUESTEDSECTOR41
            bpl getblktrsk; jmp

linkerror:  ; invalidate index
            asl INDEXTABLE,x
            bmi checkchg; branch if INDEXSPECULATIVE is set: clear index, set MSB
            ror INDEXTABLE,x; restore non-speculative index

            SKIPWORD; skip to NUMFILES = sei
:           sec            ; refill directory buffer when disk has been changed
            ror NUMFILES   ; NUMFILES = $78 = OPC_SEI
readerror:  ; sync timeout or invalid track/sector link
checkchg:   ldx #.lobyte(stackend - 3); $04
            txs
            lax (V2B - $04,x); VIA2_PRB, check light sensor for disk removal
            eor DISKCHANGEBUFFER
            stx DISKCHANGEBUFFER
            alr #WRITE_PROTECT; clc
            bne :-
            ; read error: z = 1, c = 0
            rts

sanitychck:;ldy #0
           ;sec
            ldx LOADEDSECTOR
            lda SECTORLINKTABLE41,x
            bpl blkreturn; branch if valid sector link to this track known already, no sanity check required, save some
                         ; cycles to catch next block in time when not transferring the currently loaded block
            jsr sertorawd
            sta LINKTRACK41
           ;sta TRACKLINKTABLE,x

            jsr sertorawdt; sector link or block size
            sta LINKSECTOR41

           ;ldy #$fe
            sty SECTORLINKTABLE41,x; mark block as loaded (linked sector is known)

            ; sector link sanity check
.ifdef GUI64_DIRECTORY_COMPAT
            jsr mdLastBlockSize41
            nop; same size as the original cmp/ldy pair
.else
            cmp #2
            ldy LINKTRACK41
.endif
            beq chklastbsz; branch if file's last block

            cpy #MAXTRACK41 + 1; check whether track link is within the valid range
            bcs linkerror

getnumscts: ldx #21 - 1; number of blocks minus one
            cpy #18
            bcc :+; bit-rate $60
            ldx #19 - 1
            cpy #25
            bcc :+; bit-rate $40
            dex; 18 - 1
            cpy #31
            bcc :+; bit-rate $20
            dex; 17 - 1
:           and bitrates - (17 - 1),x
            bmi return

            cpx LINKSECTOR41; check whether sector link is within the valid range
chklastbsz: ; branch if invalid block size (0..1 = 1..2 bytes)
            bcc linkerror; branch if sector number too large

           ;lda LINKSECTOR41 ; loaded block's sector link sector number or block size
            ldx LOADEDSECTOR ; return the loaded block's sector number
           ;ldy LINKTRACK41
            cpy CURRTRACK41
            beq sametrack
            asl           ; must not be $ff
            ora #%10000000; link is not to same track
sametrack:  cmp LOADEDSECTOR        ; block must not link to itself
            beq linkerror           ; (but larger cycles are not detected)
            sta SECTORLINKTABLE41,x ; set sector link
            sec; operation successful
blkreturn:  rts

readmod:    ldx #%11100000
            stx densty11sw
            axs #$70
            lda BRANCHTAB - $10,x
            sta bitrateswt
            cpx #%01110000; OPC_BVS, bit-rate %11
            lda #BR3 + 2
            bcs :+
            ldx #OPC_BVC; bit-rates %10, %01, %00
            lda #$fe; bvc *
:           stx densityswt
            sta densityswt + 1
            ror densty11sw; bit-rate %11: $f0 = OPC_BEQ, bit-rates %10, %01, %00: $70 = OPC_BVS
            stx denstyhalf

waitsyncdb: ldx #%11111110
            lda (.lobyte(TI1 - $fe),x) ; TIMER
            beq readerror
waitsynchb: lda (.lobyte(V2B - $fe),x) ; VIA2_PRB, wait for SYNC
            bmi readmod

            lda (.lobyte(V2A - $fe),x) ; VIA2_PRA, clear latch
            clv
            tya                        ; header: $52, data: $55
            bvc *                      ;  2
            cmp (.lobyte(V2A - $fe),x) ;  8, VIA2_PRA
            bne return                 ; 11, 17
            ror
            arr #%10000000             ; header: %01000000, data: %11000000
            clv
            bvc *                      ;  2
            eor VIA2_PRA               ;  6 ; 11222223, reads %-H222223 (header) or %HH222223 (data)
            clv                        ;  8
            sax decode2 + 1            ; 11 ; --22222-
return:     rts                        ; 17

            ; validate block indices according to currently-known links
idxloop41:  tax; link sector
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
specmanage: lda SECTORLINKTABLE41,x; linked sector
            bpl idxloop41; branch if there is a linked sector on the same track

            .assert .hibyte(*) = .hibyte(idxloop41), error, "***** Page boundary crossing in idxloop41 *****"

            stx MAXCONFIRMEDBLKIDXSEC
            sty MAXCONFIRMEDBLOCKINDEX
mdCopySpecHook:

            lda #255 - BLOCKPENDING
            SKIPWORD
rspeculate: ldy SPECWRAPSECTORINDEX
            stx SPECSTARTSECTOR
            bcc :+; branch if mis-speculated block index detected

            ; on first track, respeculate from last speculative index when it now has a known link
            lax SPECWRAPSECTOR
            ora SECTORLINKTABLE41,x
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
            lda SECTORLINKTABLE41,x
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

            FNAMEHASH 1541

wdogentr41:
:           inc CURRTRACK41
uninstall = * - 1; CURRTRACK41 = $7a = nop
            ldy #18; ROM dir track
            jsr trkseek41
           ;lda VIA2_PRB
            bit _02; the stepper bits are set to %00 after reset: ensure full-track
            bne :- ; stepper alignment (may still be off by a half-track)

            dec reset; OPC_ADC_ABS -> OPC_JMP_ABSI

           ;ldy #0
            dey
            sty LEDSTATE41

mayfadeled: bpl :+

fadeledidl: jsr fadeled
            bne fadeledidl

reset:      adc RESET_VECTOR

            SKIPBYTE; prpnxtfjmp = eor ($06,x)
            ; jump from prpnxtfjmp when drive has cached next file's first block
prpdnxtfil: inc prpnxtfjmp; OPC_JMP_ABS -> OPC_EOR_ABS

idleloop41: jsr fadeled
:           jsr checkchg; check light sensor for disk removal

           ;lda #0
            tax
            lda (V1B,x); VIA1_PRB
            bpl uninstall
            lsr
            bcs idleloop41; wait until there is something to do

            sta ERRORCOUNT; reset to >= $40, as spinning up the motor would yield errors

            FILENAME = FILENAME41
            GET_FILENAME 1541

            ; matches against hash of filename in FILENAMEHASHLO/HI
            NUMFILES41 = NUMFILES
            findfile41 = findfile
mdCopyFindHook:
            FIND_FILE 1541
mdCopyFindEndHook:

            ldy DIRSECTORS,x
            sty FILESECTOR41; = NEXTCONTIGUOUSBLOCK
            ldy DIRTRACKS,x
loadfile41:
filenotfnd: ; y = $ff = diskio::status::FILE_NOT_FOUND
           ;lda #0
            sta PREVBLOCKINDEXPLUS1
            sta NUMCONTIGUOUSBLOCKS

blocksent:  bmi blockloop
            bne trackloop

            ; loading is finished
            ; y = $00 = diskio::status::OK
            jmp DECODETABLE + .lobyte(finishload)

TRACKLINKTABLE: ; will be overwritten with TRACKLINKTABLE (MAXNUMSECTORS = 21 bytes)
            .byte 0; padding

toidleloop: lda #OPC_BNE
            sta idmmbranch
            lda #OPC_LAX_ZP
            sta trkchkswtc
            lda #LOADEDSECTOR
            sta trkchkswtc + 1
            lda #OPC_DEC_ZP
            sta chksumswtc

FILENAMEHASHVALSLO = * ; will be overwritten with FILENAMEHASHVALSLO (DIRBUFFSIZE = 11 bytes)

            inc sanitychsw + 1
            ldy CURRTRACK41
            jsr initlinktb
            jsr getblkdir; accu contains a negative number for any sector

NUMBLOCKS:  ; will be overwritten with NUMBLOCKS (DIRBUFFSIZE = 11 bytes)

            jmp idleloop41

            .byte 0, 0, 0, 0, 0, 0, 0, 0; padding

            brk; spare

trackloop:  sta BLOCKINDEXBASE

            lda #BUSY_LED | MOTOR
            jsr trackseekx

            ; accu is negative with INDEXSPECULATIVE set, sector indices are unknown
           ;ldx NUMSECTORSONTRACK
            inx
trackinit:  sta INDEXTABLE - 2,x
            dex
            bpl trackinit; sets REQUESTEDSECTOR41 = FILETRACK = INDEXTABLE - 1 with x = 1 and LEDSTATE41 = INDEXTABLE - 2 with x = 0

            .assert .hibyte(*) = .hibyte(trackinit), error, "***** Page boundary crossing in trackinit loop. *****"

            ldx FILESECTOR41; = NEXTCONTIGUOUSBLOCK
            cpx NUMSECTORSONTRACK; illegal sector is checked only here because NUMSECTORSONTRACK
            bcs findloop         ; is only here set correctly for the file's starting track

            ; set non-speculative block indices according to known links, build initial speculative block index table for this track
           ;ldx NEXTCONTIGUOUSBLOCK
           ;ldy #0; initial block index
            jsr speculinit

            lda FILESECTOR41; = NEXTCONTIGUOUSBLOCK
            cmp LOADEDSECTOR
            beq servecache

            SKIPWORD
idxinvalid:;sec
            ror INDEXTABLE,x; clear index, set MSB

blockloop:  jsr getblkscan
            bcc blockloop; branch if block fetch not successful

            ldy CURRBLOCKINDEX
            bmi blockloop; branch if block conceivably not belonging to the file
            beq refuspecil; if first file block on track: skip PREVBLOCKINDEXPLUS1 check

mdCopyFirstHook:
            ldy PREVBLOCKINDEXPLUS1; first file block must be loaded first in order to know the load address to be able to place successive
            beq blockloop          ; blocks in host memory, skip loaded blocks until the required one is read

            ; determine speculative interleave
           ;sec
           ;lda LINKSECTOR41
            tax
            sbc LOADEDSECTOR; determine likely interleave
            ; validate speculative interleave
            beq refuspecil
            cmp #MAXINTERLEAVE + 1
            bcs refuspecil
            ; only accept interleave if distances to this block and the next match
            tay
            adc LINKSECTOR41
            cmp NUMSECTORSONTRACK
            bcc :+
            sbc NUMSECTORSONTRACK
:           cmp SECTORLINKTABLE41,x
            bne refuspecil - 1
            sty SPECULATIVEINTERLEAVE; SPECULATIVEINTERLEAVE = $38 = OPC_SEC
refuspecil:
            ldx MAXCONFIRMEDBLKIDXSEC
            ldy MAXCONFIRMEDBLOCKINDEX
           ;sec; set no mis-speculation detected flag
            jsr specmanage
servecache:
            sec
            ldx LOADEDSECTOR

            .assert .lobyte(*) = OPC_EOR_ZPXI, error, "***** Invalid prpnxtfjmp optimisation. *****"

prpnxtfjmp: eor prpdnxtfil; is changed to jmp prpdnxtfil when caching the next file's first block

            lda #255 - BLOCKPENDING
            and INDEXTABLE,x
            sta INDEXTABLE,x

           ;sec
            sbc #INDEXSPECULATIVE
            ; c = 1 if index speculative, 0 otherwise

            bcc sendblock; branch if block index is not speculative

            sta CURRBLOCKINDEX; speculative index might have changed
            lda MAXCONFIRMEDBLOCKINDEX
            cmp CURRBLOCKINDEX; do not transfer block if another block has been confirmed to have the same or a
            bcs idxinvalid    ; higher index (this block's index is wrong or wouldn't be speculative any more)

sendblock:  lda NEXTCONTIGUOUSBLOCK
:           tax
            ldy INDEXTABLE,x
            cpy #MAXNUMSECTORS
            bcs :+; branch if block has not been confirmed and transferred already
mdCopyIncHook:
            inc NUMCONTIGUOUSBLOCKS
            lda SECTORLINKTABLE41,x
            bpl :-; branch if there is a linked sector on the same track

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in contiguous blocks update loop. *****"

            alr #%00011111 * 2
           ;clc
:           sta FILESECTOR41; = NEXTCONTIGUOUSBLOCK, first file sector on the linked track
            bcs :+

            ldy TRACKLINKTABLE,x
            sty FILETRACK
            beq nostep

            ; perform Shrydar Stepping (R)(TM) to minimise single-track stepping overhead:
            ; nudge the R/W head in the right direction, then wait a few bycles while it gains momentum,
            ; then enable the destination track's stepper magnet long before the head has reached the intermediate half-track's magnet,
            ; relying on the head's inertia, then send over the block while the head keeps moving beyond the intermediate half-track's stepper magnet
            ; and ultimately settles on the destination track.
            ; sending the block over takes at least 72 bycles

            ldx #$80 | SINGLESTEPSPEED
            jsr trackstep
nostep:
            ; calculate block index in host memory
            sec
:           lda BLOCKINDEXBASE
            adc CURRBLOCKINDEX
            tax
            sec
            sbc PREVBLOCKINDEXPLUS1
            stx PREVBLOCKINDEXPLUS1

            ldy #0
            cpy LINKTRACK41
            rol; last block: set lsb, clear lsb otherwise
            jsr rawtoser
            sta (SFE),y; STACK + $fe, block index and last block size/next contiguous block index flag

mdCopyControlHook:
            lda NUMCONTIGUOUSBLOCKS
            ldx #1
            asl LINKTRACK41
            bne :+
            ; handle file's last block
            txa
           ;clc
            sbc LINKSECTOR41; the file's last block's length (last byte offset)
            tax
:
            ; accu: next contiguous block index/block size
            dey; block: $00 -> $ff

sendstatus: stx BLOCKSIZE
            jsr rawtoser
           ;sta BLOCKINDEX_STATUS

            ; restore block buffer on stack
            ldx #4
:           lda STACKBUFFER - 1,x
            sta stackbottom - 1,x
            dex
            bne :-

            ; send block ready signal and wait for the signal to begin transferring the block
            lax EF; ~ATNA_OUT = $ef
            sta (.lobyte(TI2 - $ef),x); TIMER2, poll two cascaded timers: an extra-long time-out period is needed since
            sta (.lobyte(TI1 - $ef),x); TIMER,  the host may still be busy decompressing a large chunk of data

            sax (.lobyte(V1B - $ef),x); VIA1_PRB, clear ATNA_OUT, set DATA_OUT now, before clearing CLK, so that there is a flank only on CLK (but not DATA) when signalling ready,
            ; such that just one serial bus read can safely determine block ready (CLK clear) and drive present (DATA set) vs device not present (both clear)
            sre (.lobyte(V1B - $ef),x); VIA1_PRB, block ready signal: %1xy01111 -> %11xy0011, DATA_OUT as signal of presence remains set regardless of ATNA_OUT state

waitready:  lda TIMER2LO; see if the watchdog barked
            cmp TIMERLO
.if ::DISABLE_WATCHDOG
            bmi :+
:
.else
            bmi timeout; and trigger watchdog on time-out
.endif
            lda #CLK_IN
            bit VIA1_PRB
            bmi waitready; wait for ATN_IN clear

            .assert .hibyte(*) = .hibyte(waitready), error, "***** Page boundary crossing in block send wait loop. *****"

            bne closefile

            stx TIMER; reset watchdog time-out and clear possibly set IRQ flag
timeout:    ENABLE_WATCHDOG

            lda BLOCKINDEX_STATUS
            sax (.lobyte(V1B - $ef),x); VIA1_PRB

sendloop:   asl            ; 2
            ora #ATNA_OUT  ; 2
                           ; = 17

:           bit VIA1_PRB   ; 4
            bpl :-         ; 2
            sta VIA1_PRB   ; 4
            ror            ; 2
            alr #%11110000 ; 2
            stx TIMER      ; 4 - reset watchdog time-out
                           ; = 18

:           bit VIA1_PRB   ; 4
            bmi :-         ; 2
            sta VIA1_PRB   ; 4
            lsr            ; 2
            alr #%00110000 ; 2
            cpy BLOCKSIZE  ; 3
                           ; = 17

:           bit VIA1_PRB   ; 4
            bpl :-         ; 2
            sta VIA1_PRB   ; 4
            dey            ; 2
            lda (STK),y    ; 5
                           ; = 17

:           bit VIA1_PRB   ; 4
            bmi :-         ; 2
            sax VIA1_PRB   ; 4
            bcs sendloop   ; 3
                           ; = 69

            .assert .hibyte(*) = .hibyte(sendloop), error, "***** Page boundary crossing in sendloop. *****"

            bcc DECODETABLE + .lobyte(senddone); jmp

            .assert * >= $0700, error, "***** 1541 watchdog handler below $0700. *****"
            .assert * <= $0700, error, "***** 1541 watchdog handler above $0700. *****"

            jmp uninstall

            ; will be overwritten with DIRTRACKS ($0703..$070d) and DIRSECTORS ($070e..$0718)
findtrkstp:;lda #BTR
            sta BITRATECOUNT
            dec ERRORCOUNT
            bne :+
            asl seekswitch; OPC_BMI -> OPC_RTS
           ;clc
            ldx #$80 | (MINSTEPSPEED + 1)
            jsr halftrkdwn
            sta ERRORCOUNT
            lsr seekswitch; OPC_RTS -> OPC_BMI
:           jmp findtrknum

            .assert * <= (DIRSECTORS + DIRBUFFSIZE), error, "***** 'findtrkstp' code too large. *****"

closefile:  ldy FILEDIRBUFFERINDEX
            bmi :+; branch if file not found: DATA_OUT remains set

            ; clear DATA_OUT otherwise
           ;lda #CLK_IN
            sta (.lobyte(V1B - $ef),x); VIA1_PRB

:           sec
            bcs DECODETABLE + .lobyte(fileclosed); jmp

            .assert * <= (DECODETABLE + DECTABOFFS), error, "***** 1541 drive code too large. *****"

dcodinit41: lda #ATNA_OUT | CLK_OUT; drive idle
            sta VIA1_PRB

           ;ldx ROMOS_TRACK_DIFF
            txa
            ldx #.lobyte(topofstack41); $06
            txs
            pha

            lda #T1_FREE_RUNNING | PA_LATCHING_ENABLE; watchdog IRQ: count phi2 pulses, 16-bit free-running,
            sta VIA2_ACR                             ; enable port A latching to grab one GCR byte at a time rather
                                                     ; than letting the GCR bitstream scroll through than port A
                                                     ; (applies to 1541 and Oceanic OC-118, but not 1541-II)
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

            ; timers/watchdog initialisation
            ; for the long timeout between block-ready and block-send, use two arithmetically cascaded timers:
            ; their periods differ, so their counters drift further apart every time either timer resets.
            ; the effective timeout is reached as soon as the difference between the counters is >= 128, which for
            ; the used periods' difference of 7 cycles with the counter periods of $ef00 and $ef07 is at least
            ; floor(128 / 7) * $ef07 = 18 * $ef07 = 1,101,438 cycles at 1 MHz, i.e., roughly 1 second.
            ; a few cycles are added to or subtracted from the effective timeout: added because as a counter reset
            ; apparently takes 2 cycles, so the effective periods are $ef02 and $ef09, subtracted because the counters'
            ; difference does not increase by 7 on counter $ef07 reset, but increases by 7 and then decreases by 2.

            lda #$07
            sta VIA1_T1L_L; VIA1 timer 1 (TIMER2) low-order latch
            ldx #$00
            stx VIA2_T1L_L; VIA2 timer 1 (TIMER)  low-order latch
            stx VIA1_PRA; one MHz if running on 1570/71 with option ONLY_1541_AND_COMPATIBLE

            lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS; $7f
            sta VIA1_IER; no IRQs from VIA 1
            sta VIA2_IER; no IRQs from VIA 2
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA2_IER; timer 1 IRQs from VIA 2

            lda (V1B,x); VIA1_PRB
            and #DEVICE_NUMBER
            ora getbytecmp41 + 1
            sta getbytecmp41 + 1

            ; fade off the busy LED if lit
            lda LEDSTATE41
            pha
            lda (V2B,x); VIA2_PRB
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection
            and #BUSY_LED
            beq :+
            dex
:           stx LEDSTATE41
:           jsr fadeled
            bne :-
            pla
            sta LEDSTATE41

            pla
           ;lda ROMOS_TRACK_DIFF
            bne :+
            jmp stepperfix; motor on via trackstep
:           lda #MOTOR
            jsr setbv2b41
            jmp stepperok

drvcodeend41:

TRAMPOLINEOFFSET = $27; dgetrout - dinstall + 1

            .org * - TRAMPOLINEOFFSET

dinstall:   sei
            ldx ROMOS_TRACK_DIFF; $42
            txs

            lda #VIA_ATN_IN_INPUT | VIA_DEVICE_NUMBER_INPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT
            sta VIA1_DDRB
            lda VIA1_PRB
            and #DEVICE_NUMBER
            ora getbyteinstall + 1
            sta getbyteinstall + 1

            lda #ATNA_OUT | CLK_OUT
            sta VIA1_PRB
:           lda VIA1_PRB; wait for ATN_IN set and DATA_IN clear
            lsr
            bcs :-
            lda #ATNA_OUT
            sta VIA1_PRB

            ldx #.lobyte(drvcodebeg41 - 1)
dgetrout:   inx

            .assert * >= drvcodeend41, error, "***** 1541 trampoline too low in memory. *****"

            bne :+
            inc dgetputhi
:           DRIVEGETBYTE 1541, getbyteinstall; there is no watchdog while installing
dgetputhi = * + 2
            sta a:.hibyte(drvcodebeg41 - 1) << 8,x
            cpx #.lobyte(drvcodeend41 - 1)
            bne dgetrout
            dec drvcodebeg41
            bne dgetrout

            tsx; ROMOS_TRACK_DIFF
            jmp dcodinit41

drvprgend41:
            .assert * <= $0800, error, "***** 1541 drive code too large. *****"

            .reloc
