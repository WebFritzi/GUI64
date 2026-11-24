
.ifndef _DRIVECODE_COMMON_INC_
_DRIVECODE_COMMON_INC_ = 1

MIN_DEVICE_NO   =  8 ; these two settings define the device number range when scanning for devices in install.s,
MAX_DEVICE_NO   = 30 ; devices beyond this range will not be recognized or useable

; 1541/41-C/41-II/41U/70/71/71CR
MINSTEPSPEED    = $18 ; min. R/W head stepping speed
MAXSTEPSPEED    = $10 ; max. R/W head stepping speed
STEPPERACC      = $0c ; R/W head stepping acceleration, smaller is slower acceleration
SINGLESTEPSPEED = $0c ; time between two consecutive half-track steps for a single-track step

.macro ENABLE_WATCHDOG
    .if ::DISABLE_WATCHDOG
            sei
    .else
            cli
    .endif
.endmacro

.macro DRIVEGETBYTE drivetype, comparelbl, nozeroes
   .if .not .xmatch (drivetype, 1581)
            .local OFFSET
            .local getbit

            ; must not clobber x

        .if .paramcount < 3
            OFFSET = 0

            ldy VIA1_PRB + OFFSET
        .else
            OFFSET = 16
        .endif

            lda #%10000000
getbit:     cpy VIA1_PRB + OFFSET
            beq getbit
            ldy VIA1_PRB + OFFSET
comparelbl: cpy #ATN_IN | ATNA_OUT | CLK_IN
            ror
            bcc getbit

            .assert .hibyte(*) = .hibyte(getbit), error, "***** Page boundary crossing in getbyte loop, fatal cycle loss. *****"
    .else
            .local getbit
            .local waitbit

            lda #%10000000
getbit:     pha
            lda CIA_PRB
waitbit:    cmp CIA_PRB
            beq waitbit
            lda CIA_PRB
            and #CLK_IN
            cmp #CLK_IN
            pla
            ror
            bcc getbit

            .assert .hibyte(*) = .hibyte(getbit), error, "***** Page boundary crossing in getbyte loop, fatal cycle loss. *****"
    .endif
.endmacro

.macro DRIVESENDBYTE drivetype, sendvalue
            .local sendbit
            .local waitbit

            sta sendvalue
            ldy #8
sendbit:    asl sendvalue
            lda #0
   .if .not .xmatch (drivetype, 1581)
            adc #ATNA_OUT | DATA_IN
            sta VIA1_PRB
            lda VIA1_PRB
waitbit:    cmp VIA1_PRB
    .else
            adc #DATA_IN
            sta CIA_PRB
            lda CIA_PRB
waitbit:    cmp CIA_PRB
    .endif
            beq waitbit
            dey
            bne sendbit
.endmacro

.macro GET_FILENAME drivetype
   .if .not .xmatch (drivetype, 1581)
           ;ldx #0
        .if .xmatch (drivetype, 1571)
            lda #ATNA_OUT; ATNA_OUT set, CLK_OUT and DATA_OUT clear: drive is ready
            sta (V1B,x); VIA1_PRB
        .endif
getfilenam:
        .if .xmatch (drivetype, 1541)
            inx
            getbytewdog41 = getbytewdg
            getbyterts41 = getbyterts
getbytewdg: lda #$f5
            sta VIA1_PRB; ATNA_OUT set, CLK_OUT and DATA_OUT clear: drive is ready
            sta TIMER; reset watchdog time-out
            ENABLE_WATCHDOG
            getbyte41 = getbyte
getbyte:    DRIVEGETBYTE 1541, getbytecmp41
getbyterts: sta a:FILENAME - 1,x
        .else
            inx
            jsr getbytewdg
            sta .lobyte(FILENAME - 1),x
        .endif
    .else; 1581
            ldx #0
            stx CIA_PRB; CLK_OUT and DATA_OUT clear: drive is ready
getfilenam: inx
            jsr getbyte
            sta filename - 1,x
    .endif
            bne getfilenam

            sei; disable watchdog
            cpx #16 + 3
    .if .xmatch (drivetype, 1541)
            bcs getcustom
    .else
            bcc :+
            jmp getcustom
:
    .endif
    .if .not .xmatch (drivetype, 1581)
        .if .xmatch (drivetype, 1541)
            dec VIA1_PRB; %1xy10000 -> %1xy01111: drive busy, set CLK_OUT and DATA_OUT
        .else
            lda #ERR | ATNA_OUT | CLK_OUT | DATA_OUT; drive busy
            sta VIA1_PRB
            sta ERRORCOUNT; reset, as spinning up the motor would yield errors
        .endif
    .else
            lda #CLK_OUT | DATA_OUT; drive busy
            sta CIA_PRB
    .endif
    .if .xmatch (drivetype, 1571)
            inc prpnxtfjmp; OPC_JMP_ABS -> OPC_EOR_ABS, disable block caching: do not jump to prpdnxtfil (idleloop) after fetching the file's first block
    .endif
:           dex              ; if zero-length filename,
            beq loadnextfile ; use hash values of next file
            cpx #FILENAME_MAXLENGTH + 1
            bcs :-
findfile:   sec
            jsr gethashval
    .if .not .xmatch (drivetype, 1581)
setnextfile:
    .endif
            sta FILENAMEHASHLO
            sty FILENAMEHASHHI
loadnextfile:
.endmacro; GET_FILENAME drivetype

; matches against hash of filename in FILENAMEHASHLO/HI

.macro FIND_FILE drivetype
    .if .not .xmatch (drivetype, 1581)
chknewdisk:
        .if .xmatch (drivetype, 1541)
            lda #$fc
            sta CYCLESTARTENDSECTOR
            ldx NUMFILES
            bpl findloop

            ; a new disk has been inserted
            sta DIRBLOCKPOS
            jsr getblkbam; store ID, sector link sanity check
            bcc chknewdisk
            jsr checkchg
            lda #1; first dir sector
        .else
            lda #0; BAM sector
            sta CYCLESTARTENDSECTOR
            ldx NUMFILES
            bpl findloop

            ; a new disk has been inserted
           ;lda #0; BAM sector
            sta DIRBLOCKPOS

            ldx #.lobyte(storeid - (idswitch + 2))
            jsr getblkstid; store ID, sector link sanity check
            bcs chknewdisk
            jsr checkchg
            lda #1; first dir sector

            ; set seek boundaries according to number of disk sides
            ldx #MAXTRACK71 + 1
            ldy #NUMTRACKS_A
            bit BLOCKBUFFER71 + TWOSIDEDOFFSET
            bmi :+
            ldx #NUMTRACKS_SINGLESIDED + 1
            ldy #NUMTRACKS_SINGLESIDED
:           stx MAXTRACK
            sty MAXTRACK_A
        .endif

           ;lda LINKSECTOR
    .else; 1581
            inc prpnxtfjmp; OPC_JMP_ABS -> OPC_EOR_ABS, do not jump to after prepare-next-file routine

prpnxtfile: lda NUMFILES
            bmi newdisk
diskchangd: lda #$00
            bne newdisk
            jmp samedisk

newdisk:    ; a new disk has been inserted
            jsr getdirtrk
            ldy #$00
            sty diskchangd + 1
            sty CYCLESTARTENDTRACK
            sty CYCLESTARTENDSECTOR
            jsr getblock81
            bcs newdisk

           ;lda LINKTRACK
           ;ldx LOADEDSECTOR; $00
           ;ldy LINKSECTOR
            sty FIRSTDIRSECTOR
    .endif; 1581

            ; directory cycling: fill the directory buffer with the next file entries,
            ; this is also executed upon file not found in the currently buffered directory segment
            ; (with accu containing NEXTDIRBLOCKSECTOR's value)
nextdirseg:
    .if .not .xmatch (drivetype, 1581)
            ldy #$ff
    .else
            ldx #$ff
    .endif
            ; file not found (yet)
    .if .xmatch (drivetype, 1541)
            ldx CYCLESTARTENDSECTOR
            beq filenotfnd; branch if cycle complete with z = 0 and y = $ff = diskio::status::FILE_NOT_FOUND
    .else; !1541
            bit CYCLESTARTENDSECTOR
            bpl :+ ; branch if cycle not complete
            sec
            jmp filenotfnd
:
    .endif; !1541
    .if .not .xmatch (drivetype, 1581)
            sty NUMFILES

        .if .xmatch (drivetype, 1541)
nextdirsct: ; a = sector
            jsr getblkdir; compare ID, sector link sanity check
            bcc chknewdisk
           ;lda LINKSECTOR41
           ;ldx LOADEDSECTOR
            tay
            bpl :+; branch if not wrapping to first dir block
            lda #1; wrap around to first dir sector
:           sta NEXTDIRBLOCKSECTOR
        .else; 1571
            SKIPWORD; do not set CYCLESTARTENDSECTOR
dircycle:   sta CYCLESTARTENDSECTOR
nextdirsct: ; a = sector
            jsr getblkchid; compare ID, sector link sanity check
            bcs chknewdisk

           ;lda LINKSECTOR
           ;ldy LINKTRACK
           ;cpy CURTRACK
            beq :+; branch if not wrapping to first dir block
            lda #1; wrap around to first dir sector
:           sta NEXTDIRBLOCKSECTOR
        .endif
    .else; 1581
            stx NUMFILES

nextdirsct: ; a = track
            ; y = sector
            sta CURRDIRBLOCKTRACK
            jsr getblock81
            bcs newdisk; start over on error
           ;ldy LINKSECTOR
           ;lda LINKTRACK
            bne :+; branch if not wrapping to first dir block
            jsr getdirtrk
            ldy FIRSTDIRSECTOR
:           sta NEXTDIRBLOCKTRACK
            sty NEXTDIRBLOCKSECTOR

            ldy #0
    .endif; 1581

getdirloop:
    .if .xmatch (drivetype, 1541)
            ldy DIRBLOCKPOS
            jsr sertorawr; get file's start track
            beq notafile; skip non-files denoted by track 0
            cmp #MAXTRACK41 + 1
            bcs notafile; or files with invalid start tracks
            inc NUMFILES
            ldx NUMFILES
            sta DIRTRACKS,x
            jsr sertorawd; get file's start sector
            sta DIRSECTORS,x
            lda (BLP),y; number of file blocks (low-byte)
            jsr sertoraw41
            sta NUMBLOCKS,x
    .elseif (.xmatch (drivetype, 1571))
            ldy DIRBLOCKPOS
            lda BLOCKBUFFER + 3 + TRACKOFFSET,y; get file's start track
            beq notafile; skip non-files denoted by track 0
            cmp MAXTRACK
            bcs notafile; or files with invalid start tracks
            inc NUMFILES
            ldx NUMFILES
            sta .lobyte(DIRTRACKS),x
            lda BLOCKBUFFER + 3 + SECTOROFFSET,y; get file's start sector
            sta .lobyte(DIRSECTORS),x
            lda BLOCKBUFFER + BLOCKSOFFSET,y
            sta .lobyte(NUMBLOCKS),x
    .else; 1581
            lda BLOCKBUFFER + 3 + TRACKOFFSET,y; get file's start track
            beq notafile; skip non-files denoted by track 0
            inc NUMFILES
            ldx NUMFILES
            sta DIRTRACKS,x
            lda BLOCKBUFFER + 3 + SECTOROFFSET,y; get file's start sector
            sta DIRSECTORS,x
            tya
            pha
    .endif; 1581
            ldx #0
            jsr fnamehash
            ldx NUMFILES; restore x
            sta FILENAMEHASHVALSLO,x
    .if .not .xmatch (drivetype, 1581)
            sty FILENAMEHASHVALSHI,x
    .else
            tya
            sta FILENAMEHASHVALSHI,x
            pla
            tay
    .endif
            ; advance to next file or quit loop
            cpx #DIRBUFFSIZE - 1
            bcs dirbuffull
notafile:
    .if .xmatch (drivetype, 1541)
            lax DIRBLOCKPOS
            axs #$20; 8 entries per block, $20 bytes per entry
            stx DIRBLOCKPOS
            bcs getdirloop; process all entries in a dir block
    .elseif .xmatch (drivetype, 1571)
            lax DIRBLOCKPOS
            axs #.lobyte(-$20); 8 entries per block, $20 bytes per entry
            stx DIRBLOCKPOS
            bcc getdirloop; process all entries in a dir block
    .else; 1581
            tya
            and #%11100000; 8 entries per block, $20 bytes per entry
           ;clc
            adc #$20
            tay
            bcc getdirloop; process all entries in a dir block
    .endif
            ; process next dir block

    .if .xmatch (drivetype, 1541)
            lax NEXTDIRBLOCKSECTOR
            tay
            eor CYCLESTARTENDSECTOR
            bmi :+; set CYCLESTARTENDSECTOR
            bne :++
            ; cycle complete
            tay
:           sty CYCLESTARTENDSECTOR
:           txa
            bpl nextdirsct; jmp
    .elseif .xmatch (drivetype, 1571)
            lda NEXTDIRBLOCKSECTOR
            ldy CYCLESTARTENDSECTOR
            beq dircycle; set CYCLESTARTENDSECTOR
            cmp CYCLESTARTENDSECTOR
            bne nextdirsct

            ; cycle complete
           ;sec
            ror CYCLESTARTENDSECTOR
        .if (DIRBUFFSIZE < 16)
            ; always fill up a dir buffer that can hold fewer than 2 dir blocks, because if the dir is 2 blocks
            ; big, the dir buffer may otherwise only hold parts of 2 blocks despite having more space
            bne nextdirsct; jmp
        .endif
    .else; 1581
            jsr getdirtrk
            ldy NEXTDIRBLOCKSECTOR
            ldx CYCLESTARTENDTRACK
            bne :+
            sta CYCLESTARTENDTRACK
            sty CYCLESTARTENDSECTOR
            beq nextdirsct; jmp
:           cmp CYCLESTARTENDTRACK
            bne nextdirsct
            cpy CYCLESTARTENDSECTOR
            bne nextdirsct

            ; cycle complete
           ;sec
            ror CYCLESTARTENDSECTOR
    .endif; 1581
dirbuffull: lda LOADEDSECTOR
            sta NEXTDIRBLOCKSECTOR
    .if .not .xmatch (drivetype, 1581)
        .if DIRBUFFSIZE < 16
           ;ldx NUMFILES; actually number of files - 1
        .else
            ldx NUMFILES; actually number of files - 1
        .endif
    .else
            lda CURRDIRBLOCKTRACK
            sta NEXTDIRBLOCKTRACK

            ; the disk was not changed, or the dir has just been read
samedisk:   ldx NUMFILES; actually number of files - 1
    .endif; 1581

findloop:
    .if .not .xmatch (drivetype, 1581)
            lda NEXTDIRBLOCKSECTOR
    .else; !1581
            lda NEXTDIRBLOCKTRACK
            ldy NEXTDIRBLOCKSECTOR
    .endif
            dex ; skip the last file entry to keep it as an overflow entry for load-next and PREPARE_NEXT_FILE
    .if .xmatch (drivetype, 1541)
            stx FILEDIRBUFFERINDEX
    .endif
            bmi nextdirseg; if the dir buffer does not contain the file, cycle through the directory to find it

nextfile:   lda FILENAMEHASHVALSLO,x
            eor FILENAMEHASHLO
            bne findloop
            ldy FILENAMEHASHVALSHI,x
            cpy FILENAMEHASHHI
            bne findloop

            ; file found

    .if .xmatch (drivetype, 1541)
           ;sec
    .elseif (.xmatch (drivetype, 1571))
            ; check for illegal track or sector
           ;sec
            stx FILEDIRBUFFERINDEX
            lda .lobyte(DIRTRACKS),x
            jsr getnumscts
            stx TEMP
            ldx FILEDIRBUFFERINDEX
            ldy .lobyte(DIRTRACKS),x
            lda .lobyte(DIRSECTORS),x
            cmp TEMP
           ;bcs filenfound
filenotfnd:
    .else; 1581
           ;lda #$00
            sta CYCLESTARTENDTRACK
            sta CYCLESTARTENDSECTOR

            lda DIRTRACKS,x
            ldy DIRSECTORS,x
            clc
filenotfnd:
prpnxtfjmp: jmp prpdnxtfil; is changed to eor prpdnxtfil
            dec prpnxtfjmp; OPC_EOR_ABS -> OPC_JMP_ABS
    .endif; 1581
.endmacro; FIND_FILE drivetype

.macro FNAMEHASH drivetype

fnamehash:
        .if .xmatch (drivetype, 1541)
            jsr sertorawd
        .else
            lda BLOCKBUFFER + 5,y
            iny
        .endif
            cmp #' ' | $80; $a0 = end of filename
            beq gethashval
        .if .not .xmatch (drivetype, 1581)
            sta .lobyte(FILENAME),x
        .else; 1581
            sta filename,x
        .endif; 1581
            inx
            cpx #FILENAME_MAXLENGTH
            bcc fnamehash

            ; fall through

gethashval: txa
            tay
           ;sec
hashloop:   sta TEMP
            tya
    .if .not .xmatch (drivetype, 1581)
            adc .lobyte(FILENAME - 1),x
    .else; 1581
            adc filename - 1,x
    .endif; 1581
            tay
            adc TEMP
            dex
            bne hashloop

    .if .xmatch (drivetype, 1541)
specdone:  ;sec
            ror SPECWRAPSECTOR
getcustom:
    .endif
            rts

.endmacro; FNAMEHASH drivetype

.endif; !_DRIVECODE_COMMON_INC_
