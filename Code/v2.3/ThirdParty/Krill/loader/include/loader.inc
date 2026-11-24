
.ifndef _LOADER_INC_
_LOADER_INC_ = 1

DISABLE_WATCHDOG = 0 ; disable the drive-side watchdog - the watchdog will reset the drive safely when the host resets at any point,
                     ; however, the drive hardware (1541 and 1571) only allows for a maximum time-out period of 65536 cycles: this means
                     ; that letting the loader starve for a few video frames (about 1.5 frames with a 1571 running at 2 MHz) will reset
                     ; the drive, which can be prevented using this option.

; C-128 only: for burst transfers, use the CLK line for the incoming data-sent signal rather than the
;             SERIAL_IRQ flag in CIA1_ICR ($dc0d) in order to avoid interference by CIA1 interrupt handlers,
;             note that this slows down transfer speed
ASYNCHRONOUS_BURST_HANDSHAKE = 0

; set to 0 if loading via KERNAL fallback is problematic with non-standard KERNALs, VICE virtual drives, IEEE-488 interfaces or devices like IDE64
.ifndef KERNAL_FALLBACK_SEI_WORKAROUNDS
KERNAL_FALLBACK_SEI_WORKAROUNDS = 1
.endif
KERNAL_FALLBACK_OPEN_SEI_WORKAROUNDS = 0; this does not work with VICE virtual device traps

CONFIG_INTERNAL = ((KERNAL_FALLBACK_SEI_WORKAROUNDS = 0) << 3) | (KERNAL_FALLBACK_OPEN_SEI_WORKAROUNDS << 2) | (ASYNCHRONOUS_BURST_HANDSHAKE << 1) | DISABLE_WATCHDOG

.pushseg

.segment "CODE"; import symbols as absolute by default

.scope diskio

.scope platform
    COMMODORE_64            =  64
    COMMODORE_128           = 128
    COMMODORE_16            =  16
.endscope; platform

.scope drivetype
    DRIVES_1541             = $00
    DRIVES_157X             = $10
    DRIVES_1581_CMD         = $20
    DRIVES_MASK             = $f0

    DRIVE_1541              = $01
    DRIVE_1541_C            = $02
    DRIVE_1541_II           = $03
    DRIVE_1541U             = $04
    DRIVE_1570              = $15
    DRIVE_1571              = $16
    DRIVE_1571CR            = $17
    DRIVE_1581              = $28
    DRIVE_CMD_FD_2000       = $29
    DRIVE_CMD_FD_4000       = $2a
    DRIVE_CMD_HD            = $2b

    DRIVE_GENERIC_SERIAL    = $fc ; -4
    DRIVE_GENERIC_BURST     = $fd ; -3
    DRIVE_GENERIC_PARALLEL  = $fe ; -2
    DEVICE_NONE             = $ff ; -1
.endscope; drivetype

.scope status
    OK                      =   0

    FILE_TOO_LARGE          = $f7 ; -9
    FILE_ON_DISK_TOO_SMALL  = $f8 ; -8
    FILE_ON_DISK_TOO_LARGE  = $f9 ; -7
    WRITE_PROTECT_ON        = $fa ; -6

    DEVICE_INCOMPATIBLE     = $fb ; -5, cannot upload loader drive code to active device, if LOAD_VIA_KERNAL_FALLBACK != 0, do not regard this as an error
    TOO_MANY_DEVICES        = $fc ; -4, a device on the bus other than the active device is not compatible, if LOAD_VIA_KERNAL_FALLBACK != 0, do not regard this as an error
    GENERIC_KERNAL_ERROR    = $fd ; -3, an error occured during installation or while loading without installed drive code via KERNAL fallback, check the x register for further information
    DEVICE_NOT_PRESENT      = $fe ; -2
    FILE_NOT_FOUND          = $ff ; -1
.endscope; status

.endscope; diskio

.enum DECOMPRESSORS

    BITNAX                  =   1 ; Doynax and Bitbreaker's Bitnax
    BYTEBOOZER2             =   2 ; HCL/Booze Design's ByteBoozer2
    DOYNAX_LZ               =   3 ; Doynax LZ, aka Doynamite
    EXOMIZER                =   4 ; Magnus Lind's Exomizer
    LEVELCRUSH              =   5 ; Taboo Levelcrush
    LZSA2                   =   6 ; Emmanuel Marty's LZSA2 with modifications by Bitbreaker
    NUCRUNCH                =   7 ; ChristopherJam aka Shrydar's NuCrunch
    PUCRUNCH                =   8 ; Pasi Ojala's Pucrunch
    SUBSIZER                =   9 ; tlr's Subsizer
    TINYCRUNCH              =  10 ; ChristopherJam aka Shrydar's tinycrunch
    TSCRUNCH                =  11 ; Antonio Savona's TSCrunch
    ZX0                     =  12 ; Einar Saukas' ZX0/Salvador with Dali modifications by Bitbreaker

    NONE                    =   0

.endenum


.ifdef EXTCONFIGPATH
    .include "loaderconfig.inc"
.else
    .include "config.inc"
.endif; !EXTCONFIGPATH

.if .defined(FORCE_ASYNCHRONOUS_BURST_HANDSHAKE)
USE_ASYNCHRONOUS_BURST_HANDSHAKE = 1
.else
USE_ASYNCHRONOUS_BURST_HANDSHAKE = ASYNCHRONOUS_BURST_HANDSHAKE
.endif


.ifndef __NO_LOADER_SYMBOLS_IMPORT
.ifndef install

.if PLATFORM <> diskio::platform::COMMODORE_16
    ; Set the VIC bank
    ; in:  a - VIC bank (0..3)
    ; out: undefined
    .macro SET_VIC_BANK bank
            lda #bank & 3
            sta CIA2_PRA
    .endmacro

    ; allow for arbitrary $DD00 writes ($00-$FF) when the loader is idle,
    ; good for raster routines with LDA #value:STA $D018:STA $DD00, e.g.

    ; call after load
    .macro ENTER_BUS_LOCK
            ; nothing to do
    .endmacro

    ; call before load
    .macro LEAVE_BUS_LOCK
            ; when the loader is idle, the user is
            ; allowed to write anything to CIA2_DDRA ($DD00) -
            ; set it to a known and valid state here.
            ;php; without these, there's a race condition and thus a small
            ;sei; chance for video glitches, but with these, there's a
            INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT
            ;plp; chance for actual crashes with stable raster routines
    .endmacro

    .if ALLOW_2_MHZ_ON_C128 | (PLATFORM = diskio::platform::COMMODORE_128)
    ; For 2 MHz in interrupt handlers during loading,
    ; to be run at the start of every IRQ handler
    .macro LOADER_IRQ_HANDLER_PROLOGUE
            ; buffer clock setting and continue at 2 MHz
            lda VIC2_C128_CLOCK
            pha
            lda #C128_TWO_MHZ
            sta VIC2_C128_CLOCK
    .endmacro

    ; For 2 MHz in interrupt handlers during loading,
    ; to be run at the end of every IRQ handler
    .macro LOADER_IRQ_HANDLER_EPILOGUE
            pla
            sta VIC2_C128_CLOCK
    .endmacro
    .endif
.else
    ; To be run at the start of every IRQ handler
    .macro LOADER_IRQ_HANDLER_PROLOGUE
            ; buffer force single clock flag
            ; and allow double clock
            lda TED_CHARGEN_ADDR
            pha
            and #255 - FORCE_SINGLE_CLOCK
            sta TED_CHARGEN_ADDR
    .endmacro

    ; To be run at the end of every IRQ handler
    .macro LOADER_IRQ_HANDLER_EPILOGUE
            ; restore force single clock flag
            .local fast

            pla
            lsr
            lsr
            lda TED_CHARGEN_ADDR
            and #255 - FORCE_SINGLE_CLOCK
            bcc fast
            ora #FORCE_SINGLE_CLOCK
fast:       sta TED_CHARGEN_ADDR
    .endmacro
.endif


; Install the loader
; note: KERNAL ROM must be enabled, and the I flag will be cleared (CLI)
; in:  nothing
; out: c   - set on error
;      a   - status
;      x   - drive type (one of diskio::drivetypes)
;      y   - if status is diskio::status::OK, zeropage address of version string address
.import install
.macro LOADER_INSTALL
            jsr install
.endmacro

.if LOAD_RAW_API
; Load a file without decompression
; in:  x/y - x: lo, y: hi to 0-terminated filename string,
;            zero-length file name will load next file
;      c   - if LOAD_TO_API != 0, c = 0: load to address as stored in the file
;                                 c = 1: load to caller-specified address (loadaddrlo/hi)
; out: c   - set on error
;      a   - status
.import loadraw
.macro LOADRAW name
    .if LOAD_TO_API
            clc
    .endif
            ldx #<(name)
            ldy #>(name)
            jsr loadraw
.endmacro

.macro LOADNEXTRAW
            lda #0
            LOADRAW * - 1
.endmacro

.if LOAD_TO_API
    .macro LOADRAW_LOADTO name, dest
            sec
            lda #<(dest)
            sta loadaddrlo
            lda #>(dest)
            sta loadaddrhi
            ldx #<(name)
            ldy #>(name)
            jsr loadraw
    .endmacro

    .macro LOADNEXTRAW_LOADTO dest
            lda #0
            LOADRAW_LOADTO * - 1, dest
    .endmacro
.endif; LOAD_TO_API
.endif; LOAD_RAW_API

.if LOAD_COMPD_API
; Load a compressed file
; in:  x/y - x: lo, y: hi to 0-terminated filename string,
;            zero-length file name will load next file
;      c   - if DECOMPLOAD_TO_API != 0, c = 0: load to address as stored in the file
;                                       c = 1: load with caller-specified address offset (loadaddroffslo/hi)*
; out: c   - set on error
;      a   - status
;      x/y - if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH, x: lo, y: hi of the loaded file's execution address

; * this is an offset rather than an absolute destination, as the original destination address required for calculating this offset is
;   stored in the file, but to retrieve it, the first file block would have to be loaded first to the original load address or to an extra buffer

.import loadcompd
.import loadcompdopen
.import loadcompdexecute
.macro LOADCOMPD name
            ldx #<(name)
            ldy #>(name)
            jsr loadcompd
.endmacro

.macro LOADNEXTCOMPD
            lda #0
            LOADCOMPD * - 1
.endmacro

.if LOAD_TO_API
    .macro LOADCOMPD_RELTO name, load_address_offset
            sec
            lda #<(load_address_offset)
            sta loadaddroffslo
            lda #>(load_address_offset)
            sta loadaddroffshi
            ldx #<(name)
            ldy #>(name)
            jsr loadcompd
    .endmacro

    .macro LOADNEXTCOMPD_RELTO dest_lo, dest_hi
            lda #0
            LOADCOMPD_RELTO #<(* - 1), #>(* - 1), dest_lo, dest_hi
    .endmacro
.endif; LOAD_TO_API
.endif; LOAD_COMPD_API

.if FILE_EXISTS_API
; Check if file exists
; in:  x/y - x: lo, y: hi to 0-terminated filename string
; out: c - set on file not found or error
;      a - status
.import fileexists
.macro FILEEXISTS name
            ldx #<(name)
            ldy #>(name)
            jsr fileexists
.endmacro
.endif; FILE_EXISTS_API

.if MEM_DECOMP_API
; Decompress a compressed file from memory
; in:  x/y - x: lo, y: hi of compressed file in memory
;      c   - if MEMDECOMP_TO_API != 0, c = 0: decompress to address as stored in the file
;                                      c = 1: decompress to caller-specified address (loadaddrlo/hi)
; out: x/y - if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH, x: lo, y: hi of the file's execution address
.import memdecomp
.macro MEMDECOMP source_lo, source_hi
    .if MEM_DECOMP_TO_API
            clc
    .endif
            ldx source_lo
            ldy source_hi
            jsr memdecomp
.endmacro

.if MEM_DECOMP_TO_API
    .macro MEMDECOMP_TO source_lo, source_hi, dest
            sec
            lda #<(dest)
            sta decdestlo
            lda #>(dest)
            sta decdesthi
            ldx source_lo
            ldy source_hi
            jsr memdecomp
    .endmacro
.endif; MEM_DECOMP_TO_API
.endif; MEM_DECOMP_API

.endif; !install
.endif; !__NO_LOADER_SYMBOLS_IMPORT

.struct drivecode
            entry  .word; drive
            to     .word; drive
            length .word; bytes
            from   .word; host
.endstruct

.struct swapparams
            buffer      .word; $0800 bytes for swapped loader drive code

            drivecode41 .tag drivecode
    .if ONLY_1541_AND_COMPATIBLE = 0
            drivecode71 .tag drivecode
            drivecode81 .tag drivecode
    .endif; ONLY_1541_AND_COMPATIBLE = 0
.endstruct

.ifndef __NO_CUSTOMDRIVECODE_SYMBOLS_IMPORT
.ifndef swapdrvcod
.import swapdrvcod
.import restoreldr
.endif; !swapdrvcod

; Execute custom code in the drive, buffer loader code on host side before
; in:  x/y - x: lo, y: hi of swapparams structure
; out: c   - set on error
;      a   - status
.macro SWAP_DRIVECODE params
            ldx #<(params)
            ldy #>(params)
            jsr swapdrvcod
.endmacro

; Restore drive-side loader code
; in:  nothing
; out: undefined
.macro RESTORE_LOADER
            jsr restoreldr
.endmacro
.endif; !__NO_CUSTOMDRIVECODE_SYMBOLS_IMPORT

.struct saveparams
            filename    .word; existing file to overwrite, pointer to 0-terminated filename string
            from        .word; pointer to file data
            length      .word; length of file data in bytes
            loadaddress .word; usually same as from
            buffer      .word; pointer to $0800 bytes for swapped loader drive code
.endstruct

.ifndef __NO_SAVE_SYMBOLS_IMPORT
.ifndef save

.import save
; Save a file by overwriting an existing file of the same block-size
; in:  x/y - x: lo, y: hi of saveparams structure
; out: c   - set on error
;      a   - status
.macro SAVE_OVERWRITE params
            ldx #<(params)
            ldy #>(params)
            jsr save
.endmacro

.endif; !save
.endif; !__NO_SAVE_SYMBOLS_IMPORT

.ifndef __NO_LOADER_SYMBOLS_IMPORT

.if UNINSTALL_API
; Uninstall the loader
; in:  nothing
; out: undefined
.import uninstall
.macro LOADER_UNINSTALL
            jsr uninstall
.endmacro
.endif; UNINSTALL_API

.if CLOSE_FILE_API
; Close an open file
; in:  nothing
; out: undefined
.import closefile
.endif; CLOSE_FILE_API


; linker-generated loader segments symbols

.import __DISKIO_ZP_START__
.import __DISKIO_ZP_END__
.import __DISKIO_ZP_LOAD__
.import __DISKIO_ZP_RUN__
.import __DISKIO_ZP_SIZE__

.import __DISKIO_INSTALL_START__
.import __DISKIO_INSTALL_END__
.import __DISKIO_INSTALL_LOAD__
.import __DISKIO_INSTALL_RUN__
.import __DISKIO_INSTALL_SIZE__

.import __DISKIO_START__
.import __DISKIO_END__
.import __DISKIO_LOAD__
.import __DISKIO_RUN__
.import __DISKIO_SIZE__

.endif; !__NO_LOADER_SYMBOLS_IMPORT


.ifndef __NOIMPORTVARS
.ifndef loadaddrlo
    .importzp loadaddrlo
    .importzp loadaddrhi

    .importzp loadaddroffslo
    .importzp loadaddroffshi

    .importzp decdestlo
    .importzp decdesthi

    .importzp endaddrlo
    .importzp endaddrhi

    .importzp bytesloadedlo
    .importzp bytesloadedhi
.endif; !loadaddrlo
.endif; !__NOIMPORTVARS

.if PLATFORM <> diskio::platform::COMMODORE_16
    .macro INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT
            ; this is executed during install and LEAVE_BUS_LOCK,
            ; hence it accesses the port register rather than the data direction register
            lda #3
            and CIA2_PRA
            sta CIA2_PRA
    .endmacro
.endif

; custom drive code API

.ifndef ID041

.importzp ID041
.importzp CURRTRACK41
.importzp NUMFILES41
.importzp SECTORLINKTABLE41

.import topofstack41
.import idleloop41
.import getbytecmp41
.import BLOCKBUFFER41

.importzp V1B41
.importzp V2B41
.importzp LEDSTATE41
.importzp FILESECTOR41
.importzp FILENAME41
.importzp LINKTRACK41
.importzp LINKSECTOR41
.importzp REQUESTEDSECTOR41
.import trkseek41
.import initlink41
.import sertoraw41
.import getbytewdog41
.import getbyte41
.import getbyterts41
.import setbv2b41
.import getblock41
.import idxloop41
.import wdogentr41
.import findfile41
.import loadfile41

.endif

; Return-to-loader macros
; upon execution, i-flag must be set, ATN must be asserted, DATA and CLK must be cleared

.macro RETURNTOLOADER41 hook
            .scope
            .assert * >= $0100, error, "Return to 1541 loader code too low in memory"

            lda #$1a; ATNA_OUT | CLK_OUT | DATA_OUT
            sta $1800; VIA1_PRB

            lda $1800; VIA1_PRB
            and #$60; DEVICE_NUMBER
            ora #$94; ATN_IN | ATNA_OUT | CLK_IN
            sta compare + 1
            ldx #0

            lda #$10; ATNA_OUT
            sta $1800; VIA1_PRB

            ldy $1800; VIA1_PRB
getbyte:    lda #$80
getbit:     cpy $1800; VIA1_PRB
            beq getbit
            ldy $1800; VIA1_PRB
compare:    cpy #0
            ror
            bcc getbit
            sta $00,x
            inx
            bne getbyte

            hook

            jmp $00da

            .assert * <= $0800, error, "Return to 1541 loader code too high in memory"
            .endscope
.endmacro

.ifndef NUMFILES71

.importzp CURRTRACK71
.importzp LEDSTATE71
.importzp CLEARSECTORLINKTABLE71
.importzp FILENAME71
.importzp LINKTRACK71
.importzp LINKSECTOR71
.importzp SECTORLINKTABLE71
.importzp DIRSECTORS71
.importzp CUSTOMZPBUFFER71
.importzp CUSTOMUPLOADSIZE71
.importzp getbytrt71
.importzp CUSTOMPARAM71

.importzp bsetv2b71
.importzp trkseek71
.import initlink71
.import onemhz71
.import topofstack71
.import getblock71
.import idleloop71
.import findfile71
.import loadfile71
.import idxloop71
.import BLOCKBUFFER71

.endif

.macro RETURNTOLOADER71 hook
            .scope
            .assert * >= $0100, error, "Return to 1571 loader code too low in memory"

            ldx #0

            lda #$10; ATNA_OUT
            sta $1800

            ldy $1800; VIA1_PRB
getbyte:    lda #$80
getbit:     cpy $1800; VIA1_PRB
            beq getbit
            ldy $1800; VIA1_PRB
            cpy #$94; ATN_IN | ATNA_OUT | CLK_IN
            ror
            bcc getbit
            sta $00,x
            inx
            bne getbyte

            hook

            jmp $0037

            .assert * <= $0800, error, "Return to 1571 loader code too high in memory"
            .endscope
.endmacro

.ifndef dcodinit81

.importzp CUSTOMPARAM81

.import dcodinit81
.import DRVCODEND81
.import BLOCKBUFFER81
.import CUSTOMRECEIVE81
.import filename81
.import swapzp81
.import getblock81
.import initwdog81
.import enablwdg81
.import initcntr81
.import bsyledon81
.import findfile81
.import loadfile81
.import DIRTRACKS81

.endif

.macro RETURNTOLOADER81 hook
            .scope
            .assert * >= DRVCODEND81, error, "Return to 1581 loader code too low in memory"

            ldy #0
            sty $4001; CIA_PRB
getbyte:    lda #$80
getbit:     pha
            lda $4001; CIA_PRB
waitbit:    cmp $4001; CIA_PRB
            beq waitbit
            lda $4001; CIA_PRB
            and #$04; CLK_IN
            cmp #$04; CLK_IN
            pla
            ror
            bcc getbit
restore:    sta $0300 & $ff00,y
            iny
            bne :+
            inc restore + 2
:           cpy #<DRVCODEND81
            bne getbyte
            lda restore + 2
            cmp #>DRVCODEND81
            bne getbyte

            hook

waitrun:    lda $4001; CIA_PRB
            lsr
            bcc waitrun
            jmp dcodinit81

            .endscope
.endmacro

.popseg

.if (ALLOW_2_MHZ_ON_C128 & (PLATFORM = diskio::platform::COMMODORE_64)) | (PLATFORM = diskio::platform::COMMODORE_128)
    USE_2_MHZ = 1
.else
    USE_2_MHZ = 0
.endif

.if LOAD_VIA_KERNAL_FALLBACK
    .if DIRTRACK <> 18
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires DIRTRACK to be 18 *****"
    .endif
    .if DIRTRACK81 <> 40
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires DIRTRACK81 to be 40 *****"
    .endif

    .if (PLATFORM = diskio::platform::COMMODORE_128) & ASYNCHRONOUS_BURST_HANDSHAKE
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires ASYNCHRONOUS_BURST_HANDSHAKE = 0 *****"
    .endif

    .macro CHECK_LOADER_ZP_ADDRESSES
        .assert (loader_zp_first > STATUS) || (loader_zp_last < STATUS), error, "Loader zeropage variables cross KERNAL STATUS register at $90"
        .assert (loader_zp_first > C3PO) || (loader_zp_last < C3PO), error, "Loader zeropage variables cross KERNAL serial buffer output char buffered flag C3PO at $94"
        .assert (loader_zp_first > BSOUR) || (loader_zp_last < BSOUR), error, "Loader zeropage variables cross KERNAL serial buffer byte BSOUR at $95"

        .if PLATFORM = diskio::platform::COMMODORE_16
        .assert (loader_zp_first > LDTND) || (loader_zp_last < LDTND), error, "Loader zeropage variables cross KERNAL logical file index/open files count at $97"
        .assert (loader_zp_first > DFLTN) || (loader_zp_last < DFLTN), error, "Loader zeropage variables cross KERNAL input device variable DFLTN at $98"
        .assert (loader_zp_first > DFLTO) || (loader_zp_last < DFLTO), error, "Loader zeropage variables cross KERNAL output device variable DFLTO at $99"
        .assert (loader_zp_first > R2D2) || (loader_zp_last < R2D2), error, "Loader zeropage variables cross KERNAL serial bus EOI flag R2D2 at $a6"
        .assert (loader_zp_first > BSOUR1) || (loader_zp_last < BSOUR1), error, "Loader zeropage variables cross KERNAL serial bus shift counter BSOUR1 at $a8"
        .assert (loader_zp_first > COUNT) || (loader_zp_last < COUNT), error, "Loader zeropage variables cross KERNAL serial bus counter COUNT at $aa"
        .assert (loader_zp_first > USE4DY) || (loader_zp_last < USE4DY), error, "Loader zeropage variables cross KERNAL parallel drive state register USE4DY at $f9"
        .else; PLATFORM <> diskio::platform::COMMODORE_16
        .assert (loader_zp_first > LDTND) || (loader_zp_last < LDTND), error, "Loader zeropage variables cross KERNAL logical file index/open files count at $98"
        .assert (loader_zp_first > DFLTN) || (loader_zp_last < DFLTN), error, "Loader zeropage variables cross KERNAL input device variable DFLTN at $99"
        .assert (loader_zp_first > DFLTO) || (loader_zp_last < DFLTO), error, "Loader zeropage variables cross KERNAL output device variable DFLTO at $9a"
        .assert (loader_zp_first > R2D2) || (loader_zp_last < R2D2), error, "Loader zeropage variables cross KERNAL serial bus EOI flag R2D2 at $a3"
        .assert (loader_zp_first > BSOUR1) || (loader_zp_last < BSOUR1), error, "Loader zeropage variables cross KERNAL serial bus shift counter BSOUR1 at $a4"
        .assert (loader_zp_first > COUNT) || (loader_zp_last < COUNT), error, "Loader zeropage variables cross KERNAL serial bus counter COUNT at $a5"
        .endif; PLATFORM <> diskio::platform::COMMODORE_16
    .endmacro
.else
    .macro CHECK_LOADER_ZP_ADDRESSES
    .endmacro
.endif

.endif; _LOADER_INC_
