
; configuration
; set .defines to non-0 to enable the corresponding features

; see loader.inc for function calls and convenience macros

; parameters

.ifndef PLATFORM
PLATFORM                        = diskio::platform::COMMODORE_64; available are COMMODORE_64, COMMODORE_128 and COMMODORE_16
.endif

; parameter, this changes the host-side code only

DECOMPRESSOR                    = DECOMPRESSORS::ZX0; available are NONE, BITNAX (recommended for demos), BYTEBOOZER2, DOYNAX_LZ, EXOMIZER (not recommended for demos), LEVELCRUSH, LZSA2 (recommended for demos), NUCRUNCH, PUCRUNCH, SUBSIZER, TINYCRUNCH (recommended for demos), TSCRUNCH (strongly recommended for demos), ZX0 (strongly recommended for demos)


; features

; following settings are independent from the installed drive code, several host-side
; resident binaries with different features may be used with the same installed drive code


; basic features, different settings can be run with the same installed drive code, increase host-side code size

.define LOAD_COMPD_API            1 ; include the loadcompd routine to load and depack compressed files on the fly
                                    ; requires DECOMPRESSOR != DECOMPRESSORS::NONE

.define LOAD_RAW_API              1 ; include the loadraw routine to load files without decompressing

.define NTSC_COMPATIBILITY        0 ; C-64/128 only: be able to run on both PAL and NTSC machines, this slightly decreases loading speed on PAL,
                                    ; note that PAL vs. NTSC is not detected by the install routine, and no error is returned when running on an
                                    ; NTSC machine with the NTSC_COMPATIBILITY option disabled: detect, then select either of both incarnations
                                    ; of the resident portion (with and without NTSC support) for maximum speed with NTSC and PAL

.define PREFER_SPEED_OVER_SIZE    0 ; For TSCrunch or ZX0, use a bigger but potentially faster decompression routine

.define UNINSTALL_API             0 ; include an uninstallation routine


; extended features, different settings can be run with the same installed drive code, increase host-side code size

.define FILE_EXISTS_API           0 ; include the fileexists routine for simple multi-disk handling

.define LOAD_UNDER_D000_DFFF      0 ; C-64/128: enable loading (and decompression) to the RAM at $D000..$DFFF,
                                    ; note that this does not slow down loading when not loading to RAM at $D000..$DFFF,
                                    ; as there are two separate routines to load data (one regular, the other to RAM at $D000..$DFFF).
                                    ; the IRQ handlers will need to change $01 ($FF00 on C-128) to enable the I/O registers at $D000..$DFFF,
                                    ; so make sure the IRQ handlers restore the $01 status on C-64 to the value as when they are called.
                                    ; the IRQs must run via $FFFE/F, since the ROM is disabled when accessing the RAM at $D000-$DFFF
                                    ; this is not needed when only memdecompressing to $D000..$DFFF (simply set $01 to $30 on C-64 and jsr memdecomp in that case)

.define ALLOW_2_MHZ_ON_C128       0 ; C-64 only: allow 2 MHz usage on C-128 in C-64 mode,
                                    ; this does not increase raw loading speed but increases combined loading + decompression speed using loadcompd.
                                    ; the clock is temporarily switched to 1 MHz while loading,
                                    ; interrupt handlers changing the clock speed must restore it upon return to the mainline thread.

.define MEM_DECOMP_API            0 ; include a routine for memory decompression, that is, loading and decompression can be separated.
                                    ; C-64: decompression to $D000..$DFFF need not have LOAD_UNDER_D000_DFFF enabled,
                                    ; just enable 64 kB of RAM before jsr memdecomp.
                                    ; requires DECOMPRESSOR != DECOMPRESSORS::NONE
                                    ; this option does not implicitly turn on the LOAD_RAW_API

.define MEM_DECOMP_TO_API         0 ; if carry is set on decompression, the decompressor will use the address set in decdestlo/decdesthi as
                                    ; decompression destination address and ignore the file's decompression address.
                                    ; requires MEM_DECOMP_API != 0

.define LOAD_TO_API               0 ; if the carry flag is set on load, override load and decompression destination addresses.
                                    ; load raw files: use the address set in loadaddrlo/loadaddrhi as absolute loading address
                                    ; load compressed files: use relative loading address offset in loadaddroffslo/loadaddroffshi, it is added to the load/depack addresses

.define END_ADDRESS_API           0 ; during and after loading, the file's current and then final end address (address of last file byte + 1) is stored in
                                    ; endaddrlo and endaddrhi. for loading compressed files using loadcompd, the end address of the compressed data is stored.
                                    ; the file's loading address can be found in loadaddrlo/loadaddrhi during and after loading, so polling the current
                                    ; difference of endaddrlo/hi and loadaddrlo/hi can be used to implement progress displays (take care for update race conditions).

.define LOAD_VIA_KERNAL_FALLBACK  0 ; loads via the KERNAL API if drive code installation was not successful
                                    ; (i.e., if it cannot installed due to an incompatible drive - possible if it is not
                                    ; a 1541, 1541-C, 1541-II, 1541U, 1570, 1571, 1571CR, 1581, or FD2000/4000),
                                    ; or true drive emulation being disabled.
                                    ; note that this does not necessarily mean slow or non-IRQ loading, as custom KERNALs like JiffyDOS or IDEDOS
                                    ; use the original KERNAL API as well.
                                    ; the IRQ handlers can be delayed for some rasterlines up to several frames due to KERNAL routines
                                    ; temporarily disabling IRQ (but that is unlikely for devices not using the serial bus).
                                    ; for the sake of compatibility, only disable this option if the space is really needed.
                                    ; C-64:
                                    ;  Attention: KERNAL, BASIC, and possible cartridge ROMs are enabled, so IRQ handlers are not
                                    ;   allowed in the ranges $8000..$BFFF and $D000..$FFFF.
                                    ;  Attention: KERNAL routines may execute CLI, so make sure to have valid IRQ vectors and handlers,
                                    ;   or disable all IRQ sources (not via SEI), also make sure to correctly handle the different
                                    ;   IRQ conditions when called via KERNAL vector ($0314) vs. non-KERNAL vector ($FFFE), both are possible -
                                    ;   best have KERNAL and BASIC enabled before calling the loader, so only the KERNAL vector IRQ handler is
                                    ;   needed (please note that the handler code is delayed a little when called via $0314 rather than $FFFE).
                                    ; C-128:
                                    ;  Attention: System ROM is enabled, so IRQ handlers are not allowed in the range $C000..$FFFF.
                                    ;  Attention: KERNAL routines may execute CLI, so make sure to have valid IRQ vectors and handlers,
                                    ;   or disable all IRQ sources (not via SEI), also make sure to correctly handle the different
                                    ;   IRQ conditions when called via KERNAL vector ($0314) vs. non-KERNAL vector ($FFFE) - best have System ROM
                                    ;   enabled before calling the loader, so only the KERNAL vector IRQ handler is needed (please note that
                                    ;   the handler code is delayed a little when called via $0314 rather than $FFFE).
                                    ; C-64/128:
                                    ;  Attention: KERNAL routines use CIA1 timer A ($DC04/5).
                                    ; Plus/4:
                                    ;  Attention: The ROM space in the upper memory half is enabled, so IRQ handlers are not allowed
                                    ;   in the range $8000..$FFFF.
                                    ;  Attention: The ROM routines may execute CLI, so make sure to have valid IRQ vectors and handlers,
                                    ;   or disable all IRQ sources (not via SEI), also make sure to correctly handle the different
                                    ;   IRQ conditions when called via ROM vector ($0314) vs. non-ROM vector ($FFFE) - best have ROM enabled
                                    ;   before calling the loader, so only the ROM vector IRQ handler is needed (please note that
                                    ;   the handler code is delayed a little when being called via $0314 rather than $FFFE).
                                    ; requires ONLY_1541_AND_COMPATIBLE = 0

.define CLOSE_FILE_API            0 ; include the closefile call to close an open file


; these options change drive-side code

.define DIRTRACK                 18 ; actual directory track, this can be changed to have a shadow directory so that the
                                    ; normal directory does not list the files and can be used entirely for bootstrap and dir-art
.define DIRTRACK81               40 ; (i.e., the loader's directory can be relocated to hide it from the normal directory command).
                                    ; DIRTRACK must be 18 when LOAD_VIA_KERNAL_FALLBACK != 0
                                    ; DIRTRACK81 must be 40 when LOAD_VIA_KERNAL_FALLBACK != 0

.define FILENAME_MAXLENGTH       16 ; maximum length of filename, if a directory is capable of holding longer names, extra characters are ignored,
                                    ; to facilitate dir-art, set to, e.g., 2, then load files as "01*", "02*", etc.


; this option reduces host-side install code

.define ONLY_1541_AND_COMPATIBLE  0 ; reduces host-side install code by omitting any native custom drive code for non-1541 compatible
                                    ; drives, treats any drive as 1541, using an incompatible drive will cause undefined behaviour
