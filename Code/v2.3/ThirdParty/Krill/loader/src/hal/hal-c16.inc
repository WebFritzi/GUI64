
.ifndef _HAL_C16_INC_
_HAL_C16_INC_ = 1

.include "cpu.inc"
.include "pio.inc"
.include "ted.inc"


.macro CHECK_INSTALL_END_ADDRESS
    .assert * <= $8000, error, "Install code exceeds $8000, please make sure the DISKIO_INSTALL segment ends below $8000"
.endmacro

.if LOAD_VIA_KERNAL_FALLBACK
    .macro CHECK_RESIDENT_START_ADDRESS
        RESIDENT_START_ADDRESS = *
    .endmacro

    .macro CHECK_RESIDENT_END_ADDRESS
        .assert * <= $8000, error, "Resident code exceeds $8000, please make sure the DISKIO segment ends below $8000"
    .endmacro
.else
    .macro CHECK_RESIDENT_START_ADDRESS
        .assert * <= $fd00, error, "Resident code exceeds $fd00, please make sure the DISKIO segment ends below $fd00"
    .endmacro

    .macro CHECK_RESIDENT_END_ADDRESS
        .assert * <= $fd00, error, "Resident code exceeds $fd00, please make sure the DISKIO segment ends below $fd00"
    .endmacro
.endif


.macro OK_CLC
            lda #diskio::status::OK; $00
            clc; all ok
.endmacro

.macro PREPARE_DRIVE_DISTURBANCE_VALIDATION
            ; disregard drive if it is a 1551, as
            ; it is not connected to the serial bus
            lda #$01
            sta USE4DY
.endmacro

.macro BRANCH_IF_DRIVE_DOES_NOT_DISTURB_SERIAL_BUS to
            lda USE4DY; calling LISTEN will set USE4DY to $00 if 1551 at #9,
            eor #$01  ; $30 if 1551 at #8, and leave it at $01 otherwise,
            bne to    ; then drvlistn will return with $80, $b0, or $01
.endmacro


.if LOAD_VIA_KERNAL_FALLBACK

    .macro BUFFER_MEMCONFIG
            GET_MEMCONFIG
            pha
    .endmacro

    .macro RESTORE_MEMCONFIG_Y
            tay
            pla
            SET_MEMCONFIG
            tya
    .endmacro

    .macro PREPARE_PARALLEL_CHECK
            lda #$01  ; calling LISTEN will set USE4DY to $00 if 1551 at #9,
            sta USE4DY; $30 if 1551 at #8, and leave it at $01 otherwise
    .endmacro

    .macro ENABLE_KERNAL_SERIAL_ROUTINES
            sta TED_ROM_ENABLE
    .endmacro

    .macro ENABLE_KERNAL_SERIAL_ROUTINES_Y
            sta TED_ROM_ENABLE
    .endmacro

    .macro ENABLE_ALL_RAM
            sta TED_RAM_ENABLE
    .endmacro

    .macro ENABLE_ALL_RAM_Y
            sty TED_RAM_ENABLE
    .endmacro

    .macro GET_MEMCONFIG
            lda #ROM_IS_ENABLED
            and TED_CHARGEN_ADDR
    .endmacro

    .macro SET_MEMCONFIG
            .local use_ram

            sta TED_RAM_ENABLE
            beq use_ram
            sta TED_ROM_ENABLE
use_ram:
    .endmacro

    .macro SET_MEMCONFIG_Y
            .local use_ram

            sty TED_RAM_ENABLE
            beq use_ram
            sty TED_ROM_ENABLE
use_ram:
    .endmacro
.endif; !LOAD_VIA_KERNAL_FALLBACK


IO_PORT_DIR_COMMON  = IO_PORT_SERIAL_DATA_IN_INPUT | IO_PORT_SERIAL_CLK_IN_INPUT | IO_PORT_CST_MTR_OUTPUT | IO_PORT_SERIAL_ATN_OUT_OUTPUT | IO_PORT_SERIAL_CLK_OUT_OUTPUT | IO_PORT_SERIAL_DATA_OUT_OUTPUT; $0f

                                         ; effectively, this is the KERNAL flag:
                                         ; 0 = input  = KERNAL,
                                         ; 1 = output = loader
IO_PORT_DIR_KERNAL  = IO_PORT_DIR_COMMON | IO_PORT_CST_RD_INPUT ; $0f

IO_PORT_DIR_OPERATE = IO_PORT_DIR_COMMON | IO_PORT_CST_RD_OUTPUT; $1f

.macro INSTALL_IDLE
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_DATA_OUT; $0f
            sta IO_PORT
            lda #IO_PORT_DIR_OPERATE
            sta IO_PORT_DIRECTION
.endmacro

.macro CLEAR store; store is ignored
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_ATN_OUT | (0 & IO_PORT_SERIAL_CLK_OUT) | (0 & IO_PORT_SERIAL_DATA_OUT); $0c
            sta IO_PORT
.endmacro

.macro CLOSE_FILE
            SYNC
            ldx #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_ATN_OUT) | IO_PORT_SERIAL_CLK_OUT | (0 & IO_PORT_SERIAL_DATA_OUT); $0a
            stx IO_PORT
:           dex
            bne :-
            lda IO_PORT
            asl
            IDLE
.endmacro

.macro SYNC
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT | (0 & IO_PORT_SERIAL_DATA_OUT); $0e
            sta IO_PORT
.endmacro

.macro PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            .local singleclk

            lda TED_CHARGEN_ADDR
            and #FORCE_SINGLE_CLOCK
            pha
            bne singleclk
            lda #FORCE_SINGLE_CLOCK
            php
            sei                  ; 2
            ora TED_CHARGEN_ADDR ; 4
            sta TED_CHARGEN_ADDR ; 4
            plp                  ; 4
singleclk:                       ; = 14
.endmacro

.macro POP_CLOCKCONFIG
            .local singleclk

            pla
            bne singleclk
            lda #255 - FORCE_SINGLE_CLOCK
            php
            sei                  ; 2
            and TED_CHARGEN_ADDR ; 4
            sta TED_CHARGEN_ADDR ; 4
            plp                  ; 4
singleclk:                       ; = 14
.endmacro

.macro SENDBYTE sendstore
            ; does not clobber y

            .local sendbyte
            .local bitset

            tax
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            txa
            ldx #$07
sendbyte:   lsr
            pha
            lda IO_PORT
            ora #IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT
            bcs bitset
            and #255 - IO_PORT_SERIAL_CLK_OUT
bitset:     eor #IO_PORT_SERIAL_DATA_OUT
            sta IO_PORT
            pla
            dex
            bpl sendbyte
            POP_CLOCKCONFIG
.endmacro

.macro RECEIVEBYTE
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            lda #$01
:           pha
            pla
            pha
            lda #IO_PORT_SERIAL_CLK_OUT
            eor IO_PORT
            ldx #IO_PORT_SERIAL_DATA_IN
            cpx IO_PORT
            sta IO_PORT
            pla
            rol
            bcc :-
            tax
            POP_CLOCKCONFIG
            txa
.endmacro

.macro SET_FLAGS_N_DATA_V_CLK
            bit IO_PORT
.endmacro

CLOCK        = IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_CLK_OUT) | (0 & IO_PORT_SERIAL_DATA_OUT)
CLOCK_ATN_HI = CLOCK |      IO_PORT_SERIAL_ATN_OUT ; 1st and 3rd bit pairs; $0c
CLOCK_ATN_LO = CLOCK | (0 & IO_PORT_SERIAL_ATN_OUT); 2nd and 4th bit pairs; $08

.macro ENABLE_WAITBUSY_KERNAL
            lda #.lobyte(~IO_PORT_SERIAL_CLK_OUT)
            and IO_PORT
            sta IO_PORT
.endmacro

.macro INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT
            lda #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_ATN_OUT) | (0 & IO_PORT_SERIAL_CLK_OUT) | (0 & IO_PORT_SERIAL_DATA_OUT); $08
            sta IO_PORT
.endmacro

.macro IDLE
            ldx #IO_PORT_CST_MTR | IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_DATA_OUT; $0f
            stx IO_PORT
.endmacro

.if LOAD_VIA_KERNAL_FALLBACK
            KERNALFILENO = 2

    .macro CLEAR_DATA_OUT_CLEAR_CLK_OUT_ASSERT_ATN
            lda IO_PORT
            and #.lobyte(~(IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_DATA_OUT))
            ora #IO_PORT_SERIAL_ATN_OUT
            sta IO_PORT
    .endmacro
.endif

.macro BRANCH_IF_INSTALLED to
            lda #IO_PORT_DIR_KERNAL
            cmp IO_PORT_DIRECTION
            bne to
.endmacro

.macro BRANCH_IF_NOT_INSTALLED to
            lda #IO_PORT_DIR_KERNAL
            cmp IO_PORT_DIRECTION
            beq to
.endmacro

.macro CHECK_AND_BRANCH_IF_DRIVE_PARALLEL to
            jsr CHECKPARALLEL
            bcc to
.endmacro

.macro BRANCH_IF_DRIVE_PARALLEL to
            lda USE4DY
            lsr
            bcc to
.endmacro

.macro POLL_BLOCK idle_eof, block_not_yet_ready, device_not_present
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_DATA_OUT; $0f
            cmp IO_PORT
            beq idle_eof; branches with carry set on idle/eof

            clc
            lda #diskio::status::DEVICE_NOT_PRESENT; $fe
            SET_FLAGS_N_DATA_V_CLK
            bvc block_not_yet_ready
            sec
            bmi device_not_present
.endmacro

.macro SEND_BLOCK_SIGNAL
            ldy #CLOCK_ATN_LO; use y to ensure that y < $fe when
            sty IO_PORT      ; calling getbyte for the 2 control bytes
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            pha; delay
            pla
.endmacro

.macro RECEIVE_SETUP
            ; nothing to do
.endmacro

.macro RECEIVE store, out
            .local loop

            ; 16 cycles per bitpair ~ 18 cycles at 1 MHz = 18 µs
            ; PAL:  16 / 886723 Hz = 18.04 µs
            ; NTSC: 16 / 894886 Hz = 17.88 µs

loop:       ldx #CLOCK_ATN_HI                       ; 2
            lda IO_PORT                             ; 3
            stx IO_PORT; ATN high                   ; 3
                                                    ; = 16

            iny                                     ; 2
            beq out                                 ; 2
            lsr                                     ; 2
            lsr                                     ; 2
            ldx #CLOCK_ATN_LO                       ; 2
            eor IO_PORT                             ; 3
            stx IO_PORT; ATN low                    ; 3
                                                    ; = 16

            lsr                                     ; 2
            lsr                                     ; 2
            nop                                     ; 2 - delay
            nop                                     ; 2 - delay
            ldx #CLOCK_ATN_HI                       ; 2
            eor IO_PORT                             ; 3
            stx IO_PORT; ATN high                   ; 3
                                                    ; = 16

            lsr                                     ; 2
            lsr                                     ; 2
            nop                                     ; 2 - delay
            eor #CLOCK_ATN_HI | (CLOCK_ATN_LO >> 2) ; 2
            ldx #CLOCK_ATN_LO                       ; 2
            eor IO_PORT                             ; 3
            stx IO_PORT; ATN low                    ; 3
                                                    ; = 16

            store                                   ; 5 - sta mem16,y
            jmp loop                                ; 3
out:
.endmacro

.macro STOREBYTE_ALLRAM
storebytio: sta $0000,y
.endmacro

.macro ENDGETBLOCK
            POP_CLOCKCONFIG
.endmacro

.macro SET_IO_KERNAL
            INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT
            lda #IO_PORT_DIR_KERNAL
            sta IO_PORT_DIRECTION
.endmacro

.endif; !_HAL_C16_INC_
