;==================================================================
; Ultimate Library
; Ultimate II(+L) cart, Ultimate 64 (Elite 1+2) board, or C64U
; with Command Interface turned on in Ultimate Settings
; 
; Important note:
; Functions use a static buffer "FREEMEM" of 256 bytes.
; FREEMEM = $0000 (here goes your buffer address)
;

; ----- Labels and Buffers ----------------------------------------
; A "target" of the Ultimate's UI functions is an instance of the
; Ultimate DOS. Only 2 targets (1 and 2) can be used. Default is 1.
U_CurTarget     !byte 1
; Used for various lengths (filenames and directory names, file sizes)
U_len           !byte 0,0
; The current path of the Ultimate's command UI (is "//" when DOS starts)
U_Path          !fill 256

;------------------------------------------------------------------

; Ultimate registers
CONTROL_REG     = $df1c  ; Control register (Write)
STATUS_REG      = $df1c  ; Status register (Read (default $00))
CMD_DATA_REG    = $df1d  ; Command data register (Write)
IDENT_REG       = $df1d  ; Identification register (Read (default $C9))
RESP_DATA_REG   = $df1e  ; Response data register (Read only)
STATUS_DATA_REG = $df1f  ; Status data regsiter (Read only)

; Status Register bits
SR_BIT_CMD_BUSY  = %00000001
SR_BIT_DATA_ACC  = %00000010
SR_BIT_ABORT_P   = %00000100
SR_BIT_ERROR     = %00001000
SR_BIT_STATE     = %00110000; 00:idle, 01:cmd busy, 10:data last, 11:data more
SR_BIT_STATE_BUSY= %00010000
SR_BIT_STAT_AV   = %01000000
SR_BIT_DATA_AV   = %10000000

; Control Register bits
CR_BIT_PUSH_CMD = %00000001
CR_BIT_DATA_ACC = %00000010
CR_BIT_ABORT    = %00000100
CR_BIT_CLR_ERR  = %00001000

; Command IDs
DOS_CMD_IDENTIFY    = $01
DOS_CMD_OPEN_FILE   = $02
DOS_CMD_CLOSE_FILE  = $03
DOS_CMD_READ_DATA   = $04
DOS_CMD_WRITE_DATA  = $05
DOS_CMD_FILE_SEEK   = $06
DOS_CMD_FILE_INFO   = $07
DOS_CMD_FILE_STAT   = $08
DOS_CMD_DELETE_FILE = $09
DOS_CMD_RENAME_FILE = $0a
DOS_CMD_COPY_FILE   = $0b
DOS_CMD_CHANGE_DIR  = $11
DOS_CMD_GET_PATH    = $12
DOS_CMD_OPEN_DIR    = $13
DOS_CMD_READ_DIR    = $14
DOS_CMD_CREATE_DIR  = $16
DOS_CMD_MOUNT_DISK  = $23
DOS_CMD_UMOUNT_DISK = $24
DOS_CMD_GET_TIME    = $26
DOS_CMD_SET_TIME    = $27

; File Open Attributes (Bits)
FA_READ          = %00000001
FA_WRITE         = %00000010
FA_CREATE_NEW    = %00000100
FA_CREATE_ALWAYS = %00001000

; Other constants
MAX_DATA = $0200; Max size of file transfer data packet

;==================================================================
; Low Level Functions
;==================================================================
; Checks if host is an Ultimate
; Result in A
IsUltimateCMD   lda IDENT_REG
                cmp #$c9
                bne +
                lda STATUS_REG
                bne +
                lda #1
                rts
+               lda #0
                rts

; Waits for handshake after reading data
U_Accept        lda CONTROL_REG
                ora #CR_BIT_DATA_ACC
                sta CONTROL_REG
                ; Wait for ack
-               lda STATUS_REG
                and #SR_BIT_DATA_ACC
                bne -
                rts

; Waits for the Ultimate command interface to not be busy
U_Wait_NoBusy   lda STATUS_REG
                and #SR_BIT_STATE_BUSY
                bne U_Wait_NoBusy
                rts

; Waits for the Ultimate command interface to be idle
U_Wait_Idle     lda STATUS_REG
                and #SR_BIT_STATE
                bne U_Wait_Idle
                rts

cmd_len         !byte 0
; Sends a command to the Ultimate
; Uses buffer FREEMEM with the following structure:
; cmd_target (usually 1), cmd_id (see above), [parameters]
U_SendCommand   ldx #0
-               lda FREEMEM,x
                sta CMD_DATA_REG
                inx
                cpx cmd_len
                bcc -
                ; Push command
                lda CONTROL_REG
                ora #CR_BIT_PUSH_CMD
                sta CONTROL_REG
                rts

; Reads a response data packet (max bytes = 512)
; Reads response data until there is no data available
; and saves it to (SMC_UltimateData+1)
; Output: Data packet length: LO=X, HI=NOT(Y)
; Error handling: only space for 512 chars; if more,
; reading stops and error_code=1
U_ReadDataPack  jsr U_Wait_NoBusy
                ldy #1
                ldx #0
                stx error_code
-               lda STATUS_REG
                and #SR_BIT_DATA_AV
                beq ++
                ; Data available
                lda RESP_DATA_REG
SMC_UltimatData sta $FFFF,x
                inx
                bne -
                inc SMC_UltimatData+2
                dey
                bpl -
                ; Error (data pack overflow)
                inx
                stx error_code
                lda #CR_BIT_ABORT
                sta CONTROL_REG
                rts
++              jmp U_Accept

; Reads status message to FREEMEM
; Out: Length of msg in X
U_ReadStatusMsg jsr U_Wait_NoBusy
                ldx #0
-               lda STATUS_REG
                and #SR_BIT_STAT_AV
                beq +
                ; Data available
                lda STATUS_DATA_REG
                sta FREEMEM,x
                inx
                jmp -
+               jsr U_Accept
                rts



;==================================================================
; High Level Functions
;==================================================================
U_Time_Indices  !byte 11,12,14,15; indices for time in date_time string
; Reads date_time string from the Ultimate to FREEMEM
U_GetTime       jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_GET_TIME
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                ; Read date & time into FREEMEM
                lda #<FREEMEM
                sta SMC_UltimatData+1
                lda #>FREEMEM
                sta SMC_UltimatData+2
                jmp U_ReadDataPack

; Retrieves the current path in U_Path (null terminated)
; Path lenth: LO=X, HI=NOT(Y)
; Possible error: path longer than 256 chars
; In this case, error_code = 1
U_GetPath       ; Clear path
                ldx #0
                lda #0
-               sta U_Path,x
                inx
                bne -
                ;
                jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_GET_PATH
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                ; Read path into U_Path
                lda #<U_Path
                sta SMC_UltimatData+1
                lda #>U_Path
                sta SMC_UltimatData+2
                jmp U_ReadDataPack

; Changes the current dir to the subdir in FREEMEM+2
; Expects dir name in FREEMEM+2 and dir name length in U_len
; Output: status msg in FREEMEM
;         error_code: 0: success; 1: no such directory
U_ChangeDir     jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_CHANGE_DIR
                sta FREEMEM+1
                ldx U_len
                inx
                inx
                stx cmd_len
                jsr U_SendCommand
                ; Check status
                jsr U_ReadStatusMsg
                ldx #0
                lda FREEMEM
                cmp #$30
                beq +
                ldx #1; No such directory
+               stx error_code
                rts

; Goes up in path
U_GoUpDir       jsr U_Wait_Idle
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_CHANGE_DIR
                sta FREEMEM+1
                lda #'.'
                sta FREEMEM+2
                sta FREEMEM+3
                lda #4
                sta cmd_len
                jsr U_SendCommand
                jsr U_Wait_NoBusy
                jmp U_Accept

; Sets Ultimate's path to Root
U_GotoRoot      jsr U_GetPath
                stx U_len
                ; Get number of /'s in path
                ldy #0
                ldx #0
-               lda U_Path,x
                cmp #47; '/'
                bne +
                iny
+               inx
                cpx U_len
                bcc -
                ;
                cpy #2
                bcc +; already root
                dey
                ; do "cd.." y times
-               jsr U_GoUpDir
                dey
                bne -
+               rts

; Opens file in FREEMEM+3
; Expects 
; * filename in FREEMEM+3
; * file name length in U_len
; * open attribute in SMC_U_Open+1
;   (i.e., FA_READ, FA_WRITE, FA_CREATE_NEW, FA_CREATE_ALWAYS)
; Output in A (0: "00,OK", 1: error, status msg in FREEMEM)
; X = Length of status msg
U_OpenFile      jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_OPEN_FILE
                sta FREEMEM+1
SMC_U_Open      lda #$FF
                sta FREEMEM+2
                ldx U_len
                inx
                inx
                inx
                stx cmd_len
                jsr U_SendCommand
                ; Check status (00,Ok)
                jsr U_ReadStatusMsg
                lda FREEMEM
                cmp #$30
                bne +
                lda FREEMEM+1
                cmp #$30
                bne +
                lda #0
                rts
+               lda #1
                rts

; Reads opened small file (up to 512 bytes) to SMC_UltimatData+1
; Expects
; * file is opened for reading
; * SMC_UltimatData+1 filled with buffer address
U_ReadSmallFile jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_READ_DATA
                sta FREEMEM+1
                lda #$00
                sta FREEMEM+2
                lda #$02
                sta FREEMEM+3
                ldx #4
                stx cmd_len
                jsr U_SendCommand
                ;
                jmp U_ReadDataPack

; Writes (up to 512 bytes) to opened file
; from SMC_WriteFile+1
; Expects
; * file is opened for writing
; * SMC_WriteFile+1 filled with source buffer address
; * file length in U_len & U_len+1 (LO/HI), not more than 512 bytes
U_WriteSmallFile
                jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta CMD_DATA_REG
                lda #DOS_CMD_WRITE_DATA
                sta CMD_DATA_REG
                lda #$00
                sta CMD_DATA_REG
                sta CMD_DATA_REG
                ;
                ldy #0
                ldx #0
SMC_WriteFile   lda $FFFF,x
                sta CMD_DATA_REG
                inx
                bne +
                iny
+               cpy U_len+1
                bcc SMC_WriteFile
                cpx U_len
                bcc SMC_WriteFile
                ; Push command
                lda CONTROL_REG
                ora #CR_BIT_PUSH_CMD
                sta CONTROL_REG
                ;
                jmp U_ReadStatusMsg

; Closes opened file
U_CloseFile     jsr U_Wait_Idle
                ;
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_CLOSE_FILE
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                jsr U_Wait_NoBusy
                jmp U_Accept