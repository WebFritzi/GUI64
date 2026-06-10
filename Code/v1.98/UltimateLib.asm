;==================================================================
;                        Ultimate Library
;==================================================================
; For Ultimate II(+L) cart, Ultimate 64 (Elite 1+2) board, or C64U
; with Command Interface turned on in Ultimate Settings
;
; Author: Friedrich M. Philipp (aka WebFritzi)
; 
; Important note:
; Functions use a static buffer "FREEMEM" of 256 bytes.
; Define this constant here, e.g.,
; FREEMEM = $c000 (here goes your buffer address)
; or elsewhere
;==================================================================

!zone VarsAndConsts
; ----- Labels and Buffers ----------------------------------------
; A "target" of the Ultimate's UI functions is an instance of the
; Ultimate DOS. Only 2 targets (1 and 2) can be used. Default is 1.
U_CurTarget     !byte 1
; Used for various lengths (filenames and directory names, file sizes)
U_len           !byte 0,0

; Paths are expected to have lengths at most 256 each
; Path address for target #1
U_PathAddrT1    !byte 0,0
; Path address for target #2
U_PathAddrT2    !byte 0,0
; Current path address
U_CurPathAddr   !byte 0,0
U_CurPathLen    !byte 0

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
SOFTIEC_CMD_LOAD_SU = $10
SOFTIEC_CMD_LOAD_EX = $11
SOFTIEC_CMD_OPEN    = $13
SOFTIEC_CMD_CLOSE   = $14

; File Open Attributes (Bits)
FA_READ          = %00000001
FA_WRITE         = %00000010
FA_CREATE_NEW    = %00000100
FA_CREATE_ALWAYS = %00001000

; Other constants
MAX_DATA = $0200; Max size of file transfer data packet

!zone LowLevelFunctions
;==================================================================
; Low Level Functions
;==================================================================
; Checks if host is an Ultimate
; Result in A
U_IsUltimateCMD lda IDENT_REG
                cmp #$c9
                bne +
                lda STATUS_REG
                bne +
                lda #1
                rts
+               lda #0
                rts

; Requires
; * target no in X
; * U_PathAddrT1 und U_PathAddrT2 filled
; Sets U_CurTarget and U_CurPathAddr
U_SetCurTargetAndPath
                stx U_CurTarget
                dex
                beq +
                lda U_PathAddrT2
                sta U_CurPathAddr
                lda U_PathAddrT2+1
                sta U_CurPathAddr+1
                rts
+               lda U_PathAddrT1
                sta U_CurPathAddr
                lda U_PathAddrT1+1
                sta U_CurPathAddr+1
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
                and #%00110000
                cmp #%00010000
                beq U_Wait_NoBusy
                rts

; Waits for the Ultimate command interface to be idle
U_Wait_Idle     lda STATUS_REG
                and #SR_BIT_STATE
                bne U_Wait_Idle
                rts

U_Abort         lda #CR_BIT_ABORT
                sta CONTROL_REG
-               lda STATUS_REG
                and #SR_BIT_ABORT_P
                bne -
                rts

U_If_not_Idle_Abort
                lda STATUS_REG
                and #SR_BIT_STATE
                beq +
                jsr U_Abort
+               rts

cmd_len         !byte 0
; Sends a command to the Ultimate
; Uses buffer FREEMEM with the following structure:
; cmd_target (usually 1), cmd_id (see above), [parameters]
; Required:
; * structure filled at FREEMEM
; * cmd_len (length of full command)
U_SendCommand   jsr U_If_not_Idle_Abort
                ldx #0
-               lda FREEMEM,x
                sta CMD_DATA_REG
                inx
                cpx cmd_len
                bcc -
U_PushCmd       ; Push command
                lda #CR_BIT_PUSH_CMD
                sta CONTROL_REG
                jmp U_Wait_NoBusy

DATA_ADDR = SMC_UltimatData+1
; Reads a response data packet (max bytes = 512)
; Reads response data until there is no data available
; and saves it to (DATA_ADDR)
; Output:
; * Data packet length: LO=X, HI=1-Y
; * Zero flag: -beq: Ok
;              -bne: overflow (more than 512 chars read)
U_ReadDataPack  ldy #1
                jmp +
; Only reads max 256 bytes
U_ReadDataPackS ldy #0
+               ldx #0
-               lda STATUS_REG
                and #SR_BIT_DATA_AV
                bne +
                jmp U_Accept
+               ; Data available
                lda RESP_DATA_REG
SMC_UltimatData sta $FFFF,x
                inx
                bne -
                inc SMC_UltimatData+2
                dey
                bpl -
                ; Error (data pack overflow)
                jsr U_Abort
                rts

U_DumpData      lda STATUS_REG
                and #SR_BIT_DATA_AV
                bne +
                rts
+               ; Data available
                lda RESP_DATA_REG
                jmp U_DumpData

; Reads status message to FREEMEM
; Out: Length of msg in X
U_ReadStatusMsg ldx #0
-               lda STATUS_REG
                and #SR_BIT_STAT_AV
                bne +
                rts
+               ; Data available
                lda STATUS_DATA_REG
                sta FREEMEM,x
                inx
                jmp -

;------------------------------------------------------------------
; Code fragments
;------------------------------------------------------------------
; Sends the DOS_CMD_READ_DIR command
U_SendReadDirCmd
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_READ_DIR
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jmp U_SendCommand

U_SetFREEMEMasDATA_ADDR
                lda #<FREEMEM
                sta DATA_ADDR
                lda #>FREEMEM
                sta DATA_ADDR+1
                rts

!zone Time
;==================================================================
; High Level Functions
;==================================================================
U_Time_Indices  !byte 11,12,14,15; indices for time in date_time string
; Reads date_time string from the Ultimate to FREEMEM
U_GetTime       lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_GET_TIME
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                ; Read date & time into FREEMEM
                jsr U_SetFREEMEMasDATA_ADDR
                jmp U_ReadDataPack

!zone Path
; Retrieves the current path in &U_CurPathAddr
; Output:
; * Path length: LO=X, HI=NOT(Y)
; * Zero flag: -beq: Ok
;              -bne: overflow (more than 256 chars read)
; * X and U_CurPathLen: length of path string
U_GetPath       lda U_CurPathAddr
                sta $fb
                sta DATA_ADDR
                lda U_CurPathAddr+1
                sta $fc
                sta DATA_ADDR+1
                ; Send command
                lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_GET_PATH
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                jsr U_ReadDataPackS
                stx U_CurPathLen
                rts

;; Sets the SoftIEC path to the current path
;U_SoftIEC_SetPath
;                jsr U_GetPath
;                jsr U_If_not_Idle_Abort
;                ; Change current SoftIEC dir via OPEN
;                lda #5
;                sta CMD_DATA_REG
;                lda #SOFTIEC_CMD_OPEN
;                sta CMD_DATA_REG
;                lda #15
;                sta CMD_DATA_REG
;                lda #$00
;                sta CMD_DATA_REG
;                lda #'c'
;                sta CMD_DATA_REG
;                lda #'d'
;                sta CMD_DATA_REG
;                lda #':'
;                sta CMD_DATA_REG
;                ldy #0
;-               lda ($fb),y; FBFC is set to path string
;                sta CMD_DATA_REG
;                iny
;                cpy U_CurPathLen
;                bcc -
;                jsr U_PushCmd
;                jsr U_Accept
;                ; Close SoftIEC channel
;                jsr U_If_not_Idle_Abort
;                lda #5
;                sta CMD_DATA_REG
;                lda #SOFTIEC_CMD_CLOSE
;                sta CMD_DATA_REG
;                lda #15
;                sta CMD_DATA_REG
;                jsr U_PushCmd
;                ;jsr U_ReadStatusMsg
;                jmp U_Accept

;U_SoftIEC_Save  jsr U_If_not_Idle_Abort
;                lda #5
;                sta CMD_DATA_REG
;                lda #$12
;                sta CMD_DATA_REG
;                lda #0
;                sta CMD_DATA_REG
;                sta CMD_DATA_REG
;                lda #$00
;                sta CMD_DATA_REG
;                lda #$28
;                sta CMD_DATA_REG
;                lda #$ff
;                sta CMD_DATA_REG
;                lda #$28
;                sta CMD_DATA_REG
;                lda #'d'
;                sta CMD_DATA_REG
;                lda #'u'
;                sta CMD_DATA_REG
;                lda #'h'
;                sta CMD_DATA_REG
;                jsr U_PushCmd
;                jsr U_ReadStatusMsg
;                jmp U_Accept

!zone Files
;; Prepares to load a file with DMA transfer to $0801
;; Required:
;; * Filename in FREEMEM+6
;; * Filename length in U_len
;; Output in A (and zero flag)
;; * 0: Ok
;; * 1: File not found
;U_SoftIEC_PrepareLoad   
;                jsr U_SoftIEC_SetPath
;                ;jsr U_SoftIEC_Save; Test
;                ;jmp*
;                lda #5
;                sta FREEMEM
;                lda #SOFTIEC_CMD_LOAD_SU
;                sta FREEMEM+1
;                lda #0
;                sta FREEMEM+2; sec_addr
;                sta FREEMEM+3; verify
;                lda #$01
;                sta FREEMEM+4; addr_lo
;                lda #$08
;                sta FREEMEM+5; addr_hi
;                lda #0
;                sta FREEMEM+6
;                sta FREEMEM+7
;                lda U_len
;                clc
;                adc #8
;                sta cmd_len
;                jsr U_SendCommand
;                jsr U_DumpData
;                jsr U_ReadStatusMsg
;                jmp U_Accept

;; Loads the file per DMA transfer to $0801
;; Copy this code to a place where it won't be overwritten by the file
;; For example, to $033c (cassette buffer)
;U_LoadRunFileDMA
;                ; If not idle, then abort
;                lda STATUS_REG
;                and #SR_BIT_STATE
;                beq +
;                lda #CR_BIT_ABORT
;                sta CONTROL_REG
;-               lda STATUS_REG
;                and #SR_BIT_ABORT_P
;                bne -
;+               ; Send command
;                lda #5
;                sta CMD_DATA_REG
;                lda #SOFTIEC_CMD_LOAD_EX
;                sta CMD_DATA_REG
;                lda #0
;                sta CMD_DATA_REG
;                sta CMD_DATA_REG
;                lda #CR_BIT_PUSH_CMD
;                sta CONTROL_REG
;                ; Wait no busy while DMA load
;-               lda STATUS_REG
;                and #%00110000
;                cmp #%00010000
;                beq -
;                ; Check status data
;                ldx #0
;-               lda STATUS_REG
;                and #SR_BIT_STAT_AV
;                beq +
;                ; Data available
;                lda STATUS_DATA_REG
;                sta $033c,x
;                inx
;                bne -
;+               ; Accept
;                lda CONTROL_REG
;                ora #CR_BIT_DATA_ACC
;                sta CONTROL_REG
;                ; Wait for ack
;-               lda STATUS_REG
;                and #SR_BIT_DATA_ACC
;                bne -
;;                ; If not idle, abort
;;                lda STATUS_REG
;;                and #SR_BIT_STATE
;;                beq +
;;                lda #CR_BIT_ABORT
;;                sta CONTROL_REG
;;-               lda STATUS_REG
;;                and #SR_BIT_ABORT_P
;;                bne -
;;+               
;                ; RUN
;                lda #21
;                sta $d018; upper case letters
;                ldx $ae
;                ldy $af
;                stx $2d   ; Set pointer in zeropage to end of
;                sty $2e   ; BASIC program (a.k.a. start of variables)
;                jsr $e544 ; clear screen
;                lda #0
;                sta $0800
;                jsr $A533 ; Re-link program
;                jsr $A659 ; Reset CLR, TXTPTR
;                jmp $A7AE ; Jump into interpreter loop
;                ;
;                ;jsr $e544; clear screen
;                ;lda #21
;                ;sta $d018; upper case letters
;                ;lda #'R'
;                ;sta $0277
;                ;lda #'U'
;                ;sta $0278
;                ;lda #'N'
;                ;sta $0279
;                ;lda #13
;                ;sta $027a
;                ;lda #4
;                ;sta $c6
;                rts
;U_LoadRunFileDMA_End
;LOADRUNCODE_LEN = U_LoadRunFileDMA_End - U_LoadRunFileDMA

U_MountDevNo    !byte 8
; Mounts disk image to drive U_MountDevNo
; Required:
; * U_MountDevNo filled
; * image filename in FREEMEM+3
; * filename length in U_len
U_MountImage    lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_MOUNT_DISK
                sta FREEMEM+1
                lda U_MountDevNo
                sta FREEMEM+2
                lda U_len
                clc
                adc #3
                sta cmd_len
                jsr U_SendCommand
                jsr U_ReadStatusMsg
                jsr U_Accept
                lda FREEMEM
                and #%00001111
                rts

U_ind           !byte 0
U_running_ind   !byte 0
; Retrieves the filename of index U_ind in current directory
; Requires: index in U_ind
; Output:
; * file attribute in FREEMEM+1
; * filename string in FREEMEM+2
; * filename length in U_len
U_GetFilenameByIndex
                jsr U_OpenDir
                beq +
                lda #0
                sta U_len
                rts
+               lda #0
                sta U_running_ind
                jsr U_SendReadDirCmd
                lda #<(FREEMEM+1)
                sta DATA_ADDR
                lda #>(FREEMEM+1)
                sta DATA_ADDR+1
-               jsr U_ReadDataPack
                txa
                beq +
                lda U_running_ind
                cmp U_ind
                bcs +
                inc U_running_ind
                jmp -
+               dex
                stx U_len
                ; Abort
                lda #4
                sta CONTROL_REG
                jmp U_Wait_Idle

; Opens file in FREEMEM+3
; Required: 
; * filename in FREEMEM+3
; * file name length in U_len
; * open attribute in SMC_U_Open+1
;   (i.e., FA_READ, FA_WRITE, FA_CREATE_NEW, FA_CREATE_ALWAYS)
; Output in A (0: "00,OK", 1: error, status msg in FREEMEM)
; X = Length of status msg
U_OpenFile      lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_OPEN_FILE
                sta FREEMEM+1
SMC_U_Open      lda #$FF
                sta FREEMEM+2
                lda U_len
                clc
                adc #3
                sta cmd_len
                jsr U_SendCommand
                ; Check status (00,Ok)
                jsr U_ReadStatusMsg
                jsr U_Accept
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

; Retrieves information about the opened file
; Output:
; * A:
;   0: success
;   5: no file open
;   8: no information available
; * file data record in FREEMEM in the following form
;   4 bytes: size
;   2 bytes: date
;   2 bytes: time
;   3 bytes: file extension
;   1 byte: attribute
;   x bytes: filename
U_GetFileInfo   lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_FILE_INFO
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                ;
                jsr U_ReadStatusMsg
                lda FREEMEM+1
                and #%00001111
                pha
                beq +
                jsr U_Accept
                jmp ++
+               jsr U_SetFREEMEMasDATA_ADDR
                jsr U_ReadDataPack
++              pla
                rts

; Reads opened small file (up to 512 bytes) to DATA_ADDR
; Requires:
; * file is opened for reading
; * DATA_ADDR filled with buffer address
U_ReadSmallFile lda U_CurTarget
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
                jmp U_ReadDataPack

; Writes (up to 512 bytes) to opened file
; from SMC_WriteFile+1
; Requires:
; * file is opened for writing
; * SMC_WriteFile+1 filled with source buffer address
; * file length in U_len & U_len+1 (LO/HI), not more than 512 bytes
U_WriteSmallFile
                ;jsr U_Wait_Idle
                lda STATUS_REG
                and #SR_BIT_STATE
                beq +
                jsr U_Abort
+               lda U_CurTarget
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
                jsr U_PushCmd
                jsr U_ReadStatusMsg
                jmp U_Accept

; Closes opened file
U_CloseFile     lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_CLOSE_FILE
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                jmp U_Accept

!zone Directories
; Changes the current dir to the subdir in FREEMEM+2
; Requires dir name in FREEMEM+2 and dir name length in U_len
; Output:
; * status msg in FREEMEM
; * X: 0: success; 1: no such directory
U_ChangeDir     lda U_CurTarget
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
                jsr U_Accept
                ldx #0
                lda FREEMEM
                cmp #$30
                beq +
                ldx #1; No such directory
+               rts

; Opens a directory for reading
; Output in A:
; * 0: Ok
; * 1: Empty directory
; * 6: Can't open error
U_OpenDir       lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_OPEN_DIR
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                jsr U_ReadStatusMsg
                jsr U_Accept
                lda FREEMEM+1
                and #%00000111
                rts

U_NumDirStrings !byte 0
U_FileNameMaxLen!byte 0; maximal file name length
U_AbbrevChar    !byte 42; '*'
; Internal
U_max_fn_len    !byte 0
U_is_img        !byte 0
; Reads the current directory into DATA_ADDR
; If file name is too long, the last char is replaced by
; U_AbbrevChar (to be chosen by the programmer)
; Required: U_FileNameMaxLen, U_AbbrevChar, DATA_ADDR
; Output:
; * number of entries read in U_NumDirStrings
; * strings in (DATA_ADDR)
; Each entry consists of U_FileNameMaxLen + 2 bytes in screen code:
; 1 byte for the file type (U_DIR_CHAR: dir, U_FILE_CHAR: file)
; 1 byte: space (32)
; U_FileNameMaxLen bytes: (possibly abbreviated) file name
U_ReadDirMaxLen jsr U_OpenDir
                beq ++
                cmp #1
                bne +
                ; Dir empty
                rts
+               ; Can't open error
                rts
++               ; Prepare loop
                lda DATA_ADDR
                sta $fb
                lda DATA_ADDR+1
                sta $fc
                ; Write ".." to start of list
                lda #0
                sta U_NumDirStrings
                lda #2
                sta U_max_fn_len
                ldy #(FILE_RECORD_LENGTH-1)
-               lda Str_DirUp,y
                sta ($fb),y
                dey
                bpl -
                ; Read dir command
                jsr U_SendReadDirCmd
---             ; Read loop
                lda #FILE_RECORD_LENGTH
                clc
                adc DATA_ADDR
                sta DATA_ADDR
                sta $fb
                bcc +
                inc DATA_ADDR+1
                inc $fc
--              ;
+               inc U_NumDirStrings
                lda U_NumDirStrings
                cmp #255
                bne +
                lda #1
                sta bTooManyFiles
                jmp U_If_not_Idle_Abort
                ;
+               jsr U_ReadDataPack
                txa; data pack length
                bne +
                jmp ++
+               pha
                ; Fill U_is_img
                dex; Filename length
                txa
                pha
                tay
                ldx #3
-               lda ($fb),y
                sta FREEMEM,x
                dey
                dex
                bpl -
                lda #2
                sta U_len
                jsr UltIsFileImage
                sta U_is_img
                pla
                tax 
                ;
                cpx U_FileNameMaxLen
                bcc +
                beq +
                ldx U_FileNameMaxLen
+               cpx U_max_fn_len
                bcc +
                stx U_max_fn_len
+               ; Convert dir string to output format (see above)
                txa
                pha
                tay
-               lda ($fb),y
                sec
                sbc #$20
                tax
                lda AsciiToPetscii,x; <--- char conversion to Petscii
                iny
                sta ($fb),y
                dey
                dey
                bne -
                ; Y=0
                lda ($fb),y
                and #%00010000
                bne +
                ; file
                lda #"P"
                !byte $2c
+               ; dir
                lda #"F"
                ldy #19
                sta ($fb),y
                ; Y=0,1,after last char
                lda U_is_img
                ldy #0
                sta ($fb),y
                iny
                lda #0
                sta ($fb),y
                ;
                pla
                tay
                iny
                iny
                lda #0
                sta ($fb),y
                ; Set abbrev char if necessary
                pla
                sec
                sbc #2
                cmp U_FileNameMaxLen
                bcc +
                lda U_AbbrevChar
                ldy #17
                sta ($fb),y
                ;
+               lda bFileIsImage
                beq +
                lda U_NumDirStrings
                cmp #1
                bne +
                jmp --
+               jmp ---
++              lda bFileIsImage
                beq +
                lda U_NumDirStrings
                cmp #2
                bcc +
                dec U_NumDirStrings
+               rts

; Goes up in path
; Output in A:
; * 0: Ok
; * 3: No such directory
U_GoUpDir       lda U_CurTarget
                sta FREEMEM
                lda #DOS_CMD_CHANGE_DIR
                sta FREEMEM+1
                lda #'.'
                sta FREEMEM+2
                sta FREEMEM+3
                lda #4
                sta cmd_len
                jsr U_SendCommand
                jsr U_ReadStatusMsg
                jsr U_Accept
                lda FREEMEM+1
                and #%00000011
                rts

; Sets Ultimate's path to Root
U_GotoRoot      jsr U_GetPath
                lda U_CurPathAddr
                sta $fb
                lda U_CurPathAddr+1
                sta $fc
                ; Get number of /'s in path
                ldx #0
                ldy #0
-               lda ($fb),y
                cmp #47; '/'
                bne +
                inx
+               iny
                cpy U_CurPathLen
                bcc -
                ;
                cpx #2
                bcc +; already root
                dex
                txa
                tay
                ; do "cd.." y times
-               jsr U_GoUpDir
                dey
                bne -
+               rts

!zone Conversions
; Starts at ASCII $20
AsciiToPetscii  !byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F;20-2f
                !byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F;30-3f
                !byte $00,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F;40-4f
                !byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$5B,$2f,$5D,$1E,$A4;50-5f (5c ("\") --> "/")
                !byte $27,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F;60-6f (60 ("`") --> "'")
                !byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$3f,$7d,$3f,$3f,$3f;70-7f (7b,7d-7f: "?"; 7c ("|") --> 7d)
!zone System
U_Reboot        lda #4
                sta FREEMEM
                lda #6
                sta FREEMEM+1
                lda #2
                sta cmd_len
                jsr U_SendCommand
                rts