channel         !byte 0
file_size       !byte 0,0; 4 digit hexadecimal number
num_files       !byte 0
real_num_files  !byte 0,0
oldy            !byte 0
y_in_fd         !byte 0
file_size_dec   !byte 0,0,0; 6 digit decimal number (actually 5 digits only)
str_file_size   !pet "1234",$22
str_occupied    !pet "1234",$22
str_num_files   !pet "1234",$22
str_free_blocks !pet "1234",$22
str_disk_size   !pet "1234",$22
disk_size       !byte 0,0
free_blocks     !byte 0,0
occupied        !byte 0,0

ShowDiskErrorEx ; Write "DISK ERROR" into titlebar of drive wnd
                ldx CurDeviceInd
                lda Str_Title_DrvLo,x
                sta $fb
                lda Str_Title_DrvHi,x
                sta $fc
                ldy #15
-               lda Str_Disk_Error,y
                sta ($fb),y
                dey
                bpl -
                ; An earlier version of the dialog also showed the error code
                ;lda error_code
                ;sta file_size
                ;lda #0
                ;sta file_size+1
                ;jsr ConvertToDecStr
                ;lda str_file_size+2
                ;sta Str_Mess_Error+12
                ;lda str_file_size+3
                ;sta Str_Mess_Error+13
                ;
ShowDiskError   jsr PaintTaskbar
                lda error_code
                cmp #30
                bcc ++
                sec
                sbc #30
                tax
                lda CustomErrorsLO,x
                sta err_pos
                lda CustomErrorsHI,x
                sta err_pos+1
                jmp +++
++              jsr Find_BASIC_Err
+++             jsr CopyErrToStr
                +ShowErrorMsg <Str_Mess_Error, >Str_Mess_Error
                rts

; Copies error string at err_pos to Str_Mess_Error
CopyErrToStr    lda #55
                sta $01
                ; String pos in BASIC ROM (or RAM)
                lda err_pos
                sta $fb
                lda err_pos+1
                sta $fc
                ; Clear string
                ldx #20
                lda #32
-               sta Str_Mess_Error,x
                dex
                bpl -
                ; Copy string
                ldx error_code
                inx
                lda Err_Offset_Tab,x
                tay
                lda #0
                sta Str_Mess_Error,y
                dey
-               lda ($fb),y
                bpl +
                and #%01111111
+               sta Str_Mess_Error,y
                dey
                bne -
                ;
                lda ($fb),y
                ora #%10000000
                sta Str_Mess_Error
                ;
                lda #53
                sta $01
                rts

Err_Offset_Tab  !byte 0,0,14,9,13,14,18,14,15,17,21,16,6,20,11,16,8,13,17,13,13,16,14,13,15,9,19,14,16,6,4
                !byte 20,13; GUI64 errors
err_pos         !byte 0,0
; Finds address of error string in BASIC ROM 
; with index error_code and writes it to err_pos
Find_BASIC_Err  ldx #0
                stx err_pos
                stx err_pos+1
                ldx error_code
                ; Calculate offset
-               lda Err_Offset_Tab,x
                clc
                adc err_pos
                sta err_pos
                lda err_pos+1
                adc #0
                sta err_pos+1
                dex
                bpl -
                ; Calculate address
                lda #<BASIC_ERR_START
                clc
                adc err_pos
                sta err_pos
                lda #>BASIC_ERR_START
                adc err_pos+1
                sta err_pos+1
                rts

UninstallIRQ    lda $d012
                cmp #200
                bne UninstallIRQ
                ;
                jsr DeinstallIRQ
                lda #0;#%00001100
                sta VIC+21
                ; Wait for 1 screen frame
-               lda $d012
                cmp #199
                bne -
                rts

ShowDirectory   jsr FakeTaskbar
                jsr FakeTaskbarBox
                jsr LoadDirectory
                lda error_code
                beq no_error
                jmp ShowDiskErrorEx
no_error        lda WindowType
                cmp #WT_DRIVE_A
                beq +
                +ControlSetStringListByte <STRING_LIST_DRIVEB, >STRING_LIST_DRIVEB, num_files
                lda num_files
                sta NumStrings+1
                jmp ++
+               +ControlSetStringListByte <STRING_LIST_DRIVEA, >STRING_LIST_DRIVEA, num_files
                lda num_files
                sta NumStrings
++              +ControlSetTopIndex 0
                jsr SetDrvWndWidth
                jsr UpdateWindow
                jsr RepaintAll
                lda bTooManyFiles
                beq +
                ; Show message if there are more than 255 files in directory
                lda CurDeviceInd
                asl
                asl
                tax
                inx
                inx
                inx
                ldy #2
-               lda Str_NumFiles,x
                sta Str_TooManyFiles+28,y
                dex
                dey
                bpl -
                +ShowMessage <Str_TooManyFiles, >Str_TooManyFiles
+               rts

LoadDirectory   jsr UninstallIRQ
                +SelectControl 1
                lda #0
                sta ControlNumStr
                lda #$ff
                sta ControlHilIndex
                jsr UpdateControl
                ; Print "LOADING..." into file list box ...
                jsr PaintCurWindow
                +SelectControl 1
                jsr GetCtrlBufPos
                ldx BufWidth
                inx
                txa
                jsr AddToFD
                lda #<Str_Loading
                sta $fb
                lda #>Str_Loading
                sta $fc
                jsr PrintStringLC
                ; Print "Loading..." into window title bar
                lda #<Str_LoadingUC
                sta $fb
                lda #>Str_LoadingUC
                sta $fc
                jsr GetCurDeviceNo
                jsr SetFDFEToTitle
                ldy #10
-               lda ($fb),y
                sta ($fd),y
                dey
                bpl -
                lda #1
                jsr PaintTitleBar
                jsr WindowToScreen
                ;
                jsr SetFDFEToTitle
                lda StringListDrvLo,x
                sta $fb
                lda StringListDrvHi,x
                sta $fc
                ; Collect disk/drive info
                jsr DetectDriveType
                jsr IsItADiskDrive
                jsr IsWriteProtect
                ; 
                ldx #0
                stx num_files
                dex
                stx max_entries; max_entries = 255
                ldx CurDeviceInd
                lda IsDiskDrive,x
                bne +
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ; If non-disk: write ".." to FB and increase FB by FILE_RECORD_LENGTH
                inc num_files
                ldy #(FILE_RECORD_LENGTH-1)
-               lda Str_DirUp,y
                sta ($fb),y
                dey
                bpl -
                lda #FILE_RECORD_LENGTH
                jsr AddToFB
                ; Go to SD2IEC root if necessary
                ldx CurDeviceNo
                lda bMayRoot,x
                beq +
                jsr GotoRootFullExt
                lda error_code
                bne ++
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
+               ; Load the directory
                ;
                jsr LoadDir
                lda error_code
                beq +
                ; An error occured
++              jmp InstallIRQ
                ;
+               ldx CurDeviceInd
                lda IsDiskDrive,x
                bne +
                ; If non-disk: search for folders and move them to top
                jsr SimpleSortList
+               ; Fill static disk info structs
                jsr GetDiskValues
                ;
                lda #53
                sta $01
                jmp InstallIRQ

; Sets FDFE to Str_Title_DriveA/B + 2
SetFDFEToTitle  ldx CurDeviceInd
                lda Str_Title_DrvLo,x
                sta $fd
                lda Str_Title_DrvHi,x
                sta $fe
                rts

bTooManyFiles   !byte 0
dirname         !pet "$"
max_entries     !byte 0
bReadsFiles     !byte 0
MAX_ROW_COUNT = 1
; Load the directory
; Required: 
; * load address in FBFC
; * CurDeviceNo filled
; * max_entries filled
LoadDir         jsr error_codeTo0
                sta max_fn_len_plus2
                sta MOTION_COUNTER
                sta is_left_move
                sta bTooManyFiles
                sta bReadsFiles
                sta disk_size
                sta disk_size+1
                sta real_num_files+1
                lda $fb
                sta $02
                lda $fc
                sta $03
                ;
                lda #$01      ; logical number
                ldx CurDeviceNo ; device number
                ldy #$00      ; secondary address
                jsr SETLFS    ; set file parameters ("OPEN 1,X,0")
                ldx #<dirname
                ldy #>dirname
                lda #$01      ; filename length
                jsr SETNAM    ; set filename
                ;
                jsr OPEN        ; open directory as file
                bcc +
                sta error_code
                jmp close_me
+               ldx #$01
                jsr CHKIN       ; set input device
                jsr DrawActiveBar
                jsr GETIN       ; read start address LSB and ignore
                lda $90
                beq +
                lda #4; FILE NOT FOUND
                sta error_code
                jmp close_me
+               jsr GETIN       ; read start address MSB and ignore
                jsr GETIN       ; read link address LSB and ignore
                ;
row             jsr MoveActiveBar
                jsr GETIN       ; read link address MSB and ignore
                ldy #0
                jsr GETIN       ; read blocks LSB
                sta ($02),y
                iny
                jsr GETIN       ; read blocks MSB
                sta ($02),y
                iny
getchar         jsr GETIN       ; read char
                sta ($02),y
                iny
                cmp #$00
                bne getchar     ; repeat until end of line
                ;
                jsr GETIN       ; read char
                cmp #$00
                beq close_dir_chan
                lda bReadsFiles
                bne +
                jsr ReadDiskName
                inc bReadsFiles
                jmp row
+               jsr Parse
                lda #FILE_RECORD_LENGTH
                jsr AddTo02
                inc num_files
                lda num_files
                cmp max_entries
                bcs load_rest
                jmp row
                ;
close_dir_chan  lda num_files
                sta real_num_files
                jmp close_dir_chanE
load_rest       lda num_files
                sta real_num_files
                sta bTooManyFiles; it's <> 0
                ; Loads the rest of the directory without saving the data to memory
--              jsr MoveActiveBar
                jsr GETIN       ; read link address MSB and ignore
                jsr GETIN       ; read blocks LSB
                ldy #0
                sta ($02),y
                jsr GETIN       ; read blocks MSB
                ldy #1
                sta ($02),y
-               jsr GETIN       ; read char
                cmp #$00
                bne -           ; repeat until end of line
                jsr GETIN       ; read char
                cmp #$00
                beq close_dir_chanE
                inc real_num_files
                bne +
                inc real_num_files+1
+               jsr AddToDiskSize
                jmp --
                ;
close_dir_chanE lda disk_size
                sta occupied
                lda disk_size+1
                sta occupied+1
                ; Get free blocks and add them to disk_size
                ldy #0
                lda ($02),y
                sta free_blocks
                clc
                adc disk_size
                sta disk_size
                iny
                lda ($02),y
                sta free_blocks+1
                adc disk_size+1
                sta disk_size+1
                bcc close_me
                ; Carry is set --> 16 bit overflow (> $ffff)
                ; E.g., root dirs of SD2IECs
                lda #$ff
                sta disk_size
                sta disk_size+1
                ;
close_me        jsr CLRCHN      ; end data input/output of file
                lda #$01 
                jsr CLOSE       ; close file
                rts

ACTION_WIDTH = 12
DrawActiveBar   ldx MOTION_COUNTER
                ldy #ACTION_WIDTH
                lda #239
-               sta SCRMEM+880+40+4,x
                inx
                dey
                bpl -
                rts

is_left_move    !byte 0
MoveActiveBar   ldx MOTION_COUNTER
                lda is_left_move
                beq +
                ; Left move
                ;lda #114
                ;sta SCRMEM+880+4+ACTION_WIDTH,x
                lda #160
                sta SCRMEM+880+40+4+ACTION_WIDTH,x
                ;lda #25
                ;sta SCRMEM+880+80+4+ACTION_WIDTH,x
                lda #239
                sta SCRMEM+880+40+3,x
                dec MOTION_COUNTER
                bne ++
                dec is_left_move
                rts
                ; Right move
+               ;lda #114
                ;sta SCRMEM+880+4,x
                lda #160
                sta SCRMEM+880+40+4,x
                ;lda #25
                ;sta SCRMEM+880+80+4,x
                inx
                lda #239
                sta SCRMEM+880+40+4+ACTION_WIDTH,x
                stx MOTION_COUNTER
                cpx #(33-ACTION_WIDTH)
                bcc ++
                inc is_left_move
++              rts

AddToDiskSize   ldy #0
                lda ($02),y
                clc
                adc disk_size
                sta disk_size
                iny
                lda ($02),y
                adc disk_size+1
                sta disk_size+1
                rts

max_fn_len_plus2!byte 0; (maximal filename length) + 2
Parse           ; Add file size in ($02) to disk_size
                jsr AddToDiskSize
                sty y_in_fd
                ; Find first "
-               iny
                lda ($02),y
                cmp #$22
                bne -
                ; Copy filename to $02,2
-               iny
                inc y_in_fd
                lda ($02),y
                cmp #$22
                beq +
                sty oldy
                ldy y_in_fd
                sta ($02),y
                ldy oldy
                jmp -
                ; Terminate with 0
+               sty oldy
                ldy y_in_fd
                lda #0
                sta ($02),y
                ; Update max_fn_len_plus2 if necessary
                cpy max_fn_len_plus2
                bcc +
                sty max_fn_len_plus2
+               ; Find first char after spaces
                ldy oldy
-               iny
                lda ($02),y
                cmp #$20
                beq -
                ; Save char at end of record
                sty oldy
                ldy #19
                sta ($02),y
                ; If it's DIR, replace by "F"
                cmp #"D"
                bne +
                ldy oldy
                iny
                lda ($02),y
                cmp #"E"
                beq +
                lda #"F"
                ldy #19
                sta ($02),y
+               rts

ReadDiskName    lda #4
                sta Val
                tay
                jsr SubValFromFD
-               lda ($02),y
                sta ($fd),y
                cmp #$22
                beq +
                iny
                cpy #20
                bcc -
+               lda #0
                sta ($fd),y
                lda #4
                jsr AddToFD
                ldy #15
                jsr KillSpaces
                rts

; Puts folders to top of list
SimpleSortList  ldx CurDeviceInd
                lda StringListDrvLo,x
                sta $fb
                sta $fd
                lda StringListDrvHi,x
                sta $fc
                sta $fe
                lda #FILE_RECORD_LENGTH
                jsr AddToFB
                jsr AddToFD
                ldx #1
                ;
--              ldy #(FILE_RECORD_LENGTH-1)
                lda ($fb),y
                cmp #$46; "F" (folder)
                bne +
                ; FB is folder
                ; Swap FB and FD
                ldy #(FILE_RECORD_LENGTH-1)
-               lda ($fb),y
                sta $d0
                lda ($fd),y
                sta ($fb),y
                lda $d0
                sta ($fd),y
                dey
                bpl -
                lda #FILE_RECORD_LENGTH
                jsr AddToFD
+               ; FB not a folder
                lda #FILE_RECORD_LENGTH
                jsr AddToFB
                inx
                cpx num_files
                bcc --
                rts

; Requires: disk_size, num_files
GetDiskValues   ; Convert free blocks number
                lda free_blocks
                sta file_size
                lda free_blocks+1
                sta file_size+1
                jsr ConvertToDecStr
                ldx #3
-               lda str_file_size,x
                sta str_free_blocks,x
                dex
                bpl -
                ; Convert occupied space number
                lda occupied
                sta file_size
                lda occupied+1
                sta file_size+1
                jsr ConvertToDecStr
                ldx #3
-               lda str_file_size,x
                sta str_occupied,x
                dex
                bpl -
                ; Convert num_files
                lda real_num_files
                sta file_size
                lda real_num_files+1
                sta file_size+1
                jsr ConvertToDecStr
                ldx #3
-               lda str_file_size,x
                sta str_num_files,x
                dex
                bpl -
                ; Convert disk size
                lda disk_size
                sta file_size
                lda disk_size+1
                sta file_size+1
                jsr ConvertToDecStr
                ldy #3
-               lda str_file_size,y
                sta str_disk_size,y
                dey
                bpl -
                ; Fill static data
                ldx CurDeviceInd
                lda max_fn_len_plus2
                sta Max_Fn_Len_Plus2,x
                lda free_blocks
                sta BlocksFreeHexLo,x
                lda free_blocks+1
                sta BlocksFreeHexHi,x
                lda disk_size
                sta DiskSizeHexLo,x
                lda disk_size+1
                sta DiskSizeHexHi,x
                ; X = 4X+3
                txa
                asl
                asl
                clc
                adc #3
                tax
                ;
                ldy #3
-               lda str_free_blocks,y
                sta Str_BlocksFree,x
                lda str_occupied,y
                sta Str_Occupied,x
                lda str_num_files,y
                sta Str_NumFiles,x
                dex
                dey
                bpl -
                ;
                ;ldy DriveType
                ;dey
                ;dey
                ;cpy #3
                ;bcc ++
                ; Before: Drive types 0,1,5,6,7,8
                inx
                txa
                clc
                adc #3
                tax
                ldy #3
-               lda str_disk_size,y
                sta Str_DiskSize,x
                dex
                dey
                bpl -
                jmp ++
++              rts

; Converts file_size (hex) to string str_file_size
ConvertToDecStr jsr ConvertToDec
                jmp ConvertToString

MakeHiNybChar   lsr
                lsr
                lsr
                lsr
                ora #$30
                rts

MakeLoNybChar   and #$0f
                ora #$30
                rts

; Converts file_size_dec to 4 char string str_file_size
ConvertToString ; Deal with sizes greater than 9999 blocks
                lda file_size_dec+2
                beq +
                jsr MakeLoNybChar
                sta str_file_size
                lda file_size_dec+1
                jsr MakeHiNybChar
                sta str_file_size+1
                lda #$20
                sta str_file_size+2
                lda #$4b; "K"
                sta str_file_size+3
                rts
                ;
+               lda file_size_dec+1
                jsr MakeHiNybChar
                sta str_file_size
                ;
                lda file_size_dec+1
                jsr MakeLoNybChar
                sta str_file_size+1
                ;
                lda file_size_dec
                jsr MakeHiNybChar
                sta str_file_size+2
                ;
                lda file_size_dec
                jsr MakeLoNybChar
                sta str_file_size+3
                ; Correction
                ldx #0
-               lda str_file_size,x
                cmp #$30
                bne +
                lda #$20
                sta str_file_size,x
                inx
                cpx #3
                bcc -
+               rts

; Converts file_size (hex) to file_size_dec (decimal)
ConvertToDec    SED                   ; Decimal-Mode aktivieren
                LDA #0                ; Ergebnis auf 0 initialisieren
                STA file_size_dec+0
                STA file_size_dec+1
                STA file_size_dec+2
                LDX #8                ; Anzahl der Binärwert-Bits
CNVBIT          ASL file_size         ; Ein Binär-Bit ins Carry
                LDA file_size_dec+0   ; und in BCD-Wert übernehmen
                ADC file_size_dec+0   ; ebenso durch Verdopplung
                STA file_size_dec+0   ; mittels Addition mit
                LDA file_size_dec+1   ; sich selbst in 4 BCD-Ziffern
                ADC file_size_dec+1
                STA file_size_dec+1
                DEX                   ; nächstes Binärwert-Bit
                BNE CNVBIT
                ;
                lda file_size+1
                beq +
                ; Add 256
-               lda file_size_dec
                clc
                adc #$56
                sta file_size_dec
                lda #$02
                adc file_size_dec+1
                sta file_size_dec+1
                lda #0
                adc file_size_dec+2
                sta file_size_dec+2
                dec file_size+1
                bne -
+               CLD             ; wieder Binary-Mode
                rts

;======================================================================

last_bytes      !byte 0
copypastelength !byte 0
write_appendix  !pet ",p,w"
write_fn        !pet "0123456789abcdef",0,0,0,0,0
write_length    !byte 0
; Copies and pastes file in Str_FileName from Disk
; DiskToCopyFrom to DiskToCopyTo in chunks of $100
; bytes (i.e. pages)
CopyPasteFile   lda #0
                sta Param
                jsr GetControlPtr
                lda $fb
                sta prog_bar
                lda $fc
                sta prog_bar+1
                ;
                jsr FakeTaskbar
                jsr UninstallIRQ
                ;
                ldx #0
-               lda Str_FileName,x
                beq +
                inx
                jmp -
+               stx copypastelength
                ; Copy Str_FileName to write_fn
                ldx #16
-               lda Str_FileName,x
                sta write_fn,x
                dex
                bpl -
                ; Append ",p,w" to end of write_fn
                ldy #0
                ldx copypastelength
-               lda write_appendix,y
                sta write_fn,x
                inx
                iny
                cpy #4
                bcc -
                stx write_length
                lda #0
                sta write_fn,x
                ; Setup buffer in memory
                lda #<FREEMEM
                sta $AE
                lda #>FREEMEM
                sta $AF
                ; Prepare source channel
                ;
                lda copypastelength
                ldx #<Str_FileName
                ldy #>Str_FileName
                jsr SETNAM
                ;NOP
                lda #$03      ; file number 3
                ldx DiskToCopyFrom
                ldy #$03      ; secondary address 3
                jsr SETLFS
                jsr OPEN
                bcc +
                ; Error
                sta error_code
                jmp copy_close
+               ; Prepare destination channel
                ;
                lda write_length
                ldx #<write_fn
                ldy #>write_fn
                jsr SETNAM
                lda #$02      ; file number 2
                ldx DiskToCopyTo
                ldy #$02      ; secondary address 2
                jsr SETLFS
                jsr OPEN
                bcc +
                ; Error
                sta error_code
                jmp copy_close
+               ;
--              ; Read a page from source
                ldx #$03      ; filenumber 3
                jsr CHKIN     ; file 3 now used as input
                ldy #$00
-               jsr READST
                bne copy_eof  ; either EOF or read error
                jsr CHRIN     ; get a byte from file
                sta ($AE),y   ; write byte to memory
                iny
                bne -
                jsr CLRCHN
                ; Write page to destination
                ldx #$02      ; filenumber 2
                jsr CHKOUT    ; file 2 now used as output
                ldy #$00
-               jsr READST
                bne writeerror; write error
                lda ($AE),y   ; get byte from memory
                jsr CHROUT    ; write byte to file
                iny
                bne -
                jsr UpdateProgbar
                jsr CLRCHN
                jmp --
copy_eof        and #$40
                beq copy_readerror
                ; End of file, write last bytes to dest file
                sty last_bytes
                LDX #$02      ; filenumber 2
                JSR CHKOUT    ; filenumber 2 is std output
                ldy #0
-               jsr READST
                bne writeerror; write error
                lda ($AE),y   ; get byte from memory
                jsr CHROUT    ; write byte to file
                iny
                cpy last_bytes
                bcc -
                jsr UpdateProgbar
copy_close      jsr CLRCHN
                lda #$03      ; filenumber 3
                jsr CLOSE
                lda #$02      ; filenumber 2
                jmp CLOSE

copy_readerror  sta error_code; STATUS byte
                jmp copy_close

writeerror      lda #31
                sta error_code
                lda DiskToCopyTo
                sta CurDeviceNo
                jsr IsWriteProtect
                ldx CurDeviceInd
                lda WriteProtected,x
                beq +
                ; Destination disk write protected
                lda #30
                sta error_code
+               jmp copy_close

prog_bar        !byte 0,0
UpdateProgbar   lda prog_bar
                sta $fb
                lda prog_bar+1
                sta $fc
                ldy #CTRLSTRUCT_VAL_LO
                lda ($fb),y
                tax
                inx
                txa
                sta ($fb),y
                bcc +
                ldy #CTRLSTRUCT_VAL_HI
                lda ($fb),y
                tax
                inx
                txa
                sta ($fb),y
+               jmp ShowTheDialog

; Reads file in Str_FileName from Disk with
; CurDeviceNo to buffer in FILEVIEWERDATA
ReadFileToViewerBuf
                jsr FakeTaskbar
                jsr UninstallIRQ
                ;
                lda #<FILEVIEWERBUF_END
                sta ViewerEOF
                lda #>FILEVIEWERBUF_END
                sta ViewerEOF+1
                ;
                ldx #0
                stx error_code
-               lda Str_FileName,x
                beq +
                inx
                jmp -
+               stx copypastelength
                ; Setup buffer in memory
                lda #<FILEVIEWERBUF_START
                sta $AE
                lda #>FILEVIEWERBUF_START
                sta $AF
                ; Prepare source channel
                lda copypastelength
                ldx #<Str_FileName
                ldy #>Str_FileName
                jsr SETNAM
                lda #$03      ; file number 3
                ldx CurDeviceNo
                ldy #$03      ; secondary address 3
                jsr SETLFS
                jsr OPEN
                bcc +
                ; Error
                jmp readerr
+               ; Read from file and save
                ldx #3        ; filenumber 3
                jsr CHKIN     ; file 3 now used as input
                ldx #(FILEVIEWERBUF_BLOCKS - 1)
                ldy #0
-               jsr READST
                bne eof       ; either EOF or read error
                jsr CHRIN     ; get a byte from file
                sta ($AE),y   ; write byte to memory
                iny
                bne -
                inc $AF
                dex
                bpl -
                jmp close
eof             and #$40
                beq readerr
                ; End of file
                tya
                clc
                adc $AE
                sta ViewerEOF
                lda $AF
                adc #0
                sta ViewerEOF+1
-               lda #32
                sta ($AE),y
                iny
                bne -
                inc $AF
                dex
                bpl -
close           jsr CLRCHN
                lda #$03      ; filenumber 3
                jsr CLOSE
                jmp InstallIRQ

readerr         sta error_code; STATUS byte
                jmp close

; Deletes highlighted file or directory in filelistview from disk
DeleteFile      jsr FakeTaskbar
                jsr GetFile
                lda Str_FileType
                cmp #"F"
                bne deletefile
                ; Delete directory
                lda #"R"
                sta FREEMEM
                lda #"D"
                sta FREEMEM+1
                lda #":"
                sta FREEMEM+2
                ldx #$ff
-               inx
                lda Str_FileName,x
                sta FREEMEM+3,x
                bne -
                inx
                inx
                inx
                jmp +
deletefile      ; Delete file
                lda #"S"
                sta FREEMEM
                lda #":"
                sta FREEMEM+1
                ldx #$ff
-               inx
                lda Str_FileName,x
                sta FREEMEM+2,x
                bne -
                inx
                inx
+               jsr UninstallIRQ
                txa ; command length
                jmp DiskSendCommand

RenameFile      jsr FakeTaskbar
                jsr GetFile
                lda #"R"
                sta FREEMEM
                lda #":"
                sta FREEMEM+1
                +SelectControl 3
                ldx ControlIndex+CTRLSTRUCT_CARRETPOS
                lda #"="
                sta FREEMEM+2,x
                dex
-               lda Str_FilenameEdit,x
                sta FREEMEM+2,x
                dex
                bpl -
                ldx ControlIndex+CTRLSTRUCT_CARRETPOS
                inx
                txa
                clc
                adc #<(FREEMEM+2)
                sta $fb
                lda #>(FREEMEM+2)
                adc #0
                sta $fc
                ldy #$ff
-               iny
                lda Str_FileName,y
                sta ($fb),y
                bne -
                ;
                jsr UninstallIRQ
                ldx #$ff
-               inx
                lda FREEMEM,x
                bne -
                txa
                jmp DiskSendCommand

renam_dsk_cmd1  !pet "u1 8 0 18 0",0
renam_dsk_cmd2  !pet "b-p 8 144",0
renam_dsk_cmd3  !pet "u2 8 0 18 0",0
renam_dsk_cmd4  !pet "i0",0

RenameDisk      jsr FakeTaskbar
                jsr UninstallIRQ
                ;
                lda #15
                sta channel
                jsr CloseChannel
                lda #"I"
                sta FREEMEM
                lda #"0"
                sta FREEMEM+1
                lda #":"
                sta FREEMEM+2
                lda #3
                jsr OpenChannel
                lda error_code
                bne ++
                ;
                lda #8
                sta channel
                jsr CloseChannel
                lda #"#"
                sta FREEMEM
                lda #0
                sta FREEMEM+1
                lda #2
                jsr OpenChannel
                lda error_code
                bne ++
                ; Map BASIC back in for STROUT
                lda #55
                sta $01
                ; print#15,"u1:"8;0;18;0:rem track 18,sector 0 lesen
                ldx #15
                jsr CHKOUT
                lda #<renam_dsk_cmd1
                ldy #>renam_dsk_cmd1
                jsr STROUT
                jsr CLRCHN
                ; print#15,"b-p:"8;144:rem pointer aufdisknamen setzen
                ldx #15
                jsr CHKOUT
                lda #<renam_dsk_cmd2
                ldy #>renam_dsk_cmd2
                jsr STROUT
                jsr CLRCHN
                ; print#8,nn$;:rem neuen disknamen setzen
                ldx #8
                jsr CHKOUT
                lda #<Str_FilenameEdit
                sta $fd
                lda #>Str_FilenameEdit
                sta $fe
                lda $fd
                ldy $fe
                jsr STROUT
                jsr CLRCHN
                ldy #15
                jsr KillSpaces
                ; print#15,"u2:"8;0;18;0:rem auf disk schreiben
                ldx #15
                jsr CHKOUT
                lda #<renam_dsk_cmd3
                ldy #>renam_dsk_cmd3
                jsr STROUT
                jsr CLRCHN
                ; print#15,"i0"
                ldx #15
                jsr CHKOUT
                lda #<renam_dsk_cmd4
                ldy #>renam_dsk_cmd4
                jsr STROUT
                jsr CLRCHN
                ;
++              ; Close channels
                lda #8
                jsr CLOSE
                lda #15
                jsr CLOSE
                lda #53
                sta $01
                rts

; Replaces spaces at end of string in FDFE with 0
; Required: end string index in Y
KillSpaces      lda ($fd),y
                cmp #$20
                bne +
                lda #0
                sta ($fd),y
                dey
                bpl KillSpaces
+               rts

; Formats disk in device with DeviceNumber
FormatDisk      ; Select edit control in dialog
                +SelectControl 1
                lda ControlIndex+CTRLSTRUCT_CARRETPOS
                bne +
                ; No disk name specified
                rts
+               pha
                jsr FakeTaskbar
                lda #"N"
                sta FREEMEM
                lda #":"
                sta FREEMEM+1
                pla
                tax
                tay
                iny
                iny
                dex
-               lda Str_FilenameEdit,x
                sta FREEMEM+2,x
                dex
                bpl -
                ; Check radio buttons
                tya
                pha
                +SelectControl 2
                pla
                tay
                lda ControlHilIndex
                beq +
                ; Full format (with ID)
                lda #","
                sta FREEMEM,y
                iny
                lda #"I"
                sta FREEMEM,y
                iny
                lda #"D"
                sta FREEMEM,y
                iny
+               jsr UninstallIRQ
                tya
                jmp DiskSendCommand

writeprot_cmd   !pet "m-r", $1e, 0, 1
; Detects write protection of disk if 1541
; Expects DeviceNumber filled
; Output: 0/1/2 in WriteProtected
;         0: no, 1: yes, 2: n.a.
IsWriteProtect  lda #2
                sta res
                lda DriveType
                cmp #2
                bne ++
                ; It's a 1541 drive
                lda #0
                sta res
                ; Wait a second...
                ldx #00
--              ldy #00
-               dey
                bne -
                dex
                bne --
                ;
                lda #0
                sta $90
                lda CurDeviceNo
                jsr LISTEN
                lda #$6f
                jsr LSTNSA
                bit $90
                bmi +
                ldy #$00
-               lda writeprot_cmd,y
                jsr IECOUT
                iny
                cpy #06
                bne -
                jsr UNLSTN
                lda #$00
                sta $90
                lda CurDeviceNo
                jsr TALK
                lda #$6f
                jsr TALKSA
                jsr IECIN
                cmp #$10
                beq +
                lda #1
                sta res
+               php
                jsr UNTALK
                plp
                ; Write to WriteProtected
++              ldx CurDeviceInd
                lda res
                sta WriteProtected,x
                rts

; CMD drive info at $fea4 in drive ROM
cmdinfo         !pet "m-r"
                !byte $a4,$fe,$02,$0d
; CBM drive info at $e5c5 in drive ROM
cbminfo         !pet "m-r"
                !byte $c5,$e5,$02,$0d
; 1581 drive info at $a6e8 in drive ROM
info1581        !pet "m-r"
                !byte $e8,$a6,$02,$0d
DriveType       !byte 0
Str_DriveTypes  !pet "n.a.","n.a.","1541","1571","1581"," FDD"," HDD"," RDD","RAML"
;Str_DiskSpace   !pet "n.a.","n.a."," 664"," 664","3160","n.a.","n.a.","n.a.","n.a."
;DiskSpaceHexHi  !byte $27  , $27  , $02  , $02  , $0c  , $27  , $27  , $27  , $27
;DiskSpaceHexLo  !byte $0f  , $0f  , $98  , $98  , $58  , $0f  , $0f  , $0f  , $0f

; Detects the drive type of device in DeviceNumber
; Expects DeviceNumber filled
; Output in DriveType8/9 as string (4 letters)
; Available representations in DriveType:
; 0 - No serial device available
; 1 - foreign drive (MSD, Excelerator, Lt.Kernal, etc.)
; 2 - 1541 drive
; 3 - 1571 drive
; 4 - 1581 drive
; 5 - FD drive
; 6 - HD drive
; 7 - RD drive
; 8 - RAMLink
DetectDriveType lda #0
                sta DriveType
                lda CurDeviceNo
                sta $ba
                ; Check drive
                ldy #0
                sty STATUS
                jsr LISTEN; opens the device for listening
                lda #$ff; Secondary address - $0f OR'ed with $f0 to open
                jsr LSTNSA; opens the channel with sa of 15
                lda STATUS; check the status byte
                bpl +
                ; ERROR
                ;
                rts
                ;
+               jsr openchannel
                ldx #<cmdinfo; check to see if it is a CMD drive first
                ldy #>cmdinfo
                jsr opentwo
                jsr CHRIN
                cmp #70; is it 'f' for FD series drives?
                bne +
                jsr CHRIN; get next character
                cmp #68; is it 'd' for FD series drives?
                bne l2
                ldy $ba; get device number
                lda #5;#$e0; indicates that it is a FD drive at device number
                jmp getdrive
+               cmp #72; is it 'h' for HD series drives?
                bne +
                jsr CHRIN; get next character
                cmp #68; is it 'd' for HD series drives?
                bne l2
                ldy $ba; get device number
                lda #6;#$c0; indicates that it is a HD drive at device number
                bne getdrive; relative JMP
+               cmp #82; is it 'r' for RL/RD series?
                bne l2
                jsr CHRIN; get next character
                cmp #68; is it 'd' for RD series?
                bne +
                ldy $ba; get device number
                lda #7;#$f0; indicates that it is a RD drive at device number
                bne getdrive; relative JMP
+               cmp #76; is it 'l' for RL series?
                bne l2
                ldy $ba; get device number
                lda #8;#$80; indicates that it is a RAMLink drive at device number
                bne getdrive; relative JMP
l2              ; Check for CBM devices
                jsr closechannel; close command channel
                jsr openchannel
                ldx #<cbminfo; check to see if it is a 1541/1571 drive
                ldy #>cbminfo
                jsr opentwo
                jsr CHRIN; gets the drive info
                cmp #53; is it '5' for the 15xx drives?
                bne l3
                jsr CHRIN; gets the next number
                cmp #52; is it '4' for the 1541?
                bne +
                ldy $ba; get device number
                lda #2;#41; indicates a 1541 at that device number
                bne getdrive; relative JMP
+               cmp #55; is it '7' for the 1571?
                bne l3
                ldy $ba; get device number
                lda #3;#71; indicates a 1571 at that device number
                bne getdrive; relative JMP
l3              ; Polls for a 1581 drive
                jsr closechannel; closes the command channel
                jsr openchannel
                ldx #<info1581; check to see if it is a 1581 drive
                ldy #>info1581
                jsr opentwo
                jsr CHRIN; gets the drive info
                cmp #53; is it a '5' for a 15xx drive?
                bne l4
                jsr CHRIN; gets the next drive number
                cmp #56; is it a '8' for a 1581?
                bne l4
                ldy $ba; gets device number
                lda #4;#81; indicates a 1581 at that device number
                bne getdrive; relative JMP
l4              ; foreign drive- just mark it as foreign
                ldy $ba; get device number
                lda #1; indicates a foreign device number
                bne getdrive; relative JMP
getdrive        sta DriveType
                jsr closechannel
                ;
                ;ldy CurDeviceInd
                ;;
                ;ldx DriveType
                ;lda DiskSpaceHexLo,x
                ;sta DiskSizeHexLo,y
                ;lda DiskSpaceHexHi,x
                ;sta DiskSizeHexHi,y
                ;
                ldx CurDeviceInd
                ldy y_tab,x
                lda cmp_tab,x
                sta smc_y+1
                lda DriveType
                asl
                asl
                tax
-               lda Str_DriveTypes,x
                sta Str_DriveType,y
                ;lda Str_DiskSpace,x
                ;sta Str_DiskSize,y
                inx
                iny
smc_y           cpy #4
                bcc -
                rts

y_tab           !byte 0,4
cmp_tab         !byte 4,8

closechannel    jsr CLRCHN
                lda #$0f; lfn
                jmp CLOSE; and closes it

; Opens the command channel and issues a command
openchannel     lda #$0f; lfn
                tay; sa for command channel
                ldx $ba
                jsr SETLFS; set up the open sequence
                lda #$07; length of command (m-r command)
                rts

opentwo         jsr SETNAM; sends the command
                jsr OPEN; opens the file
                ldx #$0f; lfn
                jmp CHKIN; redirect input

; Detects if current drive is a disk drive
; Puts result into IsDiskDrive,X
IsItADiskDrive  ldx CurDeviceInd
                lda #0
                ldy DriveType
                cpy #2
                bcc +
                cpy #5
                bcs +
                lda #1
+               sta IsDiskDrive,x
                rts

; Opens channel #15 and sends command in FREEMEM
; Required:
; * Command length required in A
; * CurDeviceNo filled
DiskSendCommand ldx #$0f
                stx channel
                jsr OpenChannel
                lda error_code
                beq +
                ; Error (channel has already been closed)
                rts
+               jmp CloseChannel

; Opens Channel [channel] with command in FREEMEM
; Requires:
; * command length in A
; * channel filled
; * CurDeviceNo filled
OpenChannel     ldx #0
                stx error_code
                ldx #<FREEMEM
                ldy #>FREEMEM
                jsr SETNAM    ; call SETNAM
                lda channel   ; file number [channel]
                ldx CurDeviceNo
                ldy channel   ; secondary address [channel]
                jsr SETLFS
                jsr OPEN
                bcs +
                rts
+               ; Error
                sta error_code
CloseChannel    jsr CLRCHN    ; call CLRCHN
                lda channel   ; filenumber [channel]
                jmp CLOSE     ; call CLOSE