dirname         !pet "$"
channel         !byte 0
file_size       !byte 0,0; 4 digit hexadecimal number
num_files       !byte 0
exit_char       !byte 0
oldy            !byte 0
y_in_fd         !byte 0
file_size_dec   !byte 0,0; 4 digit decimal number
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
                lda #2
                jsr AddToFB
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
                ;+AddBitExToCurWnd BIT_EX_WND_ISERRMSG
                ;jsr UpdateWindow
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
                lda #%00001100
                sta VIC+21
                ; Wait for 1 screen frame
-               lda $d012
                cmp #199
                bne -
                rts

ShowDirectory   jsr BlackTaskbar
                jsr LoadDirectory
                lda error_code
                beq no_error
                jmp ShowDiskErrorEx
no_error        lda WindowType
                cmp #WT_DRIVE_A
                beq +
                +ControlSetStringListByte <STRING_LIST_DRIVEB, >STRING_LIST_DRIVEB, num_files
                jmp ++
+               +ControlSetStringListByte <STRING_LIST_DRIVEA, >STRING_LIST_DRIVEA, num_files
++              +ControlSetTopIndex 0
                jsr RepaintAll
                jmp PaintTaskbar

LoadDirectory   jsr UninstallIRQ
                +SelectControl 1
                lda #0
                sta ControlNumStr
                lda #$ff
                sta ControlHilIndex
                jsr UpdateControl
                ; Print "Loading..." into list box ...
                jsr PaintCurWindow
                +SelectControl 1
                jsr GetCtrlBufPos
                ldx BufWidth
                inx
                stx dummy
                txa
                jsr AddToFD
                lda #<Str_Loading
                sta $fb
                lda #>Str_Loading
                sta $fc
                jsr PrintStringLC
                ; ... and into window title
                lda #<Str_LoadingUC
                sta $fb
                lda #>Str_LoadingUC
                sta $fc
                jsr GetCurDeviceNo
                lda CurDeviceInd
                pha
                jsr SetFDFE
                ldy #0
                sty exit_char
                sty y_in_fd
                jsr ReadDirString
                lda #1
                sta Param
                jsr PaintTitleBar
                jsr WindowToScreen
                ;
                pla
                jsr SetFDFE
                lda StringListDrvLo,x
                sta $fb
                lda StringListDrvHi,x
                sta $fc
                ; Collect disk/drive info
                jsr DetectDriveType
                jsr IsItADiskDrive
                jsr IsWriteProtect
                ; If not disk drive, write ".." to FB and increase FB by 32
                ldx CurDeviceInd
                lda IsDiskDrive,x
                bne +
                ldy #31
-               lda Str_DirUp,y
                sta ($fb),y
                dey
                bpl -
                lda #32
                jsr AddToFB               
+               ; Load the directory
                jsr LoadDir
                bcc +
                ; An error occured
                sta error_code
                jmp ++
+               ; Begin parsing
                ; Read disk name and terminate with zero
                lda #$22
                sta exit_char
                ldy #6
                lda #0
                sta y_in_fd
                jsr ReadDirStringX
                ldy #16
-               dey
                lda ($fd),y
                cmp #$20
                beq -
                iny
                lda #0
                sta ($fd),y
                ;
                jsr ManipulStrList
                ;
                jsr GetDiskValues
                ;
                lda #53
                sta $01
                ;jsr PaintTaskbar
++              jmp InstallIRQ

; Sets FDFE to Str_Title_DriveA/B + 2
SetFDFE         tax
                lda Str_Title_DrvLo,x
                sta $fd
                lda Str_Title_DrvHi,x
                sta $fe
                lda #2
                jsr AddToFD
                rts

GetDiskValues   ; Get free blocks
                lda file_size
                sta free_blocks
                lda file_size+1
                sta free_blocks+1
                ; Get occupied space
                lda disk_size
                sec
                sbc free_blocks
                sta occupied
                lda disk_size+1
                sbc free_blocks+1
                sta occupied+1
                ; Convert free blocks number
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
                lda num_files
                sta file_size
                lda #0
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
                ;
                lda CurDeviceInd
                tay
                tax
                lda free_blocks
                sta BlocksFreeHexLo,x
                lda free_blocks+1
                sta BlocksFreeHexHi,x
                ;
                ldx #3
                tya
                bne +
                ; #8
-               lda str_free_blocks,x
                sta Str_BlocksFreeA,x
                lda str_occupied,x
                sta Str_OccupiedA,x
                lda str_num_files,x
                sta Str_NumFilesA,x
                dex
                bpl -
                ;
                ldx DriveType
                dex
                dex
                cpx #3
                bcc ++
                ; Drive types 0,1,5,6,7,8
                ldx #3
-               lda str_disk_size,x
                sta Str_DiskSizeA,x
                dex
                bpl -
                lda disk_size
                sta DiskSizeHexLo
                lda disk_size+1
                sta DiskSizeHexHi
                jmp ++
+               ; #9
-               lda str_free_blocks,x
                sta Str_BlocksFreeB,x
                lda str_occupied,x
                sta Str_OccupiedB,x
                lda str_num_files,x
                sta Str_NumFilesB,x
                dex
                bpl -
                ldx DriveType
                dex
                dex
                cpx #3
                bcc ++
                ; Drive types 0,1,5,6,7,8
                ldx #3
-               lda str_disk_size,x
                sta Str_DiskSizeB,x
                dex
                bpl -
                lda disk_size
                sta DiskSizeHexLo+1
                lda disk_size+1
                sta DiskSizeHexHi+1
++              rts

; Load the directory via the Kernal
; Required: 
; * load address in FBFC
; * DeviceNumber filled
LoadDir         lda #0
                sta error_code
                lda #$01      ; logical number
                ldx CurDeviceNo ; device number
                ldy #$00      ; secondary address
                jsr SETLFS    ; set file parameters ("OPEN 1,8,0")
                ldx #<dirname
                ldy #>dirname
                lda #$01      ; filename length
                jsr SETNAM    ; set filename
                ldx $fb       ; set load address
                ldy $fc       ; 
                lda #$00      ; load flag (01-ff for verify command)
                jmp LOAD      ; load the directory to STRING_LIST_DRIVEX

ManipulStrList  ; Prepare read/write
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                lda #0
                sta num_files
                sta disk_size
                sta disk_size+1
                ldx CurDeviceInd
                lda IsDiskDrive,x
                bne +
                inc num_files
+
--              ; Read file size and filename
                lda #32
                jsr AddToFB
                ;+AddValToFB 32
                ldy #0
                sty y_in_fd
                lda ($fb),y
                sta file_size
                clc
                adc disk_size
                sta disk_size
                iny
                lda ($fb),y
                sta file_size+1
                adc disk_size+1
                sta disk_size+1
                ; Write file size as word to end of record
                ldy #30
                lda file_size
                sta ($fd),y
                iny
                lda file_size+1
                sta ($fd),y
                ;
                ldy #1
                jsr CountSpaces
                cpx #0
                bne +
                ; End of dir
                rts
+               inc num_files
                iny
                lda #$22
                sta exit_char
                jsr ReadDirString; filename
                
                jsr PrintDelimiter
                
                jsr CountSpaces
                sty oldy
                ldy y_in_fd
                lda #$20
-               sta ($fd),y
                iny
                dex
                bne -
                sty y_in_fd
                ldy oldy
                ; Read file type
                lda #$20
                sta exit_char
                jsr ReadDirString; type
                
                jsr PrintDelimiter
                
                sty oldy
                ldy y_in_fd
                lda #$20
                sta ($fd),y
                iny
                cpx #4
                bcs +
                sta ($fd),y
                iny
+               sty y_in_fd
                ; Read file size
                jsr ConvertToDecStr
                ldx #0
-               lda str_file_size,x
                sta ($fd),y
                iny
                inx
                cpx #4
                bcc -
                lda #0
                sta ($fd),y
                lda $fb
                sta $fd
                lda $fc
                sta $fe
                jmp --

PrintDelimiter  sty oldy
                ldy y_in_fd
                lda #1
                sta ($fd),y
                inc y_in_fd
                ldy oldy
                rts

; Reads string from FBFC,y to FDFE,[y_in_fd]
; Waits for pet char in exit_char
; Returns strlen in X
ReadDirString   ldx #0
-               lda ($fb),y
                cmp exit_char
                bne +
                ; Has reached exit_char
                rts
+               sty oldy
                ldy y_in_fd
                sta ($fd),y
                ldy oldy
                iny
                inx
                inc y_in_fd
                jmp -

ReadDirStringX  ldx #0
--              lda ($fb),y
                cmp exit_char
                bne +
                ; Has reached exit_char
-               rts
+               sty oldy
                ldy y_in_fd
                sta ($fd),y
                ldy oldy
                iny
                inx
                cpx #16
                bcs -
                inc y_in_fd
                jmp --

; Counts spaces from ($fb),(y+1)
; Returns number of spaces in X
CountSpaces     ldx #$ff
-               iny
                inx
                lda ($fb),y
                cmp #$20
                beq -
                rts

; Converts file_size (hex) to string str_file_size
ConvertToDecStr jsr ConvertToDec
                jmp ConvertToString

; Converts file_size_dec to string str_file_size
ConvertToString ; Deal with sizes greater than 9999 blocks
                lda file_size_dec
                cmp #$ff
                bne +
                lda #$3e; ">"
                sta str_file_size
                lda #$31; "1"
                sta str_file_size+1
                lda #$30; "0"
                sta str_file_size+2
                lda #$4b; "K"
                sta str_file_size+3
                ;
+               lda file_size_dec+1
                lsr
                lsr
                lsr
                lsr
                ora #$30
                sta str_file_size
                ;
                lda file_size_dec+1
                and #%00001111
                ora #$30
                sta str_file_size+1
                ;
                lda file_size_dec
                lsr
                lsr
                lsr
                lsr
                ora #$30
                sta str_file_size+2
                ;
                lda file_size_dec
                and #%00001111
                ora #$30
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
ConvertToDec    ; Deal with sizes greater than 9999 blocks
                lda file_size+1
                cmp #$27
                bcc ++
                bne +
                lda file_size
                cmp #$10
                bcc ++
+               lda #$ff
                sta file_size_dec
                rts
                ;
++              SED                   ; Decimal-Mode aktivieren
                LDA #0                ; Ergebnis auf 0 initialisieren
                STA file_size_dec+0
                STA file_size_dec+1
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
                jsr BlackTaskbar
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
                bne copy_eof       ; either EOF or read error
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
                lda ($ae),y   ; get byte from memory
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
; DeviceNumber to buffer in FILEVIEWERDATA
ReadFileToViewerBuf
                jsr BlackTaskbar
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

; Deletes highlighted file in filelistview from disk
DeleteFile      jsr BlackTaskbar
                jsr GetFileName
deletefile      lda #"S"
                sta FREEMEM
                lda #":"
                sta FREEMEM+1
                ldx #$ff
-               inx
                lda Str_FileName,x
                sta FREEMEM+2,x
                bne -
                ;
                lda #0
                sta error_code
                jsr UninstallIRQ
                inx
                inx
                txa           ; command length
                jmp DiskSendCommand

RenameFile      jsr BlackTaskbar
                jsr GetFileName
                lda #"R"
                sta FREEMEM
                lda #":"
                sta FREEMEM+1
                +SelectControl 3
                ldx ControlParent+CTRLSTRUCT_CARRETPOS
                lda #"="
                sta FREEMEM+2,x
                dex
-               lda Str_FilenameEdit,x
                sta FREEMEM+2,x
                dex
                bpl -
                ldx ControlParent+CTRLSTRUCT_CARRETPOS
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

RenameDisk      jsr BlackTaskbar
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
                ldy #>Str_FilenameEdit
                jsr STROUT
                jsr CLRCHN
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
++              ; Closing #15 closes all channels
                lda #15
                sta channel
                jsr CloseChannel
                lda #53
                sta $01
                rts

; Formats disk in device with DeviceNumber
FormatDisk      ; Select edit control in dialog
                +SelectControl 1
                lda ControlParent+CTRLSTRUCT_CARRETPOS
                bne +
                ; No disk name specified
                rts
+               pha
                jsr BlackTaskbar
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
; Detects write protection of disk
; Expects DeviceNumber filled
; Output: 0/1 in WriteProtected
IsWriteProtect  lda #0
                sta res
                ldx CurDeviceInd
                lda IsDiskDrive,x
                beq ++
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
                ldx CurDeviceInd
++              lda res
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
Str_DiskSpace   !pet "n.a.","n.a."," 664"," 664","3160","n.a.","n.a.","n.a.","n.a."
DiskSpaceHexHi  !byte $27  , $27  , $02  , $02  , $0c  , $27  , $27  , $27  , $27
DiskSpaceHexLo  !byte $0f  , $0f  , $98  , $98  , $58  , $0f  , $0f  , $0f  , $0f

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
                lda CurDeviceInd
                pha
                ;
                tay
                ldx DriveType
                lda DiskSpaceHexLo,x
                sta DiskSizeHexLo,y
                lda DiskSpaceHexHi,x
                sta DiskSizeHexHi,y
                ;
                lda DriveType
                asl
                asl
                tax
                ldy #0
                pla
                bne +
                ; #A
-               lda Str_DriveTypes,x
                sta Str_DriveTypeA,y
                lda Str_DiskSpace,x
                sta Str_DiskSizeA,y
                inx
                iny
                cpy #4
                bcc -
                rts
+               ; #B
-               lda Str_DriveTypes,x
                sta Str_DriveTypeB,y
                lda Str_DiskSpace,x
                sta Str_DiskSizeB,y
                inx
                iny
                cpy #4
                bcc -
                rts

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

;DeleteFile      jsr BlackTaskbar
;                jsr GetFileName
;deletefile      lda #"S"
;                sta FREEMEM
;                lda #":"
;                sta FREEMEM+1
;                ldx #$ff
;-               inx
;                lda Str_FileName,x
;                sta FREEMEM+2,x
;                bne -
;                ;
;                lda #0
;                sta error_code
;                jsr UninstallIRQ
;                inx
;                inx
;                txa           ; command length
;                jmp DiskSendCommand

; Changes directory of non disk drive device
; Requires:
;  * Str_FileName filled
ChangeDir       jsr BlackTaskbar
                lda #"C"
                sta FREEMEM
                lda #"D"
                sta FREEMEM+1
                lda #":"
                sta FREEMEM+2
                lda ControlHilIndex
                bne +
                lda #95; Left arrow
                sta FREEMEM+3
                jsr UninstallIRQ
                lda #4
                jmp DiskSendCommand
                ;
+               ldx #$ff
-               inx
                lda Str_FileName,x
                sta FREEMEM+3,x
                bne -
                lda #0
                sta error_code
                jsr UninstallIRQ
                txa
                clc
                adc #3
                jmp DiskSendCommand

; Opens channel #15 and sends command
; Required:
; * Command length required in A
; * CurDeviceNo filled
DiskSendCommand ldx #$0f
                stx channel
                jsr OpenChannel
                lda error_code
                beq +
                ; Error
                rts
+               jmp CloseChannel

; Opens Channel [channel] with command in FREEMEM
; Requires:
; * command length in A
; * channel filled
; * CurDeviceNo filled
OpenChannel     ldx #0
                stx error_code
                NOP
                ldx #<FREEMEM
                ldy #>FREEMEM
                jsr SETNAM    ; call SETNAM
                NOP
                lda channel   ; file number [channel]
                ldx CurDeviceNo
                ldy channel   ; secondary address [channel]
                jsr SETLFS
                NOP
                jsr OPEN
                bcs +
                rts
+               ; Error
                sta error_code
CloseChannel    NOP
                jsr CLRCHN    ; call CLRCHN
                lda channel   ; filenumber [channel]
                jmp CLOSE     ; call CLOSE