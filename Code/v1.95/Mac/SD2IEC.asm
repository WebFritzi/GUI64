; PATH:
; string of path, example: "Dir1/Dir2/Dir3/"
; last byte: 0
; Root: 0
; Path length is saved in PathLength (Data.asm)
;-------------------------------------------------

; Called from LoadDirectory
; Roots SD2IEC at dev no CurDeviceNo
; Sets bMayRoot, DnpEndPosInPath, and PATH_A/B to 0
; Required:
; CurDeviceNo and CurDeviceInd filled
GotoRootFullExt jsr GotoRootFull
                lda error_code
                bne +
                ; Set DnpEndPosInPath, bMayRoot, and PATH_A/B
                ldx CurDeviceNo
                lda #0
                sta DnpEndPosInPath
                sta bMayRoot,x
                ldx CurDeviceInd
                bne ++
                sta PATH_A
                sta PathLength
+               rts
++              sta PATH_B
                sta PathLength+1
                rts

; Roots SD2IEC at dev no CurDeviceNo
GotoRootFull    jsr GotoRoot
                lda error_code
                bne +
                ;; Comment this out for use in VICE
                lda #95; Left arrow
                sta FREEMEM+2
                lda #3
                jsr DiskSendCommand
                lda error_code
                bne +
                jsr GotoRoot
                lda error_code
                bne +
+               rts

; Sets SD2IEC back to root of current image
GotoRoot        jsr WriteCDtoFreMem
                lda #47; "/"
                sta FREEMEM+2
                sta FREEMEM+3
                lda #4
                jmp DiskSendCommand

WriteCDtoFreMem lda #"C"
                sta FREEMEM
                lda #"D"
                sta FREEMEM+1
                rts

; Retrieves pointer in 0203 to PATH_A/B depending on CurDeviceInd
GetPath02       lda #<PATH_A
                sta $02
                lda #>PATH_A
                sta $03
                ldx CurDeviceInd
                beq +
                inc $03
+               rts

; Sends drive with CurDeviceNo to path in 0203
GotoPath        jsr GotoRootFull
                lda error_code
                bne +++
                ; SD2IEC "/"
                ; VICE ":"
                lda #"/"
                sta FREEMEM+2

                ldx CurDeviceInd
                lda PathLength,x
                tay
                beq + ; path is root
                ; Copy path to FREEMEM
                dey
-               lda ($02),y
                sta FREEMEM+3,y
                dey
                bpl -
                ;
                ldy DnpEndPosInPath
                bne ++
                lda PathLength,x
                clc
                adc #2;#3
                jsr DiskSendCommand
+               rts
                ; It's a path with dnp file
++              iny
                iny
                iny
                tya
                jsr DiskSendCommand
                lda error_code
                bne +++
                ldy DnpEndPosInPath
                iny
                lda ($02),y
                beq +++ ; path is root of dnp
                dey
                ldx #2
-               iny
                inx
                lda ($02),y
                sta FREEMEM,x
                bne -
                ;dex
                txa
                jsr DiskSendCommand
+++             rts

; Changes directory of non disk drive device
; Requires:
;  * Str_FileName filled
ChangeDir       jsr UninstallIRQ
                jsr Pause
                jsr GetPath02
                jsr GotoPath
                lda error_code
                beq +
path_err        jmp InstallIRQ
+               lda ControlHilIndex
                bne +++
                ;------------------
                ; Go BACKWARDS (..)
                ;------------------
                ldx CurDeviceInd
                lda PathLength,x
                beq ++ ; path is root
                ; path is not root
                lda #95; Left arrow
                sta FREEMEM+2
                lda #3
                jsr DiskSendCommand
                lda error_code
                bne path_err
                ; Correct DnpEndPos, path string and length
                ldx CurDeviceInd
                lda PathLength,x
                tay
                dey
                cpy DnpEndPosInPath
                bne +
                ldx #0
                stx DnpEndPosInPath
+               ldx #0
-               inx
                dey
                cpy #$ff
                beq +
                lda ($02),y
                cmp #47; "/"
                bne -
+               iny
                lda #0
                sta ($02),y
                ; new path length
                stx dummy
                ldx CurDeviceInd
                lda PathLength,x
                sec
                sbc dummy
                sta PathLength,x
++              rts
                ;------------------
+++             ; Go FORWARD
                ;------------------
                lda #":"
                sta FREEMEM+2
                ldx #$ff
-               inx
                lda Str_FileName,x
                sta FREEMEM+3,x
                bne -
                stx dummy; str len of dirname
                txa
                clc
                adc #3
                jsr DiskSendCommand
                lda error_code
                bne path_err
                ; Correct path string and length
                ldy #0
                ldx CurDeviceInd
                lda PathLength,x
                tay
                iny
                ; path length
                tya
                clc
                adc dummy
                sta PathLength,x
                ;
                tay
                lda #0
                sta ($02),y
                dey
                lda #47; "/"
                sta ($02),y
                
                lda is_dnp
                beq +
                sty DnpEndPosInPath
                
+               dey
                ldx dummy
                dex
-               lda Str_FileName,x
                sta ($02),y
                dey
                dex
                bpl -
                ;
                rts

; Creates a subdirectory in the current directory
; Requires Str_ImageEdit filled
CreateDirectory lda #"M"
                sta FREEMEM
                lda #"D"
                sta FREEMEM+1
                lda #":"
                sta FREEMEM+2
                ;
                lda #<Str_ImageEdit
                sta $fd
                lda #>Str_ImageEdit
                sta $fe
                ldy #15
                jsr KillSpaces
                ;
                ldx #$ff
-               inx
                lda Str_ImageEdit,x
                sta FREEMEM+3,x
                bne -
                ;
                jsr error_codeTo0
                jsr UninstallIRQ
                inx
                inx
                inx
                txa
                jmp DiskSendCommand
                rts

; Sizes of disk images (* indicates disks with error information)
; d64 : 174848 (minus one: 174847 = $02 AA FF -> FF AA 02)
; d64*: 175531 (minus one: 175530 = $02 AD AA -> AA AD 02)
; d71 : 349696 (minus one: 349695 = $05 55 FF -> FF 55 05)
; d71*: 351062 (minus one: 351061 = $05 5B 55 -> 55 5B 05)
; d81 : 819200 (minus one: 819199 = $0C 7F FF -> FF 7F 0C)
; dnp : multiple of 65536 (minus one: ?????? = $?? ?? ??)

ImageNameSuffix !pet ".xxx,p,w"
ImageName       !pet "0123456789ab.xxx,p,w"
Channel2Str     !pet "p",3,255,170,2
drive_data      !byte $ff, $aa, $02 ; d64
                !byte $ff, $55, $05 ; d71
                !byte $ff, $7f, $0c ; d81
                !byte $ff, $ff, $0f ; dnp: fixed size 1 00 00
createFile_cmd  !pet "x",0

; Creates an image file in the current directory
; Requires Str_ImageEdit and Ctrl_NF_ImgType filled
CreateImageFile jsr error_codeTo0
                jsr UninstallIRQ
                ; Prepare edit string
                lda #<Str_ImageEdit
                sta $fd
                lda #>Str_ImageEdit
                sta $fe
                ldy #11
                jsr KillSpaces
                ldx #3
-               lda Ctrl_NF_ImgType,x
                sta ImageNameSuffix,x
                dex
                bpl -
                ; Prepare ImageName
                ldx #$ff
-               inx
                lda Str_ImageEdit,x
                sta ImageName,x
                bne -
                ldy #0
-               lda ImageNameSuffix,y
                sta ImageName,x
                inx
                iny
                cpy #8
                bcc -
                lda #0
                sta ImageName,x
                ; open
                txa ; string length
                ldx #<ImageName
                ldy #>ImageName
                jsr SETNAM    ; call SETNAM
                lda #1   ; file number
                ldx CurDeviceNo
                ldy #3   ; secondary address
                jsr SETLFS
                jsr OPEN
                bcc +
                sta error_code
                jmp closem
                ; open2,8,15,"p"+chr$(3)+chr$(255)+chr$(170)+chr$(2) (for d64)
                ;
                ; Prepare Channel2Str
+               lda #<drive_data
                sta $fd
                lda #>drive_data
                sta $fe
                lda #3
                jsr SelectControl
                lda ControlHilIndex
                asl
                clc
                adc ControlHilIndex
                sec
                sbc #3
                jsr AddToFD
                ldy #2
-               lda ($fd),y
                sta Channel2Str+2,y
                dey
                bpl -
                ; open
                lda #5
                ldx #<Channel2Str
                ldy #>Channel2Str
                jsr SETNAM    ; call SETNAM
                lda #2   ; file number
                ldx CurDeviceNo
                ldy #15   ; secondary address
                jsr SETLFS
                jsr OPEN
                bcc +
                sta error_code
                jmp closem
+               ; Map BASIC in for STROUT
                lda #55
                sta $01
                ; print#1,"x"
                ldx #1
                jsr CHKOUT
                lda #<createFile_cmd
                ldy #>createFile_cmd
                jsr STROUT
                jsr CLRCHN
closem          ; close1
                lda #1
                jsr CLOSE
                lda #2
                jsr CLOSE
                ; Map BASIC out
                lda #53
                sta $01
                rts