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
                sta bMayRoot-8,x
                ldx CurDeviceInd
                bne ++
                sta PATH_A
                sta PathLength
+               rts
++              sta PATH_B
                sta PathLength+1
                rts

SendLeftArrow   lda #95
                sta FREEMEM+2
                lda #3
                jmp DiskSendCommand

; Roots SD2IEC at dev no CurDeviceNo
GotoRootFull    jsr GotoRoot
                lda error_code
                bne +; rts in GotoPath
                ; Comment this out for use in VICE
                jsr SendLeftArrow
                lda error_code
                bne +; rts in GotoPath
; Sets SD2IEC back to root of current image
GotoRoot        lda #"C"
                sta FREEMEM
                lda #"D"
                sta FREEMEM+1
                lda #47; "/"
                sta FREEMEM+2
                sta FREEMEM+3
                lda #4
                jmp DiskSendCommand

; Sends drive with CurDeviceNo to path in 0203
GotoPath        jsr GotoRootFull
                lda error_code
                bne +++
                ; SD2IEC "/"
                ; VICE ":"
                lda #"/"
                sta FREEMEM+2
                ;
                ldx CurDeviceInd
                ldy PathLength,x
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
                jmp DiskSendCommand
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
                jmp DiskSendCommand
+++             rts

; Changes directory of non disk drive device
; Requires:
;  * Str_FileName filled
ChangeDir       
!ifdef WIN{
                jsr UninstallIRQ_FakeTB
} else ifdef MAC{
                jsr UninstallIRQ
}
                jsr Pause
                ; Retrieve pointer in 0203 to PATH_A/B depending on CurDeviceInd
                lda #<PATH_A
                sta $02
                lda #>PATH_A
                sta $03
                ldx CurDeviceInd
                beq +
                inc $03
                ;
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
                jsr SendLeftArrow
                lda error_code
                bne path_err
                ; Correct DnpEndPos, path string and length
                ldx CurDeviceInd
                ldy PathLength,x
                dey
                ldx #0
                cpy DnpEndPosInPath
                bne +
                stx DnpEndPosInPath
+               
-               inx
                dey
                cpy #$ff
                beq +
                lda ($02),y
                cmp #47; "/"
                bne -
+               ; carry is set
                iny
                lda #0
                sta ($02),y
                ; new path length
                stx ZP_5F
                ldx CurDeviceInd
                lda PathLength,x
                ;sec
                sbc ZP_5F
                sta PathLength,x
++              rts
                ;------------------
+++             ; Go FORWARD
                ;------------------
                jsr Str_FnToFreeMem
                stx ZP_5F; str len of dirname
                txa
                clc
                adc #3
                jsr DiskSendCommand
                lda error_code
                bne path_err
                ; Correct path string and length
                ldy #0
                ldx CurDeviceInd
                ldy PathLength,x
                iny
                ; path length
                tya
                clc
                adc ZP_5F
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
                ldx ZP_5F
                dex
-               lda Str_FileName,x
                sta ($02),y
                dey
                dex
                bpl -
                ;
                rts

; Creates a subdirectory in the current directory
; Requires Str_DialogEdit filled
CreateDirectory 
!ifdef WIN{
                jsr UninstallIRQ_FakeTB
}
                lda #"M"
                sta FREEMEM
                lda #"D"
                sta FREEMEM+1
                lda #":"
                sta FREEMEM+2
                ;
                jsr StrDialogEditToFD
                ldy #15
                jsr KillSpaces
                ;
                ldx #$ff
-               inx
                lda Str_DialogEdit,x
                sta FREEMEM+3,x
                bne -
                ;
                jsr error_codeTo0
!ifdef MAC{
                jsr UninstallIRQ
}
                inx
                inx
                inx
                txa
                jmp DiskSendCommand

; Sizes of disk images (* indicates disks with error information)
; d64 : 174848 (minus one: 174847 = $02 AA FF -> FF AA 02)
; d64*: 175531 (minus one: 175530 = $02 AD AA -> AA AD 02)
; d71 : 349696 (minus one: 349695 = $05 55 FF -> FF 55 05)
; d71*: 351062 (minus one: 351061 = $05 5B 55 -> 55 5B 05)
; d81 : 819200 (minus one: 819199 = $0C 7F FF -> FF 7F 0C)
; dnp : multiple of 65536 (minus one: ?????? = $?? ?? ??)

ImageNameSuffix !pet ".xxx"
write_appendix  !pet ",p,w"
;ImageName       !pet "0123456789ab.xxx,p,w"
Channel2Str     !pet "p",3,255,170,2
drive_data      !byte $ff, $aa, $02 ; d64
                !byte $ff, $55, $05 ; d71
                !byte $ff, $7f, $0c ; d81
                !byte $ff, $ff, $0f ; dnp: fixed size 1 00 00
createFile_cmd  !pet "x",0

; Creates an image file in the current directory
; Requires Str_DialogEdit and Ctrl_NF_ImgType filled
CreateImageFile jsr error_codeTo0
!ifdef WIN{
                jsr UninstallIRQ_FakeTB
} else ifdef MAC{
                jsr UninstallIRQ
}
                ; Prepare edit string
                jsr StrDialogEditToFD
                ldy #11
                jsr KillSpaces
                ldx #3
-               lda Ctrl_NF_ImgType,x
                sta ImageNameSuffix,x
                dex
                bpl -
                ; Prepare write_fn
                ldx #$ff
-               inx
                lda Str_DialogEdit,x
                sta write_fn,x
                bne -
                ldy #0
-               lda ImageNameSuffix,y
                sta write_fn,x
                inx
                iny
                cpy #8
                bcc -
                lda #0
                sta write_fn,x
                ; open
                txa ; string length
                ldx #<write_fn
                ldy #>write_fn
                jsr SETNAM    ; call SETNAM
                lda #1   ; file number
                ldx CurDeviceNo
                ldy #3   ; secondary address
                jsr SETLFS_OPEN
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
                jsr SETLFS_OPEN
                bcc +
                sta error_code
                jmp closem
+               ; print#1,"x"
                ldx #1
                jsr CHKOUT
                lda #<createFile_cmd
                ldy #>createFile_cmd
                jsr STROUT_CLRCHN
closem          ; close1
                lda #1
                jsr CLOSE
                lda #2
                jmp CLOSE