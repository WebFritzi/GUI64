; Standard drive identification in bank 6. Mutable driveType stays in RAM.
cmdinfo         !pet "m-r"
                !byte $a4,$fe,$02,$0d
; CBM drive info at $e5c5 in drive ROM
cbminfo         !pet "m-r"
                !byte $c5,$e5,$02,$0d
; 1581 drive info at $a6e8 in drive ROM
info1581        !pet "m-r"
                !byte $e8,$a6,$02,$0d
DriveDetect_ROM ldy #0
                sty driveType
                ; Check drive
                sty STATUS
                lda CurDeviceNo
                jsr LISTEN; opens the device for listening
                lda #$ff; Secondary address - $0f OR'ed with $f0 to open
                jsr LSTNSA; opens the channel with sa of 15
                lda STATUS; check the status byte
                bpl +
                ; ERROR
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
                lda #5;#$e0; indicates that it is a FD drive at device number
                bne getdrive
+               cmp #72; is it 'h' for HD series drives?
                bne +
                jsr CHRIN; get next character
                cmp #68; is it 'd' for HD series drives?
                bne l2
                lda #6;#$c0; indicates that it is a HD drive at device number
                bne getdrive; relative JMP
+               cmp #82; is it 'r' for RL/RD series?
                bne l2
                jsr CHRIN; get next character
                cmp #68; is it 'd' for RD series?
                bne +
                lda #7;#$f0; indicates that it is a RD drive at device number
                bne getdrive; relative JMP
+               cmp #76; is it 'l' for RL series?
                bne l2
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
                lda #2;#41; indicates a 1541 at that device number
                bne getdrive; relative JMP
+               cmp #55; is it '7' for the 1571?
                bne l3
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
                lda #4;#81; indicates a 1581 at that device number
                bne getdrive; relative JMP
l4              ; foreign drive- just mark it as foreign
                lda #1; indicates a foreign device number
                ;bne getdrive; relative JMP
getdrive        sta driveType
                jsr closechannel
                ;
                ldx CurDeviceInd
                lda cmp_tab,x
                sta FREEMEM+$100
                lda driveType
                sta DriveType,x
                ldy y_tab,x
                ;lda driveType
                asl
                asl
                tax
-               lda Str_DriveTypes,x
                sta Str_DriveType,y
                ;lda Str_DiskSpace,x
                ;sta Str_DiskSize,y
                inx
                iny
                cpy FREEMEM+$100
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
                ldx CurDeviceNo
                jsr SETLFS; set up the open sequence
                lda #$07; length of command (m-r command)
                rts

opentwo         jsr SETNAM; sends the command
                jsr OPEN; opens the file
                ldx #$0f; lfn
                jmp CHKIN; redirect input

