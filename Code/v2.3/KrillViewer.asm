; ROM bank 6: headerless, bounded Viewer reads; all original file bytes are kept.
; Reuse installer/App scratch. The existing $6300-$63FF discard page stages blocks.
Krill_ViewerDest = KRILL_INSTALL_STATE_RAM+16 ; $64E3-$64E4: original receive base
Krill_ViewerIndex = KRILL_INSTALL_STATE_RAM+18 ; $64E5: initial receive Y
Krill_ViewerReady = KRILL_INSTALL_STATE_RAM+19
Krill_ViewerSeen = KRILL_INSTALL_STATE_RAM+20 ; four-byte block bitmap
Krill_ViewerProbe = KRILL_INSTALL_STATE_RAM+25 ; disambiguate empty file / not found
Krill_ViewerRemaining = KRILL_INSTALL_STATE_RAM+24
Krill_ViewerBlocks = (FILEVIEWERBUF_END-FILEVIEWERBUF_START+253)/254
!if Krill_ViewerBlocks > 32 { !error "Viewer prefix exceeds block bitmap" }
!if Krill_ViewerDest != $64e3 { !error "Viewer destination differs from Krill config" }
!if Krill_ViewerIndex != $64e5 { !error "Viewer index differs from Krill config" }
!if Krill_ViewerReady != $64e6 { !error "Viewer flag differs from Krill config" }
!if Krill_ViewerProbe >= WND_HEAP { !error "Viewer state exceeds FREEMEM" }
!if (FILEVIEWERBUF_START & $ff) != 0 { !error "Viewer start must be page aligned" }
!if FILEVIEWERBUF_END != $fffa { !error "Viewer must stop before hardware vectors" }

Krill_ROMLoadViewer
                sei
                cld
                ldx #7
-               lda $e0,x
                sta Krill_AppSavedZP,x
                dex
                bpl -
                lda #0
                sta error_code
                sta KRILL_MDAPPERROR
                sta Krill_ViewerProbe
                ldx #3
-               sta Krill_ViewerSeen,x
                dex
                bpl -
                lda #Krill_ViewerBlocks
                sta Krill_ViewerRemaining
                ; Clear the display tail in advance; never touch $FFFA-$FFFF.
                lda #0
                sta $ae
                sta ViewerEOF
                lda #>FILEVIEWERBUF_START
                sta $af
                sta ViewerEOF+1
Krill_ViewerClear
                lda #32
                ldy #0
                sta ($ae),y
                jsr Krill_ViewerAdvance
                bcc Krill_ViewerClear
                lda $ba
                cmp CurDeviceNo
                beq +
                lda #5
                sta error_code
                jmp Krill_ViewerFinish
+               lda Krill_NativeActive
                bne Krill_ViewerNative
                jsr Krill_ViewerFallback
                jmp Krill_ViewerFinish
Krill_ViewerNative
                lda #<FILEVIEWERBUF_START
                sta KRILL_LOADADDRLO
                lda #>FILEVIEWERBUF_START
                sta KRILL_LOADADDRHI
                lda #3
                sta KRILL_MDAPPMODE
                ldx #<Str_FileName
                ldy #>Str_FileName
                sec             ; fixed Viewer destination, no PRG header
                jsr KRILL_CORE_OPENFILE
                bcc Krill_ViewerPoll
                jmp Krill_ViewerTransferError
Krill_ViewerPoll
                lda #0
                sta Krill_ViewerReady
                jsr KRILL_CORE_POLLBLOCK
                bcs Krill_ViewerDone
                lda Krill_ViewerReady
                beq Krill_ViewerPoll
                lda KRILL_BLOCKDESTLO ; block index; reject suffix/wrapped addresses
                cmp #Krill_ViewerBlocks
                bcs Krill_ViewerPoll
                lda Krill_ViewerDest
                sta $ae
                lda Krill_ViewerDest+1
                sta $af
                ldy Krill_ViewerIndex
Krill_ViewerCopy
                iny
                beq Krill_ViewerCopied
                tya
                clc
                adc Krill_ViewerDest
                sta $ae
                lda Krill_ViewerDest+1
                adc #0
                sta $af
                jsr Krill_ViewerAtLimit
                bcs Krill_ViewerCopied
                lda FREEMEM,y
                ldx #0
                sta ($ae,x)
                jsr Krill_ViewerAdvance
                ; ViewerEOF is a high-water mark because blocks arrive out of order.
                lda $ae
                cmp ViewerEOF
                lda $af
                sbc ViewerEOF+1
                bcc Krill_ViewerCopy
                lda $ae
                sta ViewerEOF
                lda $af
                sta ViewerEOF+1
                jmp Krill_ViewerCopy
Krill_ViewerCopied
                ; Stop early only after every block in the visible prefix arrived.
                lda KRILL_BLOCKDESTLO
                and #7
                tax
                lda Krill_ViewerBits,x
                pha
                lda KRILL_BLOCKDESTLO
                lsr
                lsr
                lsr
                tax
                pla
                tay
                and Krill_ViewerSeen,x
                bne +
                tya
                ora Krill_ViewerSeen,x
                sta Krill_ViewerSeen,x
                dec Krill_ViewerRemaining
+               jsr PaintHourglass
                lda Krill_ViewerRemaining
                beq Krill_ViewerClose
                jmp Krill_ViewerPoll
Krill_ViewerDone
                cmp #0
                beq Krill_ViewerClose
Krill_ViewerTransferError
                cmp #$fe
                beq Krill_ViewerDeviceError
                cmp #$ff
                bne Krill_ViewerFileError
                lda ViewerEOF
                cmp #<FILEVIEWERBUF_START
                bne Krill_ViewerFileError
                lda ViewerEOF+1
                cmp #>FILEVIEWERBUF_START
                bne Krill_ViewerFileError
                ; Empty native files share $FF with FILE_NOT_FOUND. Confirm
                ; the DOS stream exists after uninstall, before accepting empty.
                lda #1
                sta Krill_ViewerProbe
                bne Krill_ViewerClose
Krill_ViewerFileError
                lda #4
                !byte $2c
Krill_ViewerDeviceError
                lda #5
                sta error_code
Krill_ViewerClose
                jsr KRILL_CORE_CLOSEFILE
Krill_ViewerFinish
                jsr Krill_ViewerUninstall
                lda Krill_ViewerProbe
                beq +
                jsr Krill_ViewerProbeEmpty
+               lda #0
                sta KRILL_MDAPPMODE
                ldx #7
-               lda Krill_AppSavedZP,x
                sta $e0,x
                dex
                bpl -
                lda error_code
                rts
Krill_ViewerBits !byte 1,2,4,8,16,32,64,128

; Raw KERNAL reader: no PRG header, same bounded prefix and EOF as the native path.
Krill_ViewerOpen
                ldx #0
-               lda Str_FileName,x
                beq +
                inx
                bne -
+               stx copypastelength
                jsr PrepareSourceCh
                ldx CurDeviceNo
                jsr SETLFS_OPEN
                bcs +
                ldx #3
                jsr CHKIN
+               rts
Krill_ViewerFallback
                jsr Krill_ViewerOpen
                bcs Krill_ViewerReadError
                jsr Krill_ViewerCheckDOS
                bne Krill_ViewerReadClose
                lda #<FILEVIEWERBUF_START
                sta $ae
                lda #>FILEVIEWERBUF_START
                sta $af
Krill_ViewerRead
                jsr READST
                bne Krill_ViewerEOF
                jsr CHRIN
                ldy #0
                sta ($ae),y
                jsr Krill_ViewerAdvance
                lda $ae
                sta ViewerEOF
                lda $af
                sta ViewerEOF+1
                bcs Krill_ViewerReadClose
                lda $ae
                bne Krill_ViewerRead
                jsr PaintHourglass
                jmp Krill_ViewerRead
Krill_ViewerEOF
                and #$bf        ; EOF is success; other status bits are errors
                beq Krill_ViewerReadClose
Krill_ViewerReadError
                lda #4
                sta error_code
Krill_ViewerReadClose
                jsr CLRCHN
                lda #3
                jsr CLOSE
                lda #15
                jmp CLOSE

; Only the ambiguous native $FF result needs a DOS existence check.
Krill_ViewerProbeEmpty
                jsr Krill_ViewerOpen
                bcs Krill_ViewerReadError
                jsr Krill_ViewerCheckDOS
                jmp Krill_ViewerReadClose

; OPEN alone does not reliably report FILE NOT FOUND on IEC drives. Check
; the DOS status before accepting an empty file or beginning fallback input.
; Keep channel 15 open until the read ends: closing it also closes the
; drive's other channels. Select input 3 again after reading the status.
Krill_ViewerCheckDOS
                lda #0
                jsr SETNAM
                lda #15
                ldx CurDeviceNo
                ldy #15
                jsr SETLFS_OPEN
                bcs Krill_ViewerDOSError
                ldx #15
                jsr CHKIN
                bcs Krill_ViewerDOSError
                jsr CHRIN
                cmp #"0"
                bne Krill_ViewerDOSError
                jsr CHRIN
                cmp #"0"
                bne Krill_ViewerDOSError
                jsr READST
                beq Krill_ViewerDOSClose
Krill_ViewerDOSError
                lda #4
                sta error_code
Krill_ViewerDOSClose
                jsr CLRCHN
                ldx #3
                jsr CHKIN
                lda error_code
                rts

; $AE/$AF points to the next Viewer byte. End is exclusive, below NMI/RESET/IRQ.
Krill_ViewerAdvance
                inc $ae
                bne Krill_ViewerAtLimit
                inc $af
Krill_ViewerAtLimit
                lda $ae
                cmp #<FILEVIEWERBUF_END
                lda $af
                sbc #>FILEVIEWERBUF_END
                rts
