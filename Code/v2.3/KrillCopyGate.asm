; Cartridge copy entry. Both designs keep their normal IRQ/FakeTaskbar stop.
; Control is in bank 6; the copy-only installer/drive image is in bank 7.
CopyPasteFile
!ifdef WIN {
                jsr UninstallIRQ_FakeTB
} else {
                jsr UninstallIRQ
}
                sei
                lda #$37
                sta $01
                lda #6
                sta $de00
                jsr Krill_ROMCopy
Krill_CopyAfterROM
                lda #$80
                sta $de00
                lda #$36
                sta $01
                cli
                rts

Krill_CopyInstall
                lda #7
                sta $de00
                jsr KRILL_CORE_INSTALL
                pha
                lda #6
                sta $de00
                pla
                rts
Krill_CopyUninstall
                lda #7
                sta $de00
                jsr KRILL_CORE_UNINSTALL
                lda #6
                sta $de00
Krill_CopyNoProgress
                rts
; Stage one outgoing packet while the destination directory is visible.
; FREEMEM is idle during target output; its next source read replaces it.
Krill_CopyStageRAM
                lda #$80
                sta $de00
                lda #$36; destination B extends into RAM under BASIC
                sta $01
                ldy #0
-               lda ($ae),y
                sta FREEMEM,y
                iny
                bne -
                beq Krill_CopyMapROM
; The existing painter uses $01=$36. Never expose a ROM return address
; while its mapping is disabled; perform the complete GUI call from RAM.
Krill_CopyProgress
                lda #$80
                sta $de00
                lda #$36
                sta $01
                jsr UpdateProgbar
Krill_CopyMapROM
                lda #$37
                sta $01
                lda #6
                sta $de00
                rts
Krill_CopyGateEnd = *
