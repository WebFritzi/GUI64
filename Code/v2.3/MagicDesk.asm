; Magic Desk boot ROM. Build with Build-MagicDesk.ps1.
; Bank 0: startup/copier. Banks 1..4: GUI64.
; Bank 5: installer/App control. Bank 6: game/guard/startup. Bank 7: available.
; The GUI64 engine and its fixed API remain at their existing RAM addresses.

!ifndef MD_IMAGE_START { !error "Use Build-MagicDesk.ps1" }
!ifndef MD_IMAGE_SIZE  { !error "Use Build-MagicDesk.ps1" }
!ifndef MD_ENTRY       { !error "Use Build-MagicDesk.ps1" }

MD_IMAGE_END = MD_IMAGE_START + MD_IMAGE_SIZE
!if MD_IMAGE_START < $0300 { !error "GUI64 image overlaps boot workspace" }
!if MD_IMAGE_END > $8000 { !error "GUI64 image exceeds the supported RAM copy range" }
!if MD_IMAGE_SIZE < 1 { !error "Empty GUI64 image" }
!if MD_ENTRY < MD_IMAGE_START { !error "GUI64 entry precedes image" }
!if MD_ENTRY >= MD_IMAGE_END { !error "GUI64 entry exceeds image" }

* = $0000
!pseudopc $8000 {
                !word MD_ColdStart, MD_ColdStart
                !byte $c3, $c2, $cd, $38, $30 ; CBM80

MD_ColdStart    sei
                cld
                ldx #$ff
                txs
                lda #$37
                sta $01
                lda #$2f
                sta $00
                ; A cartridge starts before the normal KERNAL initialization.
                ; Use public entries so replacement KERNALs can initialize too.
                jsr $ff84       ; IOINIT: SID, CIAs and serial bus
                jsr $ff87       ; RAMTAS: system RAM and memory limits
                jsr $ff8a       ; RESTOR: KERNAL vectors
                jsr $ff81       ; CINT: VIC and screen editor
                sei
                lda #$7f
                sta $dc0d
                sta $dd0d
                lda $dc0d
                lda $dd0d
                lda #0
                sta $d01a
                lda #$0b        ; blank display while replacing screen RAM
                sta $d011
                ; RAMTAS saw the cartridge at $8000. Restore the normal
                ; BASIC memory limit. Initialize BASIC before copying GUI64:
                ; E3BF writes to $0800, which is part of the GUI64 image.
                lda #$00
                sta $0283
                lda #$a0
                sta $0284
                jsr $e453       ; BASIC vectors at $0300
                jsr $e3bf       ; BASIC RAM, CHRGET and memory pointers
                ; This code must run from RAM when the ROM bank changes.
                ldx #(MD_RAMEnd - MD_RAMStart - 1)
-               lda MD_RAMSource,x
                sta MD_RAMStart,x
                dex
                bpl -
                jmp MD_RAMStart

MD_RAMSource = *
!pseudopc $0100 {
MD_RAMStart     lda #$00
                sta $fb
                lda #$80
                sta $fc
                lda #<MD_IMAGE_START
                sta $fd
                lda #>MD_IMAGE_START
                sta $fe
                ldx #1
                stx $de00       ; first payload bank
                ldy #0
MD_CopyByte     lda ($fb),y
                sta ($fd),y
                inc $fd
                bne +
                inc $fe
+               lda $fd
                cmp #<MD_IMAGE_END
                bne MD_NextByte
                lda $fe
                cmp #>MD_IMAGE_END
                beq MD_CopyDone
MD_NextByte     inc $fb
                bne MD_CopyByte
                inc $fc
                lda $fc
                cmp #$a0
                bne MD_CopyByte
                lda #$80
                sta $fc
                inx
                stx $de00
                jmp MD_CopyByte
MD_CopyDone     lda #$80
                sta $de00       ; hide cartridge ROM; all C64 RAM is available again
                lda #$1b
                sta $d011
                ; Match BASIC's normal startup stack: $01FC/FD are reserved
                ; for its descriptor chain and must not hold return addresses.
                ldx #$fb
                txs
                jmp MD_ENTRY
MD_RAMEnd = *
}
                ; The upper half of the stack stays free for KERNAL calls.
!if MD_RAMEnd > $0180 { !error "Magic Desk RAM copier exceeds stack workspace" }
}
!fill $2000 - *, $ff

!for MD_Bank, 1, 4 {
    !set MD_Offset = (MD_Bank - 1) * $2000
    !set MD_Count = MD_IMAGE_SIZE - MD_Offset
    !if MD_Count > $2000 { !set MD_Count = $2000 }
    !if MD_Count > 0 {
        !ifdef MAC {
            !bin "build/GUI64.mac.cart.prg", MD_Count, MD_Offset + 2
        } else {
            !bin "build/GUI64.win.cart.prg", MD_Count, MD_Offset + 2
        }
    } else {
        !set MD_Count = 0
    }
    !fill $2000 - MD_Count, $ff
}

; Bank 5: drive images followed by the ROM-executed installer.
!if * != $a000 { !error "Expected cartridge bank 5" }
!bin "build/krill/install-bank.bin"
!ifdef MAC {
    !bin "build/GUI64.mac.krill-control.bin"
} else {
    !bin "build/GUI64.win.krill-control.bin"
}
!if * > $c000 { !error "Krill installer and drive images exceed bank 5" }
!fill $c000 - *, $ff
; Bank 6: game control, shared guard and startup-only ROM functions.
!if * != $c000 { !error "Expected cartridge bank 6" }
!ifdef MAC {
    !bin "build/GUI64.mac.krill-game.bin"
} else {
    !bin "build/GUI64.win.krill-game.bin"
}
; The extracted bank-6 image contains game control, guard and startup code.
!if * > $e000 { !error "Game control/guard/startup exceed bank 6" }
!fill $e000 - *, $ff
; Bank 7: copy-only installer/1541 variant; bank 5 remains unchanged.
!bin "build/krill/copy-bank.bin"
!if * > $10000 { !error "Krill copy bank exceeds bank 7" }
!fill $10000 - *, $ff
