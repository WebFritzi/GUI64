Old01_irq       !byte 0

RasterIRQ       pha
                txa
                pha
                tya
                pha
                cld
                ;
                lda $01
                sta Old01_irq
                lda #$35
                sta $01
                ;
                lda #$ff
                sta $d019
                ;
                jsr DisplayClock
                jsr Joystick ; in port #2
                jsr Mouse    ; in port #1
                jsr Keyboard
                ;sei
                ;lda #<Raster226
                ;sta $fffe
                ;lda #>Raster226
                ;sta $ffff
                ;lda #226
                ;sta $d012
                ;cli
                lda Old01_irq
                sta $01
                pla
                tay
                pla
                tax
                pla
                rti

;Raster226       pha
;                txa
;                pha
;                tya
;                pha
;                cld
;                ;
;                lda $01
;                sta Old01_irq
;                lda #$35
;                sta $01
;                ; Acknowledge IRQ
;                lda #$ff
;                sta $d019
;                ;
;                sei
;                lda #<RasterIRQ
;                sta $fffe
;                lda #>RasterIRQ
;                sta $ffff
;                lda #0
;                sta $d012
;                cli
;                ; Process keyboard input
;                ;jsr Keyboard
;                jmp return_irq

Disable_CIA_IRQ sei
                lda #%01111111; Bit 7 sets the value, Bit 0...4 selects the bits to be set
                sta $dc0d
                sta $dd0d
                ; Acknowledge any pending CIA irq
                lda $dc0d
                lda $dd0d
                lda #$ff
                sta $d019
                cli
                rts

InstallIRQ      lda #1
                sta MayShowClock
                sei
                lda #53
                sta $01
                lda #<RasterIRQ
                sta $fffe
                lda #>RasterIRQ
                sta $ffff
                ; Raster IRQ at y=0
                lda #0
                sta $d012
                lda $d011
                and #%01111111
                sta $d011
                ; Enable raster IRQ
                lda #1
                sta $d019
                sta $d01a
                cli
                lda #3
                sta VIC+21
                rts

DeinstallIRQ    sei
                lda #54 ; RAM / IO / Kernal
                sta $01
                lda #$31
                sta $0314
                lda #$ea
                sta $0315
                ; Disable raster IRQ
                lda $d01a
                and #%11111110
                sta $d01a
                cli
                rts