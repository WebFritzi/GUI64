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
                lda $d012
                cmp #150
                bcc L_0

;L_150
                jsr Highlight
                sei
                lda #<Raster226
                sta $fffe
                lda #>Raster226
                sta $ffff
                lda #226
                sta $d012
                cli
return_irq      lda Old01_irq
                sta $01
                pla
                tay
                pla
                tax
                pla
                rti

L_0             lda #CL_BLACK
                sta BKGCOLOR
                jsr MultiColorOff
                jsr DisplayClock
                lda desktop_d018
                sta $d018
                jsr Joystick ; in port #2
                jsr Mouse    ; in port #1
                lda #150
                sta $d012
                jmp return_irq

taskbar_d018    !byte 0
desktop_d018    !byte 0
dummy_irq       !byte 0

Raster226       pha
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
                ; Prepare Multicolor
                lda $d016
                ora #%00010000
                tax
                ; Prepare char set
                lda taskbar_d018
                ; Write into registers
                nop
                nop
                sta $d018
                stx $d016
                ldy CSTM_WindowClr
                sty BKGCOLOR
                ; Acknowledge IRQ
                lda #$ff
                sta $d019
                ;
                lda StartBtnPushed
                beq +
                jsr DrawSpritesDown
                jmp ++
+               jsr DrawSpritesUp
++              sei
                lda #<RasterIRQ
                sta $fffe
                lda #>RasterIRQ
                sta $ffff
                lda #0
                sta $d012
                cli
                ; Process keyboard input
                jsr Keyboard
                jmp return_irq

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

InstallIRQ      jsr SetTaskbarColors
                jsr PaintTaskbar
                lda #1
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
                lda $d01a
                ora #%00000001
                sta $d01a
                cli
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