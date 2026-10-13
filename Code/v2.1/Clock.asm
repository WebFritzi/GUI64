TODInit         sei             ; accounting for NMIs is not needed when
                lda #$00        ; used as part of application initialisation
                sta $dc08       ; TO1TEN start TOD - in case it wasn't running
-               cmp $dc08       ; TO1TEN wait until tenths
                beq -           ; register changes its value

                lda #$ff        ; count from $ffff (65535) down
                sta $dc04       ; TI1ALO both timer A register
                sta $dc05       ; TI1AHI set to $ff

                lda #%00010001  ; bit seven = 0 - 60Hz TOD mode
                sta $dc0e       ; CI1CRA start the timer

                lda $dc08       ; TO1TEN
-               cmp $dc08       ; poll TO1TEN for change
                beq -

                lda $dc05       ; TI1AHI expect (approximate) $7f4a $70a6 $3251 $20c0
                cli

                ldx #$01
                cmp #$51        ; about the middle (average is $50c0)
                bcs +
                ldx #$81        ; bit seven = 1 - 50Hz TOD mode
+               stx $dc0e       ; CI1CRA stop the timer and set TOD frequency
                ; WiC64
                lda Clock; std
                sta $dc0b ;Set TOD-Clock (hours)
                lda Clock+1; min
                sta $dc0a ;- (minutes)
                lda Clock+2; sec
                sta $dc09 ;- (seconds)
                lda #$00
                sta $dc08 ;- (deciseconds)
                rts

; Sets the TOD to the values in Clock
SetTOD          lda $dc0f
                and #%01111111
                sta $dc0f
                ;
                lda Clock+2
                asl
                asl
                asl
                asl
                ora Clock+3
                sta $dc0a
                ;
                lda Clock
                asl
                asl
                asl
                asl
                ora Clock+1
                ;
                bne +
                lda #$92
                bne ++       ; always taken
+               cmp #$13
                bcc ++       ; 01 to 12
                sed
                sbc #$12
                cld
                ora #%10000000
++              sta $dc0b
                lda #0
                sta $dc08
                sta $dc09
                rts

!ifdef WIN{
DisplayClock    lda #':'
                sta SCRMEM+23*40+34+2

                lda $dc0a
                tax
                lsr
                lsr
                lsr
                lsr
                sta Clock+2
                ora #$30
                sta SCRMEM+23*40+37
                txa
                and #%00001111
                sta Clock+3
                ora #$30
                sta SCRMEM+23*40+38
                ;
;                lda $dc0b
;                ldx $dc08       ; TOD-Latch freigeben
;                tax             ; Stunde retten, N = PM-Bit
;                bpl clock_am
;                ; PM
;                and #$7f
;                cmp #$12
;                beq clock_ready ; 12 PM bleibt 12
;                sed
;                clc
;                adc #$12
;                cld
;                bne clock_ready ; 01–11 PM -> 13–23
;clock_am        and #$7f
;                cmp #$12
;                bne clock_ready ; 01–11 AM bleiben unverändert
;                lda #$00        ; 12 AM -> 00
                lda $dc0b
                ldx $dc08       ; TOD latch release
                tax             ; raw hour incl. PM in X
                and #$7f
                cmp #$12
                bne +
                lda #0          ; 12 AM -> 0, 12 PM -> later +12
+               cpx #$80
                bcc clock_ready
                sed
                clc
                adc #$12
                cld
clock_ready     tax
                lsr
                lsr
                lsr
                lsr
                sta Clock
                ora #$30
                sta SCRMEM+23*40+34
                txa
                and #$0f
                sta Clock+1
                ora #$30
                sta SCRMEM+23*40+35
                rts
} else ifdef MAC{
DisplayClock    lda #$ba;#':'
                ;jsr PetUCtoDesktop
                sta SCRMEM+36

                lda $dc0a
                tax
                lsr
                lsr
                lsr
                lsr
                sta Clock+2
                ora #$b0
                ;jsr PetUCtoDesktop
                sta SCRMEM+37
                txa
                and #%00001111
                sta Clock+3
                ora #$b0
                ;jsr PetUCtoDesktop
                sta SCRMEM+38
                ;
                lda $dc0b
                ldx $dc08       ; TOD latch release
                tax             ; raw hour incl. PM in X
                and #$7f
                cmp #$12
                bne +
                lda #0          ; 12 AM -> 0, 12 PM -> later +12
+               cpx #$80
                bcc clock_ready
                sed
                clc
                adc #$12
                cld
clock_ready     tax
                lsr
                lsr
                lsr
                lsr
                sta Clock
                ora #$b0
                ;jsr PetUCtoDesktop
                sta SCRMEM+34
                txa
                and #$0f
                sta Clock+1
                ora #$b0
                ;jsr PetUCtoDesktop
                sta SCRMEM+35
                rts
                
                ;lda $dc0b
;                ldx $dc08
;                tax
;                and #%01111111
;                cmp #$12
;                bne +
                
;                ; 12 pm = noon or 12 am = midnight
;                txa
;                and #%10000000
;                tax
                
;+               txa
;                and #%10000000
;                beq am
;                ; pm
;                txa
;                and #%01111111
;                sed
;                clc
;                adc #$12
;                cld
;                tax
;                jmp +
;am              txa
;                and #%01111111
;+               lsr
;                lsr
;                lsr
;                lsr
;                sta Clock
;                ora #$30
;                jsr PetUCtoDesktop
;                sta SCRMEM+34
;                txa
;                and #%00001111
;                sta Clock+1
;                ora #$30
;                jsr PetUCtoDesktop
;                sta SCRMEM+35
;                rts
}