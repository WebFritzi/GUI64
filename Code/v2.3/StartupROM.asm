; ROM bank 6, after the shared Krill guard. Startup-only, before GUI IRQ.
; X=0: clock init; X=1: globals. All mutable values remain in GUI64 RAM.
Startup_ROMDispatch
                txa
                bne SetGlobals
                jsr GetWiC64Time
                jmp TODInit
;Once called, never changes
SetGlobals      ; Install mouse pointer sprites
                lda #<SP_Mouse0
                sta SPRPTR_0
                lda #<SP_Mouse1
                sta SPRPTR_1
                ; Set initial values
                ldx #0; CL_BLACK
                stx col0
                stx FRAMECOLOR
                stx BKGCOLOR
                stx MULTICOLOR2
                stx PATH_A
                stx PATH_A+1
                stx PATH_B
                stx PATH_B+1
                stx PATH_U
                stx PATH_U+1
                inx; CL_WHITE
                stx col1
                stx MULTICOLOR1
                ; Turn on sprites
                +LDA_WM %00111111, %00000011
                sta VIC+21
                ;
                lda #"a"
                sta PATH_A_EX
                lda #"b"
                sta PATH_B_EX
                lda #"U"
                sta PATH_U_EX
                lda #":"
                sta PATH_A_EX+1
                sta PATH_B_EX+1
                sta PATH_U_EX+1
!ifdef WIN{
                lda #3
                ldx bIsUltimate
                beq +
                lda #4
+               sta CbmMenuItems
                asl
                tax
                dex
                stx Menu_Start+2
                ;
                inx
                inx
                stx CbmMenuHeight
                
                ;; Draw Commodore sprites
                ;lda #<SP_Commodore1
                ;sta SPRPTR_2
                ;lda #<SP_Commodore2
                ;sta SPRPTR_3
                ;lda #CL_DARKBLUE
                ;sta col2
                ;lda #CL_RED
                ;sta col3
                ;lda #28
                ;sta xPos2
                ;sta xPos3
                ;lda #232
                ;sta yPos2
                ;sta yPos3
}
!ifdef MAC{
                ldy #3
                ldx bIsUltimate
                beq +
                iny
+               sty Menu_System+2
}
                rts
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
    !source "WiC64_Time.asm"
