!zone Keyboard
KEY_DELAY = 25

keycode         !byte 0
oldkeycode      !byte 0
shifted         !byte 0
actkey          !byte 0
key_counter     !byte 0

Keyboard        jsr keyscan
                lda keycode
                cmp #$ff
                bne +
                lda #1
                sta key_counter
                rts
+               cmp oldkeycode
                bne +
                dec key_counter
                bne ++
+               sta oldkeycode
                lda #EC_KEYPRESS
                sta exit_code
                lda #KEY_DELAY
                sta key_counter
++              rts

keyscan         lda #0
                sta keycode
                sta shifted
                lda #%11111110
                sta keyscanrowloop+1
                lda #%11111101
                sta $dc00
                lda $dc01
                and #%10000000
                beq keyscanshifted ;left shift pressed
                lda #%10111111
                sta $dc00
                lda $dc01
                and #%00010000
                beq keyscanshifted ;right shift pressed
                lda #<TabKey2Pet
                sta $0e
                lda #>TabKey2Pet
                sta $0f
                jmp +
keyscanshifted  lda #1
                sta shifted
                lda #<TabKey2PetShift
                sta $0e
                lda #>TabKey2PetShift
                sta $0f
                ;
+               ldx #$07
keyscanrowloop  lda #$00
                jsr joyactcheck
                sta $dc00
                lda $dc01
                cmp #$ff
                beq keyscannextrow
                jsr joyactcheck
                sta keyscanbyteloop+1
                ldy #$08
-               dey
                bpl keyscanbyteloop
                jmp keyscannextrow
keyscanbyteloop lda #$00
                lsr keyscanbyteloop+1
                bcc +
                jmp -
+               lda ($0e),y
                cmp #$fe
                beq -
                cmp #$ff
                beq keyscannextrow
;                   cmp lastkey
;                   bne +
;                   ldx #$ff
;                   jmp keyscanend2
;+
;                   tax
                jmp keyscanend
keyscannextrow  sec
                rol keyscanrowloop+1
                lda $0e
                clc
                adc #$08
                sta $0e
                bcc +
                inc $0f
+               ;
                lda keycode
                clc
                adc #8
                sta keycode
                ;
                dex
                bpl keyscanrowloop
                ; No key
                lda #$ff
                ;
                sta keycode
                sta actkey
                rts
                ;
keyscanend      ;stx lastkey
;keyscanend2
                sta actkey
                ;
                tya
                clc
                adc keycode
                sta keycode
                ;
                rts
joyactcheck     stx xsave+1
                ldx #$ff
                stx $dc00
                cpx $dc01
                beq xsave
                stx actkey
                ;
                stx keycode
                ;
                pla
                pla
                rts
xsave           ldx #$00
                rts

; Legend:
; ff: not allowed
; fe: left/right shift
; fd: backspace (DEL)
; fc: return
; fb: cursor down/up
; fa: CBM
; f9: HOME
TabKey2Pet      !byte $fb, $ff, $ff, $ff, $ff, $ff, $fc, $fd  ; CRSR DOWN, F5, F3, F1, F7, CRSR RIGHT, RETURN, INST DEL
                !byte $fe, $45, $53, $5a, $34, $41, $57, $33  ; LEFT SHIFT, "E", "S", "Z", "4", "A", "W", "3"
                !byte $58, $54, $46, $43, $36, $44, $52, $35  ; "X", "T", "F", "C", "6", "D", "R", "5"
                !byte $56, $55, $48, $42, $38, $47, $59, $37  ; "V", "U", "H", "B", "8", "G", "Y", "7"
                !byte $4e, $4f, $4b, $4d, $30, $4a, $49, $39  ; "N", "O", "K", "M", "0", "J", "I", "9"
                !byte $2c, $40, $3a, $2e, $2d, $4c, $50, $2b  ; ",", "@", ":", ".", "-", "L", "P", "+"
                !byte $2f, $5e, $3d, $fe, $f9, $3b, $2a, $24  ; "/", "^", "=", RIGHT SHIFT, HOME, ";", "*", "£"
                !byte $ff, $51, $fa, $20, $32, $ff, $5f, $31  ; RUN STOP, "Q", "C=" (CBM), " " (SPC), "2", "CTRL", "<==", "1"

TabKey2PetShift !byte $fb, $ff, $ff, $ff, $ff, $ff, $fc, $ff  ; CRSR DOWN, F5, F3, F1, F7, CRSR RIGHT, RETURN, INST DEL
                !byte $fe, $65, $73, $7a, $24, $61, $77, $23  ; LEFT SHIFT, "E", "S", "Z", "4", "A", "W", "3"
                !byte $78, $74, $66, $63, $26, $64, $72, $25  ; "X", "T", "F", "C", "6", "D", "R", "5"
                !byte $76, $75, $68, $62, $28, $67, $79, $27  ; "V", "U", "H", "B", "8", "G", "Y", "7"
                !byte $6e, $6f, $6b, $6d, $30, $6a, $69, $29  ; "N", "O", "K", "M", "0", "J", "I", "9"
                !byte $3c, $40, $5b, $3e, $2d, $6c, $70, $2b  ; ",", "@", ":", ".", "-", "L", "P", "+"
                !byte $3f, $5e, $3d, $fe, $f9, $5d, $2a, $24  ; "/", "^", "=", RIGHT SHIFT, HOME, ";", "*", "£"
                !byte $ff, $71, $fa, $20, $22, $ff, $5f, $21  ; RUN STOP, "Q", "C=" (CBM), " " (SPC), "2", "CTRL", "<==", "1"
Forbidden       !byte $22,$3f,$3d,$2a,$40,0; Not allowed for filenames and disknames (", ?, =, *, @)

!zone Mouse
acc      = 1     ; accelaration (fast: 1, slow: 128)
;---------------------------------
potx     = SID+$19
poty     = SID+$1a
maxx     = 319 ;Screen Width
maxy     = 199 ;Screen Height
offsetx  = 24  ;Sprite left border edge
offsety  = 50  ;Sprite top  border edge
TIMER_TOOLTIP_MAX = 100

musposx         !byte 0,0;220,0
musposy         !byte 0,0; The initial y-value can be set in oldpoty+1
old_xPos        !byte 0
moved           !byte 0
dc_counter      !byte 0
block_release   !byte 0
IsLBtnPressed   !byte 0; documents left press at any time
IsRBtnPressed   !byte 0; documents right press at any time
scroll_counter  !byte 2
dc_delay        !byte 15
no_move_timer   !byte TIMER_TOOLTIP_MAX
bMayTooltip     !byte 1

Mouse           jsr GetClicks
                jsr scanmovs
                lda moved
                beq ++
                ; moved
                ldx IsLBtnPressed
                bne +
                ldx IsRBtnPressed
                bne +
                ldx #TIMER_TOOLTIP_MAX
                stx no_move_timer
                ldx #1
                stx bMayTooltip
+               jmp boundmus
++              ; not moved
                lda bMayTooltip
                beq +
                dec no_move_timer
                bne +
                ; not moved for a second
                ldx #TIMER_TOOLTIP_MAX
                stx no_move_timer
                ldx #0
                stx bMayTooltip
                lda #EC_TOOLTIP
                sta exit_code
+               rts

;---------------------------------------
GetClicks       lda dc_counter
                beq +
                dec dc_counter
                bne +
                ; time's up for a double click
                lda IsLBtnPressed
                bne +
                lda #EC_LBTNRELEASE
                sta exit_code
                ;
+               lda $dc01
                cmp #%11110111
                bne +
                ; Scroll wheel down
                dec scroll_counter
                bne +
                lda #EC_SCROLLWHEELDOWN
                sta exit_code
                lda #2
                sta scroll_counter
                rts
+               cmp #%11111011
                bne +
                ; Scroll wheel up
                dec scroll_counter
                bne +
                lda #EC_SCROLLWHEELUP
                sta exit_code
                lda #2
                sta scroll_counter
                rts
+               cmp #%11101111
                bne noLbtn
                ; left btn pressed
                lda IsLBtnPressed
                beq ++
                ; btn was pressed before
                ;lda LongClickCounter
;                beq +
;                dec LongClickCounter
;                rts
;+               lda #EC_LLBTNPRESS
;                sta exit_code
                ;lda #EC_LLBTNPRESS
                ;sta exit_code
                rts
++              ; btn was not pressed before
                lda #1
                sta IsLBtnPressed
                lda #TIMER_TOOLTIP_MAX
                sta no_move_timer
                lda #0
                sta bMayTooltip
                lda dc_counter
                bne dblclk
                lda dc_delay
                sta dc_counter
                ; event click
                lda #EC_LBTNPRESS
                sta exit_code
                rts
dblclk          ; event double click
                lda #EC_DBLCLICK
                sta exit_code
                lda #0
                sta dc_counter
                rts
noLbtn          ; left btn not pressed
                ;lda #10
                ;sta LongClickCounter
                lda IsLBtnPressed
                beq checkRight;button was also not pressed before
                ; left btn was pressed before
                lda #0
                sta IsLBtnPressed
                lda dc_counter
                bne out_here
                lda #EC_LBTNRELEASE
                sta exit_code
out_here        rts
checkRight      lda $dc01
                cmp #254
                bne noRbtn
                ; right btn pressed
                lda IsRBtnPressed
                bne out_here; right btn was also pressed before
                ; right btn was not pressed before
                lda #1
                sta IsRBtnPressed
                lda #EC_RBTNPRESS
                sta exit_code
                rts
noRbtn          ; right btn not pressed
                lda IsRBtnPressed
                beq out_here; right btn was also not pressed before
                ; right btn was pressed before
                lda #0
                sta IsRBtnPressed
                lda #EC_RBTNRELEASE
                sta exit_code
                rts
;---------------------------------------
scanmovs        lda #2
                sta moved
                ;--- X Axis ---
                lda potx
oldpotx         ldy #0
                jsr movechk
                ;beq noxmove
                bne +
                dec moved
                jmp noxmove
+               sty oldpotx+1

                clc
                adc musposx
                sta musposx
                txa            ;upper 8-bits
                adc musposx+1
                sta musposx+1
noxmove         ;--- Y Axis ---
                lda poty
oldpoty         ldy #0
                jsr movechk
                bne +
                dec moved
                jmp noymov
+               sty oldpoty+1

                clc
                eor #$ff       ;Reverse Sign
                adc #1

                clc
                adc musposy
                sta musposy
                txa            ;Upper 8-bits
                eor #$ff       ;Reverse Sign
                adc musposy+1
                sta musposy+1
noymov          rts
;---------------------------------------
movechk         ;Y -> Old Pot Value
                ;A -> New Pot Value
                sty oldvalue+1
                tay
                sec

oldvalue        sbc #$ff
                and #%01111111
                cmp #%01000000
                bcs neg

                lsr ;remove noise bit
                beq nomove

                cmp #acc ;Acceleration Speed
                bcc *+3
                asl ;X2

                ldx #0
                cmp #0

                ;A > 0
                ;X = 0 (sign extension)
                ;Y = newvalue
                ;Z = 0

                rts

neg             ora #%10000000
                cmp #$ff
                beq nomove

                sec    ;Keep hi negative bit
                ror ;remove noise bit

                cmp #256-acc ;Acceleration Speed
                bcs *+3
                asl ;X2

                ldx #$ff

                ;A < 0
                ;X = $ff (sign extension)
                ;Y = newvalue
                ;Z = 0

                ;fallthrough

nomove          ;A = -
                ;X = -
                ;Y = -
                ;Z = 1
                rts

;---------------------------------------
boundmus        ldx musposx+1
                bmi zerox
                beq chky

                ldx #1
                stx musposx+1
                ldx #(maxx-256)
                cpx musposx
                bcs chky

                stx musposx
                bcc chky

zerox           ldx #0
                stx musposx
                stx musposx+1

chky            ldy musposy+1
                bmi zeroy
                beq loychk

                ;dec musposy+1
                ldy #0
                sty musposy+1
                ldy #maxy
                sty musposy
                bne movemus

loychk          ldy #maxy
                cpy musposy
                bcs movemus

                sty musposy
                bcc movemus

zeroy           ldy #0
                sty musposy
                sty musposy+1

movemus         lda xPos0
                sta old_xPos
                
                lda musposx
                clc
                adc #offsetx
                sta xPos0
                sta xPos1
                
                lda musposx+1
                adc #0
                beq clearxhi

                ;set x sprite pos high
                lda xposmsb
                ora #%00000011         
                bne +
         
clearxhi        ;set x sprite pos low
                lda xposmsb
                and #%11111100
+               sta xposmsb
                
                lda xPos0
                cmp old_xPos
                beq make_ymove; no x move
                ; EC_MOUSEMOVE can't override other exit_codes
                lda #EC_MOUSEMOVE
                sta exit_code

make_ymove      lda musposy
                clc
                adc #offsety
                cmp yPos0
                beq no_ymove
                sta yPos0
                sta yPos1

                ; EC_MOUSEMOVE can't override other exit_codes
                lda #EC_MOUSEMOVE
                sta exit_code
no_ymove        rts


!zone Joystick
joy_delay = 13
bFirePressed    !byte 0
dx              !byte 0 ;joystick
dy              !byte 0 ;directions
dc_counter_joy  !byte 0

Joystick        lda dc_counter_joy
                beq +
                dec dc_counter_joy
                bne +
                ; time's up
                lda bFirePressed
                bne +
                lda #EC_LBTNRELEASE
                sta exit_code
+               jmp ProcessJoystick


ProcessJoystick jsr JoyDecoder
                bcs NoFire
                ;Fire pressed
                lda bFirePressed
                bne OnlyMove; Fire was also pressed before
                ; Fire not pressed before
                lda #1
                sta bFirePressed
                lda #TIMER_TOOLTIP_MAX
                sta no_move_timer
                lda #0
                sta bMayTooltip
                lda dc_counter_joy
                bne joydblclk
                lda dc_delay
                sta dc_counter_joy
                ; event click
                lda #EC_LBTNPRESS
                sta exit_code
                jmp OnlyMove
joydblclk       ; event double click
                lda #EC_DBLCLICK
                sta exit_code
                sta block_release
                lda #0
                sta dc_counter_joy
                rts
NoFire          ;Fire not pressed
                lda bFirePressed
                beq OnlyMove;fire was also not pressed before
                lda #0;fire was pressed before
                sta bFirePressed
                lda dc_counter_joy
                bne OnlyMove
                lda block_release
                bne +
                lda #EC_LBTNRELEASE
                sta exit_code
+               lda #0
                sta block_release

OnlyMove        lda dx
                ora dy
                bne move
                ; no move
                lda bMayTooltip
                beq +
                dec no_move_timer
                bne +
                ; not moved for a second
                ldx #TIMER_TOOLTIP_MAX
                stx no_move_timer
                ldx #0
                stx bMayTooltip
                lda #EC_TOOLTIP
                sta exit_code
+               rts
move            lda #EC_MOUSEMOVE
                sta exit_code
                ;
                ldx bFirePressed
                bne +
                ldx #TIMER_TOOLTIP_MAX
                stx no_move_timer
                ldx #1
                stx bMayTooltip
                ;
+               lda dx
                asl
                asl
                clc
                adc VIC
                sta VIC
                sta VIC+2
                lda dx
                bpl right
                ;left
                bcc ovf_lr
                lda $d010
                and #%00000011
                bne change_y
                lda VIC
                cmp #24
                bcs change_y
                lda #24
                sta VIC
                sta VIC+2
                jmp change_y
right           bcs ovf_lr

                lda $d010
                and #%00000011
                beq change_y
                lda VIC
                cmp #88
                bcc change_y
                lda #87
                sta VIC
                sta VIC+2
                jmp change_y
                
                jmp change_y
ovf_lr          lda $d010
                eor #%00000011
                sta $d010
change_y        lda dy
                asl
                asl
                clc
                adc VIC+1
                sta VIC+1
                sta VIC+3
                cmp #50
                bcc correct_yup
                cmp #250
                bcs correct_ydown
                rts
correct_yup     lda #50
                sta VIC+1
                sta VIC+3
                rts
correct_ydown   lda #249
                sta VIC+1
                sta VIC+3
                rts


JoyDecoder      lda $dc00     ; get input from port 2 only
                ldy #0        ; this routine reads and decodes the
                ldx #0        ; joystick/firebutton input data in
                lsr           ; the accumulator. this least significant
                bcs djr0      ; 5 bits contain the switch closure
                dey           ; information. if a switch is closed then it
djr0            lsr           ; produces a zero bit. if a switch is open then
                bcs djr1      ; it produces a one bit. The joystick dir-
                iny           ; ections are right, left, forward, backward
djr1            lsr           ; bit3=right, bit2=left, bit1=backward,
                bcs djr2      ; bit0=forward and bit4=fire button.
                dex           ; at rts time dx and dy contain 2's compliment
djr2            lsr           ; direction numbers i.e. $ff=-1, $00=0, $01=1.
                bcs djr3      ; dx=1 (move right), dx=-1 (move left),
                inx           ; dx=0 (no x change). dy=-1 (move up screen),
djr3            lsr           ; dy=1 (move down screen), dy=0 (no y change).
                stx dx        ; the forward joystick position corresponds
                sty dy        ; to move up the screen and the backward
                rts           ; position to move down screen.
                              ;
                              ; at rts time the carry flag contains the fire
                              ; button state. if c=1 then button not pressed.
                              ; if c=0 then pressed.