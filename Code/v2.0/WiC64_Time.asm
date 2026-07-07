;Get time from WIC64
;Addon by RH70 in 12/2025

default_timeout = $02 ; $02 = ~ 1 sec.; adjustable via $ab (z_timeout)
; $10 = ~ 20 sec.
tmp2            = $a5
tmp             = $a6
data_pointer    = $a7 ; $a7/$a8 adress for data
bytes_send      = $a9 ; $a9/$aa number of bytes
z_timeout       = $ab ; length of timeout (1 short, max 255 - real loooong)
z_error         = z_timeout ; (=0 means timeout)
safe_area       = $0334   ; for load-routine

    
GetWiC64Time 
                jsr wic64_init
                ;
                lda $dd00
                ora #$04 
                sta $dd00
                lda #$ff 
                sta $dd03
                lda $dd0d
                ;
                lda #<timecom
                ldy #>timecom
                jsr com_out
                bcc goon
                rts          ; No WIC64 detected 
goon            sei          ; init reading
                ldy #$00     ; set port B to input
                sty $dd03   
                lda $dd00
                and #$fb     ; PA2 LOW: WiC in send-mode
                sta $dd00   
                jsr read_byte
                jsr read_byte
                jsr read_byte
                tay
                jsr read_byte
                sec
                sbc #$30
                asl
                asl
                asl
                asl
                sta std
                jsr read_byte
                sec
                sbc #$30
                ora std
                sta std
                cmp #$13
                bcc okay
                sed
                sec
                sbc #$12
                ora #$80
                sta std
                cld
okay            jsr read_byte
                jsr read_byte
                sec
                sbc #$30
                asl
                asl
                asl
                asl
                sta min
                jsr read_byte
                sec
                sbc #$30
                ora min
                sta min
                ;
                jsr read_byte
                jsr read_byte
                sec
                sbc #$30
                asl
                asl
                asl
                asl
                sta sek
                jsr read_byte
                sec
                sbc #$30
                ora sek
                sta sek
                ;
                jsr u_wic64_exit                  
                rts

read_byte       jsr wait_handshake
                lda $dd01   ; read byte from WiC64 (userport)
                rts
wait_handshake  lda z_timeout   ; handshake always with timeout
                bne +
                lda #$01    ; if z_error/z_timeout = 0 (timeout accured), shorten the following handshakes
+               sta c3      ; looplength for timeout
                sta c2      ; z_timeout * z_timeout
-               lda $dd0d   ; check handshake
                and #$10          ; wait for NMI FLAG2
                bne hs_rts    ; handshake ok - return
                dec c1      ; inner loop: 256 passes
                bne -
                dec c2      ; outer loops: z_timeout * z_timeout
                bne -
                dec c3      
                bne -
                lda #$00    ; timeout occurred!
                sta z_error   ; $00=timeout, $01-$ff=OK!
                sec
                rts
hs_rts          clc
                rts
c1              nop     ; counter 1
c2              nop     ; counter 2
c3              nop     ; counter 3

wic64_init      jmp u_wic64_init  ; WiC64 init (set ESP in read-mode)

u_wic64_init    lda $dd02
                ora #$01
                sta $dd02   ; WiC init
                lda #default_timeout  ; set timeout
                sta z_timeout
                rts
    
wic64_ESP_read  ; set WiC64 ro 'read-mode'
u_wic64_exit    lda #$ff    ; direction Port B out 
                sta $dd03
                lda $dd00
                ora #$04    ; set PA2 to HIGH = WiC64 ready for reading data from C64
                sta $dd00
                rts
        
com_out         sta data_pointer  ; set datapointer to lowbyte=A, highbyte=Y
                sty data_pointer+1
                ;
                jsr wic64_ESP_read  
                ldy #$02    
                lda (data_pointer),y  ; number of bytes to send (lowbyte)
                sta bytes_send+1
                dey
                lda (data_pointer),y  ; number of bytes to send (highbyte)
                tax
                beq +     ; special case:   lowbyte=0
                inc bytes_send+1
+               dey     ; y=0
loop_send       lda (data_pointer),y  
                jsr write_byte    ; send bytes to WiC64 in loop
                iny
                bne +
                inc data_pointer+1  
+               dex
                bne loop_send
                dec bytes_send+1
                bne loop_send       
                cli     ; enable IRQ
                rts


write_byte      sta $dd01   ; bits 0..7 parallel to WiC64 (userport PB 0-7)
                jmp wait_handshake

timecom         !TEXT "W",04,0,$15

min             !BYTE 0
std             !BYTE 0
sek             !BYTE 0