;Get time from WIC64
;Addon by RH70 in 12/2025

default_timeout = $02 ; $02 = ~ 1 sec.; adjustable via $ab (z_timeout)
; $10 = ~ 20 sec.
z_timeout       = $ab ; length of timeout (1 short, max 255 - real loooong)
z_error         = z_timeout ; (=0 means timeout)
safe_area       = $0334   ; for load-routine

c1              = MouseInfo  ; counter 1
c2              = MouseInfo+1; counter 2
c3              = MouseInfo+2; counter 3
;std             = Clock
;min             = Clock+1
;sek             = Clock+2

ReadBCD2        jsr read_byte
                jsr read_byte
                ;sec
                ;sbc #$30
                and #$0f
                asl
                asl
                asl
                asl
                sta $fb
                jsr read_byte
                ;sec
                ;sbc #$30
                and #$0f
                ora $fb
                rts

GetWiC64Time    jsr wic64_init
                ;
                lda $dd00
                ora #$04 
                sta $dd00
                lda #$ff 
                sta $dd03
                lda $dd0d
                ;
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
                bcs ++
                jsr read_byte
                jsr ReadBCD2
                sta Clock
                cmp #$13
                bcc okay
                sed; sec
                sbc #$12
                cld
                ora #$80
                sta Clock
                ;cld
okay            jsr ReadBCD2
                sta Clock+1
                ;
                jsr ReadBCD2
                sta Clock+2
                ;
                jsr u_wic64_exit
++              cli
                rts

read_byte       jsr wait_handshake
                lda $dd01   ; read byte from WiC64 (userport)
                rts

wait_handshake  lda z_timeout   ; handshake always with timeout
                bne +
                lda #$01    ; if z_error/z_timeout = 0 (timeout occured), shorten the following handshakes
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

; WiC64 init (set ESP in read-mode)
wic64_init      lda $dd02
                ora #$01
                sta $dd02   ; WiC init
                lda #default_timeout  ; set timeout
                sta z_timeout
                rts
    
wic64_ESP_read  ; set WiC64 to 'read-mode'
u_wic64_exit    lda #$ff    ; direction Port B out 
                sta $dd03
                lda $dd00
                ora #$04    ; set PA2 to HIGH = WiC64 ready for reading data from C64
                sta $dd00
                rts
        
com_out         jsr wic64_ESP_read
                ldx #0
-               lda timecom,x
                sta $dd01
                jsr wait_handshake
                bcs +
                inx
                cpx #4
                bcc -
                clc
+               rts

timecom         !TEXT "W",04,0,$15

