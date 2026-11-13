; Three-voice background music adapted from the author's YAT.asm.
; Original notes, PAL frequencies, triangle wave and AD=$0a / SR=$00.
; Original sequence: part 1 twice, part 2 once, then repeat.
; A zero holds the previous note, as in YAT (it is not a rest).
; The old CIA/KERNAL IRQ ran at about 60 Hz: 12 ticks per note = 200 ms.
; GUI64 supplies raster frames: 10 PAL frames give the same note rate.
; Only SID registers $d400..$d418 are written; no private IRQ or zero page.

MUSIC_SID       = $d400
MUSIC_FRAMES    = 10

!zone MusicReset
; Main-loop entry: serialize resets with the raster frame callback.
MusicReset      php
                sei
                jsr MusicStop
                lda #0
                sta MusicStep
                sta MusicPart
                lda #1
                sta MusicDelay                  ; first note on next active frame
                lda #$0a
                sta MUSIC_SID+5
                sta MUSIC_SID+12
                sta MUSIC_SID+19
                plp
                rts

!zone MusicStop
; Called with frame handling stopped, or IRQs masked by MusicReset.
; Leave SID paddle registers untouched (GUI64's mouse uses them).
MusicStop       lda #0
                sta MusicPlaying
                ldx #24
.clear          sta MUSIC_SID,x
                dex
                bpl .clear
                rts

!zone MusicMute
; Frame handler, or main code with IRQs masked. Preserve song position
; and voice registers while muted; resume at the remaining delay.
MusicMute       lda MusicPlaying
                beq .leave
                lda #0
                sta MUSIC_SID+24
                sta MusicPlaying
.leave          rts

!zone ToggleMusic
ToggleMusic     php
                sei
                lda MusicEnabled
                eor #1
                sta MusicEnabled
                jsr MusicMute
                plp
                ldx #SOUND_MENU_LEN-1
                lda MusicEnabled
                beq .off
.on             lda Str_SoundOn,x
                sta SoundMenuText,x
                dex
                bpl .on
                rts
.off            lda Str_SoundOff,x
                sta SoundMenuText,x
                dex
                bpl .off
                rts

!zone MusicFrame
; Existing GUI64 raster callback: short, bounded, no GUI painting,
; keyboard hardware scan or shared scratch bytes. I/O is mapped in by
; GUI64 before this callback. The enclosing IRQ saves A/X/Y and flags.
MusicFrame      lda MusicEnabled
                beq .mute
                jsr IsActive
                bcc .mute
                lda Paused
                ora GameOver
                beq .playing
.mute           jmp MusicMute
.playing        lda MusicPlaying
                bne .tick
                lda #15
                sta MUSIC_SID+24
                lda #1
                sta MusicPlaying
.tick           dec MusicDelay
                beq .note
                rts
.note           lda #MUSIC_FRAMES
                sta MusicDelay
                ldx MusicStep
                lda MusicPart
                cmp #2
                bcc .play
                txa
                ora #64                         ; second 64-note part
                tax
.play           lda MusicVoice1,x
                beq .voice2
                ldy #16                         ; triangle, gate off
                sty MUSIC_SID+4
                tay
                lda MusicNotesLo,y
                sta MUSIC_SID
                lda MusicNotesHi,y
                sta MUSIC_SID+1
                lda #17                         ; triangle, gate on
                sta MUSIC_SID+4
.voice2         lda MusicVoice2,x
                beq .voice3
                ldy #16
                sty MUSIC_SID+11
                tay
                lda MusicNotesLo,y
                sta MUSIC_SID+7
                lda MusicNotesHi,y
                sta MUSIC_SID+8
                lda #17
                sta MUSIC_SID+11
.voice3         lda MusicVoice3,x
                beq .advance
                ldy #16
                sty MUSIC_SID+18
                tay
                lda MusicNotesLo,y
                sta MUSIC_SID+14
                lda MusicNotesHi,y
                sta MUSIC_SID+15
                lda #17
                sta MUSIC_SID+18
.advance        inc MusicStep
                lda MusicStep
                cmp #64
                bcc .leave
                lda #0
                sta MusicStep
                inc MusicPart
                lda MusicPart
                cmp #3
                bcc .leave
                lda #0
                sta MusicPart
.leave          rts

MusicEnabled    !byte 1
MusicPlaying    !byte 0
MusicDelay      !byte 1
MusicStep       !byte 0
MusicPart       !byte 0
Str_SoundOn     !pet "Sound On "
Str_SoundOff    !pet "Sound Off"
!if Str_SoundOff-Str_SoundOn != SOUND_MENU_LEN { !error "Invalid Sound On text size" }
!if *-Str_SoundOff != SOUND_MENU_LEN { !error "Invalid Sound Off text size" }
MusicVoice1
MusicP1Voice1    !byte 58,0,53,54,56,0,54,53,51,0,51,54,58,0,56,54,53,0,53,54,56,0,58,0,54,0,51,0,51,0,0,0
                !byte 0,56,0,59,63,0,61,59,58,0,0,54,58,0,56,54,53,0,53,54,56,0,58,0,54,0,51,0,51,0,0,0
!if *-MusicP1Voice1 != 64 { !error "Invalid YAT music part" }
MusicP2Voice1    !byte 58,0,0,0,54,0,0,0,56,0,0,0,53,0,0,0,54,0,0,0,51,0,0,0,50,0,0,0,53,0,0,0
                !byte 58,0,0,0,54,0,0,0,56,0,0,0,53,0,0,0,54,0,58,0,63,0,63,0,62,0,0,0,0,0,0,0
!if *-MusicP2Voice1 != 64 { !error "Invalid YAT music part" }

MusicVoice2
MusicP1Voice2    !byte 53,0,50,0,0,0,51,50,46,0,0,0,0,0,0,0,50,0,50,51,53,0,0,0,0,0,0,0,0,39,41,42
                !byte 44,47,0,51,54,0,53,51,49,0,0,46,49,0,47,46,50,0,50,51,53,0,0,0,0,0,0,0,0,0,0,0
!if *-MusicP1Voice2 != 64 { !error "Invalid YAT music part" }
MusicP2Voice2    !byte 51,0,0,0,51,0,0,0,50,0,0,0,50,0,0,0,51,0,0,0,46,0,0,0,46,0,0,0,46,0,0,0
                !byte 51,0,0,0,51,0,0,0,50,0,0,0,50,0,0,0,51,0,54,0,58,0,58,0,58,0,0,0,0,0,0,0
!if *-MusicP2Voice2 != 64 { !error "Invalid YAT music part" }

MusicVoice3
MusicP1Voice3    !byte 34,46,34,46,34,46,34,46,27,39,27,39,27,39,27,39,26,38,26,38,26,38,26,38,27,39,27,39,27,27,29,30
                !byte 32,44,32,44,32,44,32,44,30,42,30,42,30,42,30,42,29,41,29,41,29,41,29,41,27,39,27,39,27,39,27,39
!if *-MusicP1Voice3 != 64 { !error "Invalid YAT music part" }
MusicP2Voice3    !byte 39,46,39,46,39,46,39,46,34,46,34,46,34,46,34,46,39,46,39,46,39,46,39,46,34,46,34,46,34,46,34,46
                !byte 39,46,39,46,39,46,39,46,34,46,34,46,34,46,34,46,39,46,39,46,39,46,39,46,34,46,34,46,34,46,34,46
!if *-MusicP2Voice3 != 64 { !error "Invalid YAT music part" }

MusicNotesLo    !byte $00,$A1,$BA,$D4,$EF,$0E
                !byte $2C,$4D,$70,$96,$BD,$E8,$12,$42,$73,$A8,$E0,$1A
                !byte $59,$9C,$E1,$2C,$7B,$CE,$27,$84,$EA,$51,$C8,$2E
                !byte $B6,$3E,$C7,$60,$F9,$A3,$4E,$09,$D5,$A2,$7F,$6E
                !byte $6D,$6C,$8E,$AF,$F3,$36,$9C,$13,$9A,$44,$FF,$DC
                !byte $C9,$D9,$0B,$5F,$D5,$6D,$39,$26,$46,$89,$FE,$B8
MusicNotesHi    !byte $00,$01,$01,$01,$01,$02
                !byte $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$04
                !byte $04,$04,$04,$05,$05,$05,$06,$06,$06,$07,$07,$08
                !byte $08,$09,$09,$0A,$0A,$0B,$0C,$0D,$0D,$0E,$0F,$10
                !byte $11,$12,$13,$14,$15,$17,$18,$1A,$1B,$1D,$1E,$20
                !byte $22,$24,$27,$29,$2B,$2E,$31,$34,$37,$3A,$3D,$41


