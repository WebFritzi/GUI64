; ROM bank 6: game transfers through the same RAM core used for Apps.
; All state that must survive a PRG write is below $0801.
Krill_GameName       = KRILL_GAME_STATE       ; 17-byte terminated filename
Krill_GameDD00       = KRILL_GAME_STATE+17
Krill_GameDD02       = KRILL_GAME_STATE+18
Krill_GameNext       = KRILL_GAME_STATE+19
Krill_GameReady      = KRILL_GAME_STATE+21
Krill_GameWritten    = KRILL_MDGAMEWRITTEN
Krill_GameSavedZP    = KRILL_GAME_STATE+24    ; $07E8-$07EF
Krill_GameWrap       = KRILL_MDGAMEWRAP
Krill_GameStatus     = KRILL_GAME_STATE+33
!if Krill_GameStatus >= KRILL_MDAPPMODE { !error "Game state overlaps transfer mode" }

Krill_ROMLoadGame
                sei
                cld
                lda Krill_SavedDD00
                sta Krill_GameDD00
                lda Krill_SavedDD02
                sta Krill_GameDD02
                ldx #7
-               lda $e0,x
                sta Krill_GameSavedZP,x
                dex
                bpl -
                ldx #16
-               lda Str_FileName,x
                sta Krill_GameName,x
                dex
                bpl -
                lda #0
                sta Krill_GameReady
                sta Krill_GameWritten
                sta Krill_GameWrap
                sta KRILL_MDAPPERROR
                sta error_code
                lda #2
                sta KRILL_MDAPPMODE
                lda $ba
                cmp CurDeviceNo
                beq +
                lda #5
                sta error_code
                jmp Krill_GameFinish
+               ldx #<Krill_GameName
                ldy #>Krill_GameName
                clc
                jsr KRILL_CORE_OPENFILE
                bcc Krill_GamePoll
                jmp Krill_GameTransferError
Krill_GamePoll jsr KRILL_CORE_POLLBLOCK
                sta Krill_GameStatus
                bcs Krill_GameDone
                lda KRILL_MDAPPERROR
                bne Krill_GameBoundsError
                jsr Krill_GameProgress
                jmp Krill_GamePoll
Krill_GameDone lda Krill_GameStatus
                bne Krill_GameTransferError
                lda KRILL_MDAPPERROR
                bne Krill_GameBoundsError
                lda Krill_GameWritten
                beq Krill_GameBoundsError ; no executable payload
                lda Krill_GameWrap
                bne +
                lda KRILL_ENDADDRLO
                cmp KRILL_LOADADDRLO
                lda KRILL_ENDADDRHI
                sbc KRILL_LOADADDRHI
                bcc Krill_GameBoundsError
                bne +
                lda KRILL_ENDADDRLO
                cmp KRILL_LOADADDRLO
                beq Krill_GameBoundsError
+               lda KRILL_LOADADDRLO
                sta jmp_boot+1
                lda KRILL_LOADADDRHI
                sta jmp_boot+2
                lda KRILL_ENDADDRLO
                sta $ae
                lda KRILL_ENDADDRHI
                sta $af
                lda Krill_GameWrap
                beq +
                lda #0
                sta $ae
                sta $af
+               jsr Krill_GameFillMotion
                jmp Krill_GameFinish
Krill_GameBoundsError
                lda #32
                sta error_code
                bne Krill_GameFinish
Krill_GameTransferError
                cmp #$fe
                beq +
                lda #4
                !byte $2c
+               lda #5
                sta error_code
Krill_GameFinish
                jsr KRILL_CORE_CLOSEFILE
                ; Uninstall directly: FREEMEM may already contain game data.
                lda #$3f
                sta $dd02
-               bit $dd00
                bpl -
                bvc -
                lda Krill_GameDD00
                sta $dd00
                lda Krill_GameDD02
                sta $dd02
                lda #0
                sta KRILL_MDAPPMODE
                lda Krill_GameWritten
                bne +
                lda #0
                sta Krill_NativeActive ; GUI remains intact on an early error.
+               ldx #7
-               lda Krill_GameSavedZP,x
                sta $e0,x
                dex
                bpl -
                lda Krill_GameWritten
                rts

; Same progress calculation as Apps; its three bytes must survive GUI overwrite.
Krill_GameProgress
                lda KRILL_ENDADDRHI
                beq Krill_GameProgressDone
                lda Krill_GameReady
                bne Krill_GameProgressCompare
                lda KRILL_LOADADDRLO
                sta Krill_GameNext
                lda KRILL_LOADADDRHI
                sta Krill_GameNext+1
                lda #1
                sta Krill_GameReady
                jsr Krill_GameProgressAdvance
Krill_GameProgressCompare
                lda MOTION_COUNTER
                cmp #17
                beq Krill_GameProgressDone
                lda KRILL_ENDADDRLO
                cmp Krill_GameNext
                lda KRILL_ENDADDRHI
                sbc Krill_GameNext+1
                bcc Krill_GameProgressDone
                jsr Krill_GameMotion
                jsr Krill_GameProgressAdvance
                jmp Krill_GameProgressCompare
Krill_GameProgressDone
                rts
Krill_GameProgressAdvance
                clc
                lda Krill_GameNext
                adc BYTES_PER_PROGBIT_LO
                sta Krill_GameNext
                lda Krill_GameNext+1
                adc BYTES_PER_PROGBIT_HI
                sta Krill_GameNext+1
                rts
Krill_GameMotion
                inc MOTION_COUNTER
                ldx MOTION_COUNTER
!ifdef WIN {
                lda #CL_DARKBLUE
                sta CLRMEM+451,x
} else {
                lda #CL_DARKGRAY
                sta CLRMEM+491,x
}
                rts
Krill_GameFillMotion
                ldx MOTION_COUNTER
                bpl +
                ldx #0
+
-               !ifdef WIN {
                    lda #CL_DARKBLUE
                    sta CLRMEM+451,x
                } else {
                    lda #CL_DARKGRAY
                    sta CLRMEM+491,x
                }
                inx
                cpx #18
                bcc -
                ldx #80
-               jsr Pause
                dex
                bpl -
                rts
