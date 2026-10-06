;------------------------------------------------------------------
; uci.asm - Ultimate Command Interface (UCI), synchron und kurz
;
; Aufruf:  Befehl nach UciCmd schreiben, Laenge nach UciCmdLen,
;          jsr UciExec
; Danach:  UciResp/UciRespLen  = Antwortdaten (max. UCI_RESP_MAX)
;          UciStat/UciStatLen  = Statustext, z.B. "00,OK"
;          Carry gesetzt       = Zeitueberschreitung / Protokollfehler
;
; Braucht zwei freie Zero-Page-Bytes nicht - alles absolut adressiert,
; damit die App GUI64s Zero Page nicht anfasst.
;
; Einstellbar vor dem !source (sonst gelten die Vorgaben):
;   UCI_CMD_MAX   Groesse des Befehlspuffers (lange URLs brauchen mehr)
;   UCI_TIMEOUT   Bilder Wartezeit auf die Antwort (max. 255, ~5 s)
;   UCI_SINK      definiert: jedes Datenbyte geht in A an UciSink (die
;                 App stellt die Routine), statt in UciResp zu landen -
;                 so passt eine 400-Byte-Antwort ohne Puffer in 4 KB.
;------------------------------------------------------------------

UCI_CTRL        = $df1c         ; schreiben: Steuerung, lesen: Status
UCI_CMD         = $df1d         ; schreiben: Befehlsdaten, lesen: ID ($c9)
UCI_RESP        = $df1e         ; Antwortdaten
UCI_STATUS      = $df1f         ; Statusdaten
UCI_ID          = $c9

UCI_PUSH        = $01
UCI_ACCEPT      = $02
UCI_ABORT       = $04
UCI_CLRERR      = $08

!ifndef UCI_RESP_MAX{
UCI_RESP_MAX = 64
}     ; Ultimate Control: Palette, 48 Byte
!ifndef UCI_STAT_MAX{
UCI_STAT_MAX = 32
}
!ifndef UCI_CMD_MAX{
UCI_CMD_MAX = 104
}     ; Ultimate Control: WRITE_DATA
!ifndef UCI_TIMEOUT{
UCI_TIMEOUT = 250
}

; Zustand in Bits 4-5 des Statusregisters
ST_IDLE         = $00
ST_BUSY         = $10
ST_LAST         = $20
ST_MORE         = $30

;------------------------------------------------------------------
; UciPresent: Z=1, wenn die Schnittstelle antwortet ($df1d = $c9)
;------------------------------------------------------------------
UciPresent      lda UCI_CMD
                cmp #UCI_ID
                rts

;------------------------------------------------------------------
; UciUnlock: ab Firmware 3.15 schaltet diese Folge die Schnittstelle
; frei, auch wenn "Command Interface" im Menue aus ist. Auf allem
; anderen sind $d036/$d038 unbenutzte VIC-Adressen - harmlos.
; Wartet bis zu ~50 Bilder auf die Antwort der Firmware.
; Ergebnis: Z=1 = Schnittstelle da
;------------------------------------------------------------------
UciUnlock       jsr UciPresent
                beq +++
                lda #$ab
                sta $d038
                lda #$cd
                sta $d036
                lda #50
                sta UciFrames
-               jsr UciPresent
                beq +++
                jsr UciNextFrame
                dec UciFrames
                bne -
                lda #1                  ; Z=0: nicht da
+++             rts

; Wartet auf den naechsten Bildbeginn (Rasterzeile 0..255-Wechsel)
UciNextFrame
-               bit $d011
                bpl -
-               bit $d011
                bmi -
                rts

;------------------------------------------------------------------
; UciExec = UciStart + Befehl aus UciCmd + UciRun
;
; Wer lange Befehle nicht puffern will (URLs), ruft UciStart, schreibt
; die Bytes selbst nach UCI_CMD und ruft dann UciRun.
;------------------------------------------------------------------
UciExec         jsr UciStart
                bcs +
                ldx #0
-               lda UciCmd,x
                sta UCI_CMD
                inx
                cpx UciCmdLen
                bcc -
                jmp UciRun
+               rts

; Schnittstelle bereit machen. C=1: geht nicht
UciStart        lda #0
                sta UciRespLen
                sta UciStatLen
                sta UciStat
                jsr UciPresent          ; ohne Schnittstelle gar nicht erst
                beq +                   ; versuchen (VICE, alter C64 ...)
                sec
                rts
+
                ; Liegt noch ein alter Vorgang an? Dann erst aufraeumen.
                lda UCI_CTRL
                and #$30
                beq .idle
                cmp #ST_BUSY
                beq .abort
                lda #UCI_ACCEPT         ; Daten stehen noch an: quittieren
                sta UCI_CTRL
                jsr .waitNotData
                jmp .check
.abort          lda #UCI_ABORT
                sta UCI_CTRL
                jsr .waitIdle
.check          lda UCI_CTRL
                and #$30
                beq .idle
                sec
                rts
.idle           lda UCI_CTRL
                and #$08                ; Fehlerbit von frueher?
                beq +
                lda #UCI_CLRERR
                sta UCI_CTRL
+               clc
                rts

; Befehl abschicken, Antwort und Status einsammeln. C=1: Fehler
UciRun          lda #UCI_PUSH
                sta UCI_CTRL
                ; Warten, bis die Ultimate antwortet
.next           jsr .waitNotBusy
                bcs .fail
                ; Daten abholen
-               lda UCI_CTRL
                bpl +                   ; Bit 7: Daten verfuegbar
                lda UCI_RESP
!ifdef UCI_SINK {
                jsr UciSink
                jmp -
} else {
                ldx UciRespLen
                cpx #UCI_RESP_MAX
                bcs -                   ; Puffer voll: verwerfen
                sta UciResp,x
                inc UciRespLen
                bne -
}
                ; Status abholen
+
-               bit UCI_CTRL
                bvc +                   ; Bit 6: Status verfuegbar
                lda UCI_STATUS
                ldx UciStatLen
                cpx #UCI_STAT_MAX
                bcs -
                sta UciStat,x
                inc UciStatLen
                bne -
+               lda UCI_CTRL
                and #$30
                tay
                lda #UCI_ACCEPT
                sta UCI_CTRL
                cpy #ST_MORE
                bne +
                ; Es kommt noch mehr: Ultimate geht wieder auf "busy"
                jmp .next
+               jsr .waitNotData
                ldx UciStatLen          ; Status mit Null abschliessen
                lda #0
                sta UciStat,x
                clc
                rts
.fail           lda #UCI_ABORT
                sta UCI_CTRL
                jsr .waitIdle
                sec
                rts

; Warten, solange "busy" (mit Zeitgrenze ~5 s). C=1: abgelaufen
.waitNotBusy    lda #UCI_TIMEOUT
                ldx #$30
                ldy #ST_BUSY
                bne .wait

; Warten, bis keine Daten mehr anstehen (idle oder wieder busy)
.waitNotData    lda #50
                ldx #$20
                ldy #$20
                ; faellt durch
; A = Bilder, X = Maske, Y = Zustand: warten, solange (Status & X) = Y
.wait           sta UciFrames
                stx .maske
                sty .wert
                lda $d011
                and #$80
                sta UciLastMsb
-               lda UCI_CTRL
                and .maske
                cmp .wert
                clc
                bne +
                jsr .tick
                bne -
                sec
+               rts
.maske          !byte 0
.wert           !byte 0

; nach einem Abbruch: erst nicht mehr busy, dann keine Daten mehr
.waitIdle       jsr .waitNotBusy
                jmp .waitNotData

; Zaehlt Bildwechsel (Rasterzeilen-Bit 8 faellt) - Z=1, wenn abgelaufen
.tick           lda $d011
                and #$80
                cmp UciLastMsb
                sta UciLastMsb
                beq +
                bcs +                   ; 0 -> 1: nichts
                dec UciFrames
                rts
+               lda #1                  ; Z=0
                rts

;------------------------------------------------------------------
; Daten
;------------------------------------------------------------------
UciFrames       !byte 0
UciLastMsb      !byte 0
UciCmdLen       !byte 0
UciRespLen      !byte 0
UciStatLen      !byte 0
UciCmd          !fill UCI_CMD_MAX, 0
UciResp         !fill UCI_RESP_MAX, 0
UciStat         !fill UCI_STAT_MAX+1, 0
