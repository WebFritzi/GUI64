;------------------------------------------------------------------
; Weather - eine Wetter-App fuer GUI64 mit Ultimate / WiC64
;
; HTTP ueber Ultimate Command Interface (ab 3.15), sonst WiC64 (ab 2.0).
; Ortsbestimmung: ip-api.com; Ortssuche und Wetter: Open-Meteo.
; Die Antworten werden weiterhin zeichenweise als CSV/JSON ausgewertet.
;
; Datei: Weather.asm, benoetigt ../gui64.inc.asm.
; UCI- und WiC64-Treiber sind integriert; nur ../gui64.inc.asm wird benoetigt.
; UCI: Antwortzustand, Blockgroesse und DATA_ACC quittieren.
;
; VICETEST: ohne Netzwerkgeraet werden Beispieldaten durch
; dieselben Auswerter geschickt - so laesst sich die Oberflaeche in
; VICE pruefen.
;------------------------------------------------------------------
;!to "Weather.d64",d64,"weather.gui","weather disk"

!source "../gui64.inc.asm"

; UCI-Treiber aus der urspruenglichen Ultimate-Version.
; Nur kleine Befehls- und Antwortpuffer; der HTTP-Body wird gestreamt.
UCI_SINK        = 1
UCI_CMD_MAX     = 4
UCI_RESP_MAX    = 2
UCI_STAT_MAX    = 12
NET_NONE        = 0
NET_ULTIMATE    = 1
NET_WIC64       = 2



!zone Konstanten
WT_WETTER       = 78
CT_ICON         = 50            ; eigenes Steuerelement: Symbol 2x2
CT_TEMP         = 51            ; eigenes Steuerelement: Text mit Gradzeichen
ID_MENU_WTR     = 10
WT_GEO          = 79
GEO_MAX         = 20
GEO_PAGE_SIZE   = 10
GEO_REC_LEN     = 88             ; Found (52) + fertige Listbox-Zeile (36)
GEO_LABEL_MAX   = 36             ; inklusive Nullbyte
NL              = $1c           ; Zeilenwechsel in Meldungen
DEG             = $b0           ; steht im Text fuer das Gradzeichen
SPACE_SCR       = $a0           ; Leerzeichen als Bildschirmcode (GUI64)

; Steuerelemente (bedienbare unter 16: GUI64 2.1 waehlt nur die ersten
; 16 eines Fensters an, der Index wird in einem Byte mal 16 gerechnet)
C_EDIT          = 3
C_BTN_SEARCH    = 4
C_ICON_NOW      = 6
C_ICON_D0       = 14

; Symbole
IC_SUN          = 1
IC_PARTLY       = 2
IC_CLOUD        = 3
IC_RAIN         = 4
IC_SNOW         = 5

; Nicht ZP_0E/0F, obwohl gui64.inc.asm sie frei nennt: GUI64s Interrupt
; nimmt $0e fuer Tastatur und Maus (gesehen: ein Zeitstempel verlor
; mitten im Einlesen zwei Zeichen). Frei vom Interrupt sind $5f/$60 und
; $fb-$fe.
AppPtr          = ZP_5F         ; Anhaengen an einen Textpuffer
TabPtr          = ZP_FB         ; Zeiger in Tabellen
DstPtr          = ZP_FD

;==================================================================
!zone Start
* = $b000               
                ldx #<TimerHandler
                ldy #>TimerHandler
                jsr GUI_InitTimer
                ldx #<CharList
                ldy #>CharList
                lda #CHAR_COUNT
                jsr GUI_RegisterChars
                ldx #<CtrlAction
                ldy #>CtrlAction
                jsr GUI_SetCtrlActionsRoutine
                ldx #<PaintCtrls
                ldy #>PaintCtrls
                jsr GUI_SetPaintCtrlsRoutine
                ldx #<Wnd_Main
                ldy #>Wnd_Main
                jsr GUI_CreateWindowEx
                jsr GUI_GetDesign       ; Z=1: WIN, Z=0: MAC
                beq +
                inc WindowPosY          ; MAC: Menueleiste oben am Schirm
                dec WindowHeight
                jsr GUI_UpdateWindow
+               jsr GUI_SelectControl0
                ldx #<Str_Menubar
                ldy #>Str_Menubar
                lda #2
                jsr GUI_SetCtrlStringList
                
                jsr GUI_SelectControl3
                jsr GUI_GetDesign
                beq +
                lda ControlBits
                ora #BIT_CTRL_DBLFRAME_TOP
                sta ControlBits                
+               ldx #<Str_Edit
                ldy #>Str_Edit
                jsr GUI_SetCtrlString
                lda #0
                ldx #16
                jsr GUI_SetEditSLInfo
                
                jsr GUI_SelectControl4
                jsr GUI_GetDesign
                bne ++
                ldx #<BtnTextWin
                ldy #>BtnTextWin
                jsr GUI_SetCtrlString
                lda #8
                sta ControlWidth
                inc ControlPosX
                bne +++
++              lda ControlBits
                ora #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_RGT)
                sta ControlBits
+++             jsr GUI_UpdateControl
                ; GUI64 2.1 laesst nach einem langen Ladevorgang "Fenster
                ; wird verschoben" ($85) stehen und schluckt damit das erste
                ; Loslassen der Maustaste (wie in ucontrol.asm)
                lda #0
                sta $85
                jsr GUI_StartTimer
                ; Zuerst Ultimate: UciUnlock erkennt auch ein bereits
                ; freigeschaltetes Command Interface.
                jsr UciUnlock
                bne .probeWiC
                lda #NET_ULTIMATE
                sta NetBackend
                jmp Locate
.probeWiC       jsr WiCProbe
                bne .keinNetz
                lda #NET_WIC64
                sta NetBackend
                jmp Locate
.keinNetz
!ifdef VICETEST {
                jmp FakeData
} else {
                ldx #<Str_NoNet1
                ldy #>Str_NoNet1
                jsr SetLoc
                ldx #<Str_NoNet2
                ldy #>Str_NoNet2
                jmp Fail
}

BtnTextWin      !pet "Search",0
;==================================================================
!zone Fensterprozedur
WndProc         jsr GUI_StdWndProc
                lda wndParam1
                bmi .raus
                beq .normal
                lda wndParam0           ; Menue offen
                cmp #EC_LBTNPRESS
                bne .raus
                jsr GUI_IsInCurMenu
                bcc .raus
                jsr GUI_GetCurMenuID
                cmp #ID_MENU_WTR
                beq .wetter
                ldx #<Str_About
                ldy #>Str_About
                jmp GUI_ShowMessage
.wetter         lda CurMenuItem         ; jeden Eintrag ausdruecklich pruefen
                bne +                   ; ($ff = Maus nicht bewegt)
                jmp Forecast            ; 0: Refresh
+               cmp #1
                bne +
                jmp Locate              ; 1: Locate me
+               cmp #2
                bne .raus
                jsr GUI_StopTimer       ; 2: Quit
                jsr GUI_KillCurWindow
                jmp GUI_Repaint
.raus           rts

.normal         lda wndParam0
                cmp #EC_KEYPRESS        ; RETURN = Suchen
                bne +
                lda actkey
                cmp #$fc                ; GUI64s Code fuer RETURN
                beq Search
                rts
+               cmp #EC_LBTNRELEASE
                bne .raus
                jsr GUI_IsInCurControl
                bcc .raus
                lda ControlIndex
                cmp #C_BTN_SEARCH
                beq Search
                rts

;==================================================================
!zone Suche
; Eingabe suchen. Die Ortssuche kennt keine Umschreibung: "koeln" findet
; ein Dorf in Indonesien, "koln" findet Koeln. Also: steht ae/oe/ue in
; der Eingabe, zuerst ohne das e suchen und den Treffer nehmen, wenn
; sein Name (in unserer Schreibweise, Koeln) genau der Eingabe
; entspricht. Sonst wie getippt - so bleiben Soest und Uelzen, was sie
; sind.
Search          lda NetBackend
                beq .raus
                jsr ReadEdit
                beq .raus
                jsr GeoSaveFoot
                jsr Busy
                jsr Collapse
                bcc .wieGetippt
                lda #1                  ; kurze Umlaut-Vorsuche
                sta GeoShortQuery
                ldx #<Collapsed
                ldy #>Collapsed
                jsr GeoQuery
                bcs .fehler
                ; Unter allen Treffern der gekuerzten Suche den exakten
                ; Ortsnamen suchen (z.B. "Koeln" -> "koln" -> "Koeln").
                ; Nicht nur den Fall mit genau einem Treffer pruefen!
                lda #0
                sta GeoMatchIndex
.pruefen        lda GeoMatchIndex
                cmp GeoCount
                bcs .wieGetippt
                jsr GeoToFound
                jsr NameIsTyped
                beq .nehmen
                inc GeoMatchIndex
                jmp .pruefen
.wieGetippt     lda #0                  ; normale Suche mit bis zu 50 Treffern
                sta GeoShortQuery
                ldx #<Typed
                ldy #>Typed
                jsr GeoQuery
                bcs .fehler
                lda GeoCount
                beq .nichtda
                cmp #1
                beq .nehmen
                jmp ShowGeoDialog
.nehmen         jsr TakeFound
                jmp Forecast
.nichtda        ldx #<Str_NotFound
                ldy #>Str_NotFound
                jmp Fail
.fehler         jmp NetError
.raus           rts

!zone
; Eingabefeld (PETSCII, mit Leerzeichen aufgefuellt) -> Typed:
; ASCII, klein, ohne Leerzeichen am Rand. Z=1: leer
; Eingabefeld (PETSCII, mit Leerzeichen aufgefuellt) -> Typed:
; ASCII, klein, ohne Leerzeichen am Rand. Z=1: leer
ReadEdit        ldx #0
                ldy #0
-               lda Str_Edit,x
                beq .ende
                and #$7f
                ; Kleinbuchstaben a-z
                cmp #$61
                bcc +
                cmp #$7b
                bcc .rein
+               ; Grossbuchstaben A-Z -> a-z
                cmp #$41
                bcc +
                cmp #$5b
                bcs .weg
                ora #$20
                bne .rein
+               ; Leerzeichen
                cmp #$20
                bne +
                cpy #0                  ; Leerzeichen am Anfang weg
                beq .weg
                bne .rein
+               ; Bindestrich und Ziffern
                cmp #"-"
                beq .rein
                cmp #"0"
                bcc .weg
                cmp #":"
                bcs .weg
.rein           sta Typed,y
                iny
.weg            inx
                cpx #16
                bcc -
.ende
-               dey                     ; Leerzeichen am Ende weg
                bmi +
                lda Typed,y
                cmp #$20
                beq -
+               iny
                lda #0
                sta Typed,y
                cpy #0
                rts

!zone
; Typed -> Collapsed ohne das e hinter a/o/u. C=1: es gab eins
Collapse        ldx #0
                ldy #0
                clc
                php
-               lda Typed,x
                sta Collapsed,y
                beq ende
                cmp #"e"
                bne +
                cpx #0                  ; ein e am Anfang bleibt
                beq +
                lda Typed-1,x
                cmp #"a"
                beq weg
                cmp #"o"
                beq weg
                cmp #"u"
                bne +
weg             plp
                sec
                php
                dey
+               inx
                iny
                bne -
ende            plp
                rts

!zone
; FoundName gleich Typed (ohne Gross/Klein)? Z=1: ja
NameIsTyped     ldx #$ff
-               inx
                lda FoundName,x
                cmp #$41
                bcc +
                cmp #$5b
                bcs +
                ora #$20
+               cmp Typed,x
                bne raus
                cmp #0
                bne -
raus            rts

!zone
; Found* -> Loc* (gleicher Aufbau)
TakeFound       ldx #FOUND_LEN-1
-               lda Found,x
                sta Loc,x
                dex
                bpl -
                rts

;==================================================================
!zone Netz
; Ort aus der IP-Adresse (ip-api.com, CSV: status,cc,city,lat,lon)
Locate          jsr Busy
                ldx #<Url_Ip
                ldy #>Url_Ip
                jsr UrlBegin
                lda #<IpTable
                ldx #>IpTable
                jsr CsvStart
                jsr HttpGet
                bcc +
                jmp NetError
+               lda FoundSt             ; "success"?
                cmp #"s"
                bne .unbekannt
                jsr TakeFound
                jmp Forecast
.unbekannt      ldx #<Str_NoPlace
                ldy #>Str_NoPlace
                jmp Fail

!zone
; Ortssuche: X/Y = Name (ASCII). Ergebnis in Found*, FoundLat leer = nichts
GeoQuery        stx .name
                sty .name+1
                lda GeoShortQuery
                beq .long
                ldx #<Url_Geo20
                ldy #>Url_Geo20
                jmp .begin
.long           ldx #<Url_Geo
                ldy #>Url_Geo
.begin          jsr UrlBegin
                ldx .name
                ldy .name+1
                jsr UrlAdd
                lda #0
                sta GeoCount
                sta GeoStarted
                sta GeoPrefixMode
                sta JsonCap
                sta Utf8Flag
                lda #$ff
                sta JsonKey
                lda #0
                sta JsonPos
                lda #<JsonByte
                ldx #>JsonByte
                jsr SetWant
                jsr HttpGet
                php
                jsr GeoCommit          ; letzten Treffer abschliessen
                lda GeoCount
                beq .done
                lda #0
                jsr GeoToFound         ; erster Treffer wie bisher in Found
.done           plp
                rts
.name           !word 0

!zone
; Wetter holen und anzeigen
Forecast        lda NetBackend
                beq rauss
                jsr Busy
                ldx #<Url_Fc1
                ldy #>Url_Fc1
                jsr UrlBegin
                ldx #<LocLat
                ldy #>LocLat
                jsr UrlAdd
                ldx #<Url_Fc2
                ldy #>Url_Fc2
                jsr UrlAdd
                ldx #<LocLon
                ldy #>LocLon
                jsr UrlAdd
                ldx #<Url_Fc3
                ldy #>Url_Fc3
                jsr UrlAdd
                lda #0
                sta CurTemp
                lda #<FcTable
                ldx #>FcTable
                jsr CsvStart
                jsr HttpGet
                bcs NetError
                lda CurTemp
                beq NetError
                jmp Show
rauss           rts

NetError        ldx #<Str_NetErr
                ldy #>Str_NetErr
                ; faellt durch
; Text (PETSCII) in die Fusszeile, alte Werte bleiben stehen
Fail            jsr SetFoot
                jmp GUI_RepaintCurWindow

; Fusszeile "Loading..." sofort zeigen - das Holen haelt GUI64 an
Busy            ldx #<Str_Loading
                ldy #>Str_Loading
                jmp Fail

;------------------------------------------------------------------
; HTTP ueber WiC64 (Userport; Protokoll "R", Firmware ab 2.0)
;------------------------------------------------------------------
; Das WiC64 meldet Status und Antwortgroesse in drei Headerbytes.
; Es liefert den HTTP-Body ohne HTTP-Statuszeile. Byteweise Ausgabe an
; den vorhandenen Parser. Einzig die URL wird gepuffert (max. 255 Byte).
;
; X/Y = erster URL-Teil (inklusive http:// oder https://)
UrlBegin        lda #0
                sta UrlLen
                sta UrlErr
                ; faellt durch
; X/Y = URL-Teil, Leerzeichen werden als %20 kodiert
UrlAdd          stx TabPtr
                sty TabPtr+1
                ldy #0
.loop           lda (TabPtr),y
                beq .done
                cmp #$20
                bne .plain
                lda #"%"
                jsr UrlPut
                lda #"2"
                jsr UrlPut
                lda #"0"
.plain          jsr UrlPut
                iny
                bne .loop
.done           rts

UrlPut          ldx UrlLen
                cpx #255
                bcc .store
                lda #1
                sta UrlErr
                rts
.store          sta UrlBuf,x
                inc UrlLen
                rts

; Vor jedem Aufruf wurde WantSink durch CsvStart/SetWant gesetzt.
; C=1: Netzwerk-/Protokollfehler
HttpGet         lda NetBackend
                cmp #NET_ULTIMATE
                beq UciHttpGet
                jmp WiCHttpGet

!zone WiCHttpZone
WiCHttpGet      lda UrlErr
                bne .error
                lda #1                  ; HTTP_GET ($01)
                sta WiCCmd
                lda UrlLen
                sta WiCLen
                lda #0
                sta WiCLen+1
                jsr WiCExchange
                php
                ; Nach dem letzten CSV-Feld kommt evtl. kein LF
                lda WantSink
                cmp #<CsvByte
                bne .finish
                lda WantSink+1
                cmp #>CsvByte
                bne .finish
                jsr CsvEnd
.finish         plp
                rts
.error          sec
                rts

;------------------------------------------------------------------
; HTTP ueber Ultimate Command Interface, kompatibel zum HTTP-Backend
; der urspruenglichen Weather-App. Die URL wird aus UrlBuf geschrieben.
; Der UCI-HTTP-Treiber erwartet Host und Pfad OHNE http(s)://.
; Der vorhandene Sink bleibt fuer CSV/JSON/Abbruch identisch.
;------------------------------------------------------------------
!zone UciHttpZone
UciHttpGet      lda UrlErr
                beq +
                jmp .error
+
                ; UCI-HTTP-Koepfe bleiben ueber Requests hinweg bestehen.
                ; Vor jedem GET alle alten Koepfe freigeben.
                lda #<SinkDiscard
                ldx #>SinkDiscard
                jsr UciSetSink
                lda #$06
                sta UciCmd
                lda #$10                ; HTTP Free All
                sta UciCmd+1
                lda #2
                sta UciCmdLen
                jsr UciExec
                bcc +
                jmp .error
+               ; HEADER_CREATE: $06 $11 $01 <host/pfad>
                jsr UciStart
                bcc +
                jmp .error
+               lda #$06
                sta UCI_CMD
                lda #$11
                sta UCI_CMD
                lda #$01                ; GET
                sta UCI_CMD
                ; http:// = 7 Bytes, https:// = 8 Bytes
                ldx #7
                lda UrlBuf+4
                cmp #"s"
                bne .sendurl
                inx
.sendurl        cpx UrlLen
                beq .header
                lda UrlBuf,x
                sta UCI_CMD
                inx
                bne .sendurl
.header         lda #<UciHeaderByte
                ldx #>UciHeaderByte
                jsr UciSetSink
                jsr UciRun
                bcc +
                jmp .error
+               lda UciRespLen
                bne +
                jmp .error
+               lda UciResp
                sta UciCmd+2          ; Header-ID
                ; DO_EXCHANGE_RAW mit angelegtem Header
                lda #$06
                sta UciCmd
                lda #$32
                sta UciCmd+1
                lda #$ff                ; kein Request-Body
                sta UciCmd+3
                lda #4
                sta UciCmdLen
                ; Der Parser kann waehrend der Antwort selbst auf
                ; SinkDiscard umschalten (volle Trefferliste).
                jsr WiCSetSink
                jsr UciExec
                php
                ; Letztes CSV-Feld auch ohne abschliessenden LF uebernehmen.
                lda WantSink
                cmp #<CsvByte
                bne .finishcsv
                lda WantSink+1
                cmp #>CsvByte
                bne .finishcsv
                jsr CsvEnd
.finishcsv      plp
                bcs .error
                lda UciStat+9           ; HTTP/1.1 2xx
                cmp #"2"
                bne .error
                clc
                rts
.error          sec
                rts

; UCI ruft UciSink auf, die gleiche Sprung-Senke wie WiC64.
UciSink         jmp WiCSink
UciSetSink      sta WiCSink+1
                stx WiCSink+2
                rts
; Die Header-Nummer wird als sehr kurze UCI-Antwort gespeichert.
UciHeaderByte   ldx UciRespLen
                cpx #UCI_RESP_MAX
                bcs .done
                sta UciResp,x
                inc UciRespLen
.done           rts

; WiC64 suchen und eine kurze Versionsabfrage versuchen.
; Z=1: Modul antwortet mit Erfolgsstatus.
!zone WiCNetZone
WiCProbe        lda #1
                sta WiCProbeActive      ; kurzer Timeout nur beim Probe
                lda #<SinkDiscard
                sta WantSink
                lda #>SinkDiscard
                sta WantSink+1
                lda #$26                ; GET_VERSION_NUMBERS
                sta WiCCmd
                lda #0
                sta WiCLen
                sta WiCLen+1
                jsr WiCExchange
                php
                lda #0
                sta WiCProbeActive
                plp
                bcs .no
                lda #0
                rts
.no             lda #1
                rts

SinkDiscard     rts

; WiC64 uebertraegt per CIA2 $dd01 (PB0..7), $dd00/PA2 und FLAG2.
; Rasterinterrupts bleiben waehrend des Transfers aktiv.
WiCExchange     jsr WiCSetSink
                jsr WiCTransfer
                bcs .bad
                lda #0
                sta WiCErr
                jmp .restore
.bad            lda #1
                sta WiCErr
.restore        ; Port B wieder Eingabe, PA2 wieder high
                lda #0
                sta $dd03
                lda $dd00
                ora #$04
                sta $dd00
                lda $dd0d               ; altes FLAG loeschen
                lda WiCErr
                beq .ok
                sec
                rts
.ok             clc
                rts

WiCTransfer     lda $dd0d               ; CIA2 FLAG-Rest quittieren
                lda $dd02
                ora #$04
                sta $dd02
                lda $dd00
                ora #$04                ; ESP auf Empfang
                sta $dd00
                lda #$ff
                sta $dd03               ; PB als Ausgang
                lda #"R"               ; Standardprotokoll
                jsr WiCSend
                bcc +
                jmp .fail
+
                lda WiCCmd
                jsr WiCSend
                bcc +
                jmp .fail
+
                lda WiCLen
                jsr WiCSend
                bcc +
                jmp .fail
+
                lda WiCLen+1
                jsr WiCSend
                bcc +
                jmp .fail
+
                ldx #0
.sendurl        cpx WiCLen
                beq .receive
                txa
                pha
                lda UrlBuf,x
                jsr WiCSend
                pla
                tax
                bcc +
                jmp .fail
+
                inx
                bne .sendurl
.receive       lda #0
                sta $dd03               ; PB als Eingang
                lda $dd00
                and #$fb                ; PA2 low: C64 empfaengt
                sta $dd00
                jsr WiCWait             ; Richtungswechsel bestaetigen
                bcc +
                jmp .fail
+
                lda $dd01               ; Dummy-Handshake
                jsr WiCRead             ; Status
                bcc +
                jmp .fail
+
                sta WiCStatus
                jsr WiCRead             ; Antwortlaenge low
                bcc +
                jmp .fail
+
                sta WiCRemaining
                jsr WiCRead             ; Antwortlaenge high
                bcc +
                jmp .fail
+
                sta WiCRemaining+1
; Der Body wird bis zum letzten Byte komplett vom WiC64 gelesen.
; Nur die zeitkritische Schleife ist kuerzer: ein JSR/RTS weniger
; pro Byte, keine unbedingte JMP-Anweisung und ein schneller
; Nulltest des niederwertigen Laengenzaehlers.
.readbody       lda WiCRemaining
                ora WiCRemaining+1
                beq .endbody
.readnext       jsr WiCWait
                bcs .fail
                lda $dd01
                jsr WiCSink             ; A -> aktiver CSV/JSON-Parser
                lda WiCRemaining
                bne .decrement
                dec WiCRemaining+1
.decrement      dec WiCRemaining
                bne .readnext
                lda WiCRemaining+1
                bne .readnext
.endbody        lda WiCStatus
                bne .fail
                clc
                rts
.fail           sec
                rts

WiCSend         sta $dd01
                jmp WiCWait
!zone WiCReadZone
WiCRead         jsr WiCWait
                bcs .bad
                lda $dd01
                clc
.bad            rts

; FLAG2 der CIA2 wird durch Lesen von $dd0d quittiert.
; Grober Timeout (auch ohne laufende IRQs).
!zone WiCWaitZone
WiCWait         lda #$10
                bit $dd0d
                bne .ready
                lda WiCProbeActive
                beq .normal
                ; Ohne WiC64 keine ~30 s Wartezeit. Fuer die reine
                ; Erkennung ca. 0,5 s, im Normalbetrieb langer Timeout.
                lda #1
                sta WiCTime2
                lda #$80
                sta WiCTime1
                lda #0
                sta WiCTime0
                beq .loop
.normal         lda #$12
                sta WiCTime2
                lda #0
                sta WiCTime1
                sta WiCTime0
.loop           lda #$10
                bit $dd0d
                bne .ready
                dec WiCTime0
                bne .loop
                dec WiCTime1
                bne .loop
                dec WiCTime2
                bne .loop
                sec
                rts
.ready          clc
                rts

; Senke fuer das Parsen der HTTP-Antwort
WiCSink         jmp SinkDiscard
; WiCSink wird vor der Antwort auf den aktuellen Parser gesetzt.
WiCSetSink      lda WantSink
                sta WiCSink+1
                lda WantSink+1
                sta WiCSink+2
                rts

WiCCmd          !byte 0
WiCLen          !word 0
WiCRemaining    !word 0
WiCStatus       !byte 0
WiCErr          !byte 0
WiCTime0        !byte 0
WiCTime1        !byte 0
WiCTime2        !byte 0
UrlLen          !byte 0
; UrlBuf nutzt den Listbox-Puffer waehrend HTTP-Anfragen: 255 Bytes
; URL-Speicher ohne eigenen Block im initialisierten App-Code.

;------------------------------------------------------------------
; Auswerter
;------------------------------------------------------------------
!zone Anhaengen
; X/Y = Puffer, A = Groesse (mit Null): leeren und dorthin anhaengen
AppTo           stx AppPtr
                sty AppPtr+1
                sta AppMax
                lda #0
                sta AppLen
                tay
                sta (AppPtr),y
                rts

; Zeichen anhaengen: an (AppPtr), bis AppMax-1 Zeichen, immer mit Null
Append          ldy AppLen
                iny
                cpy AppMax
                bcs +
                dey
                sta (AppPtr),y
                iny
                sty AppLen
                lda #0
                sta (AppPtr),y
+               rts

; UTF-8 -> ASCII vor dem Anhaengen: Umlaute als ae/oe/ue/ss, ein paar
; Akzente ohne Akzent, alles andere Mehrbyte als "?"
Utf8Put         bit Utf8Flag
                bmi .zweites
                cmp #$c3
                bne +
                ror Utf8Flag            ; C=1 nach cmp: Bit 7 setzen
                rts
+               cmp #$80
                bcc Append
                cmp #$c0
                bcc .raus               ; Folgebyte eines anderen Zeichens
                lda #"?"
                bne Append
.zweites        lsr Utf8Flag
                ldx #UTF8_ANZ-1
-               cmp Utf8In,x
                beq +
                dex
                bpl -
                lda #"?"
                bne Append
+               lda Utf8Out,x
                jsr Append
                cpx #6                  ; die ersten sechs: Umlaute -> +e,
                bcc +                   ; die siebte: ss
                bne .raus
                lda #"s"
                jmp Append
+               lda #"e"
                jmp Append
.raus           rts
Utf8Flag        !byte 0

!zone Csv
; CSV: Feldpuffer fuellen; bei Komma/Zeilenende das Feld anhand der
; Tabelle (Zeile, Feld, Ziel, Groesse) ablegen. A/X = Tabelle
CsvStart        sta CsvTab
                stx CsvTab+1
                lda #0
                sta CsvLine
                sta CsvField
                sta Utf8Flag
                jsr CsvReset
                lda #<CsvByte
                ldx #>CsvByte
                ; faellt durch
; Wohin die Antwort des naechsten Austauschs gehen soll
SetWant         sta WantSink
                stx WantSink+1
                rts

CsvReset        ldx #<CsvBuf
                ldy #>CsvBuf
                lda #20
                jmp AppTo

CsvByte         cmp #$0d
                beq .raus
                cmp #","
                beq .feld
                cmp #$0a
                bne Utf8Put
                jsr CsvEnd
                inc CsvLine
                lda #0
                sta CsvField
.raus           rts
.feld           jsr CsvEnd
                inc CsvField
                rts

CsvEnd          lda CsvTab
                sta TabPtr
                lda CsvTab+1
                sta TabPtr+1
                lda CsvLine             ; Schluessel = Zeile * 8 + Feld
                asl
                asl
                asl
                ora CsvField
                sta .key
                ldy #0
-               lda (TabPtr),y          ; Schluessel ($ff = Ende)
                bmi .fertig
                cmp .key
                bne .weiter
                iny
                lda (TabPtr),y          ; Ziel
                sta DstPtr
                iny
                lda (TabPtr),y
                sta DstPtr+1
                iny
                lda (TabPtr),y          ; Groesse (mit Null)
                tax
                ldy #0
--              lda CsvBuf,y
                sta (DstPtr),y
                beq .fertig
                iny
                dex
                bne --
                dey
                lda #0
                sta (DstPtr),y
                beq .fertig
.weiter         tya
                clc
                adc #4
                tay
                bne -
.fertig         jmp CsvReset
.key            !byte 0

!zone Json
; Streaming-Parser: Anstelle von sieben parallelen Suchmustern
; wird nur ein Kandidat verfolgt. $ff = kein Kandidat,
; $fe = erstes Zeichen nach einem Anfuehrungszeichen erwartet.
; Der eigentliche Wert wird wie bisher direkt mit Utf8Put gesammelt.
JsonByte        ldx JsonCap
                beq .suchen
                cmp JsonTerm
                beq .ende
                cmp #"}"
                beq .ende
                jmp Utf8Put
.ende           lda #0
                sta JsonCap
                rts
.suchen         sta JsonCh
                lda JsonKey
                cmp #$fe
                bne +
                jmp .waehlen
+               cmp #$ff
                bne +
                jmp .start
+
                tax
                lda JsonPos
                clc
                adc PatStart,x
                tay
                lda PatText,y
                cmp JsonCh
                beq .passt
                ; Gemeinsam beginnende Schluessel erst bei Unterscheidung
                ; verzweigen: latitude/longitude und admin1/admin2.
                cpx #1
                bne .admin
                lda JsonPos
                cmp #2
                bne .fehl
                lda JsonCh
                cmp #"o"
                bne .fehl
                ldx #2
                stx JsonKey
                jmp .passt
.admin          cpx #4
                bne .fehl
                lda JsonPos
                cmp #6
                bne .fehl
                lda JsonCh
                cmp #"2"
                bne .fehl
                ldx #5
                stx JsonKey
.passt          inc JsonPos
                lda JsonPos
                cmp PatLen,x
                bne .raus
                ; Ein kompletter Schluessel wurde erkannt.
                lda #$ff
                sta JsonKey
                cpx #6
                beq GeoBegin
                lda PatTerm,x
                sta JsonTerm
                inx
                stx JsonCap
                lda #0
                sta Utf8Flag
                ldy PatDstHi-1,x
                lda PatMax-1,x
                pha
                lda PatDst-1,x
                tax
                pla
                jmp AppTo
.fehl           lda #$ff
                sta JsonKey
.start          lda JsonCh
                cmp #$22
                bne .raus
                lda #$fe
                sta JsonKey
.raus           rts
.waehlen        ldx #6
-               lda PatStart,x
                clc
                adc #1                  ; erstes Zeichen hinter dem '"'
                tay
                lda PatText,y
                cmp JsonCh
                beq .gefunden
                dex
                bpl -
                lda #$ff
                sta JsonKey
                rts
.gefunden       cpx #2
                bne +
                ldx #1                 ; latitude/longitude erst spaeter trennen
+               cpx #5
                bne +
                ldx #4                 ; admin1/admin2 erst bei der Ziffer trennen
+               stx JsonKey
                lda #2                  ; '"' und erstes Zeichen gelesen
                sta JsonPos
                rts

!zone GeoStorage
; Ein neuer "id"-Schluessel beendet den vorigen Ergebnis-Datensatz.
!zone GeoBeginZone
GeoBegin        jsr GeoCommit
                lda #1
                sta GeoStarted
                lda #0
                ldx #FOUND_LEN-1
-               sta Found,x
                dex
                bpl -
                ldx #23
-               sta GeoAdmin,x
                sta GeoAdmin2,x
                dex
                bpl -
                rts

; Maximal GEO_MAX Treffer speichern. Vor dem ersten Praefixtreffer
; werden Fallbacktreffer gesammelt, danach nur noch Praefixtreffer.
; In jedem Fall bleibt die API-Reihenfolge erhalten.
!zone GeoCommitZone
GeoCommit       lda GeoStarted
                bne +
                rts
+               lda #0
                sta GeoStarted
                lda FoundLat
                bne +
                rts
+               jsr NameStartsTyped
                bne .fallback
                ; Praefixtreffer: beim ersten solchen Treffer die bisherige
                ; Fallbackliste verwerfen.
                lda GeoPrefixMode
                bne .store
                lda #1
                sta GeoPrefixMode
                lda #0
                sta GeoCount
                beq .store
.fallback       lda GeoPrefixMode
                beq .store
                rts
.store          ldx GeoCount
                cpx #GEO_MAX
                bcc +
                rts
                ; Beschriftung direkt im Ziel-Datensatz erstellen.
                ; GeoBuildLabel belässt DstPtr auf dem Record-Anfang.
+               jsr GeoBuildLabel
                ldy #0
-               lda Found,y
                sta (DstPtr),y
                iny
                cpy #FOUND_LEN
                bcc -
                inc GeoCount
                ; Bei der Umlaut-Vorsuche reicht der erste exakte Name.
                ; Die uebrigen Bytes weiterhin vom WiC64 empfangen!
                lda GeoShortQuery
                beq .limit
                jsr NameIsTyped
                beq .stop
.limit          ; Bei 20 passenden Namen sind alle Listenslots belegt.
                ; Fallbacktreffer duerfen die Verarbeitung nicht stoppen,
                ; da spaeter noch Praefixtreffer folgen koennen.
                lda GeoPrefixMode
                beq .done
                lda GeoCount
                cmp #GEO_MAX
                bne .done
.stop           lda #<SinkDiscard
                sta WiCSink+1
                lda #>SinkDiscard
                sta WiCSink+2
.done           rts

; Fertige PETSCII-Zeile direkt in den freien Record schreiben.
; Danach zeigt DstPtr weiterhin auf den Anfang des Records.
!zone GeoBuildLabelZone
GeoBuildLabel   ldx GeoCount
                jsr GeoSlotPtr
                lda DstPtr
                clc
                adc #FOUND_LEN
                tax
                lda DstPtr+1
                adc #0
                tay
                lda #GEO_LABEL_MAX
                jsr AppTo
                ldx #<FoundName
                ldy #>FoundName
                jsr AppStr
                lda GeoAdmin2
                beq .admin1
                jsr GeoComma
                ldx #<GeoAdmin2
                ldy #>GeoAdmin2
                jsr AppStr
.admin1         lda GeoAdmin
                beq .country
                jsr GeoComma
                ldx #<GeoAdmin
                ldy #>GeoAdmin
                jsr AppStr
.country        lda FoundCC
                beq .done
                jsr GeoComma
                ldx #<FoundCC
                ldy #>FoundCC
                jsr AppStr
.done           rts

!zone GeoCommaZone
GeoComma        ldx #<Txt_Komma
                ldy #>Txt_Komma
                jmp AppStr

; Z=1, wenn FoundName mit Typed beginnt (ASCII-Umschrift, ohne
; Gross-/Kleinschreibung). Im Gegensatz zu NameIsTyped muss der Name
; nach dem Suchbegriff nicht enden.
!zone GeoPrefixCheckZone
NameStartsTyped ldx #0
.loop           lda Typed,x
                beq .yes
                sta .expected
                lda FoundName,x
                cmp #$41
                bcc +
                cmp #$5b
                bcs +
                ora #$20
+               cmp .expected
                bne .no
                inx
                bne .loop
.no             lda #1
                rts
.yes            lda #0
                rts
.expected       !byte 0

; X = Trefferindex -> DstPtr (ZP FD/FE)
!zone GeoSlotPtrZone
GeoSlotPtr      lda GeoPtrLo,x
                sta DstPtr
                lda GeoPtrHi,x
                sta DstPtr+1
                rts

; A = Trefferindex -> Found (52 Bytes)
!zone GeoToFoundZone
GeoToFound      tax
                jsr GeoSlotPtr
                ldy #0
-               lda (DstPtr),y
                sta Found,y
                iny
                cpy #FOUND_LEN
                bcc -
                rts

; Listbox-Strings aus den gespeicherten Treffern erstellen.
; Name, Region, Land (bei langen Eintraegen auf 35 Zeichen gekuerzt).
!zone GeoMakeListZone
; Fertige PETSCII-Zeilen aus den Records fuer die angezeigte Seite
; direkt hintereinander kopieren (ohne erneute Textformatierung).
GeoMakeList     lda #<GeoListStrings
                sta GeoNext
                lda #>GeoListStrings
                sta GeoNext+1
                lda #26
                sta GeoLongest
                lda GeoCount
                sec
                sbc GeoPageStart
                cmp #GEO_PAGE_SIZE
                bcc +
                lda #GEO_PAGE_SIZE
+               sta GeoPageCount
                lda #0
                sta GeoN
.next           lda GeoN
                clc
                adc GeoPageStart
                tax
                jsr GeoSlotPtr
                ; ZP $fd/$fe: Zeilenquelle, ab Offset FOUND_LEN.
                clc
                lda DstPtr
                adc #FOUND_LEN
                sta DstPtr
                bcc +
                inc DstPtr+1
+               lda GeoNext
                sta TabPtr
                lda GeoNext+1
                sta TabPtr+1
                ldy #0
.copy           lda (DstPtr),y
                sta (TabPtr),y
                beq .copied
                iny
                cpy #GEO_LABEL_MAX
                bcc .copy
                ; Bei GeoBuildLabel ist die Terminierung garantiert.
.copied         tya                     ; Laenge ohne Nullbyte
                cmp GeoLongest
                bcc .advance
                sta GeoLongest
.advance        tya
                sec                     ; +1 fuer Nullbyte
                adc GeoNext
                sta GeoNext
                bcc +
                inc GeoNext+1
+               inc GeoN
                lda GeoN
                cmp GeoPageCount
                bcc .next
                rts

!zone GeoDialog
; Dialogbreite aus dem laengsten Eintrag ermitteln, begrenzt auf 40.
!zone ShowGeoDialogZone
ShowGeoDialog   lda #0
                sta GeoPageStart

!zone GeoShowPageZone
GeoShowPage     jsr GeoMakeList
                lda GeoLongest
                clc
                adc #5
                cmp #41
                bcc +
                lda #40
+               sta Wnd_Geo+4
                sta GeoWidth
                sec
                sbc #2
                sta DlgList+3
                lda #40
                sec
                sbc GeoWidth
                lsr
                sta Wnd_Geo+2
                ; Rechtsbuendig: Cancel, OK, ein Feld Abstand.
                lda GeoWidth
                sec
                sbc #9
                sta DlgOK+1
                lda GeoWidth
                sec
                sbc #18
                sta DlgCancel+1
                ldx #<Wnd_Geo
                ldy #>Wnd_Geo
                jsr GUI_CreateWindowEx
                ; Alle vier Dialogbuttons mit unterem Doppelrahmen.
                ldy #1
                lda #(BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_RGT)
                jsr GUI_SelectCtrl_AddBits
                ldy #2
                lda #(BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_LFT)
                jsr GUI_SelectCtrl_AddBits
                ldy #3
                lda #(BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_RGT)
                jsr GUI_SelectCtrl_AddBits
                ldy #4
                lda #(BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_RGT)
                jsr GUI_SelectCtrl_AddBits
                ;
                jsr GUI_GetDesign
                beq +
                ; MAC
                jsr GUI_SelectControl1
                dec ControlPosX
                jsr GUI_UpdateControl
                jsr GUI_SelectControl2
                lda ControlPosX
                sec
                sbc #3
                sta ControlPosX
                jsr GUI_UpdateControl
                jsr GUI_SelectControl3
                inc ControlWidth
                inc ControlWidth
                inc ControlPosX
                ldx #<BtnTextMac
                ldy #>BtnTextMac
                jsr GUI_SetCtrlString
                jsr GUI_SelectControl4
                inc ControlPosX
                jsr GUI_UpdateControl
+               ; Erst Strings setzen, danach Dialog sichtbar machen.
                jsr GeoSetList
                jmp GUI_ShowWndAsDlg
                ;jmp GUI_RepaintCurWindow
BtnTextMac      !pet " Cancel ",0

!zone GeoSetListZone
GeoSetList      jsr GUI_SelectControl0
                lda #0
                sta ControlHilIndex
                ldx #<GeoListStrings
                ldy #>GeoListStrings
                lda GeoPageCount
                jsr GUI_SetCtrlStringList
                jmp GUI_UpdateControl

!zone GeoDlgProcZone
GeoDlgProc      jsr GUI_StdWndProc
                lda wndParam0
                cmp #EC_KEYPRESS
                bne .mouse
                lda actkey
                cmp #$fc
                beq GeoAccept
                rts
.mouse          cmp #EC_DBLCLICK
                bne .release
                jsr GUI_IsInCurControl
                bcc .done
                lda WindowFocCtrl
                beq GeoAccept
.done           rts
.release        cmp #EC_LBTNRELEASE
                bne .done
                jsr GUI_IsInCurControl
                bcc .done
                lda ControlIndex
                cmp #1
                beq GeoPrev
                cmp #2
                beq GeoNextPage
                cmp #3
                beq GeoCancel
                cmp #4
                beq GeoAccept
                rts

!zone GeoPrevZone
GeoPrev         lda GeoPageStart
                beq .done
                sec
                sbc #GEO_PAGE_SIZE
                sta GeoPageStart
                jmp GeoChangePage
.done           rts

!zone GeoNextPageZone
GeoNextPage     lda GeoPageStart
                clc
                adc #GEO_PAGE_SIZE
                cmp GeoCount
                bcs .done
                sta GeoPageStart
                jmp GeoChangePage
.done           rts

!zone GeoChangePageZone
GeoChangePage   jsr GeoMakeList
                jsr GeoSetList
                ; Listbox-Auswahl zuruecksetzen und aktualisieren.
                jsr GUI_SelectControl0
                lda #0
                sta ControlHilIndex
                jsr GUI_UpdateControl
                jmp GUI_CurWndToScreen

!zone GeoAcceptZone
GeoAccept       jsr GUI_SelectControl0
                lda ControlHilIndex
                cmp GeoPageCount
                bcs GeoCancel
                clc
                adc GeoPageStart
                sta GeoSelected
                jsr GUI_KillDialog
                jsr GUI_SelectTopWindow
                lda GeoSelected
                jsr GeoToFound
                jsr TakeFound
                jmp Forecast

!zone GeoCancelZone
GeoCancel       jsr GUI_KillDialog
                jsr GUI_SelectTopWindow
                ldx #28
-               lda GeoOldFoot,x
                sta Str_Foot,x
                dex
                bpl -
                jmp GUI_RepaintCurWindow

!zone GeoSaveFootZone
GeoSaveFoot     ldx #28
-               lda Str_Foot,x
                sta GeoOldFoot,x
                dex
                bpl -
                rts

; Fenster-/Control-Definition fuer das Auswahlfenster.
Str_GeoTitle    !pet "Choose location",0
Wnd_Geo         !byte WT_GEO, 0, 2, 2, 36, 17
                !byte <Str_GeoTitle, >Str_GeoTitle, <GeoDlgProc, >GeoDlgProc
                ;0
DlgList         !byte CT_LISTBOX, 1, 1, 34, 12
                !pet 0
                ;1
DlgPrev         !byte CT_BUTTON, 1, 13, 5, 3
                !pet " < ",0
                ;2
DlgNext         !byte CT_BUTTON, 7, 13, 5, 3
                !pet " > ",0
                ;3
DlgCancel       !byte CT_BUTTON, 18, 13, 8, 3
                !pet "Cancel",0
                ;4
DlgOK           !byte CT_BUTTON, 27, 13, 8, 3
                !pet "  OK  ",0
                !byte 0

GeoPageStart    !byte 0
GeoPageCount    !byte 0
GeoCount        !byte 0
GeoStarted      !byte 0
GeoN            !byte 0
GeoSelected     !byte 0
GeoPrefixMode   !byte 0
GeoShortQuery   !byte 0
GeoMatchIndex   !byte 0
GeoLongest      !byte 0
GeoWidth        !byte 0
GeoNext         !word 0
GeoPtrLo
!for g, 0, GEO_MAX-1 {
                !byte <(GeoRecords+g*GEO_REC_LEN)
                }
GeoPtrHi
!for g, 0, GEO_MAX-1 {
                !byte >(GeoRecords+g*GEO_REC_LEN)
                }
GeoAdmin        !fill 24, 0
GeoAdmin2       !fill 24, 0
GeoOldFoot      !fill 29, 0

;==================================================================
!zone Anzeige
Show            ; Ortszeile: "Name, CC"
                ldx #<Str_Loc
                ldy #>Str_Loc
                lda #23
                jsr AppTo
                ldx #<LocName
                ldy #>LocName
                jsr AppStr
                lda LocCC
                beq +
                ldx #<Txt_Komma
                ldy #>Txt_Komma
                jsr AppStr
                ldx #<LocCC
                ldy #>LocCC
                jsr AppStr
+               ; jetzt: "-2.5°C"
                ldx #<Str_TNow
                ldy #>Str_TNow
                lda #11
                jsr AppTo
                ldx #<CurTemp
                ldy #>CurTemp
                jsr AppStr
                lda #DEG
                jsr Append
                lda #$c3                ; "C" (PETSCII gross)
                jsr Append
                ; Beschreibung und Symbol
                ldx #<CurCode
                ldy #>CurCode
                jsr WmoLookup           ; X = Eintrag
                lda WmoIcon,x
                sta IconOf
                lda WmoTxtLo,x
                pha
                lda WmoTxtHi,x
                pha
                ldx #<Str_Desc
                ldy #>Str_Desc
                lda #18
                jsr AppTo
                pla
                tay
                pla
                tax
                jsr AppStr
                ; "Wind 13 km/h"
                ldx #<Str_Wind
                ldy #>Str_Wind
                lda #14
                jsr AppTo
                ldx #<Txt_Wind
                ldy #>Txt_Wind
                jsr AppStr
                ldx #<CurWind
                ldy #>CurWind
                jsr AppRound
                ldx #<Txt_Kmh
                ldy #>Txt_Kmh
                jsr AppStr
                ; "Humidity 87%"
                ldx #<Str_Hum
                ldy #>Str_Hum
                lda #14
                jsr AppTo
                ldx #<Txt_Hum
                ldy #>Txt_Hum
                jsr AppStr
                ldx #<CurHum
                ldy #>CurHum
                jsr AppStr
                lda #"%"
                jsr Append
                ; drei Tage
                jsr Weekday0
                lda #2
                sta Day
-               jsr ShowDay
                dec Day
                bpl -
                ; Fusszeile: Quelle und Uhrzeit der Daten
                ldx #<Str_Foot
                ldy #>Str_Foot
                lda #29
                jsr AppTo
                ldx #<Txt_Source
                ldy #>Txt_Source
                jsr AppStr
                ldx #<(CurTime+11)
                ldy #>(CurTime+11)
                jsr AppStr
                jmp GUI_RepaintCurWindow

; Wochentag des ersten Tags aus dem Datum (Sakamoto, gilt 2000-2099):
; (jj + jj/4 + T[m] + t) mod 7, 0 = Sonntag. Die Folgetage zaehlen weiter.
Weekday0        ldx #5                  ; "2026-09-23": Monat ab +5
                jsr Two
                sta .m
                ldx #8                  ; Tag ab +8
                jsr Two
                sta .t
                ldx #2                  ; Jahr ab +2
                jsr Two
                ldy .m
                cpy #3
                bcs +
                sbc #0                  ; C=0: Januar/Februar zaehlen zum Vorjahr
+               sta .t+1
                lsr
                lsr
                clc
                adc .t+1
                adc SakT-1,y
                adc .t
                sta Wd
                rts
.m              !byte 0
.t              !byte 0, 0

; Tag Nr. Day: "Wed 23", Symbol, "19°/3°"
ShowDay         ldx Day
                lda DDateOfs,x
                sta .x
                txa
                clc
                adc Wd
-               cmp #7
                bcc +
                sbc #7
                bcs -
+               sta .wt
                asl
                adc .wt                 ; *3
                sta .wt
                ; "Wed 23"
                lda DayLblLo,x
                ldy DayLblHi,x
                tax
                lda #8
                jsr AppTo
                lda #3
                sta .n
-               ldx .wt
                lda DayNames,x
                jsr AppAscii
                inc .wt
                dec .n
                bne -
                lda #" "
                jsr Append
                ldx .x
                lda DDate+8,x
                cmp #"0"
                beq +
                jsr Append
                ldx .x
+               lda DDate+9,x
                jsr Append
                ; Symbol
                lda Day
                asl
                asl                     ; *4 = Groesse von DCode
                adc #<DCode
                tax
                lda #>DCode
                adc #0
                tay
                jsr WmoLookup
                lda WmoIcon,x
                ldx Day
                sta IconOf+C_ICON_D0-C_ICON_NOW,x
                ; "19°/3°"
                lda DayTmpLo,x
                ldy DayTmpHi,x
                tax
                lda #8
                jsr AppTo
                lda Day
                asl
                asl
                asl                     ; *8 = Groesse von DMax/DMin
                sta .t8
                adc #<DMax
                tax
                lda #>DMax
                adc #0
                tay
                jsr AppRound
                lda #DEG
                jsr Append
                lda #"/"
                jsr Append
                lda .t8
                clc
                adc #<DMin
                tax
                lda #>DMin
                adc #0
                tay
                jsr AppRound
                lda #DEG
                jmp Append
.x              !byte 0
.wt             !byte 0
.t8             !byte 0
.n              !byte 0
DDateOfs        !byte 0, 11, 22
DayLblLo        !byte <Str_Day0, <Str_Day1, <Str_Day2
DayLblHi        !byte >Str_Day0, >Str_Day1, >Str_Day2
DayTmpLo        !byte <Str_TDay0, <Str_TDay1, <Str_TDay2
DayTmpHi        !byte >Str_TDay0, >Str_TDay1, >Str_TDay2

; zwei ASCII-Ziffern ab DDate+X -> A
Two             lda DDate,x
                and #$0f
                sta .z
                asl
                asl
                adc .z
                asl
                sta .z
                lda DDate+1,x
                and #$0f
                adc .z
                rts
.z              !byte 0

!zone Texte
; Text von X/Y (ASCII) anhaengen, als PETSCII
AppStr          stx .lies+1
                sty .lies+2
.lies           lda $ffff
                beq +
                jsr AppAscii
                inc .lies+1
                bne .lies
                inc .lies+2
                bne .lies
+               rts

; ein ASCII-Zeichen als PETSCII anhaengen
AppAscii        cmp #$61
                bcc +
                cmp #$7b
                bcs ++
                sbc #$1f                ; a-z -> $41-$5a (C=0: -$20)
                jmp Append
+               cmp #$41
                bcc ++
                cmp #$5b
                bcs ++
                ora #$80                ; A-Z -> $c1-$da
++              jmp Append

; Dezimalzahl (ASCII, z.B. "-2.5") gerundet anhaengen
AppRound        stx TabPtr
                sty TabPtr+1
                jsr Number              ; .wert, Y hinter den Ziffern
                lda (TabPtr),y
                cmp #"."
                bne +
                iny
                lda (TabPtr),y
                cmp #"5"
                bcc +
                inc NumVal
+               lda NumVal
                beq +                   ; keine "-0"
                lda NumNeg
                beq +
                lda #"-"
                jsr Append
+               lda NumVal
                ldx #$2f                ; "0"-1
                sec
-               inx
                sbc #10
                bcs -
                adc #$3a                ; "0"+10
                pha
                txa
                cmp #"0"
                beq +
                jsr Append
+               pla
                jmp Append

; Ganzzahl ab (TabPtr) lesen: NumVal, NumNeg; Y = erstes Nicht-Ziffer
Number          ldy #0
                sty NumNeg
                sty NumVal
                lda (TabPtr),y
                cmp #"-"
                bne +
                inc NumNeg
                iny
+
-               lda (TabPtr),y
                cmp #"0"
                bcc +
                cmp #":"
                bcs +
                and #$0f
                pha
                lda NumVal
                asl
                asl
                adc NumVal
                asl
                sta NumVal
                pla
                adc NumVal
                sta NumVal
                iny
                bne -
+               rts
NumVal          !byte 0
NumNeg          !byte 0

; X/Y = WMO-Code als Text -> X = Eintrag in den Wmo-Tabellen
WmoLookup       stx TabPtr
                sty TabPtr+1
                jsr Number
                ldx #0
-               lda WmoMax,x
                cmp NumVal
                bcs +
                inx
                cpx #WMO_ANZ-1
                bcc -
+               rts

; Fusszeile / Ortszeile setzen (X/Y = PETSCII-Text)
SetFoot         lda #<Str_Foot
                sta DstPtr
                lda #>Str_Foot
                bne +
SetLoc          lda #<Str_Loc
                sta DstPtr
                lda #>Str_Loc
+               sta DstPtr+1
                stx TabPtr
                sty TabPtr+1
                ldy #$ff
-               iny
                lda (TabPtr),y
                sta (DstPtr),y
                bne -
                rts

;==================================================================
!zone Steuerelemente
CtrlAction      rts

; X = Typ; ($fd) = Bildpuffer, ($02) = Farbpuffer der Steuerelement-Ecke
PaintCtrls      cpx #CT_ICON
                beq PaintIcon
                cpx #CT_TEMP
                beq PaintTemp
                rts

; Symbol aus IconOf[ControlIndex] - ohne GUI_SelectControl, damit es
; auch fuer Steuerelemente ab Nummer 16 geht
PaintIcon       jsr GUI_GetCSTMWindowColor
                sta IconCol
                ldx ControlIndex
                lda IconOf-C_ICON_NOW,x
                tax
                ldy #0
                lda IconTL,x
                jsr .zelle
                lda IconTL,x
                beq +
                clc
                adc #1
+               jsr .zelle
                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                ldy #0
                lda IconBL,x
                jsr .zelle
                lda IconBL,x
                beq .zelle
                clc
                adc #1
.zelle          bne Zelle
                lda #SPACE_SCR          ; 0 = leer
; Zeichen A an Stelle Y, in Fensterfarbe; Y weiter (Z=0)
Zelle           sta ($fd),y
                lda IconCol
                sta ($02),y
                iny
                rts

; Text (PETSCII) aus dem Steuerelement-String, DEG wird zum Gradzeichen;
; der Rest der Breite wird geloescht
PaintTemp       jsr GUI_GetCSTMWindowColor
                sta IconCol
                ldy #0
-               lda (ControlStrings),y
                beq +
                jsr PetToScr
                jsr Zelle
                cpy ControlWidth
                bcc -
                rts
+
-               cpy ControlWidth
                bcs +
                lda #SPACE_SCR
                jsr Zelle
                bne -
+               rts
IconCol         !byte 0

; PETSCII -> Bildschirmcode, wie GUI64 es fuer Labels tut, plus DEG
PetToScr        cmp #DEG
                bne +
                lda #CH_GRAD
                rts
+               cmp #$20
                bcc .leer
                cmp #$5b
                bcs +
                ora #$80
                rts
+               cmp #$c0
                bcc .leer
                cmp #$db
                bcs .leer
                sbc #$3f                ; C=0 hier: -$40
                rts
.leer           lda #SPACE_SCR
                rts

;==================================================================
!zone Uhr
; GUI64 ruft das zehnmal je Sekunde; alle 15 Minuten neu holen
TimerHandler    lda Ticks
                bne +
                dec Ticks+1
+               dec Ticks
                lda Ticks
                ora Ticks+1
                bne .raus
                lda #<9000
                sta Ticks
                lda #>9000
                sta Ticks+1
                lda #WT_WETTER          ; Fenster noch offen?
                sta Param0
                jsr GUI_FindWndByType
                bcs +
                jmp GUI_StopTimer
+               jmp Forecast
.raus           rts

!ifdef VICETEST {
;------------------------------------------------------------------
; Beispieldaten durch dieselben Auswerter schicken
FakeData        lda #<IpTable
                ldx #>IpTable
                jsr CsvStart
                ldx #<Fake_Ip
                ldy #>Fake_Ip
                jsr .fuettern
                jsr TakeFound
                lda #<FcTable
                ldx #>FcTable
                jsr CsvStart
                ldx #<Fake_Fc
                ldy #>Fake_Fc
                jsr .fuettern
                jmp Show
.fuettern       stx .lies+1             ; selbstaendernd: TabPtr und DstPtr
                sty .lies+2             ; braucht CsvEnd selbst
.lies           lda $ffff
                beq +
                jsr CsvByte
                inc .lies+1
                bne .lies
                inc .lies+2
                bne .lies
+               jmp CsvEnd
Fake_Ip         !text "success,DE,M", $c3, $bc, "nchen,48.1428,11.5801", 0
Fake_Fc         !text "latitude,longitude,elevation", $0a, "48.14,11.58,520.0", $0a, $0a
                !text "time,temperature_2m", $0a
                !text "2026-09-23T22:15,-2.5,61,12.6,87", $0a, $0a
                !text "time,weather_code", $0a
                !text "2026-09-23,2,18.5,3.6", $0a
                !text "2026-09-24,71,-0.4,-7.5", $0a
                !text "2026-09-25,0,19.6,9.1", $0a, 0
}

;==================================================================
!zone Daten
NetBackend      !byte NET_NONE
WiCProbeActive  !byte 0
AppLen          !byte 0
AppMax          !byte 0
UrlErr          !byte 0
WantSink        !word SinkDiscard
Ticks           !word 9000
Day             !byte 0
Wd              !byte 0
CsvTab          !word 0
CsvLine         !byte 0
CsvField        !byte 0
JsonCh          !byte 0
JsonCap         !byte 0
JsonTerm        !byte 0
JsonKey         !byte $ff
JsonPos         !byte 0

; CSV-Tabellen: Zeile*8+Feld, Ziel, Groesse (mit Null); $ff = Ende.
; ip-api liefert die Felder immer in dieser Reihenfolge, egal wie gefragt
IpTable         !byte 0, <FoundSt, >FoundSt, 2
                !byte 1, <FoundCC, >FoundCC, 3
                !byte 2, <FoundName, >FoundName, 25
                !byte 3, <FoundLat, >FoundLat, 12
                !byte 4, <FoundLon, >FoundLon, 12
                !byte $ff
; Open-Meteo-CSV: Zeile 4 = jetzt, Zeilen 7-9 = drei Tage
FcTable         !byte 4*8+0, <CurTime, >CurTime, 17
                !byte 4*8+1, <CurTemp, >CurTemp, 8
                !byte 4*8+2, <CurCode, >CurCode, 4
                !byte 4*8+3, <CurWind, >CurWind, 8
                !byte 4*8+4, <CurHum, >CurHum, 5
                !for t, 0, 2 {
                !byte (7+t)*8+0, <(DDate+t*11), >(DDate+t*11), 11
                !byte (7+t)*8+1, <(DCode+t*4), >(DCode+t*4), 4
                !byte (7+t)*8+2, <(DMax+t*8), >(DMax+t*8), 8
                !byte (7+t)*8+3, <(DMin+t*8), >(DMin+t*8), 8
                }
                !byte $ff

; JSON-Schluessel der Ortssuche
PatText         !text $22, "name", $22, ":", $22
PAT1 = * - PatText
                !text $22, "latitude", $22, ":"
PAT2 = * - PatText
                !text $22, "longitude", $22, ":"
PAT3 = * - PatText
                !text $22, "country_code", $22, ":", $22
PAT4 = * - PatText
                !text $22, "admin1", $22, ":", $22
PAT5 = * - PatText
                !text $22, "admin2", $22, ":", $22
PAT6 = * - PatText
                !text $22, "id", $22, ":"
PAT7 = * - PatText
PatStart        !byte 0, PAT1, PAT2, PAT3, PAT4, PAT5, PAT6
PatLen          !byte PAT1, PAT2-PAT1, PAT3-PAT2, PAT4-PAT3, PAT5-PAT4, PAT6-PAT5, PAT7-PAT6
PatDst          !byte <FoundName, <FoundLat, <FoundLon, <FoundCC, <GeoAdmin, <GeoAdmin2
PatDstHi        !byte >FoundName, >FoundLat, >FoundLon, >FoundCC, >GeoAdmin, >GeoAdmin2
PatMax          !byte 25, 12, 12, 3, 24, 24
PatTerm         !byte $22, ",", ",", $22, $22, $22

; UTF-8 (zweites Byte nach $c3) -> ASCII; die ersten sechs bekommen ein e
Utf8In          !byte $a4, $b6, $bc, $84, $96, $9c, $9f, $a9, $a8, $a0, $a1, $a7, $b3
UTF8_ANZ        = * - Utf8In
Utf8Out         !text "aouAOUseeaaco"

; Wochentage (Sakamoto: 0 = Sonntag) und Monatstabelle
DayNames        !text "SunMonTueWedThuFriSat"
SakT            !byte 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4

; WMO-Wettercodes: hoechster Code des Eintrags, Symbol, Text
WmoMax          !byte 0, 1, 2, 3, 48, 57, 61, 63, 65, 67, 77, 82, 86, 99
WMO_ANZ         = * - WmoMax
WmoIcon         !byte IC_SUN, IC_PARTLY, IC_PARTLY, IC_CLOUD, IC_CLOUD, IC_RAIN, IC_RAIN
                !byte IC_RAIN, IC_RAIN, IC_RAIN, IC_SNOW, IC_RAIN, IC_SNOW, IC_RAIN
WmoTxtLo        !byte <W0, <W1, <W2, <W3, <W4, <W5, <W6, <W7, <W8, <W9, <W10, <W11, <W12, <W13
WmoTxtHi        !byte >W0, >W1, >W2, >W3, >W4, >W5, >W6, >W7, >W8, >W9, >W10, >W11, >W12, >W13
W0              !text "Clear sky", 0
W1              !text "Mainly clear", 0
W2              !text "Partly cloudy", 0
W3              !text "Overcast", 0
W4              !text "Fog", 0
W5              !text "Drizzle", 0
W6              !text "Light rain", 0
W7              !text "Rain", 0
W8              !text "Heavy rain", 0
W9              !text "Freezing rain", 0
W10             !text "Snow", 0
W11             !text "Rain showers", 0
W12             !text "Snow showers", 0
W13             !text "Thunderstorm", 0

; Symbole: Zeichen oben links und unten links (rechts = +1), 0 = leer
IconTL          !byte 0, CH_SONNE_O, CH_SONNE_O, CH_WOLKE_O, CH_WOLKE_O, CH_WOLKE_O
IconBL          !byte 0, CH_SONNE_U, CH_TEILS_U, CH_WOLKE_U, CH_REGEN_U, CH_SCHNEE_U
IconOf          !fill 11, 0             ; je Steuerelement-Nummer ab 6

; URLs (ASCII)
; fields=16594: status+countryCode+city+lat+lon als Bitmaske (ip-api)
Url_Ip          !text "http://ip-api.com/csv/?fields=16594", 0
Url_Geo         !text "https://geocoding-api.open-meteo.com/v1/search?count=50&language=de&name=", 0
Url_Geo20       !text "https://geocoding-api.open-meteo.com/v1/search?count=20&language=de&name=", 0
Url_Fc1         !text "https://api.open-meteo.com/v1/forecast?latitude=", 0
Url_Fc2         !text "&longitude=", 0
Url_Fc3         !text "&current=temperature_2m,weather_code,wind_speed_10m,relative_humidity_2m"
                !text "&daily=weather_code,temperature_2m_max,temperature_2m_min"
                !text "&timezone=auto&forecast_days=3&format=csv", 0

; Texte (ASCII, werden beim Anhaengen zu PETSCII)
Txt_Komma       !text ", ", 0
Txt_Wind        !text "Wind ", 0
Txt_Kmh         !text " km/h", 0
Txt_Hum         !text "Humidity ", 0
Txt_Source      !text "Open-Meteo.com         ", 0

; Texte (PETSCII)
Str_Loading     !pet "Loading...", 0
Str_NetErr      !pet "No network", 0
Str_NotFound    !pet "Not found", 0
Str_NoPlace     !pet "Unknown place", 0
Str_NoNet1      !pet "No network interface", 0
Str_NoNet2      !pet "Connect Ultimate or WiC64", 0
Str_About       !pet "Weather 1.0", NL, NL, "With data from", NL, "open-meteo.com", 0
Str_Menubar     !pet "Weather",0,"?",0
MenuBar         !word Menu_Wtr, Menu_Help
Menu_Wtr        !pet ID_MENU_WTR, 9, 3, "Refresh",0,"Locate",0,"Quit",0
Menu_Help       !pet ID_MENU_WTR+1, 5, 1, "About",0
Str_Edit        !pet "                ", 0      ; 16 Zeichen, von GUI64 bearbeitet

; Weather icons
; generated by werkzeug/wetter_zeichen.py, do not change by hand
; Every icon is 2x2 chars
CH_SONNE_O  = APP_CHAR_0 + 0
CH_SONNE_U  = APP_CHAR_0 + 2
CH_WOLKE_O  = APP_CHAR_0 + 4
CH_WOLKE_U  = APP_CHAR_0 + 6
CH_TEILS_U  = APP_CHAR_0 + 8
CH_REGEN_U  = APP_CHAR_0 + 10
CH_SCHNEE_U = APP_CHAR_0 + 12
CH_GRAD     = APP_CHAR_0 + 14
CHAR_COUNT  = 15

CharList        !byte $ff,$fe,$de,$ef,$fc,$fb,$f7,$97   ; SONNE_O links
                !byte $ff,$7f,$7b,$f7,$3f,$df,$ef,$e9   ; SONNE_O rechts
                !byte $97,$f7,$fb,$fc,$ef,$de,$fe,$ff   ; SONNE_U links
                !byte $e9,$ef,$df,$3f,$f7,$7b,$7f,$ff   ; SONNE_U rechts
                !byte $ff,$ff,$ff,$ff,$ff,$f8,$f7,$ef   ; WOLKE_O links
                !byte $ff,$ff,$ff,$ff,$ff,$7f,$bf,$c7   ; WOLKE_O rechts
                !byte $cf,$bf,$bf,$c0,$ff,$ff,$ff,$ff   ; WOLKE_U links
                !byte $fb,$fd,$fd,$03,$ff,$ff,$ff,$ff   ; WOLKE_U rechts
                !byte $97,$f7,$fa,$fd,$eb,$db,$fc,$ff   ; TEILS_U links
                !byte $e9,$1f,$ef,$f1,$fe,$fe,$01,$ff   ; TEILS_U rechts
                !byte $cf,$bf,$bf,$c0,$f7,$ee,$dd,$ff   ; REGEN_U links
                !byte $fb,$fd,$fd,$03,$77,$ef,$df,$ff   ; REGEN_U rechts
                !byte $cf,$bf,$bf,$c0,$eb,$f7,$eb,$ff   ; SCHNEE_U links
                !byte $fb,$fd,$fd,$03,$af,$df,$af,$ff   ; SCHNEE_U rechts
                !byte $c7,$d7,$c7,$ff,$ff,$ff,$ff,$ff   ; Grad

;==================================================================
; Fensterdefinition. Spalte 0 ist der Fensterrand - Inhalt ab 1.
; Veraenderliche Texte stehen hier als Platzhalter (ohne Null am
; Anfang - GUI64 sucht das Ende des Texts, um das naechste Element zu
; finden) und werden an Ort und Stelle ueberschrieben.
;==================================================================
Str_Title       !pet "Weather",0
; Hoehe 21: WIN = Titel + Menue + 18 Zeilen + Rand; MAC eine weniger
Wnd_Main        !byte WT_WETTER, %00100001, 5, 0, 30, 22
                !byte <Str_Title, >Str_Title, <WndProc, >WndProc
                ; 0 Menueleiste
                !byte CT_MENUBAR, <MenuBar, >MenuBar, 0, 0
                !pet 0
                ; 1 Rahmen "Now"
                !byte CT_FRAME, 1, 5, 28, 6
                !pet "Now",0
                ; 2 Rahmen "Forecast"
                !byte CT_FRAME, 1, 12, 28, 6
                !pet "Forecast",0
                ; 3 Suchfeld
                !byte CT_EDIT_SL, 1, 0, 19, 3
                !pet 0
                ; 4 Button (Text = Breite - 2)
                !byte CT_BUTTON, 20, 0, 10, 3
                !pet " Search ",0
                ; 5 Ort
                !byte CT_LABEL, 1, 3, 22, 1
Str_Loc         !pet "Locating..."
                !fill 11, $20
                !byte 0
                ; 6 Symbol jetzt, 7 Temperatur
                !byte CT_ICON, 3, 7, 2, 2
                !pet 0
                !byte CT_TEMP, 7, 6, 10, 1
Str_TNow        !fill 10, $20
                !byte 0
                ; 8 Beschreibung
                !byte CT_LABEL, 7, 7, 17, 1
Str_Desc        !fill 17, $20
                !byte 0
                ; 9 Wind
                !byte CT_LABEL, 7, 8, 13, 1
Str_Wind        !fill 13, $20
                !byte 0
                ; 10 Luftfeuchte
                !byte CT_LABEL, 7, 9, 13, 1
Str_Hum         !fill 13, $20
                !byte 0
                ; 11 Tag 1
                !byte CT_LABEL, 3, 13, 7, 1
Str_Day0        !fill 7, $20
                !byte 0
                ; 12 Tag 2
                !byte CT_LABEL, 12, 13, 7, 1
Str_Day1        !fill 7, $20
                !byte 0
                ; 13 Tag 3
                !byte CT_LABEL, 21, 13, 7, 1
Str_Day2        !fill 7, $20
                !byte 0
                ; 14 Symbole Tag 1
                !byte CT_ICON, 4, 14, 2, 2
                !pet 0
                ; 15 Symbole Tag 2
                !byte CT_ICON, 13, 14, 2, 2
                !pet 0
                ; 16 Symbole Tag 2
                !byte CT_ICON, 22, 14, 2, 2
                !pet 0
                ; 17 Temperaturen Tag 1
                !byte CT_TEMP, 3, 16, 7, 1
Str_TDay0       !fill 7, $20
                !byte 0
                ; 18 Temperaturen Tag 2
                !byte CT_TEMP, 12, 16, 7, 1
Str_TDay1       !fill 7, $20
                !byte 0
                ; 19 Temperaturen Tag 3
                !byte CT_TEMP, 21, 16, 7, 1
Str_TDay2       !fill 7, $20
                !byte 0
                ; 20 Fusszeile
                !byte CT_LABEL, 1, 18, 28, 1
Str_Foot        !fill 28, $20
                !byte 0
                !byte 0

!zone UciDriver
; Ultimate-UCI-Treiber aus der urspruenglichen Weather-App.
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
UciRun          lda #0
                sta UciBlocks            ; Diagnose: Anzahl fertig gelesener Bloecke
                lda #1
                sta UciStage             ; 1: Warte auf ersten/naechsten Block
                lda #UCI_PUSH
                sta UCI_CTRL
.next           jsr .waitForData         ; Nur ST_LAST/ST_MORE sind gueltige Antworten
                bcs .fail
                lda #2
                sta UciStage             ; 2: Daten/Status lesen
                ; Eine UCI-Antwortwarteschlange fasst maximal $380
                ; (=896) Datenbytes. Ist sie ganz voll, kann DATA_AV
                ; gesetzt bleiben. Deshalb nicht nur auf Bit 7 warten.
                ; Zaehler beginnt bei -896 und laeuft bis 0 hoch.
                lda #$80
                sta UciBlockLeft
                lda #$fc
                sta UciBlockLeft+1
                ; Daten abholen
-               lda UCI_CTRL
                bpl +                   ; Bit 7: keine Daten mehr
                lda UCI_RESP
!ifdef UCI_SINK {
                jsr UciSink
} else {
                ldx UciRespLen
                cpx #UCI_RESP_MAX
                bcs .byteRead           ; Puffer voll: Daten verwerfen
                sta UciResp,x
                inc UciRespLen
}
.byteRead       inc UciBlockLeft
                bne -
                inc UciBlockLeft+1
                bne -
                ; Genau 896 Datenbytes: Block auch bei DATA_AV=1
                ; abschliessen und danach wie ueblich quittieren.
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
                sta UciBlockState
                inc UciBlocks
                lda #3
                sta UciStage             ; 3: Block bestaetigen
                lda #UCI_ACCEPT
                sta UCI_CTRL
                ; Nicht bloss auf ST_MORE zu pruefen: die Bestaetigung
                ; ist erst verarbeitet, wenn DATA_ACC (Bit 1) geloescht ist.
                jsr .waitAck
                bcs .fail
                lda UciBlockState
                cmp #ST_MORE
                beq .next
                ldx UciStatLen          ; Status mit Null abschliessen
                lda #0
                sta UciStat,x
                sta UciStage
                clc
                rts
.fail           lda #$ff
                sta UciStage
                lda #UCI_ABORT
                sta UCI_CTRL
                jsr .waitIdle
                sec
                rts

; Nach PUSH oder einem bestaetigten Zwischenblock erst weiterarbeiten,
; wenn der UCI-Zustand tatsaechlich ST_LAST oder ST_MORE erreicht.
; ST_IDLE ist noch keine Antwort! Zeitgrenze wie bei .waitNotBusy.
.waitForData    lda #UCI_TIMEOUT
                sta UciFrames
                lda $d011
                and #$80
                sta UciLastMsb
.waitReply      lda UCI_CTRL
                and #$30
                cmp #ST_LAST
                beq .received
                cmp #ST_MORE
                beq .received
                jsr .tick
                bne .waitReply
                sec
                rts
.received       clc
                rts

; DATA_ACC bleibt gesetzt, bis Ultimate die Bestaetigung angenommen hat.
.waitAck        lda #50
                ldx #$02
                ldy #$02
                jmp .wait

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
; Bei einer Blockade im Monitor ansehen: UciStage = 1 (warten),
; 2 (lesen), 3 (Bestaetigung), $ff (Fehler), 0 (fertig).
UciStage        !byte 0
UciBlocks       !byte 0
UciBlockLeft    !word 0        ; Diagnose: Zaehler -896..0 je Block
UciBlockState   !byte 0
UciFrames       !byte 0
UciLastMsb      !byte 0
UciCmdLen       !byte 0
UciRespLen      !byte 0
UciStatLen      !byte 0
UciCmd          !fill UCI_CMD_MAX, 0
UciResp         !fill UCI_RESP_MAX, 0
UciStat         !fill UCI_STAT_MAX+1, 0

;==================================================================
; Puffer ohne Inhalt - stehen nicht in der Datei, nur im Speicher
;==================================================================
!zone Puffer
FOUND_LEN       = 52
Loc                                     ; der angezeigte Ort
LocName         = Loc
LocCC           = Loc+25
LocLat          = Loc+28
LocLon          = Loc+40
FoundSt         = Loc+FOUND_LEN         ; 2
CsvBuf          = FoundSt+2             ; 20
CurTime         = CsvBuf+20             ; 17
CurTemp         = CurTime+17            ; 8
CurCode         = CurTemp+8             ; 4
CurWind         = CurCode+4             ; 8
CurHum          = CurWind+8             ; 5
DDate           = CurHum+5              ; 3 x 11
DCode           = DDate+33              ; 3 x 4
DMax            = DCode+12              ; 3 x 8
DMin            = DMax+24               ; 3 x 8
PUFFER_ENDE     = DMin+24
GeoRecords      = PUFFER_ENDE
GeoListStrings  = GeoRecords+GEO_MAX*GEO_REC_LEN
; Ueberlagert mit dem Listbox-Stringpuffer: waehrend HTTP ist der
; Auswahldialog geschlossen; GeoMakeList baut ihn danach neu auf.
UrlBuf          = GeoListStrings
GEO_ENDE        = GeoListStrings+GEO_PAGE_SIZE*GEO_LABEL_MAX
; Treffer und Suchtext braucht man nur, bis der Ort feststeht - danach
; kommt die Vorhersage. Beide teilen sich deshalb denselben Platz.
Found           = DDate                 ; gleicher Aufbau wie Loc
FoundName       = Found
FoundCC         = Found+25
FoundLat        = Found+28
FoundLon        = Found+40
Typed           = Found+FOUND_LEN       ; 17
Collapsed       = Typed+17              ; 17
Coll = Collapsed+17
!if Coll > PUFFER_ENDE {
                !error "Suchpuffer groesser als die Vorhersage"
}
; GUI64 reserviert fuer Apps $b000-$cfff. Der Datenpuffer darf daher
; ueber $c000 hinausreichen, muss aber vor $d000 enden.
!if GEO_ENDE > $d000 {
                !error "Weather reicht ueber den App-Speicher: ", GEO_ENDE
}
