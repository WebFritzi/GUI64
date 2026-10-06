;------------------------------------------------------------------
; Weather - eine Wetter-App fuer GUI64 auf der Ultimate
;
; Holt das Wetter ueber das HTTP-Ziel der Ultimate (Firmware 3.15):
; kein WiC64, keine Zusatzhardware. Der Ort kommt beim Start aus der
; IP-Adresse (ip-api.com), laesst sich per Suche aendern (Open-Meteo
; Geocoding), das Wetter kommt von Open-Meteo als CSV.
;
; Warum CSV und nicht JSON: das HTTP-Ziel kann JSON selbst zerlegen,
; wandelt Zahlen aber mit strtol um - aus 12.9 Grad wuerden 12. Die
; CSV-Antwort (400 Byte) wird hier Byte fuer Byte ausgewertet, waehrend
; sie hereinkommt; einen Puffer dafuer gibt es nicht. Auch die bis zu
; 242 Byte lange URL geht Zeichen fuer Zeichen direkt an die Ultimate.
;
;   acme -f cbm -o weather.gui wetter.asm
;   acme -f cbm -DVICETEST=1 -o weather_vice.gui wetter.asm
;
; VICETEST: ohne Ultimate werden mitgelieferte Beispieldaten durch
; dieselben Auswerter geschickt - so laesst sich die Oberflaeche in
; VICE pruefen.
;------------------------------------------------------------------
!to "Weather.d64",d64,"weather.gui","weather disk"

!source "../gui64.inc.asm"

UCI_SINK        = 1             ; Datenbytes gehen an UciSink
UCI_CMD_MAX     = 4             ; gepuffert wird nur der Austausch-Befehl
UCI_RESP_MAX    = 2             ; nur die Nummer eines neuen Kopfes
UCI_STAT_MAX    = 12            ; "HTTP/1.1 200" - bis zur Zahl reicht

!zone Konstanten
WT_WETTER       = 78
CT_ICON         = 50            ; eigenes Steuerelement: Symbol 2x2
CT_TEMP         = 51            ; eigenes Steuerelement: Text mit Gradzeichen
ID_MENU_WTR     = 10
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
                
                ldy #C_EDIT
                lda #BIT_CTRL_DBLFRAME_TOP
                jsr GUI_SelectCtrl_AddBits
                ldx #<Str_Edit
                ldy #>Str_Edit
                jsr GUI_SetCtrlString
                lda #0
                ldx #16
                jsr GUI_SetEditSLInfo
                
                ldy #4
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_RGT)
                jsr GUI_SelectCtrl_AddBits
                jsr GUI_GetDesign
                bne ++
                ldx #<BtnTextWin
                ldy #>BtnTextWin
                jsr GUI_SetCtrlString
                lda #8
                sta ControlWidth
                inc ControlPosX
                jsr GUI_UpdateControl
++              ; GUI64 2.1 laesst nach einem langen Ladevorgang "Fenster
                ; wird verschoben" ($85) stehen und schluckt damit das erste
                ; Loslassen der Maustaste (wie in ucontrol.asm)
                lda #0
                sta $85
                jsr GUI_StartTimer
                jsr UciUnlock
                beq .mitUci
!ifdef VICETEST {
                jmp FakeData
} else {
                ldx #<Str_NoUci1
                ldy #>Str_NoUci1
                jsr SetLoc
                ldx #<Str_NoUci2
                ldy #>Str_NoUci2
                jmp Fail
}
.mitUci         inc HasUci
                jmp Locate

BtnTextWin      !pet "Search",0
;==================================================================
!zone Fensterprozedur
WndProc         lda actkey              ; vor GUI_StdWndProc: das Eingabefeld
                sta KeyIn               ; verbraucht die Taste
!ifdef SPUR {
                ldx $02bf               ; Mitschrift: Ereignis, Taste davor
                lda wndParam0
                sta $02c0,x
                lda actkey
                sta $02c1,x
                inx
                inx
                txa
                and #$1f
                sta $02bf
}
                jsr GUI_StdWndProc
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
                lda KeyIn
                cmp #$fc                ; GUI64s Code fuer RETURN (gemessen)
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
Search          lda HasUci
                beq .raus
                jsr ReadEdit            ; -> Typed, Z=1: leer
                beq .raus
                jsr Busy
                jsr Collapse            ; -> Collapsed, C=1: etwas entfernt
                bcc .wieGetippt
                ldx #<Collapsed
                ldy #>Collapsed
                jsr GeoQuery
                bcs .fehler
                lda FoundLat
                beq .wieGetippt
                jsr NameIsTyped
                beq .nehmen
.wieGetippt     ldx #<Typed
                ldy #>Typed
                jsr GeoQuery
                bcs .fehler
                lda FoundLat
                beq .nichtda
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
ReadEdit        ldx #0
                ldy #0
-               lda Str_Edit,x
                beq .ende
                and #$7f
                cmp #$41                ; Buchstabe?
                bcc +
                cmp #$5b
                bcs .weg
                ora #$20                ; -> a-z
                bne .rein
+               cmp #$20
                bne +
                cpy #0                  ; Leerzeichen am Anfang weg
                beq .weg
                bne .rein
+               cmp #"-"
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
                jsr HttpFree
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
                jsr HttpFree
                ldx #<Url_Geo
                ldy #>Url_Geo
                jsr UrlBegin
                ldx .name
                ldy .name+1
                jsr UrlAdd              ; Leerzeichen werden dort zu %20
                lda #0
                sta JsonCap
                ldx #FOUND_LEN-1
-               sta Found,x
                dex
                bpl -
                ldx #3
-               sta JsonM,x
                dex
                bpl -
                lda #<JsonByte
                ldx #>JsonByte
                jsr SetWant
                jmp HttpGet
.name           !word 0

!zone
; Wetter holen und anzeigen
Forecast        lda HasUci
                beq rauss
                jsr Busy
                jsr HttpFree
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
; HTTP ueber das UCI-Ziel $06
;------------------------------------------------------------------
; Alle Kopf- und Rumpf-Plaetze der Ultimate freigeben (die ueberleben
; sogar einen C64-Reset)
HttpFree        lda #$06
                sta UciCmd
                lda #$10
                sta UciCmd+1
                lda #2
                sta UciCmdLen
                jmp UciExec

!zone
; Kopf anlegen: $06 $11 $01 <url>, direkt in die Befehlswarteschlange
UrlBegin        stx .text
                sty .text+1
                jsr UciStart
                ror UrlErr              ; C=1: Schnittstelle nicht bereit
                lda #$06
                sta UCI_CMD
                lda #$11
                sta UCI_CMD
                lda #$01                ; GET
                sta UCI_CMD
                ldx .text
                ldy .text+1
                ; faellt durch
; X/Y = ASCII-Text anhaengen (Leerzeichen als %20)
UrlAdd          stx TabPtr
                sty TabPtr+1
                ldy #0
-               lda (TabPtr),y
                beq .raus
                cmp #$20
                bne +
                lda #"%"
                sta UCI_CMD
                lda #"2"
                sta UCI_CMD
                lda #"0"
+               sta UCI_CMD
                iny
                bne -
.raus           rts
.text           !word 0

!zone
; Kopf abschicken, dann roh austauschen; Antwortbytes an WantSink.
; C=1: Fehler (kein "2xx")
HttpGet         jsr SinkReset           ; der Kopf antwortet mit seiner Nummer
                bit UrlErr
                bmi .fehlt
                jsr UciRun
                bcs .fehlt
                lda UciRespLen
                beq .fehlt
                lda UciResp             ; Kopfnummer
                sta UciCmd+2
                lda #$06                ; DO_EXCHANGE_RAW <kopf> <kein rumpf>
                sta UciCmd
                lda #$32
                sta UciCmd+1
                lda #$ff
                sta UciCmd+3
                lda #4
                sta UciCmdLen
                lda WantSink
                ldx WantSink+1
                jsr SinkSet
                jsr UciExec
                php
                jsr CsvEnd              ; letztes Feld ohne Zeilenende
                jsr SinkReset
                plp
                bcs .fehlt
                ; Status = Antwortkopf "HTTP/1.1 200 OK ..."
                lda UciStat+9
                cmp #"2"
                bne .fehlt
                clc
                rts
.fehlt          sec
                rts

SinkReset       lda #<SinkStore
                ldx #>SinkStore
; Senke setzen (A/X = Routine)
SinkSet         sta UciSink+1
                stx UciSink+2
                rts

; Die Senke, die uci.asm aufruft (Ziel wird umgebogen)
UciSink         jmp SinkStore

SinkStore       ldx UciRespLen
                cpx #UCI_RESP_MAX
                bcs +
                sta UciResp,x
                inc UciRespLen
+               rts

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
; JSON (Ortssuche): vier Schluessel suchen, den Wert dahinter einsammeln
JsonByte        ldx JsonCap
                beq .suchen
                cmp JsonTerm            ; Ende des Werts?
                beq .ende
                cmp #"}"
                beq .ende
                jmp Utf8Put
.ende           lda #0
                sta JsonCap
                rts
.suchen         sta JsonCh
                ldx #3
.muster         lda JsonM,x
                clc
                adc PatStart,x
                tay
                lda PatText,y
                cmp JsonCh
                bne .anders
                inc JsonM,x
                lda JsonM,x
                cmp PatLen,x
                bne .naechstes
                ; Schluessel komplett: Wert in sein Ziel sammeln
                lda PatTerm,x
                sta JsonTerm
                inx
                stx JsonCap
                lda #0
                sta Utf8Flag
                ldy #3
-               sta JsonM,y
                dey
                bpl -
                ldy PatDstHi-1,x
                lda PatMax-1,x
                pha
                lda PatDst-1,x
                tax
                pla
                jmp AppTo
.anders         lda #0                  ; alle Muster beginnen mit '"'
                sta JsonM,x
                lda JsonCh
                cmp #$22
                bne .naechstes
                inc JsonM,x
.naechstes      dex
                bpl .muster
                rts

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
HasUci          !byte 0
KeyIn           !byte 0
AppLen          !byte 0
AppMax          !byte 0
UrlErr          !byte 0
WantSink        !word SinkStore
Ticks           !word 9000
Day             !byte 0
Wd              !byte 0
CsvTab          !word 0
CsvLine         !byte 0
CsvField        !byte 0
JsonCh          !byte 0
JsonCap         !byte 0
JsonTerm        !byte 0
JsonM           !fill 4, 0

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
PatStart        !byte 0, PAT1, PAT2, PAT3
PatLen          !byte PAT1, PAT2-PAT1, PAT3-PAT2, PAT4-PAT3
PatDst          !byte <FoundName, <FoundLat, <FoundLon, <FoundCC
PatDstHi        !byte >FoundName, >FoundLat, >FoundLon, >FoundCC
PatMax          !byte 25, 12, 12, 3
PatTerm         !byte $22, ",", ",", $22

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
Url_Ip          !text "ip-api.com/csv/?fields=16594", 0
Url_Geo         !text "geocoding-api.open-meteo.com/v1/search?count=1&language=de&name=", 0
Url_Fc1         !text "api.open-meteo.com/v1/forecast?latitude=", 0
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
Str_NoUci1      !pet "No Ultimate", 0
Str_NoUci2      !pet "Enable Command Interface", 0
Str_About       !pet "Weather 1.0", NL, "by SanstarR", 0
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
Wnd_Main        !byte WT_WETTER, %00100001, 5, 0, 30, 21
                !byte <Str_Title, >Str_Title, <WndProc, >WndProc
                ; 0 Menueleiste
                !byte CT_MENUBAR, <MenuBar, >MenuBar, 0, 0
                !pet 0
                ; 1-2 Rahmen
                !byte CT_FRAME, 1, 5, 28, 6
                !pet "Now",0
                !byte CT_FRAME, 1, 11, 28, 6
                !pet "Forecast",0
                ; 3 Suchfeld, 4 Knopf (Text = Breite - 2)
                !byte CT_EDIT_SL, 1, 0, 19, 3
                !pet 0
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
                ; 8-10 Beschreibung, Wind, Luftfeuchte
                !byte CT_LABEL, 7, 7, 17, 1
Str_Desc        !fill 17, $20
                !byte 0
                !byte CT_LABEL, 7, 8, 13, 1
Str_Wind        !fill 13, $20
                !byte 0
                !byte CT_LABEL, 7, 9, 13, 1
Str_Hum         !fill 13, $20
                !byte 0
                ; 11-13 Tage
                !byte CT_LABEL, 3, 12, 7, 1
Str_Day0        !fill 7, $20
                !byte 0
                !byte CT_LABEL, 12, 12, 7, 1
Str_Day1        !fill 7, $20
                !byte 0
                !byte CT_LABEL, 21, 12, 7, 1
Str_Day2        !fill 7, $20
                !byte 0
                ; 14-16 Symbole der Tage
                !byte CT_ICON, 4, 13, 2, 2
                !pet 0
                !byte CT_ICON, 13, 13, 2, 2
                !pet 0
                !byte CT_ICON, 22, 13, 2, 2
                !pet 0
                ; 17-19 Temperaturen der Tage
                !byte CT_TEMP, 3, 15, 7, 1
Str_TDay0       !fill 7, $20
                !byte 0
                !byte CT_TEMP, 12, 15, 7, 1
Str_TDay1       !fill 7, $20
                !byte 0
                !byte CT_TEMP, 21, 15, 7, 1
Str_TDay2       !fill 7, $20
                !byte 0
                ; 20 Fusszeile
                !byte CT_LABEL, 1, 17, 28, 1
Str_Foot        !fill 28, $20
                !byte 0
                !byte 0

!source "uci.asm"

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
!ifndef VICETEST {                      ; (die Testfassung darf groesser sein)
!if PUFFER_ENDE > $c000 {
                !warn "Puffer reichen ueber $c000: ", PUFFER_ENDE
}
}
