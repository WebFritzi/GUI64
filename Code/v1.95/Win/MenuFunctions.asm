; Expects Param filled with item
SelectMenuItem  lda Param
                bmi ++
                cmp CurMenuItem
                beq ++
                sta CurMenuItem
                jsr Menubar_ShowMenu
                ;
                ldx Param
                inx
                jsr SelectMenuLine
                ldx Param
                bne +
                jsr SelectMenuLine
+               inx
                inx
                inx
                cpx CurMenuHeight
                bne ++
                dex
                jsr SelectMenuLine
++              rts

; Selects menu line at y pos in X
SelectMenuLine  txa
                pha
                ldy CurMenuPosX
                txa
                clc
                adc CurMenuPosY
                tax
                jsr PosToClrMem02
                ;
                ldy CurMenuWidth
                dey
                lda CSTM_MenuSelClr
-               sta ($02),y
                dey
                bpl -
                pla
                tax
                rts

; Expects mouse in cur menu
; Writes result into res
GetMenuItem     jsr GetMouseInfo
                lda MouseInfo+1
                sec
                sbc CurMenuPosY
                tax
                beq ++
                inx
                cpx CurMenuHeight
                bne +
                dex
+               dex
                dex
++              stx res
                rts

IsInCurMenu     jsr GetMouseInfo
                lda MouseInfo
                cmp CurMenuPosX
                bcc +
                sec
                sbc CurMenuPosX
                cmp CurMenuWidth
                bcs +
                lda MouseInfo+1
                cmp CurMenuPosY
                bcc +
                sec
                sbc CurMenuPosY
                cmp CurMenuHeight
                bcs +
                lda #1
                rts
+               lda #0
                rts

; Expects menu ptr in FBFC
PaintMenuToBuf  ldy #0
                lda ($fb),y
                sta CurMenuID
                iny
                lda ($fb),y
                clc
                adc #2
                sta BoxWidth
                sta BufWidth
                sta CurMenuWidth
                iny
                lda ($fb),y
                clc
                adc #2
                sta BoxHeight
                sta BufHeight
                sta CurMenuHeight
                lda CSTM_WindowClr
                sta BoxColor
                lda #<SCR_BUF
                sta $fd
                lda #>SCR_BUF
                sta $fe
                lda #<CLR_BUF
                sta $02
                lda #>CLR_BUF
                sta $03
                jsr PaintBoxToFD02; changes FD
                ; Fill menu with items
                ;
                ldx BufWidth
                inx
                stx dummy
                lda #<SCR_BUF
                clc
                adc dummy
                sta $fd
                lda #>SCR_BUF
                adc #0
                sta $fe
                ; Get number of items in X
                ldy #2
                lda ($fb),y
                tax
                ; Set ptr to string list
                lda #3
                jsr AddToFB
                ; Now buf ptr is in FDFE, and string list is in FBFC
-               jsr PrintStringLC
                ; Y is str len
                iny
                tya
                jsr AddToFB
                jsr AddBufWidthToFD
                dex
                bne -
                rts

; Paints menu of selected menubar item into
; buffer
Menubar_ShowMenu
                ; Get pointer to menu list
                lda ControlPosX
                sta $fd
                lda ControlPosY
                sta $fe
                ; Get pointer to menu (in FBFC)
                ;
                lda ControlHilIndex
                asl
                tay
                lda ($fd),y
                sta $fb
                sta CurrentMenu
                iny
                lda ($fd),y
                sta $fc
                sta CurrentMenu+1
                ; Paint menu box to buffer
                jsr PaintMenuToBuf
                ; Set menu pos on screen ----
                ;
                lda WindowPosX
                clc
                adc offsetL
                sta CurMenuPosX
                ldx WindowPosY
                inx
                inx
                stx CurMenuPosY
                lda #MT_NORMAL
                sta CurMenuType
                ; Correction if necessary
                lda #40
                sta dummy
                ; check Y
                lda CurMenuPosY
                clc
                adc CurMenuHeight
                cmp #23
                bcc +
                sec
                sbc #22
                sbc CurMenuPosY
                jsr minus
                stx CurMenuPosY
                lda offsetR
                sec
                sbc offsetL
                tax
                inx
                stx dummy
                txa
                clc
                adc CurMenuPosX
                sta CurMenuPosX
                sec
                sbc dummy
                sta dummy
+               ; check X
                lda CurMenuPosX
                clc
                adc CurMenuWidth
                cmp #41
                bcc +
                sbc dummy
                sbc CurMenuPosX
                jsr minus
                txa
                sta CurMenuPosX
+               ; Bring buffer to screen ----
                ;
                ldx CurMenuPosY
                ldy CurMenuPosX
                jsr PosToScrMemFB
                jmp BufToScreen

; Expects menubar in local control struct
; Returns selected menubar index in res
SelMenubarEntry lda ControlStrings
                sta $fd
                lda ControlStrings+1
                sta $fe
                lda #0
                sta offsetL
                ldx #0
                ;
-               jsr NextString
                ldy res
                iny
                tya
                clc
                adc offsetL
                sta offsetR
                ;
                lda MousePosInWndX
                cmp offsetL
                bcc +
                lda offsetR
                cmp MousePosInWndX
                bcc +
                ; Mouse is in item X
                stx ControlHilIndex                
                txa
                pha
                jsr UpdateControl
                jsr PaintMenuBar
                jsr WindowToScreen
                pla
                sta res
                rts
                ;
+               ldy res
                iny
                iny
                tya
                clc
                adc offsetL
                sta offsetL
                inx
                cpx ControlNumStr
                bcc -
                ; Mouse is not in any item
                lda #$ff
                sta res
                rts