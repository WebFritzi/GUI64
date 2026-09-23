;offsetL         !byte 0 ZP
offsetR         !byte 0

; Expects Param0 filled with item
SelectMenuItem  lda Param0
                bmi ++
                cmp CurMenuItem
                beq ++
                sta CurMenuItem
                jsr Menubar_ShowMenu
                ;
                ldx Param0
                inx
                jsr SelectMenuLine
                ldx Param0
                bne +
                jsr SelectMenuLine
+               inx
                inx
                inx
                cpx CurMenuHeight
                bne ++
                dex
                jmp SelectMenuLine
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

; Result in carry
IsInCurMenu     jsr GetMouseInfo
                lda MouseInfo
                cmp CurMenuPosX
                bcc +
                sbc CurMenuPosX
                cmp CurMenuWidth
                bcs +
                lda MouseInfo+1
                cmp CurMenuPosY
                bcc +
                sbc CurMenuPosY
                cmp CurMenuHeight
                bcs +
                sec
                rts
+               clc
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
                lda #<DESKTOP_BUF
                sta $fd
                lda #>DESKTOP_BUF
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
                stx ZP_5F
                lda #<DESKTOP_BUF
                clc
                adc ZP_5F
                sta $fd
                lda #>DESKTOP_BUF
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

; Paints menu of selected menubar item into buffer and to screen
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
                iny
                lda ($fd),y
                sta $fc
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
                sta ZP_5F
                ; check Y
                lda CurMenuPosY
                clc
                adc CurMenuHeight
                cmp #23
                bcc +
                lda #22
                ; carry is set
                sbc CurMenuHeight
                sta CurMenuPosY
                lda offsetR
                sec
                sbc offsetL
                tax
                inx
                stx ZP_5F
                txa
                clc
                adc CurMenuPosX
                sta CurMenuPosX
                sec
                sbc ZP_5F
                sta ZP_5F
+               ; check X
                lda CurMenuPosX
                clc
                adc CurMenuWidth
                cmp #41
                bcc +
                lda ZP_5F
                ;sec
                sbc CurMenuWidth
                sta CurMenuPosX
+               ; Bring buffer to screen ----
                ;
                ldx CurMenuPosY
                ldy CurMenuPosX
                jsr PosToScrMemFB
                jmp BufToScreen

; Expects menubar in local control struct
; Returns selected menubar index in res
SelMenubarEntry jsr CtrlStringsToFD
                lda #0
                sta offsetL
                tax
                ;
-               jsr NextString
                ;ldy res
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