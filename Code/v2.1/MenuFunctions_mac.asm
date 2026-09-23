;offsetL         !byte 0 ZP
offsetR         !byte 0

PaintEmptyMenuBarToBuf
                ldx #39
-               lda #160
                sta MENUBAR_BUF,x
                lda #CL_WHITE
                sta MENUBAR_CLR_BUF,x
                dex
                bpl -
                ;
                lda #220
                sta MENUBAR_BUF
                lda #63
                sta MENUBAR_BUF+1
                lda #48
                sta MENUBAR_BUF+2
                lda #221
                sta MENUBAR_BUF+39
                lda #54
                sta MENUBAR_BUF+32
                rts

; Paints menu bar of cur wnd (needs to be top window) to buffer
PaintMenubarToBuf
                lda CurrentWindow
                bmi +
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                lda WindowNumCtrls
                beq +
                jsr SelectControl0
                lda #<MENUBAR_BUF
                sta $fd
                sta $02
                lda #>MENUBAR_BUF
                sta $fe
                lda #>MENUBAR_CLR_BUF
                sta $03
                ; Paint menu entries
                lda #4
                jsr AddToFD
                jsr CtrlStringsToFB
                ldx ControlNumStr
                dex
-               jsr PrintStringLC
                ;inc res
                ;lda res
                iny
                tya
                jsr AddToFB
                ;inc res
                ;lda res
                iny
                tya
                jsr AddToFD
                dex
                bpl -
                ; Highlights index if menubar pressed
                lda ControlHilIndex
                bmi +
                ldy offsetR
                lda CSTM_MenuSelClr
-               sta ($02),y
                dey
                bmi +
                cpy offsetL
                bcs -
+               rts

MenubarToScreen jsr PaintEmptyMenuBarToBuf
                jsr PaintMenubarToBuf
                ;
                ldx #33
-               lda MENUBAR_BUF,x
                sta SCRMEM,x
                lda MENUBAR_CLR_BUF,x
                sta CLRMEM,x
                dex
                bpl -
                ldx #5
                lda #CL_WHITE
-               sta CLRMEM+34,x
                dex
                bpl -
                lda #221
                sta SCRMEM+39
                rts

SelectSysMenu   lda #<Menu_System
                sta CurrentMenu
                lda #>Menu_System
                sta CurrentMenu+1
                lda #$ff
                sta CurMenuItem
                lda #0
                sta CurMenuPosX
                lda #1
                sta CurMenuPosY
                rts

;; Expects sys menu selected
;PaintSystemMenu lda CurrentMenu
;                sta $fb
;                lda CurrentMenu+1
;                sta $fc
;                jsr PaintMenuToBuf
;                ldx #1
;                lda ScrTabLo,x
;                sta $fb
;                lda ScrTabHi,x
;                sta $fc
;                jsr BufToScreen
;                rts

; Expects sys menu or wnd man menu selected
PaintSysOrWndManMenu
                lda CurrentMenu
                sta $fb
                lda CurrentMenu+1
                sta $fc
                jsr PaintMenuToBuf
                ldx #1
                lda ScrTabLo,x
                sta $fb
                lda ScrTabHi,x
                sta $fc
                lda CurMenuPosX
                jsr AddToFB
                jmp BufToScreen

max_wndman_len  !byte 0
BuildWndManMenu ldx #0
                stx max_wndman_len
                stx window_counter
                ;
                lda #<WND_HEAP
                sta $fb
                lda #>WND_HEAP
                sta $fc
--              ; Get string ptr
                ldy #8
                lda ($fb),y
                sta $fd
                iny
                lda ($fb),y
                sta $fe
                ; Copy string
                ldy #0
-               lda ($fd),y
                sta FREEMEM+3,x
                beq +
                iny
                inx
                bne -; jmp -
+               cpy max_wndman_len
                bcc +
                sty max_wndman_len
+               ldy #0
                lda ($fb),y
                ldy window_counter
                sta FREEMEM+256,y
                inx
                lda #16
                jsr AddToFB
                inc window_counter
                lda window_counter
                cmp AllocedWindows
                bcc --
                lda #ID_MENU_WNDMAN
                sta FREEMEM
                lda max_wndman_len
                sta FREEMEM+1
                lda AllocedWindows
                sta FREEMEM+2                
                rts

SelectWndManMenu
                lda #<FREEMEM
                sta CurrentMenu
                lda #>FREEMEM
                sta CurrentMenu+1
                lda #$ff
                sta CurMenuItem
                ldy #31
                ldx max_wndman_len
                inx
                inx
                txa
                clc
                adc #31
                sec
                sbc #40
                bmi +
                sta ZP_5F
                lda #31
                sec
                sbc ZP_5F
                tay
+               sty CurMenuPosX
                lda #1
                sta CurMenuPosY
                rts

; Result in carry
IsInSystemMenu  clc
                lda MouseInfo+1
                bne +
                lda #2
                cmp MouseInfo
+               rts

; Result in carry
IsInMenubar     lda MouseInfo+1
                bne +
                lda MouseInfo
                cmp #31
                bcs +
                cmp #3
                bcs ++
+               clc
++              rts
                
; Expects Param0 filled with item
SelectMenuItem  lda Param0
                bmi ++
                cmp CurMenuItem
                beq ++
                sta CurMenuItem
                jsr Menubar_ShowMenu
                ;
SelMenuItemTail ldx Param0
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
                lda #<DESKTOP_CLR_BUF
                sta $02
                lda #>DESKTOP_CLR_BUF
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

; Paints menu of selected menubar item (ControlHilIndex) into buffer
; and to screen
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
                lda offsetL
                sta CurMenuPosX
                ldx #1
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
                ;sec
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
                ; Carry ist durch cmp #41 gesetzt
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
                lda #3
                sta offsetL
                ldx #0
                ;
-               jsr NextString
                ;ldy res
                iny
                tya
                clc
                adc offsetL
                sta offsetR
                ;
                lda MouseInfo
                sta ZP_5F
                cmp offsetL
                bcc +
                lda offsetR
                cmp ZP_5F
                bcc +
                ; Mouse is in item X
                stx ControlHilIndex                
                txa
                pha
                jsr UpdateControl
                jsr RepaintAll
                pla
                sta res
                rts
                ;
+               ldy res
                iny
                iny
                tya
                ;clc
                adc offsetL
                sta offsetL
                inx
                cpx ControlNumStr
                bcc -
                ; Mouse is not in any item
                lda #$ff
                sta res
                rts