; Needs wndParam filled with exit code
; and local control struct filled with the control
ControlsProc    lda ControlType
                cmp #CT_MENUBAR
                bne +
                jmp ActionInMenubar
+               cmp #CT_BUTTON
                bne +
                jmp ActionInButton
+               cmp #CT_FILELISTSCROLLBOX
                bne +
                jmp ActionInFileListScrollBox
+               cmp #CT_COLORPICKER
                bne +
                jmp ActionInColorPicker
+               cmp #CT_RADIOBUTTONGROUP
                bne +
                jmp ActionInRadioButtonGroup
+               cmp #CT_UPDOWN
                bne +
                jmp ActionInUpDown
+               cmp #CT_EDIT_SL
                bne +
                jmp ActionInEdit_SL
+               rts

ActionInMenubar ;
                rts

ActionInEdit_SL lda wndParam
                cmp #EC_LBTNPRESS
                bne +
                jmp PaintMe
+               cmp #EC_KEYPRESS
                bne ++
                lda WindowFocCtrl
                cmp ControlIndex
                bne ++
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                lda ControlParent+CTRLSTRUCT_FORBIDDEN+1
                beq +
                sta $fe
                lda ControlParent+CTRLSTRUCT_FORBIDDEN
                sta $fd
                ldy #0
-               lda ($fd),y
                beq +
                cmp actkey
                beq ++
                iny
                jmp -
+               lda actkey
                ; Special keys
                cmp #$fd; backspace
                bne +
                ; Backspace
                ldy ControlParent+CTRLSTRUCT_CARRETPOS
                beq ++
                dey
                lda #32
                sta ($fb),y
                sty ControlParent+CTRLSTRUCT_CARRETPOS
                jsr UpdateControl                
                jmp PaintMe
+               cmp #$fc; return
                beq ++
                ; Usual key
                ldy ControlParent+CTRLSTRUCT_CARRETPOS
                cpy ControlParent+CTRLSTRUCT_MAX_STRLEN
                bcs ++
                lda actkey
                sta ($fb),y
                iny
                sty ControlParent+CTRLSTRUCT_CARRETPOS
                jsr UpdateControl
PaintMe         jsr PaintControl
                jsr WindowToScreen
++              rts

ActionInUpDown  lda wndParam
                cmp #EC_KEYPRESS
                bne ++
                lda actkey
                cmp #$fb
                beq +
                rts
+               lda shifted
                bne +
                jmp down
+               jmp up
++              ldx ControlPosY
                inx
                cpx MousePosInWndY
                bne ++++
                ;
                lda ControlParent+CTRLSTRUCT_DIGIT_HI
                asl
                asl
                asl
                asl
                ora ControlParent+CTRLSTRUCT_DIGIT_LO
                sta dummy
                ;
                lda wndParam
                cmp #EC_LBTNPRESS
                beq ++
                cmp #EC_SCROLLDOWN
                beq +
                cmp #EC_SCROLLUP
                bne ++++
                ; Mouse scroll wheel up
                jmp up
+               ; Mouse scroll wheel down
                jmp down
++              ; Button click
                ldx ControlPosX
                inx
                inx
                inx
                cpx MousePosInWndX
                bne ++++
                ldx MouseInfo+3
                dex
                dex
                txa
                and #%00000100
                bne down
up              ; up
                sed
                lda dummy
                cmp ControlParent+CTRLSTRUCT_UPPERLIMIT
                bcc +
                lda ControlParent+CTRLSTRUCT_LOWERLIMIT
                jmp ++
+               clc
                adc #1
                jmp ++
down            ; down
                sed
                lda ControlParent+CTRLSTRUCT_LOWERLIMIT
                cmp dummy
                bcc +
                lda ControlParent+CTRLSTRUCT_UPPERLIMIT
                jmp ++
+               lda dummy
                sec
                sbc #1
                ; for both directions
++              sta dummy
                cld
                lsr
                lsr
                lsr
                lsr
                sta ControlParent+CTRLSTRUCT_DIGIT_HI
                lda dummy
                and #%00001111
                sta ControlParent+CTRLSTRUCT_DIGIT_LO
                jsr UpdateControl
                jsr PaintControl
                jsr WindowToScreen
++++            rts

ActionInRadioButtonGroup
                lda wndParam
                cmp #EC_LBTNPRESS
                bne +
                ; Find index
                lda MousePosInWndY
                sec
                sbc ControlPosY
                sta ControlHilIndex
                jsr UpdateControl
                jsr PaintControl
                jsr WindowToScreen
+               rts

ActionInButton  lda wndParam
                cmp #EC_LBTNPRESS
                bne +
                ; Left btn press in button
                lda ControlBits
                ora #BIT_CTRL_ISPRESSED
                sta ControlBits
                jsr UpdateControl
                jsr PaintButton
                jsr WindowToScreen
                rts
+               ; Left mouse button released in button
                cmp #EC_LBTNRELEASE
                bne + 
                ; Left button released
                lda #BIT_CTRL_ISPRESSED
                eor #%11111111
                and ControlBits
                sta ControlBits
                jsr UpdateControl
                jsr PaintButton
                jsr WindowToScreen
                rts
+               ; Mouse was moved
                ; only occurs when mouse enters or leaves button
                jsr PaintButton
                jsr WindowToScreen
                rts

ActionInFileListScrollBox
                lda ControlNumStr
                beq +
                lda wndParam
                cmp #EC_LBTNPRESS
                beq ++
                cmp #EC_LLBTNPRESS
                beq ++
                cmp #EC_KEYPRESS
                beq keypress
                cmp #EC_SCROLLUP
                beq scroll_up
                cmp #EC_SCROLLDOWN
                beq scroll_down
+               rts
++              ; LBTNPRESS
                ldx MousePosInWndX
                inx
                cpx ControlWidth
                beq in_scrollbar
                ; In files section
                ldx MousePosInWndY
                beq +
                inx
                cpx ControlHeight
                bcs +
                dex
                dex
                txa
                clc
                adc ControlTopIndex
                cmp ControlNumStr
                bcs +
                sta ControlHilIndex
-               jsr UpdateControl
                jsr PaintFileListScrollBox
                jsr WindowToScreen
                rts
+               lda #$ff
                sta ControlHilIndex
                jmp -
                rts
in_scrollbar    ; In Scrollbar
                lda MousePosInWndY
                cmp #2
                bcc scroll_up
                ldx ControlHeight
                dex
                cpx MousePosInWndY
                bne +
                rts
+               dex
                cpx MousePosInWndY
                beq scroll_down
                ;
                ldx MousePosInWndY
                dex
                dex
                cpx scroll_caret_pos
                bcc scroll_pg_up
                lda scroll_caret_pos
                clc
                adc scroll_caret_height
                sta dummy
                cpx dummy
                bcs scroll_pg_down
                rts
keypress        jmp FLSB_KeyPress
scroll_up       lda ControlTopIndex
                beq +
                dec ControlTopIndex
                jmp finish
+               rts
scroll_down     ldx ControlHeight
                dex
                dex
                txa
                clc
                adc ControlTopIndex
                cmp ControlNumStr
                bcs +
                inc ControlTopIndex
                jmp finish
+               rts
scroll_pg_up    ldx ControlTopIndex
                inx
                inx
                txa
                sec
                sbc ControlHeight
                bmi +
                sta ControlTopIndex
                jmp finish
+               lda #0
                sta ControlTopIndex
                jmp finish
                rts
scroll_pg_down  ldx ControlTopIndex
                dex
                dex
                txa
                clc
                adc ControlHeight
                sta ControlTopIndex
finish          jsr UpdateControl
                jsr PaintCurWindow
                jsr WindowToScreen
                rts
FLSB_KeyPress   lda actkey
                cmp #$fb
                bne ++++
                ; Crsr up/down pressed
                ldx ControlHilIndex
                lda shifted
                beq ++
                ; go up list
                dex
                bmi +++
                cpx ControlTopIndex
                bcs +
                dec ControlTopIndex
+               stx ControlHilIndex
                jmp finish
++              ; go down list
                inx
                cpx ControlNumStr
                bcs +++
                lda ControlTopIndex
                clc
                adc ControlHeight
                sec
                sbc #2
                sta dummy
                cpx dummy
                bne +
                inc ControlTopIndex
+               stx ControlHilIndex
                jmp finish
+++             rts
++++            cmp #$fc
                bne ++
                ; Return pressed
                lda shifted
                beq +
                lda #EC_BOOTFILE
                sta exit_code
                rts
+               lda #EC_RUNFILE
                sta exit_code
++              rts

ActionInColorPicker
                lda wndParam
                cmp #EC_LBTNPRESS
                beq +
                rts
+               lda wndParam+1
                beq ++
                ; Click in menu mode
                jsr IsInCurMenu
                lda res
                bne +
                rts
+               jsr GetMenuItem
                lda res
                sta ControlColor
                jsr UpdateControl
                rts
++              ; Click in normal mode
                ; Fill menu struct
                lda #ID_MENU_COLORPICKER
                sta CurMenuID
                lda #MT_COLORPICKER
                sta CurMenuType
                lda #4
                sta CurMenuWidth
                lda #18
                sta CurMenuHeight
                lda #<Menu_ColorPicker
                sta $fb
                sta CurrentMenu
                lda #>Menu_ColorPicker
                sta $fc
                sta CurrentMenu+1
                ; Paint color picker menu
                jsr PaintMenuToBuf
                lda #<SCR_BUF
                sta $fb
                lda #>SCR_BUF
                sta $fc
                lda #<CLR_BUF
                sta $02
                lda #>CLR_BUF
                sta $03
                ;
                ldx #0
                ldy #5
-               lda #10
                sta ($fb),y
                txa
                sta ($02),y
                iny
                sta ($02),y
                lda #12
                sta ($fb),y
                iny
                iny
                iny
                inx
                cpx #16
                bcc -
                ; Find destination position
                lda ControlPosY
                clc
                adc WindowPosY
                tax
                sec
                sbc #5
                bmi +
                ldx #4
+               lda ControlPosX
                clc
                adc WindowPosX
                tay
                iny
                iny
                cpy #37
                bcc +
                tya
                sec
                sbc #6
                tay
+               sty CurMenuPosX
                stx CurMenuPosY; MenuPosY = 4 or = ControlPosY + WindowPosY
                jsr PosToScrMemFB
                jsr BufToScreen
                lda #GM_MENU
                sta GameMode
                rts

GetMenubarWidth lda #0
                sta $02
                lda WindowCtrlPtr
                sta $fb
                lda WindowCtrlPtr+1
                sta $fc
                ldy #CTRLSTRUCT_NUMSTRINGS
                lda ($fb),y
                tax
                ; Write string address to FDFE
                ldy #CTRLSTRUCT_STRINGS
                lda ($fb),y
                sta $fd
                iny
                lda ($fb),y
                sta $fe
                ; Go
-               jsr GetStrLen
                lda $02
                clc
                adc res
                clc
                adc #2; 2 for every menu item
                sta $02
                inc res
                lda res
                jsr AddToFD
                ;+AddByteToFD res
                dex
                bne -
                lda $02
                sta res
                rts

; Copies local ControlStruct to memory, uses 0203
UpdateControl   lda ControlOnHeap
                sta $02
                lda ControlOnHeap+1
                sta $03
                ldy #15
-               lda ControlParent,y
                sta ($02),y
                dey
                bpl -
                rts

; Finds string addr in string list in 0203 and assigns it to cur control
; Input:
; 0203: address of string list
; Param: position of the string in string list
SetCtrlString   lda Param
                bne +
                lda #0
                sta dummy
                jmp ++
+               ldy #$ff
                ldx #0
                ;
-               iny
                lda ($02),y
                bne -
                inx
                cpx Param
                beq +
                jmp -
+               iny
                sty dummy
++              lda $02
                clc
                adc dummy
                sta ControlStrings
                lda $03
                adc #0
                sta ControlStrings+1
                jsr UpdateControl
                rts

; Finds control index in cur wnd from mouse pos in MousePosInWndX/Y
; Returns control index in res ($ff if mouse is in no control)
; If successfull, control is cur control
; Does NOT select control
GetCtrlFromPos  ldx WindowNumCtrls
                dex
-               txa
                jsr SelectControl
                jsr IsInCurControl
                lda res
                beq +
                ; Exclude frames
                lda ControlType
                cmp #CT_FRAME
                beq +
                stx res
                rts
+               dex
                bpl -
                lda #$ff
                sta res
                rts

; Checks if mouse cursor is in current control
; Returns res
IsInCurControl  lda #0
                sta res
                lda MousePosInWndX
                cmp ControlPosX
                bcc +
                sec
                sbc ControlPosX
                cmp ControlWidth
                bcs +
                ;
                lda MousePosInWndY
                cmp ControlPosY
                bcc +
                sec
                sbc ControlPosY
                cmp ControlHeight
                bcs +
                lda #1
                sta res
+               rts

; Checks if mouse cursor is in control at FBFC
; Returns res
IsInCtrlMiddle  lda #0
                sta res
                ldy #CTRLSTRUCT_POSX
                lda ($fb),y
                cmp MousePosInWndX
                bcs +
                clc
                ldy #CTRLSTRUCT_WIDTH
                adc ($fb),y
                tax
                dex
                dex
                cpx MousePosInWndX
                bcc +
                ;
                ldy #CTRLSTRUCT_POSY
                lda ($fb),y
                cmp MousePosInWndY
                bcs +
                clc
                ldy #CTRLSTRUCT_HEIGHT
                adc ($fb),y
                tax
                dex
                dex
                cpx MousePosInWndY
                bcc +
                lda #1
                sta res
+               rts

; Gets pointer to control struct with index in Param
; Result is in FBFC
GetControlPtr   lda WindowCtrlPtr
                sta $fb
                lda WindowCtrlPtr+1
                sta $fc
                lda #0
                sta Param+1
                ; x 16
                asl Param
                rol Param+1
                asl Param
                rol Param+1
                asl Param
                rol Param+1
                asl Param
                rol Param+1
                ;
                +AddWordToFB Param, Param+1
                rts

; Copies control struct of control with index in Param
; into local control struct
; Expects control index in A
SelectControl   sta Param
                jsr GetControlPtr
                ldy #15
-               lda ($fb),y
                sta ControlParent,y
                dey
                bpl -
                lda $fb
                sta ControlOnHeap
                lda $fc
                sta ControlOnHeap+1
                rts