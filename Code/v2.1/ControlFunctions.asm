;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ControlFunctions.asm                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In this file the default behavior of controls is defined ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Needs wndParam0 filled with exit code
; and local control struct filled with the control
ControlsProc    ldx ControlType
                cpx #(MAX_CT_ACTION+1)
                bcs +
                dex
                lda CtrlActionLo,x
                sta CtrlActionJmp+1
                lda CtrlActionHi,x
                sta CtrlActionJmp+2
CtrlActionJmp   jmp $ffff
+               cpx #MIN_CT_APP
                bcc +
                jmp (App_CtrlActions)
+               rts

CtrlActionLo !byte <ActionInMenubar,<ActionInButton,<ActionInListBox,<ActionInFileListScrollBox
             !byte <ActionInColorPicker, <ActionInRadioButtonGroup, <ActionInUpDown, <ActionInEdit_SL
             !byte <ActionInTextViewBox, <ActionInCheckBox

CtrlActionHi !byte >ActionInMenubar, >ActionInButton, >ActionInListBox, >ActionInFileListScrollBox
             !byte >ActionInColorPicker, >ActionInRadioButtonGroup, >ActionInUpDown, >ActionInEdit_SL
             !byte >ActionInTextViewBox, >ActionInCheckBox

ActionInCheckBox
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne ++
                ; Left button pressed
                lda #1
                ldx ControlHilIndex
                beq +
                lda #0
+               sta ControlHilIndex
                jsr UpdateControl
                jmp RepaintAll
++              rts

ActionInMenubar ;
                rts

OnScrollbarClick
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
                cpx ScrollCaretPos
                bcc scroll_pg_up
                lda ScrollCaretPos
                clc
                adc ScrollCaretHeight
                sta ZP_5F
                cpx ZP_5F
                bcs scroll_pg_down
                rts
scroll_up       lda #EC_SCROLL_UP
                bne +
scroll_down     lda #EC_SCROLL_DOWN
                bne +
scroll_pg_up    lda #EC_SCROLL_PG_UP
                bne +
scroll_pg_down  lda #EC_SCROLL_PG_DOWN
+               sta wndParam0
                jmp ControlsProc

ActionInFileListScrollBox
                lda ControlNumStr
                beq +
                lda wndParam0
                cmp #EC_LBTNPRESS
                beq ++
                cmp #EC_KEYPRESS
                beq keypress
                cmp #EC_SCROLLWHEELUP
                beq flb_scroll_up
                cmp #EC_SCROLLWHEELDOWN
                beq flb_scroll_down
                cmp #EC_SCROLL_DOWN
                beq flb_scroll_down
                cmp #EC_SCROLL_UP
                beq flb_scroll_up
                cmp #EC_SCROLL_PG_DOWN
                beq flb_scroll_pgdn
                cmp #EC_SCROLL_PG_UP
                beq flb_scroll_pgup
+               rts
keypress        jmp FLSB_KeyPress
++              ; LBTNPRESS
                jsr FLSB_GetMouseArea
                beq +
                cpy #2
                beq OnScrollbarClick;in_scrollbar
                sta ControlHilIndex
-               jsr UpdateControl
!ifdef WIN{
                jsr PaintFileListScrollBox
} else ifdef MAC{
                jsr PaintControl
}
                jmp WindowToScreen
                ;
+               lda #$ff
                sta ControlHilIndex
                bne -; jmp -
;in_scrollbar    jmp OnScrollbarClick
flb_scroll_up   lda ControlTopIndex
                beq +
                dec ControlTopIndex
                jmp finish
flb_scroll_down ldx ControlHeight
                dex
                dex
                txa
                clc
                adc ControlTopIndex
                cmp ControlNumStr
                bcs +
                inc ControlTopIndex
                bcc finish; jmp finish
+               rts
flb_scroll_pgup ldx ControlTopIndex
                inx
                inx
                txa
                sec
                sbc ControlHeight
                bcc TopIndexToZero_finish
                sta ControlTopIndex
                bcs finish
TopIndexToZero_finish
                lda #0
                sta ControlTopIndex
                beq finish
flb_scroll_pgdn ldx ControlNumStr
                inx
                inx
                txa
                sec
                sbc ControlHeight
                sta ZP_5F; highest possible top index
                inc ZP_5F
                ldx ControlTopIndex
                dex
                dex
                txa
                clc
                adc ControlHeight
                cmp ZP_5F
                bcs +
                sta ControlTopIndex
                bcc finish; jmp finish
+               ldx ZP_5F
                dex
                stx ControlTopIndex
finish          jsr UpdateControl
!ifdef WIN{
                jmp PaintCurWndViaBufToScreen
}
!ifdef MAC{
                jsr PaintControl
                jmp WindowToScreen
}

FLSB_KeyPress   lda actkey
                cmp #$fb
                bne +++
                ; Crsr up/down pressed
                ldx ControlHilIndex
                lda key_shifted
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
                ;clc
                adc ControlHeight
                sec
                sbc #2
                sta ZP_5F
                cpx ZP_5F
                bne +
                inc ControlTopIndex
+               stx ControlHilIndex
                jmp finish
+++             cmp #$f9
                bne +
                beq TopIndexToZero_finish
+               rts

; Required: MousePosInWndX/Y
; Result:
; Y=1: In files, A=index
; Y=2: In scrollbar
; Y=0: none of the above
FLSB_GetMouseArea
                ldx MousePosInWndX
                beq ++
                inx
                cpx ControlWidth
                bne +
                ldy #2
                rts
+               ; In files section
                inx
                cpx ControlWidth
                beq ++
                ldx MousePosInWndY
                beq ++
                inx
                cpx ControlHeight
                bcs ++
                dex
                dex
                txa
                ;clc
                adc ControlTopIndex
                cmp ControlNumStr
                bcs ++
                ldy #1
                rts
++              ldy #0
                rts

ActionInTextViewBox
                lda wndParam0
                cmp #EC_LBTNPRESS
                beq view_lbtnpress
                cmp #EC_SCROLLWHEELUP
                beq tvb_scroll_up
                cmp #EC_SCROLLWHEELDOWN
                beq tvb_scroll_down
                cmp #EC_SCROLL_DOWN
                beq tvb_scroll_down
                cmp #EC_SCROLL_UP
                beq tvb_scroll_up
                cmp #EC_SCROLL_PG_DOWN
                beq tvb_scroll_pgdn
                cmp #EC_SCROLL_PG_UP
                beq tvb_scroll_pgup
                rts
view_lbtnpress  ; EC_LBTNPRESS
                ldx MousePosInWndX
                inx
                cpx ControlWidth
                beq in_viewscrolbar
                ; Not in Scrollbar
                rts
in_viewscrolbar jmp OnScrollbarClick
tvb_scroll_up   ; EC_SCROLLWHEELUP
                lda ControlIndex+TEXTVIEWBOX_ISTEXT
                beq +
                jsr ScrollUpText
                jmp view_finish
+               jsr ScrollUpHex
                jmp view_finish
tvb_scroll_down ; EC_SCROLLWHEELDOWN
                lda ControlIndex+TEXTVIEWBOX_ISTEXT
                beq +
                jsr ScrollDownText
                jmp view_finish
+               jsr ScrollDownHex
                jmp view_finish
tvb_scroll_pgdn ; Page scroll down
                lda ControlIndex+TEXTVIEWBOX_ISTEXT
                bne ScrollPgDownText
                ;ScrollPgDownHex
                ldy HeightMinus2
                dey
-               jsr ScrollDownHex
                dey
                bpl -
                bmi view_finish
tvb_scroll_pgup ; Page scroll up
                lda ControlIndex+TEXTVIEWBOX_ISTEXT
                bne ScrollPgUpText
                ;ScrollPgUpHex
                ldy HeightMinus2
                dey
-               jsr ScrollUpHex
                dey
                bpl -
view_finish     jsr UpdateControl
PaintMe         jsr PaintControl
                jmp WindowToScreen

ScrollPgDownText
                ldy HeightMinus2
                dey
-               tya
                pha
                jsr ScrollDownText
                pla
                tay
                dey
                bpl -
                bmi view_finish

ScrollPgUpText  ldy HeightMinus2 
                dey
-               tya
                pha
                jsr ScrollUpText
                pla
                tay
                dey
                bpl -
                bmi view_finish

ScrollDownText  lda ControlIndex+TEXTVIEWBOX_TOPLO
                sta $fb
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sta $fc
                ldx HeightMinus2
                dex
                stx ZP_5F
-               jsr GetNextLinePos
                inx
                txa
                clc
                adc $fb
                sta $fb
                lda #0
                adc $fc
                sta $fc
                ldx ZP_5F
                inx
                cpx HeightMinus2
                bne +
                jsr FBFC_To_FDFE
+               jsr Cmp_FBFC_ViewerEOF
                bcc +
                rts
+               dec ZP_5F
                bpl -
                lda $fd
                sta ControlIndex+TEXTVIEWBOX_TOPLO
                lda $fe
                sta ControlIndex+TEXTVIEWBOX_TOPHI
                rts

Cmp_FBFC_ViewerEOF
                lda $fb
                cmp ViewerEOF
                lda $fc
                sbc ViewerEOF+1
                rts

LastReturn      !byte 0,0
; Finds the last return position before FBFC
; No last Return: A=0 and LastReturn = TotalStart
; Last Return: A=1 and result in LastReturn
GetLastReturn   lda TotalStart
                sta LastReturn
                lda TotalStart+1
                sta LastReturn+1
                jsr FBFC_To_FDFE
                ldy #255
--              dec $fe
                ;
                lda TotalStart
                cmp $fd
                lda TotalStart+1
                sbc $fe
                bcc ++
                inc $fe
                lda $fd
                sbc TotalStart
                tay
                beq +++
                dey
                lda TotalStart
                sta $fd
                lda TotalStart+1
                sta $fe
++
-               lda ($fd),y
                cmp #13
                beq +
                dey
                cpy #255
                bne -
                beq --
+               tya
                clc
                adc $fd
                sta LastReturn
                lda #0
                adc $fe
                sta LastReturn+1
                sec
                rts
+++             clc
                rts

ScrollUpText    lda TotalStart
                cmp ControlIndex+TEXTVIEWBOX_TOPLO
                lda TotalStart+1
                sbc ControlIndex+TEXTVIEWBOX_TOPHI
                bcc +
                rts
+               lda ControlIndex+TEXTVIEWBOX_TOPLO
                sec
                sbc #2
                sta $fb
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sbc #0
                sta $fc
                ; Check for Return
                ldy #1
                lda ($fb),y
                cmp #13
                bne +
                lda #1
                jsr AddToFB
                jmp ++
+               cmp #10
                bne +
                dey
                lda ($fb),y
                cmp #13
                beq ++
+               ; No Return just before ==> check for space
                lda #1
                jsr AddToFB
                ldy #0
                lda ($fb),y
                cmp #32
                beq ++
                ; Neither Return nor space just before ==> Full line
                lda ControlIndex+TEXTVIEWBOX_TOPLO
                sec
                sbc WidthMinus3
                sta ControlIndex+TEXTVIEWBOX_TOPLO
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sbc #0
                sta ControlIndex+TEXTVIEWBOX_TOPHI
                rts
++              ; Find return (maybe) way before
                jsr GetLastReturn
                ldx $fb
                stx $fd
                ldx $fc
                stx $fe
                ldx LastReturn
                stx $fb
                ldx LastReturn+1
                stx $fc
                bcc ++; No Return found
                ldx #1
                ldy #1
                lda ($fb),y
                cmp #10
                bne +
                inx
+               txa
                jsr AddToFB
++
-               jsr GetNextLinePos
                inx
                lda $fb
                sta $02
                lda $fc
                sta $03
                txa
                jsr AddToFB
                lda $fb
                cmp $fd
                lda $fc
                sbc $fe
                bcc -
                lda $02
                sta ControlIndex+TEXTVIEWBOX_TOPLO
                lda $03
                sta ControlIndex+TEXTVIEWBOX_TOPHI
                rts

ScrollDownHex   ; Retrieves first byte in next line in FBFC
                ; That is, FBFC = Top + (Height-2) * BytesPerLine
                ldx HeightMinus2
                dex
                lda ControlIndex+TEXTVIEWBOX_TOPLO
                sta $fb
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sta $fc
                ; Check if we're already at the end
-               lda BytesPerLine
                jsr AddToFB
                dex
                bpl -
                jsr Cmp_FBFC_ViewerEOF
                bcc +
                rts
+               ; Update TopLo/Hi
                lda ControlIndex+TEXTVIEWBOX_TOPLO
                ;clc
                adc BytesPerLine
                sta ControlIndex+TEXTVIEWBOX_TOPLO
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                adc #0
                sta ControlIndex+TEXTVIEWBOX_TOPHI
                rts

ScrollUpHex     lda ControlIndex+TEXTVIEWBOX_TOPLO
                sec
                sbc BytesPerLine
                sta ZP_5F
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sbc #0
                sta ZP_60
                cmp #>FILEVIEWERBUF_START
                bne +
                lda ZP_5F
                cmp #<FILEVIEWERBUF_START
+               bcs +
                lda #<FILEVIEWERBUF_START
                sta ZP_5F
                lda #>FILEVIEWERBUF_START
                sta ZP_60
+               lda ZP_5F
                sta ControlIndex+TEXTVIEWBOX_TOPLO
                lda ZP_60
                sta ControlIndex+TEXTVIEWBOX_TOPHI
                rts

ActionInListBox lda wndParam0
                cmp #EC_LBTNPRESS
                bne ++
                ; Left button pressed
                lda #$ff
                sta ControlHilIndex
                lda MousePosInWndY
                ;sec
                sbc ControlPosY
                tax
                beq +
                inx
                cpx ControlHeight
                bcs +
                dex
                dex
                stx ControlHilIndex
+               jmp view_finish
++              rts

ActionInEdit_SL lda wndParam0
                cmp #EC_LBTNPRESS
                bne +
                jmp PaintMe
+               cmp #EC_KEYPRESS
                bne ++
                ; Key pressed in single-line Edit
                lda WindowFocCtrl
                cmp ControlIndex
                bne ++
                jsr CtrlStringsToFB
                lda ControlIndex+EDITSL_FORBIDDEN+1
                beq +
                sta $fe
                lda ControlIndex+EDITSL_FORBIDDEN
                sta $fd
                ldy #0
-               lda ($fd),y
                beq +
                cmp actkey
                beq ++
                iny
                bne -; jmp -
+               lda actkey
                ; Special keys
                cmp #$fd; backspace
                bne +
                ; Backspace
                ldy ControlIndex+EDITSL_CARETPOS
                beq ++
                dey
                lda #32
                sta ($fb),y
                sty ControlIndex+EDITSL_CARETPOS
                jmp view_finish
+               cmp #$fc; return
                beq ++
                ; Usual key
                ldy ControlIndex+EDITSL_CARETPOS
                cpy ControlIndex+EDITSL_MAX_STRLEN
                bcs ++
                lda actkey
                sta ($fb),y
                iny
                sty ControlIndex+EDITSL_CARETPOS
                jmp view_finish
++              rts

ActionInUpDown  lda wndParam0
                cmp #EC_KEYPRESS
                bne ++
                lda actkey
                cmp #$fb
                beq +
                rts
+               lda key_shifted
                bne up
                beq down
++              ldx ControlPosY
                lda ControlBitsEx
                and #BIT_EX_CTRL_NOFRAME_TOP
                bne +
                inx
+               cpx MousePosInWndY
                bne ++++
                ;
                lda ControlIndex+UPDOWN_DIGIT_HI
                asl
                asl
                asl
                asl
                ora ControlIndex+UPDOWN_DIGIT_LO
                sta ZP_5F
                ;
                lda wndParam0
                cmp #EC_LBTNPRESS
                beq ++
                cmp #EC_SCROLLWHEELDOWN
                beq down; Mouse scroll wheel down
                cmp #EC_SCROLLWHEELUP
                beq up
                bne ++++
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
                lda ZP_5F
                cmp ControlIndex+UPDOWN_UPPERLIMIT
                bcc +
                lda ControlIndex+UPDOWN_LOWERLIMIT
                jmp ++
+               ;clc
                adc #1
                jmp ++
down            ; down
                sed
                lda ControlIndex+UPDOWN_LOWERLIMIT
                cmp ZP_5F
                bcc +
                lda ControlIndex+UPDOWN_UPPERLIMIT
                jmp ++
+               lda ZP_5F
                sec
                sbc #1
                ; for both directions
++              sta ZP_5F
                cld
                lsr
                lsr
                lsr
                lsr
                sta ControlIndex+UPDOWN_DIGIT_HI
                lda ZP_5F
                and #%00001111
                sta ControlIndex+UPDOWN_DIGIT_LO
                jmp view_finish
++++            rts

ActionInRadioButtonGroup
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne +
                ; Find index
                lda MousePosInWndY
                ;sec
                sbc ControlPosY
                sta ControlHilIndex
                jmp view_finish
+               rts

ActionInButton  lda wndParam0
                cmp #EC_LBTNPRESS
                bne +
                ; Left btn press in button
                lda ControlBits
                ora #BIT_CTRL_ISPRESSED
                bne finalize; jmp finalize
+               cmp #EC_LBTNRELEASE
                bne + ; Mouse was moved (only occurs when mouse enters or leaves button)
                ; Left btn released
                lda ControlBits
                and #($ff-BIT_CTRL_ISPRESSED)
                ;jmp finalize
finalize        sta ControlBits
                jsr UpdateControl
+               jsr PaintControl
                jmp WindowToScreen

ActionInColorPicker
                lda wndParam0
                cmp #EC_LBTNPRESS
                beq +
                rts
+               lda wndParam1
                beq ++
                ; Click in menu mode
                jsr IsInCurMenu
                bcs +
                rts
+               jsr GetMenuItem
                ldy res
                jsr SetCtrlColor
++              ; Click in normal mode
                ; Fill menu struct
                lda #ID_MENU_COLORPICKER
                sta CurMenuID
                ;lda #MT_COLORPICKER = ID_MENU_COLORPICKER
                sta CurMenuType
                lda #4
                sta CurMenuWidth
                lda #18
                sta CurMenuHeight
                lda #<Menu_ColorPicker
                sta $fb
!ifdef MAC{                
                sta CurrentMenu
}
                lda #>Menu_ColorPicker
                sta $fc
!ifdef MAC{
                sta CurrentMenu+1
}
                ; Paint color picker menu
                jsr PaintMenuToBuf
                lda #<DESKTOP_BUF
                sta $fb
                lda #>DESKTOP_BUF
                sta $fc
                +LDA_WM <CLR_BUF,<DESKTOP_CLR_BUF
                sta $02
                +LDA_WM >CLR_BUF,>DESKTOP_CLR_BUF
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
!ifdef WIN{
                lda ControlPosY
                clc
                adc WindowPosY
                tax
                sec
                sbc #5
                bmi +
                ldx #4
} else ifdef MAC{
                lda WindowPosY
                clc
                adc ControlPosY
                tax
                inx
                inx
                cpx #8
                bcc +
                ldx #7
}
+               lda ControlPosX
                clc
                adc WindowPosX
                tay
                iny
                iny
                cpy #37
                bcc +
                tya
                ;sec
                sbc #6
                tay
+               sty CurMenuPosX
                stx CurMenuPosY; MenuPosY = 4 or = ControlPosY + WindowPosY
                jsr PosToScrMemFB
                jsr BufToScreen
                lda #PM_MENU
                sta ProgramMode
                rts

;!ifdef WIN{
;GetMenubarWidth lda #0
;                sta $02
;                lda WindowCtrlPtr
;                sta $fb
;                lda WindowCtrlPtr+1
;                sta $fc
;                ldy #CTRLSTRUCT_NUMSTRINGS
;                lda ($fb),y
;                tax
;                ; Write string address to FDFE
;                ldy #CTRLSTRUCT_STRINGS
;                jsr AddrInFBtoFD
;                ; Go
;-               jsr GetStrLen
;                lda $02
;                clc
;                adc res
;                clc
;                adc #2; 2 for every menu item
;                sta $02
;                inc res
;                lda res
;                jsr AddToFD
;                dex
;                bne -
;                lda $02
;                sta res
;                rts
;}

; Copies local ControlStruct to memory
UpdateControl   ldy #15
-               lda ControlIndex,y
                sta (ControlOnHeap),y
                dey
                bpl -
                rts

; Assigns string list in Addr(X,Y) to cur control
; Input:
; XY: address of string list
; A: number of strings in list
SetCtrlStringList
                sta ControlNumStr
; Assigns string in Addr(X,Y) to cur control
; Input:
; XY: address of string list
SetCtrlString   stx ControlStrings
                sty ControlStrings+1
                jmp UpdateControl

;SetCtrlString   stx ControlStrings
;                sty ControlStrings+1
                ;stx $02
;                sty $03
;                sta Param0
;                bne +
;                lda #0
;                sta ZP_5F
;                jmp ++
;+               ldy #$ff
;                ldx #0
;                ;
;-               iny
;                lda ($02),y
;                bne -
;                inx
;                cpx Param0
;                beq +
;                jmp -
;+               iny
;                sty ZP_5F
;++              lda $02
;                clc
;                adc ZP_5F
;                sta ControlStrings
;                lda $03
;                adc #0
;                sta ControlStrings+1
;                jmp UpdateControl

; Finds control index in cur wnd from mouse pos in MousePosInWndX/Y
; Returns control index in res ($ff if mouse is in no control)
; If successfull, control is cur control
; Does NOT select control
GetCtrlFromPos  ldx WindowNumCtrls
                dex
-               txa
                jsr SelectControl
                jsr IsInCurControl
                bcc +
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
IsInCurControl  lda MousePosInWndX
                cmp ControlPosX
                bcc +
                sbc ControlPosX
                cmp ControlWidth
                bcs +
                ;
                lda MousePosInWndY
                cmp ControlPosY
                bcc +
                sbc ControlPosY
                cmp ControlHeight
                bcs +
                sec
                rts
+               clc
                rts

;; Checks if mouse cursor is in control at FBFC
;; Result in carry
;IsInCtrlMiddle  ldy #CTRLSTRUCT_POSX
;                lda ($fb),y
;                cmp MousePosInWndX
;                bcs +
;                ldy #CTRLSTRUCT_WIDTH
;                adc ($fb),y
;                tax
;                dex
;                dex
;                cpx MousePosInWndX
;                bcc +
;                ;
;                ldy #CTRLSTRUCT_POSY
;                lda ($fb),y
;                cmp MousePosInWndY
;                bcs +
;                ldy #CTRLSTRUCT_HEIGHT
;                adc ($fb),y
;                tax
;                dex
;                dex
;                cpx MousePosInWndY
;                rts
;+               clc
;                rts

; Checks if mouse cursor is in current control
; Result in carry
IsInCtrlMiddle  lda ControlPosX
                cmp MousePosInWndX
                bcs +
                adc ControlWidth
                tax
                dex
                dex
                cpx MousePosInWndX
                bcc +
                ;
                lda ControlPosY
                cmp MousePosInWndY
                bcs +
                adc ControlHeight
                tax
                dex
                dex
                cpx MousePosInWndY
                rts
+               clc
                rts

; Sets carret position (A) and max_strlen (X)
SetEditSLInfo   sta ControlIndex+EDITSL_CARETPOS
                stx ControlIndex+EDITSL_MAX_STRLEN
                jmp UpdateControl

; Sets control color (Y)
SetCtrlColor    sty ControlColor
                jmp UpdateControl

!ifdef MAC{
; Selects control in Y
; Adds Bits in A
; Updates control
SelCtrl_AddBits pha; push bits
                tya
                jsr SelectControl; uses Y
                pla; pull bits
                ora ControlBits
                sta ControlBits
                jmp UpdateControl
}

;; Selects control in A and sets ID in X
;; Updates control!
;SelCtrlAndSetID jsr SelectControl
;                stx ControlID
;                jmp UpdateControl

SelectControl0  lda #0
                beq SelectControl

SelectControl2  lda #2
                bne SelectControl

SelectControl3  lda #3
                bne SelectControl

SelectControl4  lda #4
                bne SelectControl

SelectControl1  lda #1
; Copies control struct of control with index in A
; into local control struct
; Expects control index in A
SelectControl   asl
                asl
                asl
                asl
                clc
                adc WindowCtrlPtr
                sta ControlOnHeap
                lda WindowCtrlPtr+1
                adc #0
                sta ControlOnHeap+1
                ;
HeapToStatCtrl  ldy #15
-               lda (ControlOnHeap),y
                sta ControlIndex,y
                dey
                bpl -
                rts