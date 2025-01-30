; Paints controls into current window buffer
PaintControls   ; Get number of controls
                lda WindowNumCtrls
                bne +
                rts
+               ; Get pointer to first control
                lda WindowCtrlPtr
                sta ControlOnHeap
                lda WindowCtrlPtr+1
                sta ControlOnHeap+1
                lda #0
                sta control_counter
                jmp +
-               ; Increase ControlOnHeap by 16
                lda ControlOnHeap
                clc
                adc #16
                sta ControlOnHeap
                bcc +
                inc ControlOnHeap+1
+               ; Increase control_counter
                jsr PaintControl
                inc control_counter
                lda control_counter
                cmp WindowNumCtrls
                bcc -
                rts

; Paints control in ControlOnHeap into current window buffer
PaintControl    lda ControlOnHeap
                sta $fb
                lda ControlOnHeap+1
                sta $fc
                ; Fill static control struct
                ldy #15
-               lda ($fb),y
                sta ControlParent,y
                dey
                bpl -
                ; Check if control is maximized and adjust if necessary
                lda ControlBits
                and #BIT_CTRL_ISMAXIMIZED
                beq PaintCurCtrl
                jsr MaximizeCurCtrl
PaintCurCtrl    ; Check for types
                lda ControlType
                cmp #CT_MENUBAR
                bne +
                jmp PaintMenuBar
+               cmp #CT_BUTTON
                bne +
                jmp PaintButton
+               cmp #CT_LISTBOX
                bne +
                jmp PaintListBox
+               cmp #CT_FILELISTSCROLLBOX
                bne +
                jmp PaintFileListScrollBox
+               cmp #CT_LABEL
                bne +
                jmp PaintLabel
+               cmp #CT_LABEL_ML
                bne +
                jmp PaintLabel_ML
+               cmp #CT_FRAME
                bne +
                jmp PaintFrame
+               cmp #CT_COLORPICKER
                bne +
                jmp PaintColorpicker
+               cmp #CT_RADIOBUTTONGROUP
                bne +
                jmp PaintRadioButtonGroup
+               cmp #CT_UPDOWN
                bne +
                jmp PaintUpDown
+               cmp #CT_EDIT_SL
                bne +
                jmp PaintEditSL
+               cmp #CT_PROGRESSBAR
                bne +
                jmp PaintProgressBar
+               cmp #CT_COLBOXLABEL
                bne +
                jmp PaintColBoxLabel
+               cmp #CT_TEXTVIEWBOX
                bne +
                jmp PaintTextViewBox
+               rts

;--------------------------------------------------------------
; All control paint functions refer to control at ControlOnHeap
; (comes also in FB)
;--------------------------------------------------------------

offsetL         !byte 0
offsetR         !byte 0
PaintMenuBar    lda WndAddressInBuf
                sta $fb
                sta $02
                lda WndAddressInBuf+1
                sta $fc
                clc
                adc #$04
                sta $03
                lda BufWidth
                jsr AddToFB
                jsr AddBufWidthTo02
                ;
                ldy WindowWidth
                dey
                lda #41
                sta ($fb),y
                dey
                lda #4
-               sta ($fb),y
                dey
                bpl -
                ldy #0
                lda #37
                sta ($fb),y
                ; Paint menu entries
                lda #1
                jsr AddToFB
                ;+AddValToFB 1
                lda ControlStrings
                sta $fd
                lda ControlStrings+1
                sta $fe
                ldx ControlNumStr
                dex
-               jsr PrintIntString
                inc res
                lda res
                jsr AddToFD
                inc res
                lda res
                jsr AddToFB
                dex
                bpl -
                ; Color
                ldy WindowWidth
                dey
                lda CSTM_WindowClr
-               sta ($02),y
                dey
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

PaintProgressBar
                jsr GetCtrlBufPos
                ldy ControlWidth
                cpy #3
                bcs +
                rts
+               dey
                lda #12
                sta ($fd),y
                lda #11
                dey
-               sta ($fd),y
                dey
                bne -
                lda #10
                sta ($fd),y
                ; Color: white
                ldy ControlWidth
                dey
                lda #CL_WHITE
-               sta ($02),y
                dey
                bpl -
                ; Color: darkblue
                lda ControlParent+CTRLSTRUCT_VAL_LO
                sta multiplier
                lda ControlParent+CTRLSTRUCT_VAL_HI
                sta multiplier+1
                lda ControlWidth
                sta multiplicand
                lda #0
                sta multiplicand+1
                jsr Mult16
                ;
                lda ControlParent+CTRLSTRUCT_MAX_LO
                sta divisor
                lda ControlParent+CTRLSTRUCT_MAX_HI
                sta divisor+1
                jsr Divide16Bit
                ;
                lda ControlWidth
                cmp dividend
                bcs +
                sta dividend
+               ldy dividend
                dey
                bmi +
                lda #CL_DARKBLUE
-               sta ($02),y
                dey
                bpl -
+               rts

times3          !byte 0,3,6,9,12,15,18,21,24,27,30,33,36,39
NybbleToHex     !byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$81,$82,$83,$84,$85,$86
WidthMinus3     !byte 0
WidthMinus4     !byte 0
HeightMinus2    !byte 0
BytesPerLine    !byte 0
PaintTextViewBox
                jsr PaintListBox
                jsr PaintScrollbar
                ; Find control pos in buffers and adjust
                jsr GetCtrlBufPos
                jsr AddBufWidthToFD
                lda #1
                jsr AddToFD
                ; Get ptr to string list in FBFC
                lda ControlParent+CTRLSTRUCT_TOPLO
                sta $fb
                lda ControlParent+CTRLSTRUCT_TOPHI
                sta $fc
                ;
                lda ControlBits
                and #BIT_CTRL_UPPERCASE
                beq +
                lda #<PetUCtoDesktop
                sta SMC_Convert+1
                lda #>PetUCtoDesktop
                sta SMC_Convert+2
                jmp ++
+               lda #<PetLCtoDesktop
                sta SMC_Convert+1
                lda #>PetLCtoDesktop
                sta SMC_Convert+2
                ;
++              lda ControlWidth
                sec
                sbc #3
                sta WidthMinus3
                sta WidthMinus4
                dec WidthMinus4
                lda ControlHeight
                sec
                sbc #2
                sta HeightMinus2
                ;
                lda ControlParent+CTRLSTRUCT_ISTEXT
                beq ++++
                ; Text representation
                ; 
                lda WidthMinus3
                sta BytesPerLine
                ldx #0
--              ldy #0
-               lda ($fb),y
                cmp #13
                bne SMC_Convert
                iny
                jmp +++
SMC_Convert     jsr $FFFF
                sta ($fd),y
                iny
                cpy WidthMinus3
                bcc -
                ; Return would be at start of next line
                lda ($fb),y
                cmp #13
                bne +++
                iny
+++             tya
                jsr AddToFB
                jsr AddBufWidthToFD
                inx
                cpx HeightMinus2
                bcc --
                rts
++++            ; Hex representation
                ;
                ldx #0
--              ldy #0
-               ; Check if EOF is reached
                stx dummy
                ldx $fc
                tya
                clc
                adc $fb
                bcc +
                inx
+               cmp ViewerEOF
                txa
                sbc ViewerEOF+1
                bcc +
                rts
+               ldx dummy
                ; Paint one byte
                lda ($fb),y
                pha
                lsr
                lsr
                lsr
                lsr
                stx dummy+1
                tax
                lda times3,y
                sty dummy
                tay
                lda NybbleToHex,x
                sta ($fd),y
                pla
                and #%00001111
                tax
                lda NybbleToHex,x
                ldx dummy+1
                iny
                sta ($fd),y
                ;
                ldy dummy
                iny
                lda times3,y
                cmp WidthMinus4
                bcc -
                sty BytesPerLine
                tya
                jsr AddToFB
                jsr AddBufWidthToFD
                inx
                cpx HeightMinus2
                bcc --
                rts

PaintListBox    lda ControlColor
                sta BoxColor
                lda ControlPosX
                sta BoxPosX
                lda ControlPosY
                sta BoxPosY
                inc BoxPosY
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                inc BoxPosY
+               lda ControlWidth
                sta BoxWidth
                lda ControlHeight
                sta BoxHeight
                jmp PaintBoxToBuf

scroll_caret_pos    !byte 0
scroll_caret_max    !byte 0
scroll_caret_height !byte 0
top_index           !byte 0

; Compute (top_index / ControlNumStr) * scroll_caret_max
; Result in res
GetScrollPos    lda top_index
                sta $fd
                ldx ControlNumStr
                dex
                stx $fc
                jsr DivideFDbyFC
                lda $fd
                cmp #1
                bne +
                lda scroll_caret_max
                sta res
                rts
+               lda scroll_caret_max
                sta $fd
                jsr MultiplyFDbyFE
                stx $fc
                sta $fd
                lda #0
                sta $fb
                ;
                ldx #7
-               lsr $fd
                ror $fc
                ror $fb
                dex
                bpl -
                ldx $fc
                lda $fb
                cmp #$80
                bcc +
                inx
+               stx res
                rts

; Paints scrollbar in cur control in buffers
PaintScrollbar  lda ControlHeight
                cmp #4
                bcs +
                ; Cancel if control not high enough
                rts
+               ; Find control pos in buffers
                jsr GetCtrlBufPos
                ; Paint symbols
                jsr AddBufWidthToFD
                ldy ControlWidth
                dey
                lda #5
                sta ($fd),y
                ldx ControlHeight
                dex
                dex
                dex
-               jsr AddBufWidthToFD
                dex
                bne -
                lda #6
                sta ($fd),y
                lda WindowBits
                and #BIT_WND_RESIZABLE
                beq +
                jsr AddBufWidthToFD
                lda #43
                sta ($fd),y
+               ; Fill with window color
                ldy ControlWidth
                dey
                ldx ControlHeight
                dex
-               lda CSTM_WindowClr
                sta ($02),y
                jsr AddBufWidthTo02
                dex
                bpl -
                ; Paint scroll caret in scrollbar
                ;
                lda ControlTopIndex
                bne +
                ldx ControlHeight
                dex
                dex
                cpx ControlNumStr
                bcc +
                rts
+               ; Get scroll caret pos
                lda ControlHeight
                sec
                sbc #5
                sta scroll_caret_max
                lda ControlTopIndex
                sta top_index 
                jsr GetScrollPos
                lda res
                sta scroll_caret_pos
                ; Get scroll caret height
                lda ControlNumStr
                sec
                sbc ControlHeight
                tax
                inx
                inx
                stx top_index
                jsr GetScrollPos
                lda scroll_caret_max
                sec
                sbc res
                tax
                inx
                stx scroll_caret_height
                ; Paint
                jsr GetCtrlBufPos
                ldx scroll_caret_pos
                inx
                inx
-               jsr AddBufWidthToFD
                dex
                bne -
                ldy ControlWidth
                dey
                ldx scroll_caret_height
-               lda #28
                sta ($fd),y
                jsr AddBufWidthToFD
                dex
                bne -
                rts

PaintFileListScrollBox
                jsr PaintListBox
                lda ControlNumStr
                bne +
                rts
+               ; Adjust top index
                lda ControlTopIndex
                clc
                adc ControlHeight
                tax
                dex
                dex
                dex
                cpx ControlNumStr
                bcc ++
                lda ControlNumStr
                sec
                sbc ControlHeight
                tax
                inx
                inx
                stx ControlTopIndex
                bpl +
                lda #0
                sta ControlTopIndex
+               jsr UpdateControl
++              ; Find control pos in buffers and adjust
                jsr GetCtrlBufPos
                jsr AddBufWidthToFD
                ;+AddByteToFD BufWidth
                lda #1
                jsr AddToFD
                ;+AddValToFD 1
                ; Get ptr to string list in FBFC
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                ldx ControlTopIndex
                beq +
-               lda #32
                jsr AddToFB
                ;+AddValToFB 32
                dex
                bne -
+               ; Print strings
                ldx ControlHeight
                dex
                dex
                stx dummy+1
                lda #1
                sta dummy
-               lda ControlNumStr
                sec
                sbc ControlTopIndex
                cmp dummy
                bcc +
                lda dummy+1
                cmp dummy
                bcc +
                ;jsr PrintStringUC
                jsr PrintDirString
                lda #32
                jsr AddToFB
                ;+AddValToFB 32
                jsr AddBufWidthToFD
                ;+AddByteToFD BufWidth
                inc dummy
                jmp -
+               ; Highlight line if possible
                lda ControlHilIndex
                bmi +
                cmp ControlTopIndex
                bcc +
                sec
                sbc ControlTopIndex
                tax
                inx
                cpx dummy
                bcs +
-               jsr AddBufWidthTo02
                ;+AddByteTo02 BufWidth
                dex
                bne -
                ldy WindowWidth
                dey
                dey
                dey
                lda CSTM_SelectClr
-               sta ($02),y
                dey
                bne -
+               jmp PaintScrollbar

PaintLabel      ; Find control pos in buffers
                jsr GetCtrlBufPos
paint_label     lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                lda ControlBits
                and #BIT_CTRL_UPPERCASE
                beq +
                jmp PrintStringUC
+               jmp PrintStringLC

PaintColBoxLabel
                jsr GetCtrlBufPos
                lda #3
                jsr AddToFD
                ;+AddValToFD 3
                jsr paint_label
                lda #3
                sta Val
                jsr SubValFromFD
                ;+SubValFromFD 3
                ldy #0
                lda #10;#92
                sta ($fd),y
                lda ControlColor
                sta ($02),y
                iny
                lda #12
                sta ($fd),y
                lda ControlColor
                sta ($02),y
                rts

PaintLabel_ML   ; Find control pos in buffers
                jsr GetCtrlBufPos
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                jmp PrintStringLC_ML

PaintEditSL     ; Find control pos in buffers
                jsr GetCtrlBufPos
                lda $fd
                clc
                adc BufWidth
                sta $06
                lda $fe
                adc #0
                sta $07
                lda $06
                clc
                adc BufWidth
                sta $08
                lda $07
                adc #0
                sta $09
                ; Paint edit box
                ldy ControlWidth
                dey
                lda #31
                sta ($fd),y
                lda #37
                sta ($06),y
                lda #33
                sta ($08),y
                dey
-               lda #39
                sta ($fd),y
                lda #4
                sta ($06),y
                lda #36
                sta ($08),y
                dey
                bne -
                lda #30
                sta ($fd),y
                lda #41
                sta ($06),y
                lda #32
                sta ($08),y
                ; Paint string
                +AddValTo06 1
                lda ControlStrings
                sta $fd
                lda ControlStrings+1
                sta $fe
                ldy ControlParent+CTRLSTRUCT_CARRETPOS
                beq ++
                dey
                lda ControlBits
                and #BIT_CTRL_UPPERCASE
                bne +
-               lda ($fd),y
                jsr PetLCtoDesktop
                sta ($06),y
                dey
                bpl -
                jmp ++
+
-               lda ($fd),y
                jsr PetUCtoDesktop
                sta ($06),y
                dey
                bpl -
++              ; ... and carret
                lda WindowFocCtrl
                cmp ControlIndex
                bne +
                lda #29
                ldy ControlParent+CTRLSTRUCT_CARRETPOS
                sta ($06),y
+               ; Color
                jsr AddBufWidthTo02
                ;+AddByteTo02 BufWidth
                ldy ControlWidth
                dey
                dey
                lda #CL_WHITE
-               sta ($02),y
                dey
                bne -
                rts

PaintUpDown     jsr GetCtrlBufPos
                ; First row
                lda ControlBitsEx
                and #BIT_EX_CTRL_NOFRAME_TOP
                bne ++
                ldx #39
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_TOP
                beq +
                ldx #11
+               ldy #3
                txa
-               sta ($fd),y
                dey
                bne -
                ; Middle row
                jsr AddBufWidthToFD
                jsr AddBufWidthTo02
++              ldy #3
                lda #15
                sta ($fd),y
                ldx #37
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_RGT
                beq +
                ldx #34
+               ldy #4
                txa
                sta ($fd),y
                ;
                ldx #41
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_LFT
                beq +
                ldx #34
+               ldy #0
                txa
                sta ($fd),y
                ; Put in value
                ldy #1
                lda ControlParent+CTRLSTRUCT_DIGIT_HI
                clc
                adc #$b0
                sta ($fd),y
                iny
                lda ControlParent+CTRLSTRUCT_DIGIT_LO
                clc
                adc #$b0
                sta ($fd),y
                ; Set color
                ldy #3
                lda ControlColor
-               sta ($02),y
                dey
                bne -
                ; Third row
                lda ControlBitsEx
                and #BIT_EX_CTRL_NOFRAME_BTM
                bne ++
                jsr AddBufWidthToFD
                ldx #36
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_BTM
                beq +
                ldx #11
+               ldy #3
                txa
-               sta ($fd),y
                dey
                bne -
++              rts
                
;                jsr GetCtrlBufPos
;                ; First row
;                ldy #3
;-               lda ($fd),y
;                cmp #4
;                beq +
;                cmp #39
;                beq ++
;                lda #33
;                sta ($fd),y
;                jmp ++
;+               lda #39
;                sta ($fd),y
;++              dey
;                bne -
;                ; Middle row
;                jsr AddBufWidthToFD
;                jsr AddBufWidthTo02
;                ldy #0
;                lda ($fd),y
;                cmp #4
;                bne +
;                lda #41
;                sta ($fd),y
;                jmp ++
;+               lda #34
;                sta ($fd),y
;++              ldy #4
;                lda ($fd),y
;                cmp #4
;                bne +
;                lda #37
;                sta ($fd),y
;                jmp ++
;+               lda #34
;                sta ($fd),y
;++              ldy #3
;                lda #15
;                sta ($fd),y
;                ; Put in value
;                ldy #1
;                lda ControlParent+CTRLSTRUCT_DIGIT_HI
;                clc
;                adc #$b0
;                sta ($fd),y
;                iny
;                lda ControlParent+CTRLSTRUCT_DIGIT_LO
;                clc
;                adc #$b0
;                sta ($fd),y
;                ; Set color
;                ldy #3
;                lda ControlColor
;-               sta ($02),y
;                dey
;                bne -
;                ; Third row
;                jsr AddBufWidthToFD
;                ldy #3
;-               lda ($fd),y
;                cmp #4;#58
;                beq +
;                cmp #36
;                beq ++
;                lda #33
;                sta ($fd),y
;                jmp ++
;+               lda #36
;                sta ($fd),y
;++              dey
;                bne -
;                rts

PaintRadioButtonGroup
                jsr GetCtrlBufPos
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                lda $fd
                sta $02
                lda $fe
                sta $03
                ;
                ldx #0
-               ldy #0
                lda #13
                cpx ControlHilIndex
                bne +
                lda #14
+               sta ($fd),y
                lda #2
                jsr AddToFD
                ;+AddValToFD 2
                jsr PrintStringLC
                ldy res
                iny
                sty res
                tya
                jsr AddToFB
                ;+AddByteToFB res
                jsr AddBufWidthTo02
                ;+AddByteTo02 BufWidth
                lda $02
                sta $fd
                lda $03
                sta $fe
                inx
                cpx ControlNumStr
                bcc -
                rts

; Paints button in cur control in buffers
PaintButton     ; Button has a control height of 3
                lda ControlHeight
                cmp #3
                beq +
                rts
+               ; Find control pos in buffers
                jsr GetCtrlBufPos
                ; Prepare paint
                lda ControlStrings
                sta $0a
                lda ControlStrings+1
                sta $0b
                lda $fd
                clc
                adc BufWidth
                sta $06
                lda $fe
                adc #0
                sta $07
                lda $06
                clc
                adc BufWidth
                sta $08
                lda $07
                adc #0
                sta $09
                ; Paint
                lda ControlBits
                and #BIT_CTRL_ISPRESSED
                bne +
                ; Button is not pressed
                ldy ControlWidth
                dey
                lda #18
                sta ($fd),y
                lda #19
                sta ($06),y
                lda #20
                sta ($08),y
                ;
                dey
-               lda #17
                sta ($fd),y
                dey
                lda ($0a),y
                jsr PetLCtoDesktop
                iny
                sta ($06),y
                lda #21
                sta ($08),y
                dey
                bne -
                ; y=0
                lda #16
                sta ($fd),y
                lda #37
                sta ($06),y
                lda #22
                sta ($08),y
                rts
+               ; Button is pressed
                ; Paint first without string
                ldy ControlWidth
                dey
                lda #39
                sta ($fd),y
                lda #41
                sta ($06),y
                lda #24
                sta ($08),y
                dey
-               lda #39
                sta ($fd),y
                dey
                bne -
                ; y=0
                lda #23
                sta ($fd),y
                lda #27
                sta ($06),y
                lda #26
                sta ($08),y
                ; Handle string
                ldy ControlWidth
                dey
                dey
                tya
                asl
                asl
                asl
                clc
                adc #<DT_Reserved
                sta $0c
                lda #>DT_Reserved
                adc #0
                sta $0d
                ; Prepare copy char
                lda #<CHARBASE
                sta smc1+1
                lda #>CHARBASE
                sta smc2+1
                lda #<DT_Reserved
                sta smc3+1
                lda #>DT_Reserved
                sta smc4+1
                ; Copy chars to Reserved
                dey
-               lda ($0a),y
                jsr PetLCtoDesktop
                jsr CopyCharToReserved
                dey
                bpl -
                ; Prepare copy char 2nd
                lda $0c
                sta smc3+1
                lda $0d
                sta smc4+1
                ; Copy chars to Reserved 2nd
                ldy ControlWidth
                dey
                dey
                dey
-               lda #25
                jsr CopyCharToReserved
                dey
                bpl -
                jsr PressReserved_DT
                ; Bring chars from Reserved to buffer
                ldy ControlWidth
                dey
                dey
                dey
                tya
                clc
                adc #DT_Reserved_Char
                tax
-               txa
                iny
                sta ($06),y
                dey
                dex
                dey
                bpl -
                ;
                lda #DT_Reserved_Char
                sec
                sbc #2
                clc
                adc ControlWidth
                sta dummy
                ldy ControlWidth
                dey
                dey
                dey
                tya
                clc
                adc dummy
                tax
-               txa
                iny
                sta ($08),y
                dey
                dex
                dey
                bpl -
                rts

; Paints frame in cur control in buffers
PaintFrame      lda ControlHeight
                cmp #3
                bcs +
                rts
+               ; Find control pos in buffers
                jsr GetCtrlBufPos
                ; Paint
                ldy ControlWidth
                dey
                lda #9;#22
                sta ($fd),y
                dey
                lda #8;#23
-               sta ($fd),y
                dey
                bne -
                lda #7;#16
                sta ($fd),y
                ; Paint string
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                lda #1
                jsr AddToFD
                ;+AddValToFD 1
                jsr PrintStringLC
                lda #1
                sta Val
                jsr SubValFromFD
                ;+SubValFromFD 1
                ;
                ldx ControlHeight
                dex
                dex
-               jsr AddBufWidthToFD
                ;+AddByteToFD BufWidth
                ldy #0
                lda #37;#17
                sta ($fd),y
                ldy ControlWidth
                dey
                lda #41;#21
                sta ($fd),y
                dex
                bne -
                ;
                jsr AddBufWidthToFD
                ;+AddByteToFD BufWidth
                ldy ControlWidth
                dey
                lda #42;#20
                sta ($fd),y
                dey
                lda #39;#19
-               sta ($fd),y
                dey
                bne -
                lda #38;#18
                sta ($fd),y
                rts

PaintColorpicker
                ; Find control pos in buffers
                jsr GetCtrlBufPos
                ldy #0
                lda #10
                sta ($fd),y
                lda ControlColor
                sta ($02),y
                iny
                sta ($02),y
                lda #12
                sta ($fd),y
                rts