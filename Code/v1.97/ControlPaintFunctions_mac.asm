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
                sta ControlIndex,y
                dey
                bpl -
                ; Check if control is maximized and adjust if necessary
                lda ControlBits
                and #BIT_CTRL_ISMAXIMIZED
                beq PaintCurCtrl
                jsr MaximizeCurCtrl
PaintCurCtrl    ; Check for types
                ldx ControlType
                dex
                lda PtrPaintRoutLo,x
                sta JmpPaint+1
                lda PtrPaintRoutHi,x
                sta JmpPaint+2
JmpPaint        jmp $ffff

PtrPaintRoutLo  !byte <PaintMenuBar, <PaintButton, <PaintListBox, <PaintFileListScrollBox, <PaintLabel, <PaintLabel_ML
                !byte <PaintFrame, <PaintColorpicker, <PaintRadioButtonGroup, <PaintUpDown, <PaintEditSL, <PaintProgressBar
                !byte <PaintColBoxLabel, <PaintTextViewBox
PtrPaintRoutHi  !byte >PaintMenuBar, >PaintButton, >PaintListBox, >PaintFileListScrollBox, >PaintLabel, >PaintLabel_ML
                !byte >PaintFrame, >PaintColorpicker, >PaintRadioButtonGroup, >PaintUpDown, >PaintEditSL, >PaintProgressBar
                !byte >PaintColBoxLabel, >PaintTextViewBox

;--------------------------------------------------------------
; All control paint functions refer to control at ControlOnHeap
; (comes also in FB)
;--------------------------------------------------------------

PaintMenuBar    rts

PaintProgressBar
                jsr GetCtrlBufPos
                ldy ControlWidth
                cpy #3
                bcs +
                rts
+               dey
                lda #12
                sta ($fd),y
                lda #53
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
                lda ControlIndex+CTRLSTRUCT_VAL_LO
                sta multiplier
                lda ControlIndex+CTRLSTRUCT_VAL_HI
                sta multiplier+1
                lda ControlWidth
                sta multiplicand
                lda #0
                sta multiplicand+1
                jsr Mult16
                ;
                lda ControlIndex+CTRLSTRUCT_MAX_LO
                sta divisor
                lda ControlIndex+CTRLSTRUCT_MAX_HI
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
                lda #CL_DARKGRAY
-               sta ($02),y
                dey
                bpl -
+               rts

ValForDev       !byte 0,0
; Used twice in PaintScrollbar
; Computes RoundUp([ValForDev / TotalLength] * ValForMult)
; where ValForDev < TotalLength
; ValForDev is either ClipStartRel or ClipLength
; Result in res
GetScrollValue  ; Dividend
                lda ValForDev+1
                sta N+3
                lda ValForDev
                sta N+2
                lda #0
                sta N+4
                sta N+5
                ; Divisor
                lda TotalLength
                sta N
                lda TotalLength+1
                sta N+1
                ; Divide
                jsr Div_3216
                ;
                lda N+5
                sta $fe
                lda ScrollBarArea
                sta $fd
                jsr MultiplyFDbyFE; end with ldx $fd (lobyte)
                bpl ++
                tax
                inx
                txa
++              sta res
                rts

; Set in PaintScrollbar routine
ClipStartRel      !byte 0,0
ClipLength        !byte 0,0
ScrollCaretPos    !byte 0
ScrollCaretHeight !byte 0
ScrollBarArea     !byte 0; height of scrollbar without arrows
; Required for PaintScrollbar routine
EndReached       !byte 0
TotalStart       !byte 0,0
TotalLength      !byte 0,0
ClipStart        !byte 0,0
ClipEnd          !byte 0,0
; Paints scrollbar in cur control in buffers
; Requires the following values set:
; EndReached, TotalStart, TotalLength, ClipStart, ClipEnd
PaintScrollbar  lda ControlHeight
                cmp #4
                bcs +
                ; Cancel if control not high enough
                rts
+               ; Find control pos in buffers
                jsr GetCtrlBufPos
                ;--------------------------------
                ; Paint empty scrollbar
                ;--------------------------------
                ldy ControlWidth
                dey
                ldx ControlHeight
                dex
-               lda CSTM_WindowClr
                sta ($02),y
                lda #41
                sta ($fd),y
                jsr AddBufWidthTo02
                jsr AddBufWidthToFD
                dex
                bpl -
                ; Paint symbols
                jsr GetCtrlBufPos
                ldy ControlWidth
                dey
                lda #40
                sta ($fd),y
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
+               ;--------------------------------
                ; Paint scroll caret in scrollbar
                ;--------------------------------
                ; Set ClipStartRel = ClipStart - TotalStart
                lda ClipStart
                sec
                sbc TotalStart
                sta ClipStartRel
                lda ClipStart+1
                sbc TotalStart+1
                sta ClipStartRel+1
                ; If strings fit in box: no scroll bar
                bne +
                lda ClipStartRel
                bne +
                ; ClipStart = TotalStart
                lda EndReached
                beq +
                rts
+               ; Set ClipLength = ClipEnd - ClipStart + 1
                lda ClipEnd
                sec
                sbc ClipStart
                sta ClipLength
                lda ClipEnd+1
                sbc ClipStart+1
                sta ClipLength+1
                inc ClipLength
                bne +
                inc ClipLength+1
+               ; Set ScrollBarArea = ControlHeight - 4
                lda ControlHeight
                sec
                sbc #4
                sta ScrollBarArea
                ;--------------------------------
                ; Get scroll caret height
                ;--------------------------------
                ; ScrollHeight = [ClipLength / TotalLength] * ScrollbarArea
                lda ClipLength
                sta ValForDev
                lda ClipLength+1
                sta ValForDev+1
                jsr GetScrollValue
                lda res
                sta ScrollCaretHeight
                bne +
                lda #1
                sta ScrollCaretHeight
+               ;--------------------------------
                ; Get scroll caret pos
                ;--------------------------------
                lda TotalLength
                sec
                sbc #1
                sta TotalLength
                lda TotalLength+1
                sbc #0
                sta TotalLength+1
                ; ScrollPos = [ClipStartRel / TotalLength] * ScrollbarArea
                lda ClipStartRel
                sta ValForDev
                lda ClipStartRel+1
                sta ValForDev+1
                jsr GetScrollValue
                inc TotalLength
                bne +
                inc TotalLength+1
+               lda res
                sta ScrollCaretPos
                lda ScrollBarArea
                sec
                sbc ScrollCaretHeight
                ; A = maximal possible pos
                cmp res
                bcs +
                sta ScrollCaretPos
+               ;--------------------------------
                ; Paint
                ;--------------------------------
                jsr GetCtrlBufPos
                ldx ScrollCaretPos
                inx
                inx
-               jsr AddBufWidthToFD
                dex
                bne -
                ldy ControlWidth
                dey
                ldx ScrollCaretHeight
-               lda #28
                sta ($fd),y
                jsr AddBufWidthToFD
                dex
                bne -
                rts

; If executed, add X+1 to FBFC to get next line position
; The zero flag says if something is in the current line
GetNextLinePos  ldy #0
                lda ($fb),y
                cmp #10
                bne +
                lda #1
                jsr AddToFB
+               ; Check for returns
-               lda ($fb),y
                cmp #13
                beq setX
                iny
                cpy WidthMinus3
                bcc -
                ; No return in line => Check for wordwrap
                ldy WidthMinus3
                iny
-               dey
                bmi +
                lda ($fb),y
                cmp #32
                bne -
                jmp setX
+               ; No space and no return at all
                ldx WidthMinus4
                ldy WidthMinus3
                ;bne PaintLine
                beq setX
                tya ; just to make sure Y=0 is dismissed after call
                rts
setX            ;
                tya
                tax
                lda ($fb),y
                cmp #13
                ;bne PaintLine
                beq +
                tya ; just to make sure Y=0 is dismissed after call
                rts
+               iny
                lda ($fb),y
                cmp #10
                bne +
                inx
+               dey
                rts

times3          !byte 0,3,6,9,12,15,18,21,24,27,30,33,36,39
NybbleToHex     !byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$81,$82,$83,$84,$85,$86
WidthMinus3     !byte 0
WidthMinus4     !byte 0
HeightMinus2    !byte 0
BytesPerLine    !byte 0
PaintTextViewBox
                dec ControlWidth
                jsr PaintListBoxLL
                inc ControlWidth
                ; Find control pos in buffers and adjust
                jsr GetCtrlBufPos
                jsr AddBufWidthToFD
                lda #1
                jsr AddToFD
                ; Write ptr to top pos in buffer to FBFC
                lda ControlIndex+CTRLSTRUCT_TOPLO
                sta $fb
                sta ClipStart
                lda ControlIndex+CTRLSTRUCT_TOPHI
                sta $fc
                sta ClipStart+1
                lda #0
                sta EndReached
                ; Set TotalStart and TotalLength
                lda #<FILEVIEWERBUF_START
                sta TotalStart
                lda #>FILEVIEWERBUF_START
                sta TotalStart+1
                lda ViewerEOF
                sec
                sbc TotalStart
                sta TotalLength
                lda ViewerEOF+1
                sbc TotalStart+1
                sta TotalLength+1
                bne +
                lda TotalLength
                bne +
                rts
+               ; Set HeightMinus2
                ldx ControlHeight
                dex
                dex
                stx HeightMinus2
                ; Branch to text or hex display
                lda ControlIndex+CTRLSTRUCT_ISTEXT
                bne TextRepr
                jmp HexRepr

CleanUp         lda BufWidth
                sta Val
                jsr SubValFromFD
                inx
                txa
                sta Val
                jsr SubValFromFB; one before last line
                ldy #$ff
                ;
-               iny
                cpy WidthMinus3
                bcs +
                lda #1
                jsr AddToFB
                lda $fb
                cmp ViewerEOF
                lda $fc
                sbc ViewerEOF+1
                bcc -
+               lda #160; space
-               sta ($fd),y
                iny
                cpy WidthMinus3
                bcc -
                rts

TextRepr        ;==============================
                ; Text representation
                ;==============================
                lda ControlWidth
                sec
                sbc #3
                sta WidthMinus3
                sta BytesPerLine
                sta WidthMinus4
                dec WidthMinus4
                ; Prepare char conversion
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
++              ldx HeightMinus2
                dex
                stx dummy; serves as line no
                ; Here is the loop
--              ; if eof reached, quit
                lda $fb
                cmp ViewerEOF
                lda $fc
                sbc ViewerEOF+1
                bcc +
                lda #1
                sta EndReached
                lda ViewerEOF
                sta ClipEnd
                lda ViewerEOF+1
                sta ClipEnd+1
                jsr CleanUp
                jmp PaintScrollbar
+               ; if clip area filled, quit
                lda dummy
                bpl +
                lda $fb
                sta ClipEnd
                lda $fc
                sta ClipEnd+1
                jmp PaintScrollbar
+               ; Loop through lines
                jsr GetNextLinePos
                beq ++
                ; Paint line (y is one after last char in line)
                dey
-               lda ($fb),y
SMC_Convert     jsr $FFFF
                sta ($fd),y
                dey
                bpl -
++              ; Prepare for next line
                jsr AddBufWidthToFD
                ;Add (x+1) to FBFC
                inx
                txa
                jsr AddToFB
                dec dummy
                jmp --
HexRepr         ;==============================
                ; Hex representation
                ;==============================
                ; uses $02/$03 as running number
                lda ControlIndex+CTRLSTRUCT_TOPLO
                sec
                sbc #<FILEVIEWERBUF_START
                sta $02
                lda ControlIndex+CTRLSTRUCT_TOPHI
                sbc #>FILEVIEWERBUF_START
                sta $03
                ; Compute bytes per line
                lda ControlWidth
                sec
                sbc #6
                sta dummy
                ldx #$ff
                lda #0
-               inx
                clc
                adc #3
                cmp dummy
                bcc -
                stx BytesPerLine
                ;
                ldx #0  ; line number
--              ; Loop through lines
                stx dummy+1; line number
                ldy #0
                jsr PaintRunningNumber
                ;
                ldy #0
                lda #5
                jsr AddToFD
-               ; Loop through current line
                ; Check if EOF is reached
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
                lda #1
                sta EndReached
                sty dummy
                jmp ++
+               ; Paint one byte
                sty dummy; byte number
                lda ($fb),y
                pha
                lsr
                lsr
                lsr
                lsr
                tax
                lda times3,y
                tay
                lda NybbleToHex,x
                sta ($fd),y
                pla
                and #%00001111
                tax
                lda NybbleToHex,x
                iny
                sta ($fd),y
                ;
                ldx dummy+1; line number
                ldy dummy; byte number
                iny
                cpy BytesPerLine
                bcc -
                lda BytesPerLine
                jsr AddToFB
                lda BytesPerLine
                jsr AddTo02
                jsr AddBufWidthToFD
                lda #5
                sta Val
                jsr SubValFromFD
                inx
                cpx HeightMinus2
                bcc --
                ;
                lda BytesPerLine
                sta Val
                jsr SubValFromFB
++              lda $fb
                clc
                adc dummy
                sta ClipEnd
                lda $fc
                adc #0
                sta ClipEnd+1
                jmp PaintScrollbar

PaintRunningNumber
                lda $03
                jsr ConvertToHexWord
                lda HexWord
                sta ($fd),y
                iny
                lda HexWord+1
                sta ($fd),y
                iny
                lda $02
                jsr ConvertToHexWord
                lda HexWord
                sta ($fd),y
                iny
                lda HexWord+1
                sta ($fd),y
                rts

HexWord         !byte 0,0
; Converts hex value in A to two chars
; Uses X
; Result in HexWord
ConvertToHexWord
                pha
                lsr
                lsr
                lsr
                lsr
                tax
                lda NybbleToHex,x
                sta HexWord
                pla
                and #%00001111
                tax
                lda NybbleToHex,x
                sta HexWord+1
                rts

PaintListBoxLL  lda ControlColor
                sta BoxColor
                lda ControlPosX
                sta BoxPosX
                lda ControlPosY
                sta BoxPosY
                inc BoxPosY
                lda ControlWidth
                sta BoxWidth
                lda ControlHeight
                sta BoxHeight
                jmp PaintBoxToBuf

PaintListBox    jsr PaintListBoxLL
                jsr GetCtrlBufPos
                jsr AddBufWidthToFD
                jsr AddBufWidthTo02
                lda #1
                jsr AddToFD
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                ; Paint strings
--              ldy #$ff
-               iny
                lda ($fb),y
                beq +
                jsr PetLCtoDesktop
                sta ($fd),y
                jmp -
+               tya
                beq ++
                iny
                tya
                jsr AddToFB
                jsr AddBufWidthToFD
                jmp --
++              ; Highlight line if possible
                ldx ControlHilIndex
                beq +
                cpx #$ff
                beq ++
-               jsr AddBufWidthTo02
                dex
                bne -
+               ldy ControlWidth
                dey
                lda CSTM_SelectClr
-               sta ($02),y
                dey
                bpl -
++              rts

PaintFileListScrollBox
                dec ControlWidth
                jsr PaintListBoxLL
                inc ControlWidth
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
                cpx #230
                bcc +
                lda #0
                sta ControlTopIndex
+               jsr UpdateControl
++              ; Find control pos in buffers and adjust
                jsr GetCtrlBufPos
                jsr AddBufWidthToFD
                lda #1
                jsr AddToFD
                ; Get ptr to string list in FBFC
                lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                ldx ControlTopIndex
                beq +
-               lda #FILE_RECORD_LENGTH
                jsr AddToFB
                dex
                bne -
+               ; Print strings
                ldx ControlHeight
                dex
                dex
                stx dummy+1
                lda #1
                sta dummy
                ;
                ldx WindowType
                dex
                ldy Max_Fn_Len_Plus2,x
                iny
                cpy #14
                bcs +
                ldy #13
+               sty pos_of_size
                ; Prepare PrintDirString (upper/lower case)
                lda ShowLowerCase,x
                beq +
                lda #<PetLCtoDesktop
                sta SMC_PrintDirStr+1
                lda #>PetLCtoDesktop
                sta SMC_PrintDirStr+2
                jmp ++
+               lda #<PetUCtoDesktop
                sta SMC_PrintDirStr+1
                lda #>PetUCtoDesktop
                sta SMC_PrintDirStr+2
                ;
++              lda #0
                sta EndReached; Needed for scroll bar
-               ; Print Strings Loop
                                lda ControlNumStr
                sec
                sbc ControlTopIndex
                cmp dummy
                bcs +
                lda #1
                sta EndReached
                jmp ++
+               lda dummy+1
                cmp dummy
                bcc ++
                lda ControlBitsEx
                and #BIT_EX_CTRL_SHOWSIZES
                jsr PrintDirString
                lda #FILE_RECORD_LENGTH
                jsr AddToFB
                jsr AddBufWidthToFD
                inc dummy
                jmp -
++              ; Highlight line if possible
                jsr HighlightLine
                ; Paint the scroll bar (if necessary)
                lda #0
                sta TotalStart
                sta TotalStart+1
                sta TotalLength+1
                sta ClipStart+1
                sta ClipEnd+1
                lda ControlNumStr
                sta TotalLength
                lda ControlTopIndex
                sta ClipStart
                clc
                adc ControlHeight
                sec
                sbc #3
                sta ClipEnd
                jmp PaintScrollbar

HighlightLine   lda ControlHilIndex
                cmp #$ff
                beq +
                cmp ControlTopIndex
                bcc +
                sec
                sbc ControlTopIndex
                tax
                inx
                cpx dummy
                bcs +
-               jsr AddBufWidthTo02
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
+               rts

pos_of_size     !byte 0
; Prints dir string from FBFC to FDFE
; with upper case conversion
; A=1: with file sizes
; A=0: without file sizes
PrintDirString  ldx $fe
                cpx #$ff
                beq ++++
                pha
                ; Print file type
                ldy #19
                lda ($fb),y
                ldy #0
                cmp #$46; ("F"=folder)
                bne +
                lda #13
                jmp ++
+               cmp #"P"; PRG
                bne +
                lda #14
                jmp ++
+               jsr PetUCtoDesktop
++              sta ($fd),y
                ; Print filename
                ldy #2
-               lda ($fb),y
                beq +
SMC_PrintDirStr jsr $FFFF ;PetUCtoDesktop or PetLCtoDesktop
                sta ($fd),y
                iny
                jmp -
+               pla
                beq ++++
                ; Print file size
                ldy #0
                lda ($fb),y
                sta file_size
                iny
                lda ($fb),y
                sta file_size+1
                jsr ConvertToDecStr
                ldx #3
                ldy pos_of_size
                iny
                iny
                iny
-               lda str_file_size,x
                jsr PetUCtoDesktop
                sta ($fd),y
                dey
                dex
                bpl -
++++            rts

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
                lda #160
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
                ldy ControlIndex+CTRLSTRUCT_CARRETPOS
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
                ldy ControlIndex+CTRLSTRUCT_CARRETPOS
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

dbl_frame       !byte 0
PaintUpDown     jsr GetCtrlBufPos
                ; First row
                lda ControlBitsEx
                and #BIT_EX_CTRL_NOFRAME_TOP
                bne ++
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_TOP
                sta dbl_frame
                ldy #3
-               ldx #39
                lda dbl_frame
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #39
                beq +
                ldx #53
+               txa
                sta ($fd),y
                dey
                bne -
                ; Middle row
                jsr AddBufWidthToFD
                jsr AddBufWidthTo02
++              ldy #3
                lda #15
                sta ($fd),y
                lda CSTM_WindowClr
                sta ($02),y
                ;
                ldx #37
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_RGT
                sta dbl_frame
                ldy #4
                ldx #37
                lda dbl_frame
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #37
                beq +
                ldx #34
+               txa
                sta ($fd),y
                ;
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_LFT
                sta dbl_frame
                ldy #0
                ldx #41
                lda dbl_frame
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #41
                beq +
                ldx #34
+               txa
                sta ($fd),y
                ; Put in value
                ldy #1
                lda ControlIndex+CTRLSTRUCT_DIGIT_HI
                clc
                adc #$b0
                sta ($fd),y
                iny
                lda ControlIndex+CTRLSTRUCT_DIGIT_LO
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
                jsr AddBufWidthToFD
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_BTM
                sta dbl_frame
                ldy #3
-               ldx #36
                lda dbl_frame
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #36
                beq +
                ldx #53
+               txa
                sta ($fd),y
                dey
                bne -
                rts

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
                lda #56
                cpx ControlHilIndex
                bne +
                lda #57
+               sta ($fd),y
                lda #2
                jsr AddToFD
                jsr PrintStringLC
                ldy res
                iny
                sty res
                tya
                jsr AddToFB
                jsr AddBufWidthTo02
                lda $02
                sta $fd
                lda $03
                sta $fe
                inx
                cpx ControlNumStr
                bcc -
                rts

; Paints button in cur control in buffers
PaintButton     jsr GetCtrlBufPos
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
                ; Paint button frame
                ldy ControlWidth
                dey
                ldx #37
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_RGT
                beq +
                lda ($06),y
                cmp #160
                beq +
                cmp #37
                beq +
                ldx #34
+               txa
                sta ($06),y
                ;
                dey
-               ldx #39
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_TOP
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #39
                beq +
                ldx #53
+               txa
                sta ($fd),y
                ;
                ldx #36
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_BTM
                beq +
                lda ($08),y
                cmp #160
                beq +
                cmp #36
                beq +
                ldx #53
+               txa
                sta ($08),y
                dey
                bne -
                ; y=0
                ldx #41
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_LFT
                beq +
                lda ($06),y
                cmp #160
                beq +
                cmp #41
                beq +
                ldx #34
+               txa
                sta ($06),y
                ;
                lda ControlBits
                and #BIT_CTRL_ISPRESSED
                bne +
                ; Button not pressed
                ; ------------------
                ldy ControlWidth
                dey
                dey
-               dey
                lda ($0a),y
                iny
                jsr PetLCtoDesktop
                sta ($06),y
                dey
                bne -
                ; Color
                lda BufWidth
                jsr AddTo02
                ldy ControlWidth
                dey
                dey
                lda CSTM_ButtonClr
-               sta ($02),y
                dey
                bne -
                rts
+               ; Button is pressed
                ; -----------------
                ; Color
                lda BufWidth
                jsr AddTo02
                ldy ControlWidth
                dey
                dey
                lda #CL_WHITE
-               sta ($02),y
                dey
                bne -
                ; Paint string inverted
                ldy ControlWidth
                dey
                dey ; string length
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
                jsr MapOutIO
                dey
-               lda ($0a),y
                jsr PetLCtoDesktop
                jsr CopyCharToReserved
                dey
                bpl -
                jsr InvertReserved_DT
                jsr MapInIO
                ; Bring chars from Reserved to buffer
                ldy ControlWidth
                dey
                dey
                dey
                tya
                clc
                adc #DT_Reserved_Char; last char
                tax
-               txa
                iny
                sta ($06),y
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
                jsr PrintStringLC
                lda #1
                sta Val
                jsr SubValFromFD
                ;
                ldx ControlHeight
                dex
                dex
-               jsr AddBufWidthToFD
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