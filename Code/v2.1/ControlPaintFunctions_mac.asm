CtrlStringsToFB lda ControlStrings
                sta $fb
                lda ControlStrings+1
                sta $fc
                rts

CtrlStringsToFD lda ControlStrings
                sta $fd
                lda ControlStrings+1
                sta $fe
                rts

; Paints controls into current window buffer
PaintControls   ; Get number of controls
                lda WindowNumCtrls
                beq ++
                ; Get pointer to first control
                lda WindowCtrlPtr
                sta ControlOnHeap
                lda WindowCtrlPtr+1
                sta ControlOnHeap+1
                lda #0
                sta control_counter
                jmp +
-               ; Increase ControlOnHeap by 16
                lda ControlOnHeap
                ;clc
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
++              rts

; Paints control in ControlOnHeap into current window buffer
PaintControl    ; Fill static control struct
                jsr HeapToStatCtrl
                ; Check if control is maximized and adjust if necessary
                lda ControlBits
                and #BIT_CTRL_ISMAXIMIZED
                beq PaintCurCtrl
                jsr MaximizeCurCtrl
PaintCurCtrl    jsr GetCtrlBufPos
                ; Check for types
                ldx ControlType
                cpx #MIN_CT_APP
                bcs ++
                dex
                cpx #MAX_CT_ACTION
                bcc +
                txa
                sec
                sbc #(MIN_CT_NOACTION - MAX_CT_ACTION - 1)
                tax
+               lda PtrPaintRoutLo,x
                sta JmpPaint+1
                lda PtrPaintRoutHi,x
                sta JmpPaint+2
JmpPaint        jmp $ffff
++              jmp (App_PaintCtrls)

PtrPaintRoutLo  !byte <PaintMenuBar, <PaintButton, <PaintListBox, <PaintFileListScrollBox, <PaintColorPicker
                !byte <PaintRadioButtonGroup, <PaintUpDown, <PaintEditSL, <PaintTextViewBox, <PaintCheckBox
                ; No action (from ID 31 onwards)
                !byte <PaintLabel, <PaintLabel_ML, <PaintFrame, <PaintProgressBar, <PaintColBoxLabel

PtrPaintRoutHi  !byte >PaintMenuBar, >PaintButton, >PaintListBox, >PaintFileListScrollBox, >PaintColorPicker
                !byte >PaintRadioButtonGroup, >PaintUpDown, >PaintEditSL, >PaintTextViewBox, >PaintCheckBox
                ; No action (from ID 31 onwards)
                !byte >PaintLabel, >PaintLabel_ML, >PaintFrame, >PaintProgressBar, >PaintColBoxLabel

;--------------------------------------------------------------
; All control paint functions refer to control at ControlOnHeap
; (comes also in FB)
;--------------------------------------------------------------

PaintMenuBar    rts

PaintProgressBar
                ldy ControlWidth
                dey
                lda #12
                sta ($fd),y
                dey
                lda #11
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
                lda ControlIndex+PROGBAR_VAL_LO
                sta multiplier
                lda ControlIndex+PROGBAR_VAL_HI
                sta multiplier+1
                lda ControlWidth
                sta multiplicand
                lda #0
                sta multiplicand+1
                jsr Mult16
                ;
                lda ControlIndex+PROGBAR_MAX_LO
                sta divisor
                lda ControlIndex+PROGBAR_MAX_HI
                sta divisor+1
                jsr Divide16Bit
                ;
                ldy dividend
                lda ControlWidth
                cmp dividend
                bcs +
                tay
+               dey
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
;ScrollCaretPos    !byte 0 ZP
;ScrollCaretHeight !byte 0 ZP
ScrollBarArea     !byte 0; height of scrollbar without arrows
; Required for PaintScrollbar routine
;EndReached       !byte 0 ZP
;TotalLength      !byte 0,0 ZP
;ClipStart        !byte 0,0 ZP
;ClipEnd          !byte 0,0 ZP
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
                bne +
                dec TotalLength+1
+               dec TotalLength
                ;sec
                ;sbc #1
                ;sta TotalLength
                ;lda TotalLength+1
                ;sbc #0
                ;sta TotalLength+1
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
;---------------------------------------------------------------

;------------- TextViewBox -------------------------------------
; Used in PaintTextViewBox
; If executed, add X+1 to FBFC to get next line position
; The zero flag says if something is in the current line
GetNextLinePos  ldy #0
                lda ($fb),y
                cmp #10
                bne +
                lda #1
                jsr AddToFB

+               ; Bytes bis EOF
                lda ViewerEOF
                sec
                sbc $fb
                sta ZP_60
                lda ViewerEOF+1
                sbc $fc
                beq .samePage

                ; Mehr als 255 Bytes übrig.
                ; Für setX reicht $ff als Grenze.
                lda #$ff
                sta ZP_60
                jmp .normal

.samePage
                lda ZP_60
                beq .empty

                ; <= WidthMinus3:
                ; letzte (evtl. volle) Zeile, nicht hinter EOF lesen
                cmp WidthMinus3
                bcc .lastLine
                beq .lastLine

.normal
                ; Check for returns
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
                beq setX

+               ; No space and no return at all
                ldx WidthMinus3
                dex
                ldy WidthMinus3
                tya
                bne ++

.lastLine
                ldy #0
.last
                lda ($fb),y
                cmp #13
                beq setX
                iny
                cpy ZP_60
                bcc .last

                ; Kein Return mehr: alle Restbytes anzeigen
                ldx ZP_60
                dex
                ldy ZP_60
                tya
                rts

.empty
                ldx #$ff
                ldy #0
                tya
                rts

setX
                tya
                tax
                lda ($fb),y
                cmp #13
                bne ++

                iny
                cpy ZP_60
                bcs +
                lda ($fb),y
                cmp #10
                bne +
                inx
+               dey
++              tya
                rts

;GetNextLinePos  ldy #0
;                lda ($fb),y
;                cmp #10
;                bne +
;                lda #1
;                jsr AddToFB
;+               ; Check for returns
;-               lda ($fb),y
;                cmp #13
;                beq setX
;                iny
;                cpy WidthMinus3
;                bcc -
;                ; No return in line => Check for wordwrap
;                ldy WidthMinus3
;                iny
;-               dey
;                bmi +
;                lda ($fb),y
;                cmp #32
;                bne -
;                beq setX
;+               ; No space and no return at all
;                ldx WidthMinus3
;                dex
;                ldy WidthMinus3
;                tya ; just to make sure Y=0 is dismissed after call
;                bne ++
;setX            ;
;                tya
;                tax
;                lda ($fb),y
;                cmp #13
;                ;bne PaintLine
;                beq +
;                tya ; just to make sure Y=0 is dismissed after call
;                rts
;+               iny
;                lda ($fb),y
;                cmp #10
;                bne +
;                inx
;+               dey
;++              rts

NybbleToHex     !byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$81,$82,$83,$84,$85,$86
;WidthMinus3     !byte 0 ZP
;HeightMinus2    !byte 0 ZP
;BytesPerLine    !byte 0 ZP
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
                lda ControlIndex+TEXTVIEWBOX_TOPLO
                sta $fb
                sta ClipStart
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sta $fc
                sta ClipStart+1
                lda #0
                sta EndReached
                ; Set TotalStart and TotalLength
                ;lda #<FILEVIEWERBUF_START = 0
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
                lda ControlIndex+TEXTVIEWBOX_ISTEXT
                bne TextRepr
                jmp HexRepr

CleanUp         lda BufWidth
                ;sta Val
                jsr SubAFromFD
                inx
                txa
                ;sta Val
                jsr SubAFromFB; one before last line
                ldy #$ff
                ;
-               iny
                cpy WidthMinus3
                bcs +
                lda #1
                jsr AddToFB
                jsr Cmp_FBFC_ViewerEOF
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
                ; Prepare char conversion
                lda ControlBits
                and #BIT_CTRL_UPPERCASE
                beq +
                ldx #<PetUCtoDesktop
                ;sta SMC_Convert+1
                ldy #>PetUCtoDesktop
                ;sta SMC_Convert+2
                jmp ++
+               ldx #<PetLCtoDesktop
                ;sta SMC_Convert+1
                ldy #>PetLCtoDesktop
                ;sta SMC_Convert+2
                ;
++              stx SMC_Convert+1
                sty SMC_Convert+2
                ldx HeightMinus2
                dex
                stx ZP_5F; serves as line no
                ; Here is the loop
--              ; if eof reached, quit
                jsr Cmp_FBFC_ViewerEOF
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
                lda ZP_5F
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
                dec ZP_5F
                jmp --
HexRepr         ;==============================
                ; Hex representation
                ;==============================
                ; uses $02/$03 as running number
                lda ControlIndex+TEXTVIEWBOX_TOPLO
                sec
                sbc #<FILEVIEWERBUF_START
                sta $02
                lda ControlIndex+TEXTVIEWBOX_TOPHI
                sbc #>FILEVIEWERBUF_START
                sta $03
                ; Compute bytes per line
                lda ControlWidth
                sec
                sbc #6
                sta ZP_5F
                ldx #$ff
                lda #0
-               inx
                clc
                adc #3
                cmp ZP_5F
                bcc -
                stx BytesPerLine
                ;
                ldx #0  ; line number
--              ; Loop through lines
                stx ZP_60; line number
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
                sty ZP_5F
                jmp ++
+               ; Paint one byte
                sty ZP_5F; byte number
                lda ($fb),y
                pha
                lsr
                lsr
                lsr
                lsr
                tax
                ; 3 * Y
                tya
                asl
                adc ZP_5F
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
                ldx ZP_60; line number
                ldy ZP_5F; byte number
                iny
                cpy BytesPerLine
                bcc -
                lda BytesPerLine
                jsr AddToFB
                lda BytesPerLine
                jsr AddTo02
                jsr AddBufWidthToFD
                lda #5
                ;sta Val
                jsr SubAFromFD
                inx
                cpx HeightMinus2
                bcc --
                ;
                lda BytesPerLine
                ;sta Val
                jsr SubAFromFB
++              lda $fb
                clc
                adc ZP_5F
                sta ClipEnd
                lda $fc
                adc #0
                sta ClipEnd+1
                jmp PaintScrollbar

PaintRunningNumber
                lda $03
                jsr PrintHexByte
                iny
                lda $02
                ;jmp PrintHexByte

; Converts hex value in A to two chars in ($fd),y/y+1
; Uses X
; Result in HexWord
PrintHexByte    pha
                lsr
                lsr
                lsr
                lsr
                tax
                lda NybbleToHex,x
                sta ($fd),y
                iny
                pla
                and #%00001111
                tax
                lda NybbleToHex,x
                sta ($fd),y
                rts
;---------------------------------------------------------------

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
                jsr CtrlStringsToFB
                ; Paint strings
                ;jsr PrintStringLC
                ;tya
                ;beq ++
                ;iny
                ;tya
                ;jsr AddToFB
                ;jsr AddBufWidthToFD
                ;jmp --
                
                ldx ControlNumStr
                beq ++
-               jsr PrintStringLC
                iny
                tya
                jsr AddToFB
                jsr AddBufWidthToFD
                dex
                bne -
                
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
                ;lda ControlTopIndex
;                clc
;                adc ControlHeight
;                tax
;                dex
;                dex
;                dex
;                cpx ControlNumStr
;                bcc ++
;                lda ControlNumStr
;                ;sec
;                sbc ControlHeight
;                tax
;                inx
;                inx
;                stx ControlTopIndex
;                cpx #230
;                bcc +
;                lda #0
;                sta ControlTopIndex
;+               jsr UpdateControl

                ;ldx ControlHeight
                ;dex
                ;dex                 ; X = number of visible entries
                ;stx ZP_5F
                ;lda #0              ; assume max top index = 0
                ;cpx ControlNumStr
                ;bcs +               ; whole list fits -> max = 0
                ;lda ControlNumStr
                ;sec
                ;sbc ZP_5F           ; A = highest valid ControlTopIndex
                ldx ControlHeight
                dex
                dex
                stx ZP_5F
                lda ControlNumStr
                sec
                sbc ZP_5F
                bcs +
                lda #0
+               cmp ControlTopIndex
                bcs ++              ; current top is valid
                sta ControlTopIndex
                jsr UpdateControl
++              ; Find control pos in buffers and adjust
                jsr GetCtrlBufPos
                jsr AddBufWidthToFD
                lda #1
                jsr AddToFD
                ; Get ptr to string list in FBFC
                jsr CtrlStringsToFB
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
                stx ZP_60
                lda #1
                sta ZP_5F
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
                cmp ZP_5F
                bcs +
                lda #1
                sta EndReached
                jmp ++
+               lda ZP_60
                cmp ZP_5F
                bcc ++
                lda ControlBitsEx
                and #BIT_EX_CTRL_SHOWSIZES
                ;
                jsr PrintDirString
                ;
                lda #FILE_RECORD_LENGTH
                jsr AddToFB
                jsr AddBufWidthToFD
                inc ZP_5F
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
                ;sec
                sbc ControlTopIndex
                tax
                inx
                cpx ZP_5F
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
PrintDirString  ;ldx $fe
;                cpx #$ff
;                bne +
;                rts
;+               
                pha
                ; Print file type
                ldy #19
                lda ($fb),y
                ldy #0
                cmp #$46; ("F"=folder)
                bne +
                lda #13
                bne ++; jmp ++
+               cmp #"P"; PRG
                bne +
                lda #14
                bne ++; jmp ++
+               jsr PetUCtoDesktop
++              sta ($fd),y
                ; Print filename
                ldy #2
-               lda ($fb),y
                beq +
SMC_PrintDirStr jsr $FFFF ;PetUCtoDesktop or PetLCtoDesktop
                sta ($fd),y
                iny
                bne -; jmp -
+               pla
                beq ++++
                lda WindowType
                cmp #WT_ULTIMATE
                bne +
                ; Ultimate wnd -> Print file ext
                ldy #0
                lda ($fb),y
                beq ++++
                cmp #4
                bcs ++++
                tax
;                dex
;                lda ImagesTabLo,x
;                sta here+1
;                lda ImagesTabHi,x
;                sta here+2
;                ;ldy pos_of_size
;                ;iny
;                ;iny
;                ldy #21
;                ldx #2
;here            lda $FFFF,x
;                jsr PetLCtoDesktop
;                sta ($fd),y
;                dey
;                dex
;                bpl here
;                rts
                lda ImageEnds-1,x
                tax
                ldy #21
-               lda Images,x
                jsr PetLCtoDesktop
                sta ($fd),y
                dey
                dex
                cpy #18
                bne -
                rts
+               ; Not Ultimate wnd -> Print file size
                jsr AddrInFBtofile_size
                ldy pos_of_size
                jsr ConvertToDecStr
                ldx #3
                ldy pos_of_size
-               lda ($fd),y
                jsr PetUCtoDesktop
                sta ($fd),y
                iny
                dex
                bpl -
++++            rts

Images          !pet "D64", "D71", "D81"
ImageEnds       !byte 2,5,8
;ImagesTabLo     !byte <Images, <(Images+3), <(Images+6)
;ImagesTabHi     !byte >Images, >(Images+3), >(Images+6)

PaintLabel      ldy ControlWidth
                dey
                lda ControlColor
-               sta ($02),y
                dey
                bpl -
paintlabel      jsr CtrlStringsToFB
                lda ControlBits
                and #BIT_CTRL_UPPERCASE
                jmp PrintStringCase

PaintCheckBox   ldy #0
                lda ControlColor
                sta ($02),y
                lda #52
                ldx ControlHilIndex; Checked or not $2f
                beq +
                lda #53
+               sta ($fd),y
                lda #2
                jsr AddToFD
                jmp paintlabel

PaintLabel_ML   jsr CtrlStringsToFB
                jmp PrintStringLC_ML

NextTwoBufRows  lda $fd
                clc
                adc BufWidth
                sta $06
                lda $fe
                adc #0
                sta $07
                ;
                lda $06
                clc
                adc BufWidth
                sta $08
                lda $07
                adc #0
                sta $09
                rts

PaintEditSL     jsr NextTwoBufRows
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
                inc $06
                bne +
                inc $07
+               jsr CtrlStringsToFD
                ldy ControlIndex+EDITSL_CARETPOS
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
                bmi ++
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
                ldy ControlIndex+EDITSL_CARETPOS
                sta ($06),y
+               ; Color
                jsr AddBufWidthTo02
                ldy ControlWidth
                dey
                dey
                lda #CL_WHITE
-               sta ($02),y
                dey
                bne -
                rts

PaintUpDown     ; First row
                lda ControlBitsEx
                and #BIT_EX_CTRL_NOFRAME_TOP
                bne ++
                ldy #3
-               ldx #39
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_TOP
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #39
                beq +
                ldx #11
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
                ; Right frame
                ldy #4
                ldx #37
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_RGT
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #37
                beq +
                ldx #34
+               txa
                sta ($fd),y
                ; Left frame
                ldy #0
                ldx #41
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_LFT
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
                lda ControlIndex+UPDOWN_DIGIT_HI
                clc
                adc #$b0
                sta ($fd),y
                iny
                lda ControlIndex+UPDOWN_DIGIT_LO
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
                ldy #3
-               ldx #36
                lda ControlBits
                and #BIT_CTRL_DBLFRAME_BTM
                beq +
                lda ($fd),y
                cmp #160
                beq +
                cmp #36
                beq +
                ldx #11
+               txa
                sta ($fd),y
                dey
                bne -
                rts

PaintRadioButtonGroup
                jsr CtrlStringsToFB
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
                ;ldy res
                iny
                ;sty res
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
PaintButton     ; Prepare paint
                lda ControlStrings
                sta $0a
                lda ControlStrings+1
                sta $0b
                jsr NextTwoBufRows
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
                ldx #11
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
                ldx #11
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
                jsr InvertReserved
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
+               ; Paint
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
                jsr CtrlStringsToFB
                lda #1
                jsr AddToFD
                jsr PrintStringLC
                lda #1
                ;sta Val
                jsr SubAFromFD
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
PaintFramBottom dey
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

PaintColBoxLabel
                lda #3
                jsr AddToFD
                jsr paintlabel
                lda #3
                ;sta Val
                jsr SubAFromFD
PaintColorPicker
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