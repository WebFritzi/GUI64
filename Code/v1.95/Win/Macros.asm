!macro EditSetCarretInfo carretpos, max_strlen
                lda #carretpos
                sta ControlIndex+CTRLSTRUCT_CARRETPOS
                lda #max_strlen
                sta ControlIndex+CTRLSTRUCT_MAX_STRLEN
                jsr UpdateControl
!end

!macro ShowMessage lo,hi
                lda #lo
                sta $fb
                lda #hi
                sta $fc
                jsr ShowMessage
!end

!macro JMPShowMessage lo,hi
                lda #lo
                sta $fb
                lda #hi
                sta $fc
                jmp ShowMessage
!end

!macro ShowErrorMsg lo,hi
                lda #lo
                sta $fb
                lda #hi
                sta $fc
                jsr ShowErrorMsg
!end

!macro SetCursor cur
                ;lda #cur
                ;sta Param
                ldy #cur
                jsr SetCursor
!end

!macro MenubarAddMenuList lo, hi
                ; Misused struct entries for menu bar
                lda #lo
                sta ControlPosX
                lda #hi
                sta ControlPosY
                jsr UpdateControl
!end

!macro ControlSetNumStr byte
                lda byte
                sta ControlNumStr
                jsr UpdateControl
!end

!macro ControlSetPosXPosY xi,y
                lda xi
                sta ControlPosX
                lda y
                sta ControlPosY
                jsr UpdateControl
!end

!macro ControlSetWidthHeight w,h
                lda w
                sta ControlWidth
                lda h
                sta ControlHeight
                jsr UpdateControl
!end

!macro ControlSetID id
                lda #id
                sta ControlID
                jsr UpdateControl
!end

!macro ControlSetColorByte byte
                lda byte
                sta ControlColor
                jsr UpdateControl
!end

!macro ControlSetColorVal val
                lda #val
                sta ControlColor
                jsr UpdateControl
!end

!macro ControlSetTopIndex val
                lda #val
                sta ControlTopIndex
                jsr UpdateControl
!end

!macro ControlSetHilIndexVal val
                lda #val
                sta ControlHilIndex
                jsr UpdateControl
!end

!macro ControlSetHilIndex byte
                lda byte
                sta ControlHilIndex
                jsr UpdateControl
!end

!macro ControlSetStringList lo,hi,num
                lda #lo
                sta ControlStrings
                lda #hi
                sta ControlStrings+1
                lda #num
                sta ControlNumStr
                jsr UpdateControl
!end

!macro ControlSetStringListByte lo,hi,b_num
                lda #lo
                sta ControlStrings
                lda #hi
                sta ControlStrings+1
                lda b_num
                sta ControlNumStr
                jsr UpdateControl
!end

!macro ControlSetString lo, hi, ind
                lda #lo
                sta $02
                lda #hi
                sta $03
                lda #ind
                sta Param
                jsr SetCtrlString
!end

!macro SelectControl index
                lda #index
                jsr SelectControl
!end

!macro AddBitExToCurWnd val
                lda WindowBitsEx
                ora #val
                sta WindowBitsEx
                jsr UpdateWindow
!end

!macro DelBitExFromCurWnd val
                lda #val
                eor #%11111111
                and WindowBitsEx
                sta WindowBitsEx
                jsr UpdateWindow
!end

!macro SetCurWndAttributeByte byte
                lda byte
                sta WindowAttribute
                jsr UpdateWindow
!end

!macro SetCurWndProc lo, hi
                lda #lo
                sta WindowProc
                lda #hi
                sta WindowProc+1
                jsr UpdateWindow
!end

!macro SetCurWndType type
                lda #type
                sta WindowType
                jsr UpdateWindow
!end

!macro SetCurWndTitle lo, hi
                lda #lo
                sta WindowTitleStr
                lda #hi
                sta WindowTitleStr+1
                jsr UpdateWindow
!end

!macro SetCurWndTitleByte lo, hi
                lda lo
                sta WindowTitleStr
                lda hi
                sta WindowTitleStr+1
                jsr UpdateWindow
!end

!macro SetCurWndGeometry xi,y,w,h
                lda #xi
                sta WindowPosX
                lda #y
                sta WindowPosY
                lda #w
                sta WindowWidth
                lda #h
                sta WindowHeight
                jsr UpdateWindow
!end

!macro CreateWindowByData lo, hi
                ldx #lo
                ldy #hi
                stx $fb
                sty $fc
                jsr CreateWindowByData
!end

!macro AddControl lo, hi
                lda #lo
                sta $fb
                lda #hi
                sta $fc
                jsr AddControl
!end

!macro AddValToFB val
                lda $fb
                clc
                adc #val
                sta $fb
                bcc .quitme
                inc $fc
.quitme
!end

!macro AddValTo06 val
                lda $06
                clc
                adc #val
                sta $06
                bcc .quitme
                inc $07
.quitme
!end

!macro AddValTo0c val
                lda $0c
                clc
                adc #val
                sta $0c
                bcc .quitme
                inc $0d
.quitme
!end

;!macro SubValFromFB val
;                lda $fb
;                sec
;                sbc #val
;                sta $fb
;                lda $fc
;                sbc #0
;                sta $fc
;!end

!macro SubValFromFD val
                lda $fd
                sec
                sbc #val
                sta $fd
                lda $fe
                sbc #0
                sta $fe
!end

!macro AddValToFD val
                lda $fd
                clc
                adc #val
                sta $fd
                bcc .quitme
                inc $fe
.quitme
!end

!macro AddByteTo02 ByteInAddr
                lda $02
                clc
                adc ByteInAddr
                sta $02
                bcc .quitme
                inc $03
.quitme
!end

!macro AddByteTo04 ByteInAddr
                lda $04
                clc
                adc ByteInAddr
                sta $04
                bcc .quitme
                inc $05
.quitme
!end

!macro AddValTo02 val
                lda $02
                clc
                adc #val
                sta $02
                bcc .quitme
                inc $03
.quitme
!end

!macro AddByteToFB ByteInAddr
                lda $fb
                clc
                adc ByteInAddr
                sta $fb
                bcc .quitme
                inc $fc
.quitme
!end

!macro AddByteToFD ByteInAddr
                lda $fd
                clc
                adc ByteInAddr
                sta $fd
                bcc .quitme
                inc $fe
.quitme
!end

!macro AddWordToFB lo,hi
                lda $fb
                clc
                adc lo
                sta $fb
                lda $fc
                adc hi
                sta $fc
!end

!macro AddWordToFD lo,hi
                lda $fd
                clc
                adc lo
                sta $fd
                lda $fe
                adc hi
                sta $fe
!end