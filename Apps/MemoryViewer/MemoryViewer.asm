!to "MemoryViewer.d64",d64,"memoryviewer.gui","memview disk"

!source "../gui64.inc.asm"

; GUI64 Memory Viewer
; NMOS 6502 / ACME
; Public GUI64 API only.
;
; Geometry:
;   window width : 33
;   hex box      : 31 chars at x=1
;   box interior : 29 chars
;                  0000: 00 00 00 00 00 00 00 00
;
; I/O checkbox:
;   unchecked = RAM below $D000-$DFFF
;   checked   = visible I/O in $D000-$DFFF
;
; GUI_PaintBox is expected to paint the current control as a GUI64 box and
; return with $FD/$FE and $02/$03 restored to the current control's top-left.

* = $b000

; -----------------------------------------------------------------------------
; Constants
; -----------------------------------------------------------------------------
WT_MEMVIEW       = 54
CT_HEXBOX        = 50

CTRL_HEX         = 0
CTRL_EDIT        = 1
CTRL_IO          = 2
CTRL_LEFT        = 3
CTRL_RIGHT       = 4

WINDOW_WIDTH     = 33
HEXBOX_X         = 1
HEXBOX_Y         = 3
HEXBOX_WIDTH     = 31
HEXCONTENT_WIDTH = 29

; The app runs unchanged with either GUI64 design. 17 rows is the
; largest possible snapshot (MAC); the actual read size is selected at
; runtime via GUI_GetDesign and stored in MaxPageBytes.
PAGE_BUFFER_BYTES = 17*8

KEY_RETURN       = $fc
KEY_BACKSPACE    = $fd
KEY_CRSR_LR      = $f8

; Desktop charset values used by the viewer.
CH_SPACE         = 160
CH_COLON         = 186

; -----------------------------------------------------------------------------
; Entry
; -----------------------------------------------------------------------------
                ; Do not create a second viewer window.
                lda #WT_MEMVIEW
                sta Param0
                jsr GUI_FindWndByType
                bcc StartNew
                stx Param0
                jmp GUI_SelectTopWindow

StartNew        ldx #<MemCtrlAction
                ldy #>MemCtrlAction
                jsr GUI_SetCtrlActionsRoutine

                ldx #<PaintAppControls
                ldy #>PaintAppControls
                jsr GUI_SetPaintCtrlsRoutine

                ldx #<Wnd_Memory
                ldy #>Wnd_Memory
                jsr GUI_CreateWindowEx

                ; Configure EditSL
                jsr GUI_SelectControl1
                lda #(BIT_CTRL_UPPERCASE + BIT_CTRL_DBLFRAME_TOP)
                sta ControlBits
                lda #4
                ldx #4
                jsr GUI_SetEditSLInfo

                lda #CTRL_EDIT
                sta WindowFocCtrl
                jsr GUI_UpdateWindow

                ; Use listbox-like white color for the hex box
                jsr GUI_SelectControl0
                lda #CL_WHITE
                sta ControlColor
                jsr GUI_UpdateControl
                
                ; Buttons
                jsr GUI_SelectControl3
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_RGT)
                sta ControlBits
                jsr GUI_UpdateControl
                
                jsr GUI_SelectControl4
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_RGT)
                sta ControlBits
                jsr GUI_UpdateControl
                
                jsr GUI_GetDesign
                bne StartNewMacDesign

                ; WIN: max window height 22 -> 15 visible data rows.
                lda #(15*8)
                sta MaxPageBytes
                jmp StartNewDesignDone

StartNewMacDesign
                ; MAC: max window height 24 -> 17 visible data rows.
                lda #(17*8)
                sta MaxPageBytes

                ; MAC Design: adjust buttons
                jsr GUI_SelectControl3
                lda #24
                sta ControlPosX
                lda #5
                sta ControlWidth
                ldx #<Str_MAC_Btn1
                ldy #>Str_MAC_Btn1
                jsr GUI_SetCtrlString
                
                jsr GUI_SelectControl4
                lda #28
                sta ControlPosX
                lda #5
                sta ControlWidth
                ldx #<Str_MAC_Btn2
                ldy #>Str_MAC_Btn2
                jsr GUI_SetCtrlString

StartNewDesignDone
                lda #0
                sta PageStart
                sta PageStart+1

                jmp ReadPage

; -----------------------------------------------------------------------------
; Window procedure
; -----------------------------------------------------------------------------
MemWndProc      ; Cursor left/right always pages and does not reach EditSL
                lda wndParam0
                cmp #EC_KEYPRESS
                bne MemWndStd
                lda actkey
                cmp #KEY_CRSR_LR
                bne MemWndFilterEdit
                lda key_shifted
                beq MemWndPageRight
                jsr PageLeft
                jmp MemWndRepaintRestoreFocus

MemWndPageRight jsr PageRight
                jmp MemWndRepaintRestoreFocus

MemWndFilterEdit; StdWndProc dispatches keyboard events to the selected control.
                ; Select the focused control explicitly first.
                lda WindowFocCtrl
                jsr GUI_SelectControl

                ; If EditSL has focus, shifted input is forbidden.
                ; Accept only unshifted 0-9, A-F, backspace, return.
                lda WindowFocCtrl
                cmp #CTRL_EDIT
                bne MemWndStd
                lda key_shifted
                bne MemWndRejectKey
                lda actkey
                cmp #KEY_RETURN
                beq MemWndStd
                cmp #KEY_BACKSPACE
                beq MemWndStd
                cmp #48                  ; '0'
                bcc MemWndRejectKey
                cmp #58                  ; ':' = '9'+1
                bcc MemWndStd
                cmp #65                  ; 'A'
                bcc MemWndRejectKey
                cmp #71                  ; 'G' = 'F'+1
                bcc MemWndStd
MemWndRejectKey rts

MemWndStd       jsr GUI_StdWndProc

                lda wndParam0
                cmp #EC_KEYPRESS
                beq MemWndKeyAfter
                cmp #EC_LBTNPRESS
                bne +
                ; StdWndProc/control actions may repaint and change
                ; ControlIndex. WindowFocCtrl still identifies the control
                ; that received the click.
                lda WindowFocCtrl
                cmp #CTRL_IO
                beq MemWndIOChanged
                rts
+               cmp #EC_LBTNRELEASE
                bne MemWndDone
                jsr GUI_IsInCurControl
                bcc MemWndDone
                lda WindowFocCtrl
                cmp #CTRL_LEFT
                beq MemWndClickLeft
                cmp #CTRL_RIGHT
                beq MemWndClickRight
MemWndDone      rts

MemWndIOChanged ; Only the I/O range changes interpretation.
                jsr PageTouchesIO
                bcc MemWndDone
                jsr ReadPage
                jmp MemWndRepaintRestoreFocus

MemWndClickLeft jsr PageLeft
                jmp MemWndRepaintRestoreFocus

MemWndClickRight
                jsr PageRight
                jmp MemWndRepaintRestoreFocus

MemWndKeyAfter  lda actkey
                cmp #KEY_RETURN
                bne MemWndDone
                lda WindowFocCtrl
                cmp #CTRL_EDIT
                bne MemWndDone
                jsr AddressFromEdit
                jsr ReadPage
                jmp MemWndRepaintRestoreFocus

; Full-window painting walks all controls and therefore leaves ControlIndex
; at the last control. Restore the focused control so a subsequent event such
; as LBTNRELEASE is dispatched to the same control that received LBTNPRESS.
MemWndRepaintRestoreFocus
                jsr GUI_RepaintCurWindow
                lda WindowFocCtrl
                jmp GUI_SelectControl

; -----------------------------------------------------------------------------
; Custom control actions
; -----------------------------------------------------------------------------
MemCtrlAction   rts

; -----------------------------------------------------------------------------
; Geometry / paging
; -----------------------------------------------------------------------------
PageLeft        lda PageStart+1
                bne PageLeftSubtract
                lda PageStart
                cmp PageBytes
                bcs PageLeftSubtract
                lda #0
                sta PageStart
                sta PageStart+1
                jsr PageToEdit
                jmp ReadPage

PageLeftSubtract
                lda PageStart
                sec
                sbc PageBytes
                sta PageStart
                lda PageStart+1
                sbc #0
                sta PageStart+1
                jsr PageToEdit
                jmp ReadPage

PageRight       ; No wrap. If the next page start would exceed $FFFF, stay.
                lda PageStart
                clc
                adc PageBytes
                sta TmpLo
                lda PageStart+1
                adc #0
                bcs PageRightDone
                sta TmpHi
                lda TmpLo
                sta PageStart
                lda TmpHi
                sta PageStart+1
                jsr PageToEdit
                jmp ReadPage
PageRightDone   rts

; PageStart -> EditAddress as four PETSCII hex digits.
PageToEdit      lda PageStart+1
                ldy #0
                jsr ByteToEditHex
                lda PageStart
                ldy #2
                jsr ByteToEditHex
                jsr GUI_SelectControl1
                lda #4
                ldx #4
                jmp GUI_SetEditSLInfo

; A = byte, Y = destination offset in EditAddress.
ByteToEditHex   pha
                lsr
                lsr
                lsr
                lsr
                tax
                lda HexPetscii,x
                sta EditAddress,y
                iny
                pla
                and #$0f
                tax
                lda HexPetscii,x
                sta EditAddress,y
                rts

; Parse 0..4 digits currently entered in EditSL.
; RETURN does not visually normalize the entered string.
AddressFromEdit jsr GUI_SelectControl1
                lda #0
                sta PageStart
                sta PageStart+1
                ldx ControlIndex+EditSL_CaretPos
                beq AddressFromEditDone
                ldy #0

AddressFromEditLoop
                ; value <<= 4
                asl PageStart
                rol PageStart+1
                asl PageStart
                rol PageStart+1
                asl PageStart
                rol PageStart+1
                asl PageStart
                rol PageStart+1

                lda EditAddress,y
                cmp #65                  ; 'A'
                bcc AddressFromEditDigit
                sec
                sbc #55                  ; 'A' - 10
                bcs AddressFromEditAdd

AddressFromEditDigit
                sec
                sbc #48                  ; '0'

AddressFromEditAdd
                ora PageStart
                sta PageStart
                iny
                dex
                bne AddressFromEditLoop

AddressFromEditDone
                rts

; -----------------------------------------------------------------------------
; Page range helpers
; -----------------------------------------------------------------------------
; Carry set if the currently valid page bytes touch $D000-$DFFF.
PageTouchesIO   lda PageStart+1
                cmp #$d0
                bcc PageTouchesIOBelow
                cmp #$e0
                bcc PageTouchesIOYes
                clc
                rts

PageTouchesIOBelow
                ; With a maximum page size below 256 bytes, only a page in
                ; $CFxx can cross into $D000.
                cmp #$cf
                bne PageTouchesIONo
                lda PageStart
                clc
                adc ValidBytes
                bcc PageTouchesIONo
                beq PageTouchesIONo
                bne PageTouchesIOYes

PageTouchesIONo clc
                rts

PageTouchesIOYes
                sec
                rts

; -----------------------------------------------------------------------------
; Read the maximum displayable range into PageBuffer. Resizing only changes
; how much of this snapshot is painted; it never rereads memory.
; -----------------------------------------------------------------------------
ReadPage        lda MaxPageBytes
                sta ValidBytes

                ; Only a page starting in $FFxx can hit the end of memory.
                lda PageStart+1
                cmp #$ff
                bne ReadPageCountReady
                lda PageStart
                beq ReadPageCountReady        ; $FF00 has at least 256 bytes
                eor #$ff
                clc
                adc #1                        ; 256 - low byte
                cmp ValidBytes
                bcs ReadPageCountReady
                sta ValidBytes

ReadPageCountReady
                ; Map I/O out only when the page actually touches $D000-$DFFF
                ; and the checkbox requests underlying RAM. This also avoids
                ; changing $01 when viewing zero page itself.
                jsr PageTouchesIO
                bcc ReadPageCopy
                jsr GUI_SelectControl2
                lda ControlHilIndex
                bne ReadPageCopy

                php
                sei
                jsr GUI_MapOutIO
                jsr CopyPageToBuffer
                jsr GUI_MapInIO
                plp
                rts

ReadPageCopy    jmp CopyPageToBuffer

; Copies ValidBytes from PageStart to PageBuffer with memmove semantics.
; This keeps a consistent snapshot even when the viewed range overlaps the
; viewer's own PageBuffer.
CopyPageToBuffer
                lda PageStart
                sta ZP_FB
                lda PageStart+1
                sta ZP_FC

                lda ValidBytes
                beq CopyPageDone

                ; Backward copy only if:
                ;   PageStart < PageBuffer < PageStart + ValidBytes
                lda PageStart+1
                cmp #>PageBuffer
                bcc CopyPageSourceBelow
                bne CopyPageForward
                lda PageStart
                cmp #<PageBuffer
                bcs CopyPageForward

CopyPageSourceBelow
                lda PageStart
                clc
                adc ValidBytes
                sta TmpLo
                lda PageStart+1
                adc #0
                sta TmpHi
                cmp #>PageBuffer
                bcc CopyPageForward
                bne CopyPageBackward
                lda TmpLo
                cmp #<PageBuffer
                bcc CopyPageForward
                beq CopyPageForward

CopyPageBackward
                ldy ValidBytes
                dey

CopyPageBackwardLoop
                lda (ZP_FB),y
                sta PageBuffer,y
                dey
                cpy #$ff
                bne CopyPageBackwardLoop
                rts

CopyPageForward ldy #0

CopyPageForwardLoop
                cpy ValidBytes
                bcs CopyPageDone
                lda (ZP_FB),y
                sta PageBuffer,y
                iny
                bne CopyPageForwardLoop
CopyPageDone    rts

; -----------------------------------------------------------------------------
; Custom painters
; -----------------------------------------------------------------------------
PaintAppControls
                cpx #CT_HEXBOX
                beq PaintHexBox
                rts

; Paint the GUI64 box, then its visible data rows. Geometry follows the
; current window size; the data itself comes only from PageBuffer.
PaintHexBox     lda WindowWidth
                sec
                sbc #2
                sta ControlWidth

                ; Header occupies control rows 0..2. The viewer starts at 3
                ; and leaves one blank row below it.
                lda WindowHeight
                sec
                sbc #5
                cmp #3
                bcs PaintHexHeightOK
                lda #3
PaintHexHeightOK
                sta ControlHeight
                sec
                sbc #2
                sta PageRows
                asl
                asl
                asl
                sta PageBytes

                jsr GUI_PaintBox
                ; GUI_PaintBox returns pointers at the box's top-left.
                ; Move one row down and one column right into the 29-char
                ; interior.
                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                inc ZP_FD
                bne PaintHexBoxFDReady
                inc ZP_FE

PaintHexBoxFDReady
                inc $02
                bne PaintHexBoxColorReady
                inc $03

PaintHexBoxColorReady
                lda PageStart
                sta DrawAddr
                lda PageStart+1
                sta DrawAddr+1
                lda #0
                sta DrawIndex
                sta DrawRow

PaintHexRowLoop lda DrawRow
                cmp PageRows
                bcc PaintHexHaveRow
                rts

PaintHexHaveRow ; Clear complete 29-char content row.
                ldy #(HEXCONTENT_WIDTH-1)
                lda #CH_SPACE

PaintHexClearLoop
                sta (ZP_FD),y
                dey
                bpl PaintHexClearLoop

                ; Set row color.
                ldy #(HEXCONTENT_WIDTH-1)
                lda ControlColor

PaintHexColorLoop
                sta ($02),y
                dey
                bpl PaintHexColorLoop

                ; Once $FFFF has been consumed, following rows remain blank.
                lda DrawIndex
                cmp ValidBytes
                bcs PaintHexRowDone

                ; Address + colon. The row was cleared, so position 5 is the
                ; required blank after the colon.
                ldy #0
                lda DrawAddr+1
                jsr PaintHexByte
                lda DrawAddr
                jsr PaintHexByte
                lda #CH_COLON
                sta (ZP_FD),y
                iny
                iny

                lda #0
                sta ByteInRow

PaintHexByteLoop
                lda ByteInRow
                cmp #8
                bcs PaintHexRowDone
                ldx DrawIndex
                cpx ValidBytes
                bcs PaintHexRowDone
                lda PageBuffer,x
                jsr PaintHexByte
                inc DrawIndex
                inc ByteInRow
                lda ByteInRow
                cmp #8
                bcs PaintHexRowDone
                iny
                bne PaintHexByteLoop

PaintHexRowDone inc DrawRow
                lda DrawRow
                cmp PageRows
                bcs PaintHexDone

                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02

                clc
                lda DrawAddr
                adc #8
                sta DrawAddr
                bcc PaintHexNoAddrCarry
                inc DrawAddr+1

PaintHexNoAddrCarry
                jmp PaintHexRowLoop

PaintHexDone    rts

; Convert A to two GUI64 desktop hex chars at ($FD),Y.
PaintHexByte    pha
                lsr
                lsr
                lsr
                lsr
                tax
                lda HexDesktop,x
                sta (ZP_FD),y
                iny
                pla
                and #$0f
                tax
                lda HexDesktop,x
                sta (ZP_FD),y
                iny
                rts

; -----------------------------------------------------------------------------
; Window/control definition
; -----------------------------------------------------------------------------
WND_BITS = BIT_WND_RESIZABLE + BIT_WND_FIXEDWIDTH + BIT_WND_CANMAXIMIZE + BIT_WND_CANMINIMIZE
Wnd_Memory      !byte WT_MEMVIEW,WND_BITS,4,3,WINDOW_WIDTH,15
                !word Str_Title
                !word MemWndProc

                ; 0: 31-char custom hex box. Height adjusted at runtime.
                !byte CT_HEXBOX,HEXBOX_X,HEXBOX_Y,HEXBOX_WIDTH,10
                !byte 0

                ; 1: EditSL.
                !byte CT_EDIT_SL,6,0,7,3
EditAddress     !pet "0000",0

                ; 2: I/O checkbox
                !byte CT_CHECKBOX,15,1,5,1
                !pet "I/O",0

                ; 3/4: page buttons
                !byte CT_BUTTON,25,0,3,3
                !pet "<",0
                !byte CT_BUTTON,29,0,3,3
                !pet ">",0
                
                ; 5: label
                !byte CT_LABEL,1,1,5,1
                !pet "Addr:",0

                ; end of controls
                !byte 0

Str_Title       !pet "Memory Viewer",0
Str_MAC_Btn1    !pet " < ",0
Str_MAC_Btn2    !pet " > ",0

; -----------------------------------------------------------------------------
; Tables / state
; -----------------------------------------------------------------------------
; Exact key codes used by GUI64 for unshifted hex input.
; Keep generated page addresses byte-identical to typed input.
HexPetscii      !byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
                !byte $41,$42,$43,$44,$45,$46

; Same glyphs converted to GUI64 desktop charset.
HexDesktop      !byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9
                !byte $81,$82,$83,$84,$85,$86

PageStart       !word 0
DrawAddr        !word 0
TmpLo           !byte 0
TmpHi           !byte 0
PageRows        !byte 0
PageBytes       !byte 0
MaxPageBytes    !byte 0
ValidBytes      !byte 0
DrawIndex       !byte 0
DrawRow         !byte 0
ByteInRow       !byte 0

PageBuffer      !fill PAGE_BUFFER_BYTES,0