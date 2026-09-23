!ifdef WIN{
; Checks whether window in FB is a drive window
; Result in A
IsDriveWindow   ldy #WNDSTRUCT_TYPE
                lda ($fb),y
                cmp #(WT_ULTIMATE+1);4
                bcc +
                ;cmp #WT_DRIVE_A
                ;beq +
                ;cmp #WT_DRIVE_B
                ;beq +
                ;cmp #WT_ULTIMATE
                ;beq +
                lda #0
+               rts

; Minimizes all minimizable windows
MinimizeAll     lda CurrentWindow
                bmi +
                jsr MinimizeCurWnd
                jmp MinimizeAll
+               rts
}

SetCurWndTitle  stx WindowTitleStr
                sty WindowTitleStr+1
                jmp UpdateWindow

; Gets current device index (0 or 1), based on current window type
; Result: Current device index (0 or 1) in both X and CurDeviceInd
GetCurDeviceInd ldx WindowType
                dex
                cpx #3
                bcc +
                ldx #$ff
+               stx CurDeviceInd
                rts

; Gets current device index and device no
; Result:
; X = CurDeviceInd
; A = CurDeviceNo
GetCurDeviceNo  jsr GetCurDeviceInd
                lda CSTM_DevNumbers,x
                sta CurDeviceNo
                rts

!ifdef WIN{
; Assumes mouse already in title bar
; Result in A:
; 0 if not; 1 if in close; 2 if in maximize; 3 if in minimize
IsInMinMaxClose lda WindowPosX
                clc
                adc WindowWidth
                tax
                ; Check first position
                dex
                cpx MouseInfo
                bne +
                ; Close btn pressed
                lda #1
                rts
+               ; Check second position
                dex
                cpx MouseInfo
                bne ++
                lda WindowBits
                and #BIT_WND_CANMAXIMIZE
                beq +
                ; Maximize btn pressed
                lda #2
                rts
+               lda WindowBits
                and #BIT_WND_CANMINIMIZE
                beq +++
-               lda #3
                rts
++              ; Check third position
                dex
                cpx MouseInfo
                bne +++
                lda WindowBits
                and #BIT_WND_CANMAXIMIZE
                beq +++
                lda WindowBits
                and #BIT_WND_CANMINIMIZE
                ;beq +++
                bne -;jmp -
+++             lda #0
                rts
} else ifdef MAC{
; Assumes mouse already in title bar
; Result in carry
IsInCloseSymbol lda MouseInfo+2
                and #%00000111
                ; WinPosX * 8 + 29 - WinPosX * 8 + 33
                ldx WindowPosX
                cpx MouseInfo
                bne +
                cmp #5
                rts
+               inx
                cpx MouseInfo
                bne +
                ; A>=2: NO, A<2: YES
                eor #255
                cmp #254
                rts
+               clc
                rts
}

; Writes cur wnd addr in buf to WndAddressInBuf
GetWndAddrInBuf ldx WindowPosY
!ifdef MAC{
                inx
}
                lda ScrTabLo,x
                clc
                adc WindowPosX
                sta WndAddressInBuf
                lda ScrTabHi,x
                ;+EOR_WM $44,$40
                eor #DESKBUF_XOR
                adc #0
                sta WndAddressInBuf+1
                rts

!ifdef WIN{
RepaintGUI      jsr RepaintAll
                jmp PaintTaskbar
}
!ifdef MAC{
RepaintGUI      jsr RepaintAll
                jmp MenubarToScreen
}

; Paints desktop and all windows to buffer
; and then to screen
RepaintAll      ; Set buffer bounds
                lda #40
                sta BufWidth
                +LDA_WM 22,24
                sta BufHeight
                ; Paint desktop
                jsr PaintDesktop
                ; Paints non-top windows
                +LDAV_WM VisibleWindows, AllocedWindows
                beq ++
                sta window_counter
                dec window_counter
                beq +
-               ldx window_counter
                lda WndPriorityList,x
                sta Param0
                jsr SelectWindow
!ifdef WIN{                
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne next_wnd
                jsr GetWndAddrInBuf
                lda #0
                jsr PaintWndToBuf
next_wnd        dec window_counter
                bne -
+               ; Consider top window
                lda WndPriorityList
                sta Param0
                jsr SelectWindow
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne ++
                ; Paint top window
                jsr GetWndAddrInBuf
                lda #1
} else ifdef MAC{
                jsr GetWndAddrInBuf
                lda #0
                sta active
                jsr PaintWndToBuf
                dec window_counter
                bne -
+               ; Consider top window
                lda WndPriorityList
                sta Param0
                jsr SelectWindow
                jsr GetWndAddrInBuf
                lda #1
                sta active
}
                jsr PaintWndToBuf
++              ; And paint buffer to screen
                +LDA_WM <SCRMEM,<(SCRMEM+40)
                sta $fb
                +LDA_WM >SCRMEM,>(SCRMEM+40)
                sta $fc
                jsr BufToScreen
                ; Redraw current window
                jmp PaintCurWindow

; Finds window which is clicked on (NOT! curr wnd)
; Returns wnd handle in res. Has wnd addr in $FB
WindowFromPos   ; Only if 2 or more alloced
                lda AllocedWindows
                and #%11111110
                beq +
                ; Skip through non-top windows
                ldx #1
-               lda WndPriorityList,x
                ;sta res
                ;asl
                ;asl
                ;asl
                ;asl
                ;sta $fb
                ;lda #>WND_HEAP
                ;sta $fc
                sta Param0
                sta res
                jsr GetWindowAddr
                jsr IsInWnd
                bcs ++
                inx
                cpx AllocedWindows
                bcc -
+               lda #$ff
                sta res
++              rts

; Expects mouse in cur wnd
; Result in carry
IsInTitleBar    lda WindowPosY
                +CMPV_WM MouseInfo+1,MouseInfo+5
                beq +
                clc
+               rts

; Checks if mouse is in window pointed to by $FBFC in wnd heap
; Expects MouseInfo filled
; Result in carry
IsInWnd         lda MouseInfo
                ldy #WNDSTRUCT_POSX
                cmp ($fb),y
                bcc +
                sbc ($fb),y
                ldy #WNDSTRUCT_WIDTH
                cmp ($fb),y
                bcs +
                ldy #WNDSTRUCT_POSY
                +LDAV_WM MouseInfo+1,MouseInfo+5
                cmp ($fb),y
                bcc +
                ;sec
                sbc ($fb),y
                ldy #WNDSTRUCT_HEIGHT
                cmp ($fb),y
                bcs +
                sec
                rts
+               clc
                rts

; Requires MouseInfo filled
; Result in carry
IsInCurWnd      ; Check if there is any window at all
                +LDAV_WM VisibleWindows, AllocedWindows
                beq +
                ;; Here we go
                ;lda MouseInfo
                ;cmp WindowPosX
                ;bcc +
                ;sbc WindowPosX
                ;cmp WindowWidth
                ;bcs +
                ;;
                ;+LDAV_WM MouseInfo+1,MouseInfo+5
                ;cmp WindowPosY
                ;bcc +
                ;;sec
                ;sbc WindowPosY
                ;cmp WindowHeight
                ;bcs +
                ;;
                ;sec
                ;rts
                lda #<CurrentWindow
                sta $fb
                lda #>CurrentWindow
                sta $fc
                jmp IsInWnd
+               clc
                rts

; Writes scr/clr buf positions of cur control to $fdfe/$0203
; Expects ControlPosX, ControlPosY filled
GetCtrlBufPos   jsr WndAddrInBufToFD
                jsr AddBufWidthToFD
!ifdef WIN{
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                jsr AddBufWidthToFD
+               
}
                ldx ControlPosY
                beq +
                dex
-               jsr AddBufWidthToFD
                dex
                bpl -
+               lda ControlPosX
                jsr AddToFD
                jmp SetColBufPos

; Fills MousePosInWndX/Y
; It's the mouse pos relative to the area in which controls can be placed
GetMousePosInWnd
                jsr GetMouseInfo
                ; Convert to wnd coords
                lda MouseInfo
                sec
                sbc WindowPosX
                sta MousePosInWndX
                +LDAV_WM MouseInfo+1,MouseInfo+5
                sec
                sbc WindowPosY
                sta MousePosInWndY
                ; Subtract title bar
                dec MousePosInWndY
!ifdef WIN{
                ; Care for menu
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                dec MousePosInWndY
+               
}
                rts