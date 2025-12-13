;; Checks whether window in FB is a drive window
;; Output in A and zero flag
;IsDriveWindow   ldy #WNDSTRUCT_TYPE
;                lda ($fb),y
;                cmp #WT_DRIVE_A
;                beq ++
;                cmp #WT_DRIVE_B
;                beq ++
;                lda #0
;++              tay
;                rts

; Gets current device index (0 or 1), based on current window type
; Result: Current device index (0 or 1) in both X and CurDeviceInd
GetCurDeviceInd lda #$ff
                sta CurDeviceInd
                ;
                ldx WindowType; 1 or 2
                dex
                cpx #2
                bcs +
                stx CurDeviceInd
+               rts

; Gets current device index and device no
; Result:
; X = CurDeviceInd
; A = CurDeviceNo
GetCurDeviceNo  jsr GetCurDeviceInd
                ldx CurDeviceInd
                lda DeviceNumbers,x
                sta CurDeviceNo
                rts

; Assumes mouse already in title bar
IsInCloseSymbol ; WinPosX * 8 + 29 - WinPosX * 8 + 33
                ldx WindowPosX
                cpx MouseInfo
                bne +
                lda MouseInfo+2
                and #%00000111
                cmp #5
                bcs ++
                jmp +++
+               inx
                cpx MouseInfo
                bne +++
                lda MouseInfo+2
                and #%00000111
                cmp #2
                bcc ++
                jmp +++
++              lda #1
                rts
+++             lda #0
                rts

; Writes cur wnd addr in buf to WndAddressInBuf
GetWndAddrInBuf ldx WindowPosY
                inx
                lda ScrTabLo,x
                sta WndAddressInBuf
                lda BufScrTabHi,x
                sta WndAddressInBuf+1
                lda WndAddressInBuf
                clc
                adc WindowPosX
                sta WndAddressInBuf
                bcc +
                inc WndAddressInBuf+1
+               rts

; Paints desktop and all windows to buffer
; and then to screen
RepaintAll      ; Set buffer bounds
                lda #40
                sta BufWidth
                lda #24
                sta BufHeight
                ; Paint desktop
                jsr PaintDesktop
                ; Paints non-top windows
                lda AllocedWindows
                beq ++
                sta window_counter
                dec window_counter
                beq +
-               ldx window_counter
                lda WndPriorityList,x
                sta Param
                jsr SelectWindow
                jsr GetWndAddrInBuf
                lda #0
                sta active
                jsr PaintWndToBuf
                dec window_counter
                bne -
+               ; Consider top window
                lda WndPriorityList
                sta Param
                jsr SelectWindow
                jsr GetWndAddrInBuf
                lda #1
                sta active
                jsr PaintWndToBuf
++              ; And paint buffer to screen
                lda #<(SCRMEM+40)
                sta $fb
                lda #>(SCRMEM+40)
                sta $fc
                jsr BufToScreen
                ; Redraw current window to buffer
                jmp PaintCurWindow

; Finds window which is clicked on (NOT! curr wnd)
; Returns wnd handle in res. Has wnd addr in $FB
; Uses dummy
WindowFromPos   ; Only if 2 or more alloced
                lda AllocedWindows
                and #%11111110
                bne +
                lda #$ff
                sta res
                rts
+               ; Skip through non-top windows
                ldx #1
-               lda WndPriorityList,x
                sta dummy
                asl
                asl
                asl
                asl
                sta $fb
                lda #>WND_HEAP
                sta $fc
                jsr IsInWnd
                bne +
                inx
                cpx AllocedWindows
                bcs ++
                jmp -
+               ; Put wnd handle in res
                lda dummy
                sta res
                rts
++              lda #$ff
                sta res
                rts

; Checks if mouse is in window pointed to by $FBFC in wnd heap
; Expects MouseInfo filled
IsInWnd         ldy #WNDSTRUCT_POSX
                lda MouseInfo
                cmp ($fb),y
                bcc +
                sec
                sbc ($fb),y
                ldy #WNDSTRUCT_WIDTH
                cmp ($fb),y
                bcs +
                ldy #WNDSTRUCT_POSY
                lda MouseInfo+5
                cmp ($fb),y
                bcc +
                sec
                sbc ($fb),y
                ldy #WNDSTRUCT_HEIGHT
                cmp ($fb),y
                bcs +
                lda #1
                rts
+               lda #0
                rts

; Expects mouse in cur wnd
IsInTitleBar    lda WindowPosY
                cmp MouseInfo+5
                bne +
                lda #1
                rts
+               lda #0
                rts

; requires MouseInfo filled
IsInCurWnd      ; Check if there is any window at all
                lda AllocedWindows
                beq +
                ; Here we go
                lda MouseInfo
                sta Point
                lda MouseInfo+5
                sta Point+1
                jmp IsInWndRect
                lda #1
                rts
+               lda #0
                rts

; Writes scr/clr buf positions of cur control to $fdfe/$0203
; Expects ControlPosX, ControlPosY filled
GetCtrlBufPos   lda WndAddressInBuf
                sta $fd
                lda WndAddressInBuf+1
                sta $fe
                jsr AddBufWidthToFD
                lda $fd
                sta $02
                lda $fe
                clc
                adc #4
                sta $03
                ldx ControlPosY
                beq +
                dex
-               jsr AddBufWidthToFD
                jsr AddBufWidthTo02
                dex
                bpl -
+               lda ControlPosX
                jsr AddToFD
                lda ControlPosX
                jmp AddTo02

; Fills MousePosInWndX/Y
; It's the mouse pos relative to the area in which controls can be placed
GetMousePosInWnd
                jsr GetMouseInfo
                ; Convert to wnd coords
                lda MouseInfo
                sec
                sbc WindowPosX
                sta MousePosInWndX
                lda MouseInfo+5
                sec
                sbc WindowPosY
                sta MousePosInWndY
                ; Subtract title bar
                dec MousePosInWndY
                rts