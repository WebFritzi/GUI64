; Checks whether window in FB is a drive window
; Output in A and zero flag
IsDriveWindow   ldy #WNDSTRUCT_TYPE
                lda ($fb),y
                cmp #WT_DRIVE_A
                beq ++
                cmp #WT_DRIVE_B
                beq ++
                lda #0
++              tay
                rts

; Minimizes all minimizable windows
MinimizeAll     lda CurrentWindow
                bmi +
                jsr MinimizeCurWnd
                jmp MinimizeAll
+               rts

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
GetCurDeviceNo  jsr GetCurDeviceInd
                ldx CurDeviceInd
                lda DeviceNumbers,x
                sta CurDeviceNo
                rts

; Assumes mouse already in title bar
; Returns 0 if not; 1 if in close; 2 if in maximize; 3 if in minimize
IsInMinMaxClose lda WindowPosX
                clc
                adc WindowWidth
                tax
                ; Check first position
                dex
                cpx MouseInfo
                bne +
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
                beq +++
                jmp -
+++             lda #0
                rts

; Writes cur wnd addr in buf to WndAddressInBuf
GetWndAddrInBuf ldx WindowPosY
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
                lda #22
                sta BufHeight
                ; Paint desktop
                jsr PrepareScrBuf
                ; Paints non-top windows
                lda VisibleWindows
                beq ++
                sta window_counter
                dec window_counter
                beq +
-               ldx window_counter
                lda WndPriorityList,x
                sta Param
                jsr SelectWindow
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                beq is_visible
                jmp next_wnd
is_visible      jsr GetWndAddrInBuf
                lda #0
                jsr PaintWndToBuf
next_wnd        dec window_counter
                bne -
+               ; Consider top window
                lda WndPriorityList
                sta Param
                jsr SelectWindow
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                beq +
                jmp ++
+               ; Paint top window
                jsr GetWndAddrInBuf
                lda #1
                jsr PaintWndToBuf
++              ; And paint buffer to screen
                lda #<SCRMEM
                sta $fb
                lda #>SCRMEM
                sta $fc
                jsr BufToScreen
                ; Redraw current window
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
                lda MouseInfo+1
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
                cmp MouseInfo+1
                bne +
                lda #1
                rts
+               lda #0
                rts

; requires MouseInfo filled
IsInCurWnd      ; Check if there is any window at all
                lda VisibleWindows
                beq +
                ; Here we go
                lda MouseInfo
                sta Point
                lda MouseInfo+1
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
                ;+AddByteToFD BufWidth
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                jsr AddBufWidthToFD
                ;+AddByteToFD BufWidth
+               lda $fd
                sta $02
                lda $fe
                clc
                adc #4
                sta $03
                ldx ControlPosY
                beq +
                dex
-               jsr AddBufWidthToFD
                ;+AddByteToFD BufWidth
                jsr AddBufWidthTo02
                ;+AddByteTo02 BufWidth
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
                lda MouseInfo+1
                sec
                sbc WindowPosY
                sta MousePosInWndY
                ; Subtract title bar
                dec MousePosInWndY
                ; Care for menu
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                dec MousePosInWndY
+               rts