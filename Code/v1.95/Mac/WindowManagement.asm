; Deiconizes cur wnd
DeiconizeCurWnd lda WindowHeightEx
                sta WindowHeight
                lda WindowPosY
                clc
                adc WindowHeight
                cmp #25
                bcc +
                lda #24
                sec
                sbc WindowHeightEx
                sta WindowPosY
+               lda #BIT_WND_IS_ICONIZED
                eor #%11111111
                and WindowBits
                sta WindowBits
                jmp UpdateWindow

IconizeCurWnd   lda WindowHeight
                sta WindowHeightEx
                lda #1
                sta WindowHeight
                lda #BIT_WND_IS_ICONIZED
                ora WindowBits
                sta WindowBits
                jmp UpdateWindow

MaximizeCurWnd  jsr DeiconizeCurWnd
                lda WindowBits
                and #(BIT_WND_FIXEDWIDTH + BIT_WND_FIXEDHEIGHT)
                bne +
                ; No fixed width or height
                lda #0
                sta WindowPosX
                sta WindowPosY
                lda #40
                sta WindowWidth
                lda #24
                sta WindowHeight
                jmp UpdateWindow
+               ; Fixed width or height
                lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne +
                ; Fixed height
                lda #0
                sta WindowPosX
                lda #40
                sta WindowWidth
                jmp UpdateWindow
+               ; Fixed width
                lda #0
                sta WindowPosY
                lda #24
                sta WindowHeight
                jmp UpdateWindow

MaximizeCurCtrl lda #0
                sta ControlPosX
                sta ControlPosY
                lda WindowWidth
                sta ControlWidth
                ldx WindowHeight
                dex
                txa
                sta ControlHeight
                jmp UpdateControl

; Writes wnd addr into $FBFC
; Expects: Param (handle)
GetWindowAddr   lda Param
                asl
                asl
                asl
                asl
                sta $fb
                lda #>WND_HEAP
                sta $fc
                rts

; Copies local WindowStruct to memory
UpdateWindow    lda WindowOnHeap
                sta $fb
                lda WindowOnHeap+1
                sta $fc
                ldy #15
-               lda CurrentWindow,y
                sta ($fb),y
                dey
                bpl -
                rts

; Makes wnd with handle in Param top wnd
; Expects: Param filled
SelectTopWindow ; Find wnd in PriorityList
                ldx AllocedWindows
                beq ++
                dex
-               lda WndPriorityList,x
                cmp Param
                beq +
                dex
                bpl -
                rts
+               ; Manipulate priority list
                txa
                beq SelectWindow
                tay
                dex
-               lda WndPriorityList,x
                sta WndPriorityList,y
                dey
                dex
                bpl -
                lda Param
                sta WndPriorityList
                ; goes on...
; Fills static window struct
; Expects: Param filled with window handle
SelectWindow    ; Update static wnd address
                jsr GetWindowAddr
                lda $fb
                sta WindowOnHeap
                lda $fc
                sta WindowOnHeap+1
                ; Copy wnd struct to static struct
                ldy #15
-               lda ($fb),y
                sta CurrentWindow,y
                dey
                bpl -
++              rts

; Window type required in Param
; Result:
; A: 0 (no), 1 (yes)
; X: Window handle
IsWndTypePresent
                lda #<WND_HEAP
                sta $fb
                lda #>WND_HEAP
                sta $fc
                lda AllocedWindows
                beq +
                ldx #0
                ldy #WNDSTRUCT_TYPE
-               lda ($fb),y
                cmp Param
                beq ++
                lda #16
                jsr AddToFB
                inx
                cpx AllocedWindows
                bcc -
+               lda #0
                rts
++              lda #1
                rts

; Creates a window with controls
; Requires a table in FBFC with the following data:
; window type, bits, geometry (4 bytes), lobyte of title string, hibyte of title string, lobyte of WndProc, hibyte of WndProc
; Control data:
; control type, x, y, w, h, null-terminated string (caption)
; A zero at the end
CreateWindow    jsr CreateWindowByData
                lda res
                bne +
                rts
+               lda #10
                jsr AddToFB
                ldy #0
                lda ($fb),y
                beq ++
--              jsr AddControl
                lda #5
                jsr AddToFB
                ldy #$ff
-               iny
                lda ($fb),y
                bne -
                iny
                sty dummy
                tya
                jsr AddToFB
                ldy #0
                lda ($fb),y
                bne --
++              rts

; Creates a window without controls from a table in FBFC with the following data:
; window type, bits, x, y, w, h, lobyte of title string, hibyte of title string, lobyte of WndProc, hibyte of WndProc
CreateWindowByData
                ldy #0
                lda ($fb),y
                sta WindowType
                iny
                lda ($fb),y
                sta WindowBits
                iny
                lda ($fb),y
                sta WindowPosX
                iny
                lda ($fb),y
                sta WindowPosY
                iny
                lda ($fb),y
                sta WindowWidth
                iny
                lda ($fb),y
                sta WindowHeight
                iny
                lda ($fb),y
                sta WindowTitleStr
                iny
                lda ($fb),y
                sta WindowTitleStr+1
                iny
                lda ($fb),y
                sta WindowProc
                iny
                lda ($fb),y
                sta WindowProc+1
                ; CreateWindow low level
                lda #0
                sta res
                ldx AllocedWindows
                cpx #MAX_WND_NUMBER
                bcs ++
                lda #1
                sta res
                ldx AllocedWindows
                stx CurrentWindow
                ;
                lda EofWndHeap
                sta $02
                sta WindowOnHeap
                lda EofWndHeap+1
                sta $03
                sta WindowOnHeap+1
                ; Zero-fill rest of static window struct
                lda #0
                sta WindowHeightEx
                sta WindowCtrlPtr
                sta WindowCtrlPtr+1
                sta WindowNumCtrls
                sta WindowFocCtrl
                ; Fill window struct on heap
                ldy #15
-               lda CurrentWindow,y
                sta ($02),y
                dey
                bpl -
                ; Increase AllocedWindows
                inc AllocedWindows
                lda EofWndHeap
                clc
                adc #16
                sta EofWndHeap
                bcc +
                inc EofWndHeap+1
+               ; Update priority list
                ldx #14
                ldy #15
-               lda WndPriorityList,x
                sta WndPriorityList,y
                dey
                dex
                bpl -
                lda CurrentWindow
                sta WndPriorityList
++              rts

; Adds control at fbfc
; Sets type, geometry, and string
AddControl      lda CSTM_WindowClr
                sta ControlColor
                lda #0
                sta ControlBits
                ldy #0
                lda ($fb),y
                sta ControlType
                iny
                lda ($fb),y
                sta ControlPosX
                iny
                lda ($fb),y
                sta ControlPosY
                iny
                lda ($fb),y
                sta ControlWidth
                iny
                lda ($fb),y
                sta ControlHeight
                ;
                jsr AddControlLL
                ;
                lda $fb
                clc
                adc #5
                sta ControlStrings
                lda $fc
                adc #0
                sta ControlStrings+1
                jmp UpdateControl

; Adds a control to current window with info from static ctrl struct
AddControlLL    ; Fill entries in static ctrl struct which have not 
                ; been filled yet
                lda CurrentWindow
                sta ControlIndex
                lda WindowNumCtrls
                sta ControlIndex
                lda #$ff
                sta ControlHilIndex
                lda #0
                sta ControlTopIndex
                sta ControlNumStr
                sta ControlStrings
                sta ControlStrings+1
                sta ControlID
                sta ControlBitsEx
                ; Update controls heap
                lda EofCtrlsHeap
                sta $04
                clc
                adc #16
                sta EofCtrlsHeap
                lda EofCtrlsHeap+1
                sta $05
                adc #0
                sta EofCtrlsHeap+1
                ldy #15
-               lda ControlIndex,y
                sta ($04),y
                dey
                bpl -
                lda $04
                sta ControlOnHeap
                lda $05
                sta ControlOnHeap+1
                ; Update parent window
                lda WindowOnHeap
                sta $5a
                lda WindowOnHeap+1
                sta $5b
                lda WindowNumCtrls
                bne +
                lda $04
                ldy #WNDSTRUCT_FIRSTCONTROL
                sta ($5a),y
                sta WindowCtrlPtr
                lda $05
                iny
                sta ($5a),y
                sta WindowCtrlPtr+1
+               inc WindowNumCtrls
                lda WindowNumCtrls
                ldy #WNDSTRUCT_NUMCONTROLS
                sta ($5a),y
                ; Check if it's a menu
                lda ControlType
                cmp #CT_MENUBAR
                bne +
                ; It's a menu
                lda WindowBits
                ora #BIT_WND_HASMENU
                sta WindowBits
                ldy #WNDSTRUCT_BITS
                lda ($5a),y
                ora #BIT_WND_HASMENU
                sta ($5a),y
+               rts

KillCurWindow   ;----------------------------------------------------
                ;  1. Determine gap size on control heap
                ;  2. Copy all controls at end of gap to start of gap
                ;     with decrementing entry ParentWindow
                ;  3. Adjust EofCtrlsHeap
                ;  4. Adjust CtrlPtr of all windows after me
                ;  5. Decrement handles of all windows after me by 1
                ;  6. Copy all windows after me to me
                ;  7. Adjust EofWndHeap
                ;  8. Adjust WndPriorityList
                ;  9. Adjust WndDefWidth/Height tables
                ; 10. Adjust AllocedWindows
                ; 11. Select CurrentWindow
                ;----------------------------------------------------
                ; Save WindowBits for later
                ;lda WindowBits
                ;pha
                ; Determine gap size on control heap
                lda #0
                sta dummy+1
                lda WindowNumCtrls
                sta dummy
                ;
                asl dummy
                rol dummy+1
                asl dummy
                rol dummy+1
                asl dummy
                rol dummy+1
                asl dummy
                rol dummy+1
                ;
                lda dummy+1
                bne ++
                lda dummy
                bne ++
                ; Adjust WindowCtrlPtr if wnd has no controls
                lda WindowOnHeap
                sta $fb
                lda WindowOnHeap+1
                sta $fc
                ldx CurrentWindow
-               inx
                cpx AllocedWindows
                bcc +
                jmp mov_wnd_structs; if wnd is last one
+               dex
                lda #16
                jsr AddToFB
                inx
                ldy #WNDSTRUCT_NUMCONTROLS
                lda ($fb),y
                beq -
                ;
                ldy #WNDSTRUCT_FIRSTCONTROL
                lda ($fb),y
                sta WindowCtrlPtr
                iny
                lda ($fb),y
                sta WindowCtrlPtr+1
                ;
++              ; Get copy-to-address
                lda WindowCtrlPtr
                sta $fb
                sta $fd
                lda WindowCtrlPtr+1
                sta $fc
                sta $fe
                ; Get copy-from-address
                +AddWordToFD dummy, dummy+1
                ; Copy controls and adjust Parent in struct
--              ldy #15
-               lda ($fd),y
                sta ($fb),y
                dey
                bpl -
                lda #16
                jsr AddToFB
                lda #16
                jsr AddToFD
                lda $fe
                cmp EofCtrlsHeap+1
                bcc --
                lda $fd
                cmp EofCtrlsHeap
                bcc --                
                ; Adjust EofCtrlsHeap
                lda EofCtrlsHeap
                sec
                sbc dummy
                sta EofCtrlsHeap
                lda EofCtrlsHeap+1
                sbc dummy+1
                sta EofCtrlsHeap+1
mov_wnd_structs ; Move window structs after me by 16 to the left
                ; and adjust ctrlptr and handle in wnd struct
                ldx CurrentWindow
                inx
                lda WindowOnHeap
                sta $fb
                sta $fd
                lda WindowOnHeap+1
                sta $fc
                sta $fe
--              lda #16
                jsr AddToFD
                ldy #WNDSTRUCT_FIRSTCONTROL
                lda ($fd),y
                sec
                sbc dummy
                sta ($fd),y
                iny
                lda ($fd),y
                sbc dummy+1
                sta ($fd),y
                ldy #WNDSTRUCT_HANDLE
                lda ($fd),y
                sec
                sbc #1
                sta ($fd),y
                ldy #15
-               lda ($fd),y
                sta ($fb),y
                dey
                bpl -
                lda #16
                jsr AddToFB
                inx
                cpx AllocedWindows
                bcc --
                ; Adjust EofWndHeap
                lda EofWndHeap
                sec
                sbc #16
                sta EofWndHeap
                lda EofWndHeap+1
                sbc #0
                sta EofWndHeap+1
                ; Adjust WndPriorityList
                ldy #1
                ldx #0
-               lda WndPriorityList,y
                sta WndPriorityList,x
                cmp CurrentWindow
                bcc +
                dec WndPriorityList,x
+               iny
                inx
                cpy AllocedWindows
                bcc -
                ldx AllocedWindows
                dex
                lda #$ff
                sta WndPriorityList,x
                ; Adjust AllocedWindows
                dec AllocedWindows
                ; Select (top) window
                lda AllocedWindows
                beq +
                lda WndPriorityList
                sta Param
                jsr SelectWindow
                jmp ++
+               lda #$ff
                sta CurrentWindow
++              jsr MenubarToScreen
                rts