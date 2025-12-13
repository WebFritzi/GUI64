CurWnd_SetDefSize
                ldx CurrentWindow
                lda WndDefWidth,x
                sta WindowWidth
                lda WndDefHeight,x
                sta WindowHeight
                lda WindowBits
                and #%01111111 ; not BIT_WND_ISMAXIMIZED
                sta WindowBits
                jmp UpdateWindow

MaximizeCurWnd  lda WindowBits
                and #(BIT_WND_FIXEDWIDTH + BIT_WND_FIXEDHEIGHT)
                bne +
                ; No fixed width or height
                lda #0
                sta WindowPosX
                sta WindowPosY
                lda #40
                sta WindowWidth
                lda #22
                sta WindowHeight
                jmp maximize
+               ; Fixed width or height
                lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne +
                ; Fixed height
                lda #0
                sta WindowPosX
                lda #40
                sta WindowWidth
                jmp maximize
+               ; Fixed width
                lda #0
                sta WindowPosY
                lda #22
                sta WindowHeight
maximize        lda WindowBits
                ora #BIT_WND_ISMAXIMIZED
                sta WindowBits
                jmp UpdateWindow

MaximizeCurCtrl lda #0
                sta ControlPosX
                sta ControlPosY
                lda WindowWidth
                sta ControlWidth
                ldx WindowHeight
                dex
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                dex
+               txa
                sta ControlHeight
                jmp UpdateControl

; Checks if wnd in FB is visible (not minimized)
; return val in res
IsWndVisible    ldy #WNDSTRUCT_BITS
                lda ($fb),y
                and #BIT_WND_ISMINIMIZED
                bne +
                lda #1
                rts
+               lda #0
                rts

; Minimizes current window and moves it to end of priority list
MinimizeCurWnd  ; Check if already minimized
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne +
                ; Move cur wnd to the end of priority list
                lda WindowBits
                ora #BIT_WND_ISMINIMIZED
                sta WindowBits
                jsr UpdateWindow
                lda CurrentWindow
                pha
                ldx #1
-               lda WndPriorityList,x
                dex
                sta WndPriorityList,x
                inx
                inx
                cpx #16
                bcc -
                lda #$ff
                sta WndPriorityList+15
                pla
                ldx AllocedWindows
                dex
                sta WndPriorityList,x
                ; Activate top priority wnd
                lda WndPriorityList
                sta Param
                jsr SelectTopWindow
                ; Adjust VisibleWindows and CurrentWindow
                dec VisibleWindows
                bne +
                lda #$ff
                sta CurrentWindow
+               rts

; Restores 
RestoreCurWnd   ; Check if already restored
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne +
                rts
+               ; Is minimized
                lda #BIT_WND_ISMINIMIZED
                eor #%11111111
                and WindowBits
                sta WindowBits
                jsr UpdateWindow
                inc VisibleWindows
                rts

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

; Creates a window from a table in FBFC with the following data:
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
                bcc +
                rts
+               lda WindowBits
                and #BIT_WND_CANMINIMIZE
                beq +
                lda MinableWindows
                cmp #7
                bcc +
                ;
                +ShowMessage <Str_Mess_MaxWnd, >Str_Mess_MaxWnd
                rts
                ;
+               lda #1
                sta res
                ldx AllocedWindows
                stx CurrentWindow
                ;
                lda WindowWidth
                sta WndDefWidth,x
                lda WindowHeight
                sta WndDefHeight,x
                ;
                lda EofWndHeap
                sta $02
                sta WindowOnHeap
                lda EofWndHeap+1
                sta $03
                sta WindowOnHeap+1
                ; Zero-fill rest of static window struct
                lda #0
                sta WindowBitsEx
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
                ; ... and VisibleWindows if necessary
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne +
                inc VisibleWindows
+               ; ... and MinableWindows if necessary
                lda WindowBits
                and #BIT_WND_CANMINIMIZE
                beq +
                inc MinableWindows
+               lda EofWndHeap
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
                rts

; Adds control in fbfc
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
                sta $fd
                lda WindowOnHeap+1
                sta $fe
                lda WindowNumCtrls
                bne +
                lda $04
                ldy #WNDSTRUCT_FIRSTCONTROL
                sta ($fd),y
                sta WindowCtrlPtr
                lda $05
                iny
                sta ($fd),y
                sta WindowCtrlPtr+1
+               inc WindowNumCtrls
                lda WindowNumCtrls
                ldy #WNDSTRUCT_NUMCONTROLS
                sta ($fd),y
                ; Check if it's a menu
                lda ControlType
                cmp #CT_MENUBAR
                bne +
                ; It's a menu
                lda WindowBits
                ora #BIT_WND_HASMENU
                sta WindowBits
                ldy #WNDSTRUCT_BITS
                lda ($fd),y
                ora #BIT_WND_HASMENU
                sta ($fd),y
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
                lda WindowBits
                pha
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
                ;+AddValToFB 16
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
--              ;ldy #CTRLSTRUCT_PARENT
;                lda ($fd),y
;                tax
;                dex
;                txa
;                sta ($fd),y
                ldy #15
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
                ; Adjust WndDefWidth/Height table
                ldy CurrentWindow
                tya
                tax
                iny
-               lda WndDefWidth,y
                sta WndDefWidth,x
                lda WndDefHeight,y
                sta WndDefHeight,x
                inx
                iny
                cpy AllocedWindows
                bcc -
                ; Adjust Alloced/Minable/VisibleWindows
                dec AllocedWindows
                pla
                pha
                and #BIT_WND_CANMINIMIZE
                beq +
                dec MinableWindows
+               pla
                and #BIT_WND_ISMINIMIZED
                bne +
                dec VisibleWindows
+               ; Select (top) window
                lda VisibleWindows
                beq +
                lda WndPriorityList
                sta Param
                jsr SelectWindow
                jmp ++
+               lda #$ff
                sta CurrentWindow
++              rts