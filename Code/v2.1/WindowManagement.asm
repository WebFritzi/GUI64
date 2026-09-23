!ifdef WIN{
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
                ldx #0
-               lda WndPriorityList+1,x
                sta WndPriorityList,x
                inx
                cpx #15
                bcc -
                lda #$ff
                sta WndPriorityList+15
                pla
                ldx AllocedWindows
                dex
                sta WndPriorityList,x
                ; Activate top priority wnd
                lda WndPriorityList
                sta Param0
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
                beq +
                ; Is minimized
                lda WindowBits
                and #($ff-BIT_WND_ISMINIMIZED)
                sta WindowBits
                jsr UpdateWindow
                inc VisibleWindows
+               rts
}

!ifdef MAC{
; Deiconizes cur wnd
DeiconizeCurWnd lda WindowHeightEx
                sta WindowHeight
                lda WindowPosY
                clc
                adc WindowHeight
                cmp #25
                bcc +
                lda #24
                ;sec
                sbc WindowHeightEx
                sta WindowPosY
+               lda WindowBits
                and #($ff-BIT_WND_IS_ICONIZED)
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
}

MaximizeCurWnd
!ifdef MAC{
                jsr DeiconizeCurWnd
}
                lda WindowBits
                and #(BIT_WND_FIXEDWIDTH + BIT_WND_FIXEDHEIGHT)
                bne +
                ; No fixed width or height
                lda #0
                sta WindowPosX
                sta WindowPosY
                lda #40
                sta WindowWidth
                +LDA_WM 22,24
                sta WindowHeight
!ifdef WIN{
                bne maximize; jmp maximize
} else ifdef MAC{
                bne UpdateWindow; jmp UpdateWindow
}
+               ; Fixed width or height
                lda WindowBits
                and #BIT_WND_FIXEDWIDTH
                bne +
                ; Fixed height
                lda #0
                sta WindowPosX
                lda #40
                sta WindowWidth
!ifdef WIN{
                bne maximize; jmp maximize
} else ifdef MAC{
                bne UpdateWindow; jmp UpdateWindow
}
+               ; Fixed width
                lda #0
                sta WindowPosY
                +LDA_WM 22,24
                sta WindowHeight
!ifdef WIN{
maximize        lda WindowBits
                ora #BIT_WND_ISMAXIMIZED
                sta WindowBits
}
                bne UpdateWindow

MaximizeCurCtrl lda #0
                sta ControlPosX
                sta ControlPosY
                lda WindowWidth
                sta ControlWidth
                ldx WindowHeight
                dex
!ifdef WIN{
                lda WindowBits
                and #BIT_WND_HASMENU
                beq +
                dex
+
}
                stx ControlHeight
                jmp UpdateControl

;!ifdef WIN{
;; Checks if wnd in FB is visible (not minimized)
;; Result in carry
;IsWndVisible    ldy #WNDSTRUCT_BITS
;                lda ($fb),y
;                and #BIT_WND_ISMINIMIZED
;                bne +
;                sec
;                rts
;+               clc
;                rts
;}

; Writes wnd addr into $FBFC
; Expects: Param0 (handle)
GetWindowAddr   lda Param0
                asl
                asl
                asl
                asl
                sta $fb
                lda #>WND_HEAP
                sta $fc
                rts

; Copies local WindowStruct to memory
UpdateWindow    ldy #15
-               lda CurrentWindow,y
                sta (WindowOnHeap),y
                dey
                bpl -
                rts

; Makes wnd with handle in Param0 top wnd
; and selects it
; Expects: Param0 filled
SelectTopWindow ; Find wnd in PriorityList
                ldx AllocedWindows
                beq ++
                dex
-               lda WndPriorityList,x
                cmp Param0
                beq +
                dex
                bpl -
                rts
+               ; Manipulate priority list
                txa
                beq SelectWindow
                tay
                dex
                jsr ShiftPriorList
                lda Param0
                sta WndPriorityList
; Fills static window struct
; Expects: Param0 filled with window handle
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

ShiftPriorList  lda WndPriorityList,x
                sta WndPriorityList,y
                dey
                dex
                bpl ShiftPriorList
                rts

; Window type required in Param0
; Result in carry
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
                cmp Param0
                beq ++; carry is set
                lda #16
                jsr AddToFB
                inx
                cpx AllocedWindows
                bcc -
+               clc
++              rts

WinProps        !byte 1,3,4,5,6,7,8,9,14,15
; Creates a window without controls from a table in Addr(X,Y) with the following data:
; window type, bits, x, y, w, h, lobyte of title string, hibyte of title string, lobyte of WndProc, hibyte of WndProc
; Return value in res (0: fail (too many windows), 1: ok)
CreateWindow    lda #0
                sta res
!ifdef WIN{
CreateWndNoRes
}  
                stx $fb
                sty $fc
                ldx AllocedWindows
                cpx #MAX_WND_NUMBER
                bcc +
                rts
+               ldy #9
-               lda ($fb),y
                ldx WinProps,y
                sta CurrentWindow,x
                dey
                bpl -
!ifdef WIN{
                ; CreateWindow low level
                lda WindowBits
                and #BIT_WND_CANMINIMIZE
                beq +
                lda MinableWindows
                cmp #7
                bcc +
                ;
                ldx #<Str_Mess_MaxWnd
                ldy #>Str_Mess_MaxWnd
                dec res
                jmp ShowMessage
                ;
+               inc res
                lda AllocedWindows
                sta CurrentWindow
                ;
                asl
                asl
                asl
                asl
                sta WindowOnHeap
                
                ;lda EofWndHeap
                ;sta $02
                ;sta WindowOnHeap
                ;lda EofWndHeap+1
                ;sta $03
                ;sta WindowOnHeap+1
                
                ; Zero-fill rest of static window struct
                lda #0
                sta WindowBitsEx
                sta WindowCtrlPtr
                sta WindowCtrlPtr+1
                sta WindowNumCtrls
                sta WindowFocCtrl
                ; Fill window struct on heap
                jsr UpdateWindow
                ;
                ldx CurrentWindow
                lda WindowWidth
                sta WndDefWidth,x
                lda WindowHeight
                sta WndDefHeight,x
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
;+               lda EofWndHeap
;                clc
;                adc #16
;                sta EofWndHeap
;                bcc +
;                inc EofWndHeap+1
+
} else ifdef MAC{
                ; CreateWindow low level
                inc res
                lda AllocedWindows
                sta CurrentWindow
                ;
                ;lda EofWndHeap
                ;sta $02
                ;sta WindowOnHeap
                ;lda EofWndHeap+1
                ;sta $03
                ;sta WindowOnHeap+1
                
                asl
                asl
                asl
                asl
                sta WindowOnHeap
                
                ; Zero-fill rest of static window struct
                lda #0
                sta WindowHeightEx
                sta WindowCtrlPtr
                sta WindowCtrlPtr+1
                sta WindowNumCtrls
                sta WindowFocCtrl
                ; Fill window struct on heap
                jsr UpdateWindow
                ; Increase AllocedWindows
                inc AllocedWindows
                ;lda EofWndHeap
                ;clc
                ;adc #16
                ;sta EofWndHeap
                ;bcc +
                ;inc EofWndHeap+1
}
                ; Update priority list
                ldx #14
                ldy #15
                jsr ShiftPriorList
                lda CurrentWindow
                sta WndPriorityList
                rts

; Creates a window with controls
; Requires a table in Addr(X,Y) with the following data:
; window type, bits, geometry (4 bytes), lobyte of title string, hibyte of title string, lobyte of WndProc, hibyte of WndProc
; Control data:
; control type, x, y, w, h, null-terminated string (caption)
; A zero at the end
CreateWindowEx  jsr CreateWindow
                lda res
                beq ++
                lda #10
                jsr AddToFB
                ; FBFC = ptr to control data
                ldy #0
                lda ($fb),y
                beq ++
--              jsr AddControlFBFC
                lda #5
                jsr AddToFB
                ; FBFC = string in control data
                ldy #$ff
-               iny
                lda ($fb),y
                bne -
                iny
                sty ZP_5F
                tya
                jsr AddToFB
                ; FBFC = ptr to next control or final zero byte
                ldy #0
                lda ($fb),y
                bne --
++              rts

; Adds control from data at Addr(X,Y)
; Sets type, geometry, and string
AddControl      stx $fb
                sty $fc
; Adds control from data at FBFC
; Sets type, geometry, and string
AddControlFBFC  lda CSTM_WindowClr
                sta ControlColor
                lda #0
                sta ControlBits
                ;
                ldy #4
-               lda ($fb),y
                ldx ControlProps,y
                sta ControlIndex,x
                dey
                bpl -
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

ControlProps    !byte 1,3,4,5,6

; Adds a control to current window with info from static ctrl struct
AddControlLL    ; Fill entries in static ctrl struct which have not 
                ; been filled yet
                lda WindowNumCtrls
                sta ControlIndex
                lda #$ff
                sta ControlHilIndex
                lda #0
                sta ControlTopIndex
                sta ControlNumStr
                sta ControlStrings
                sta ControlStrings+1
                ;sta ControlID
                sta ControlBitsEx
                ; Update EofCtrlsHeap and ControlOnHeap
                lda EofCtrlsHeap
                sta ControlOnHeap
                clc
                adc #16
                sta EofCtrlsHeap
                lda EofCtrlsHeap+1
                sta ControlOnHeap+1
                adc #0
                sta EofCtrlsHeap+1
                ; Fill control struct on heap
                jsr UpdateControl
                ; Update parent window's static struct and on heap
                lda WindowNumCtrls
                bne +
                lda ControlOnHeap
                ldy #WNDSTRUCT_FIRSTCONTROL
                sta (WindowOnHeap),y
                sta WindowCtrlPtr
                lda ControlOnHeap+1
                iny
                sta (WindowOnHeap),y
                sta WindowCtrlPtr+1
+               inc WindowNumCtrls
                lda WindowNumCtrls
                ldy #WNDSTRUCT_NUMCONTROLS
                sta (WindowOnHeap),y
                ; Check if it's a menubar
                lda ControlType
                cmp #CT_MENUBAR
                bne +
                ; It's a menu
                ldy #WNDSTRUCT_BITS
                lda WindowBits
                ora #BIT_WND_HASMENU
                sta WindowBits
                ;lda (WindowOnHeap),y
                ;ora #BIT_WND_HASMENU
                sta (WindowOnHeap),y
+               ; Set default values
                lda ControlType
                cmp #CT_LISTBOX
                bne +
                ; Listbox def color
                ldy #CL_WHITE
                jmp SetCtrlColor
                ;
+               cmp #CT_CHECKBOX
                bne +
                ; Checkbox def state
                lda #0
                sta ControlHilIndex
                lda #CL_WHITE
                sta ControlColor
                jmp UpdateControl
                ;
+               cmp #CT_UPDOWN
                bne +
                ; UpDown def color
                ldy #CL_WHITE
                jmp SetCtrlColor
                ; 
+               rts

KillCurWindow   ;----------------------------------------------------
                ;  1. Determine gap size on control heap
                ;  2. Copy all controls at end of gap to start of gap
                ;     with decrementing entry ParentWindow
                ;  3. Adjust EofCtrlsHeap
                ;  4. Adjust CtrlPtr of all windows after me
                ;  5. Decrement handles of all windows after me by 1
                ;  6. Copy all windows after me to me
                ;  7. Adjust WndPriorityList
                ;  8. WIN: Adjust WndDefWidth/Height tables
                ;  9. Adjust AllocedWindows
                ; 10. Select CurrentWindow
                ;----------------------------------------------------
!ifdef WIN{
                ; Save WindowBits for later
                lda WindowBits
                pha
}
                ; Determine gap size on control heap
                lda #0
                sta ZP_60
                lda WindowNumCtrls
                sta ZP_5F
                ;
                asl ZP_5F
                rol ZP_60
                asl ZP_5F
                rol ZP_60
                asl ZP_5F
                rol ZP_60
                asl ZP_5F
                rol ZP_60
                ;
                lda ZP_60
                bne ++
                lda ZP_5F
                bne ++
                ; Adjust WindowCtrlPtr if wnd has no controls
                lda WindowOnHeap
                sta $fb
                lda WindowOnHeap+1
                sta $fc
                ldx CurrentWindow
-               inx
                cpx AllocedWindows
                bcs mov_wnd_structs; if wnd is last one
                dex
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
                lda $fd
                clc
                adc ZP_5F
                sta $fd
                lda $fe
                adc ZP_60
                sta $fe
                ; Copy controls and adjust Parent in struct
--              jsr CopyStructFromFDToFB_AdvanceFB
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
                ;sec
                sbc ZP_5F
                sta EofCtrlsHeap
                lda EofCtrlsHeap+1
                sbc ZP_60
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
                sbc ZP_5F
                sta ($fd),y
                iny
                lda ($fd),y
                sbc ZP_60
                sta ($fd),y
                ldy #WNDSTRUCT_HANDLE
                lda ($fd),y
                sec
                sbc #1
                sta ($fd),y
                jsr CopyStructFromFDToFB_AdvanceFB
                inx
                cpx AllocedWindows
                bcc --
                ;; Adjust EofWndHeap
                ;lda EofWndHeap
                ;;sec
                ;sbc #16
                ;sta EofWndHeap
                ;lda EofWndHeap+1
                ;sbc #0
                ;sta EofWndHeap+1
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
!ifdef WIN{
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
                sta Param0
                jmp SelectWindow
+               lda #$ff
                sta CurrentWindow
                rts
} else ifdef MAC{
                ; Adjust AllocedWindows
                dec AllocedWindows
                ; Select (top) window
                lda AllocedWindows
                beq +
                lda WndPriorityList
                sta Param0
                jsr SelectWindow
                jmp ++
+               lda #$ff
                sta CurrentWindow
++              jmp MenubarToScreen
}

CopyStructFromFDToFB_AdvanceFB
                ldy #15
-               lda ($fd),y
                sta ($fb),y
                dey
                bpl -
                lda #16
                jmp AddToFB