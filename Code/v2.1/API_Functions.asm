!warnoff W1000

App_CtrlActions   !byte 0,0
App_PaintCtrls    !byte 0,0

; Registers chars with definitions at Addr(X/Y)
; for app
; Input:
; A: no of chars (at most 16, at least 1)
; X/Y: lo/hibyte of char table
App_RegisterChars   
                stx $fb
                sty $fc
                cmp #17
                bcs +
                jsr MapOutIO
                asl
                asl
                asl
                tay
                dey
-               lda ($fb),y
                sta DT_App_Chars,y
                dey
                bpl -
                jmp MapInIO
+               rts

; Sets the address of App_CtrlActions
; This is the routine that reacts to events
; in additional controls provided by app
; Input: Addr(X/Y)
App_SetCtrlActionsRoutine
                stx App_CtrlActions
                sty App_CtrlActions+1
                rts

; Sets the address of App_PaintCtrls
; This is the routine that paints the 
; additional controls provided by app
; Input: Addr(X/Y)
App_SetPaintCtrlsRoutine
                stx App_PaintCtrls
                sty App_PaintCtrls+1
                rts

; Retrieves ID of current menu in A
App_GetCurMenuID
                lda CurMenuID
                rts

; Retrieves custom window color
App_GetCSTMWindowColor
                lda CSTM_WindowClr
                rts

; Retrieves custom desktop color
App_GetCSTMDesktopColor
                lda CSTM_DesktopClr
                rts

App_RepaintCurWindow
                lda ProgramMode
                beq +
                rts
+               jmp PaintCurWndViaBufToScreen

App_SetTimerHandler
                stx App_TimerHandler+1
                sty App_TimerHandler+2
                rts

; Starts timer (fires every 1/10 second)
App_StartTimer  lda $dc08
-               cmp $dc08
                beq -
                lda $dc08
                sta Old_DC08
                lda #1
                sta TimerOn
                rts

; Stops timer
App_StopTimer   lda #0
                sta TimerOn
                rts

App_TimerHandler
                jmp $FFFF