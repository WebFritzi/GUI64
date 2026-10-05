; Paints taskbar during disk access altough IRQ is not running
FakeTaskbar     ; Make it solid
                ldx #39
-               lda #36
                sta SCRMEM+880,x
                lda #160
                sta SCRMEM+880+40,x
                sta SCRMEM+880+80,x
                lda CSTM_WindowClr
                sta CLRMEM+880,x
                sta CLRMEM+880+40,x
                sta CLRMEM+880+80,x
                dex
                bpl -
                ; Add disk symbol in lower left
                ldx #220
                stx SCRMEM+880
                inx
                stx SCRMEM+881
                inx
                stx SCRMEM+880+40
                inx
                stx SCRMEM+880+41
                ldx #110
                stx SCRMEM+880+80
                inx
                stx SCRMEM+880+81
                rts

; Gets task button index from mouse pos
; Expects mouse in task buttons
; Return val in A
; Returns $ff if there are no minimizable windows
; or if mouse is too far right
GetTaskBtnIndex lda #$ff
                ldy MinableWindows
                beq +
                lda TaskBtnWidths,y
                sta $fc
                sta TaskBtnWidth
                lda MouseInfo
                sec
                sbc #3
                sta $fd
                jsr DivideFDbyFC
                lda $fd
                cmp MinableWindows
                bcc +
                lda #$ff
+               rts

;; Gets x-pos of task btn with index in X
;; Requires TaskBtnWidth filled
;; Result in A
;GetTaskBtnPos   stx $fd
;                lda TaskBtnWidth
;                sta $fe
;                jsr MultiplyFDbyFE
;                txa
;                clc
;                adc #3
;                rts

tb_index        !byte 0
PaintTaskbar    ; Paint taskbar without buttons
                ldx #39
-               lda #95
                sta TASKBAR_BUF,x
                lda #32
                sta TASKBAR_BUF+40,x
                sta TASKBAR_BUF+80,x
                dex
                bpl -
                ; Wait for line 250 to avoid nasty effects
                ; with reserved area in char set
-               lda $d012
                cmp #250
                bcc -
                ldy MinableWindows
                beq ++
                ; Paint task buttons
                lda TaskBtnWidths,y
                sta TaskBtnWidth
                lda #0
                sta window_counter
                sta tb_index
                sta TaskBtnPos
-               lda #0
                ldx window_counter
                cpx CurrentWindow
                bne +
                lda #1
+               sta TaskBtnPressed
                lda window_counter
                sta Param0
                jsr GetWindowAddr
                ldy #WNDSTRUCT_BITS
                lda ($fb),y
                and #BIT_WND_CANMINIMIZE
                beq +
                ; Can minimize
                ldx tb_index
                lda window_counter
                sta TaskBtnHandles,x
                ldy #WNDSTRUCT_TITLESTRING
                jsr AddrInFBtoFD
                jsr PaintTaskBtn
                inc tb_index
                lda TaskBtnPos
                clc
                adc TaskBtnWidth
                sta TaskBtnPos
+               inc window_counter
                lda tb_index
                cmp MinableWindows
                bcc -
++              ; Paint clock button to buffer
                ldx #33
                lda #7
                sta TaskBtnWidth
                jsr DrawPressedTaskBtn
                ; To screen
                ldx #39
-               lda TASKBAR_BUF,x
                sta SCRMEM+$370,x
                lda TASKBAR_BUF+40,x
                sta SCRMEM+$370+40,x
                lda TASKBAR_BUF+80,x
                sta SCRMEM+$370+80,x
                dex
                bpl -
                ; Adjust clock color to hires and display clock
                ldx #4
                lda #0
-               sta $db98+34,x
                dex
                bpl -
                jmp DisplayClock

; Requires TaskBtnWidth and X = Task button pos
DrawPressedTaskBtn
                lda #98
                sta TASKBAR_BUF,x
                lda #96
                sta TASKBAR_BUF+40,x
                lda #97
                sta TASKBAR_BUF+80,x
                inx
                ldy TaskBtnWidth
                dey
                dey
                beq +
-               lda #99
                sta TASKBAR_BUF,x
                lda #101
                sta TASKBAR_BUF+80,x
                inx
                dey
                bne -
+               lda #100
                sta TASKBAR_BUF,x
                lda #103
                sta TASKBAR_BUF+40,x
                lda #102
                sta TASKBAR_BUF+80,x
                rts

TB_offset       !byte 0
; Expects TaskBtnPos, TaskBtnWidth, TaskBtnPressed and FDFE filled
; FDFE contains ptr to string
; FBFC is ptr to window in wndstruct memory
PaintTaskBtn    ldx TaskBtnPos
                inx
                inx
                inx
                stx TB_offset
                lda TaskBtnPressed
                beq ++
                ; Task button is pressed
                jsr DrawPressedTaskBtn
                jmp DisplayTBString
++              ; Task button is not pressed
                lda #106
                sta TASKBAR_BUF,x
                lda #104
                sta TASKBAR_BUF+40,x
                lda #105
                sta TASKBAR_BUF+80,x
                inx
                ldy TaskBtnWidth
                dey
                dey
                beq +
-               lda #107
                sta TASKBAR_BUF,x
                lda #109
                sta TASKBAR_BUF+80,x
                inx
                dey
                bne -
+               lda #108
                sta TASKBAR_BUF,x
                lda #111
                sta TASKBAR_BUF+40,x
                lda #110
                sta TASKBAR_BUF+80,x
DisplayTBString jsr IsDriveWindow
                sta Param1
                ; Adjust colors
                lda #$97
                clc
                adc TB_offset
                sta $fb
                lda #$db
                sta $fc
                ; HiRes for text
                ldy TaskBtnWidth
                dey
                lda #0
-               sta ($fb),y
                dey
                bne -
                ; Multicolor for edges
                ldy #1
                lda #9
                sta ($fb),y
                ldy TaskBtnWidth
                sta ($fb),y
                ; Print string
                ;lda $fe
                ;bmi +
                lda #<(TASKBAR_BUF+41)
                sta $fb
                lda #>(TASKBAR_BUF+41)
                sta $fc
                lda TB_offset
                jsr AddToFB
                ldx TaskBtnWidth
                dex
                dex
                dex
                stx Param0
; PrintStrTaskbar
; Prints string from FDFE to FBFC with max length in Param0
; Param1 indicates whether it's a drive window or not
; If string is too long, it terminates with "..."
                bmi +++
                ldy #0
                lda ($fd),y
                beq +++
                ; Prepare copy char
                lda #<TASKCHARBASE
                sta smc1+1
                lda #>TASKCHARBASE
                sta smc2+1
                lda #<TB_Reserved
                sta smc3+1
                lda #>TB_Reserved
                sta smc4+1
                lda #TB_Reserved_Char
                sta smc5+1
                dey
--              iny
                lda ($fd),y
                beq ++
                ldx Param1
                beq no_drv
                jsr PetUCtoTaskbar
                bne +; jmp +
no_drv          jsr PetLCtoTaskbar
+               ldx TaskBtnPressed
                beq +
                ; Press char
                jsr CopyCharToReserved
+               sta ($fb),y
                cpy Param0
                bcc --
                iny
                lda ($fd),y
                beq ++
                dey
                lda #91
                ldx TaskBtnPressed
                beq +
                jsr CopyCharToReserved
+               sta ($fb),y
++              lda TaskBtnPressed
                beq +++
                jmp PressReserved_TB
+++             rts