BlackTaskbar    ; Make it solid
                lda #0
                sta MayShowClock
                ldx #39
                lda #160;#239
-               sta SCRMEM+880,x
                sta SCRMEM+880+40,x
                sta SCRMEM+880+80,x
                dex
                bpl -
                ; Use color CSTM_WindowClr
                ldx #39
                lda CSTM_WindowClr
-               sta CLRMEM+880,x
                sta CLRMEM+880+40,x
                sta CLRMEM+880+80,x
                dex
                bpl -
                ; Add disk symbol in lower left
                lda #220
                sta SCRMEM+880
                lda #221
                sta SCRMEM+881
                lda #222
                sta SCRMEM+880+40
                lda #223
                sta SCRMEM+880+41
                lda #237
                sta SCRMEM+880+80
                lda #238
                sta SCRMEM+880+81
                ; Add "Disk access..."
                ldx #13
-               lda Str_DiskAccess,x
                jsr PetLCtoDesktop
                sta SCRMEM+880+40+3,x
                dex
                bpl -
                rts

; Gets task button index from mouse pos
; expects mouse in task buttons
; Return val in res.
; Returns $ff if there are no minimizable windows
GetTaskBtnIndex lda #$ff
                sta res
                ldy MinableWindows
                beq +
                lda TaskBtnWidths,y
                sta TaskBtnWidth
                lda MouseInfo
                sec
                sbc #3
                sta window_counter
                ldx #$ff
-               inx
                lda window_counter
                sec
                sbc TaskBtnWidth
                sta window_counter
                bpl -
                cpx MinableWindows
                bcs +
                stx res
+               rts

SetTaskbarColors
                ldx #39
-               lda #8
                sta $db70,x
                sta $db98,x
                sta $dbc0,x
                dex
                bpl -
                ; Adjust clock color to hires
                ldx #34
                lda #0
-               sta $db98,x
                inx
                cpx #39
                bcc -
                rts

DrawClockButton ldx #33
                lda #98
                sta SCRMEM+$370,x
                lda #96
                sta SCRMEM+$370+40,x
                lda #97
                sta SCRMEM+$370+80,x
                inx
                ldy #5
-               lda #99
                sta SCRMEM+$370,x
                lda #101
                sta SCRMEM+$370+80,x
                inx
                dey
                bne -
                lda #100
                sta SCRMEM+$370,x
                lda #103
                sta SCRMEM+$370+40,x
                lda #102
                sta SCRMEM+$370+80,x
                rts

tb_index        !byte 0
PaintTaskbar    ; Paint taskbar without buttons
                ldx #32
-               lda #95
                sta TASKBAR_BUF,x
                lda #32
                sta TASKBAR_BUF+40,x
                sta TASKBAR_BUF+80,x
                dex
                bpl -
                ; Paint task buttons
                ldy MinableWindows
                beq ++
                lda TaskBtnWidths,y
                sta TaskBtnWidth
                lda #0
                sta window_counter
                sta tb_index
                sta TaskBtnPos
                ; Wait for line 250 to avoid nasty effects
                ; with reserved area in char set
-               lda $d012
                cmp #250
                bcc -
-               lda #0
                ldx window_counter
                cpx CurrentWindow
                bne +
                lda #1
+               sta TaskBtnPressed
                lda window_counter
                sta Param
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
                lda ($fb),y
                sta $fd
                iny
                lda ($fb),y
                sta $fe
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
++              ; To screen
                ldx #32
-               lda TASKBAR_BUF,x
                sta SCRMEM+$370,x
                lda TASKBAR_BUF+40,x
                sta SCRMEM+$370+40,x
                lda TASKBAR_BUF+80,x
                sta SCRMEM+$370+80,x
                dex
                bpl -
                ; Clock
                jmp DrawClockButton

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
                jmp DisplayTBString
++              ; Task button is not pressed
                lda #106
                sta TASKBAR_BUF,x
                lda #104
                sta TASKBAR_BUF+40,x
                lda #105
                sta TASKBAR_BUF+80,x
                inx
                lda TaskBtnWidth
                tay
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
DisplayTBString ; Prepare Param+1 for string display
                lda #0
                sta Param+1
                ldy #WNDSTRUCT_TYPE
                lda ($fb),y
                cmp #WT_DRIVE_A
                beq +
                cmp #WT_DRIVE_B
                bne ++
+               lda #1
                sta Param+1
++              ; Adjust colors
                ; HiRes for text
                lda #$99
                clc
                adc TB_offset
                sta $fb
                lda #$db
                sta $fc
                ldy TaskBtnWidth
                dey
                dey
                dey
                lda #0
-               sta ($fb),y
                dey
                bpl -
                ; Multicolor for edges
                lda #$98
                clc
                adc TB_offset
                sta $fb
                lda #$db
                sta $fc
                ldy #0
                lda #9
                sta ($fb),y
                ldy TaskBtnWidth
                dey
                sta ($fb),y
                ; Print string
                lda $fe
                bmi +
                lda #<(TASKBAR_BUF+41)
                sta $fb
                lda #>(TASKBAR_BUF+41)
                sta $fc
                lda TB_offset
                jsr AddToFB
                ldx TaskBtnWidth
                dex
                dex
                stx Param
                jsr PrintStrTaskbar
+               rts