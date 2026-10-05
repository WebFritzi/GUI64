;ViewerEOF       !byte <FILEVIEWERBUF_START, >FILEVIEWERBUF_START (ZP)

; Creates fileviewer window
CreateViewerWnd lda #WT_FILEVIEW
                sta Param0
                jsr IsWndTypePresent
                bcc +
                ; Window already present
                stx Param0
                jsr SelectTopWindow
                jsr SelectControl1
                jsr ResetViewer
                jmp UpdateControl
+               ; Window not present
                ldx #<Wnd_FileViewer
                ldy #>Wnd_FileViewer
                jsr CreateWindowEx
                lda res
                beq ++
                ; Menu
                jsr SelectControl0
                ldx #<Str_ViewMenubar
                ldy #>Str_ViewMenubar
                lda #1
                jsr SetCtrlStringList
                jsr MenubarToScreen
                ; File viewer box
                jsr SelectControl1
                lda #BIT_CTRL_ISMAXIMIZED
                ora #BIT_CTRL_UPPERCASE
                sta ControlBits
                sta ControlIndex+TEXTVIEWBOX_ISTEXT
                lda #CL_WHITE
                sta ControlColor
                jsr ResetViewer
                ldx #<FILEVIEWERBUF_START
                ldy #>FILEVIEWERBUF_START
                jmp SetCtrlString
++              rts

ResetViewer     lda #<FILEVIEWERBUF_START
                sta ViewerEOF
                sta ControlIndex+TEXTVIEWBOX_TOPLO
                lda #>FILEVIEWERBUF_START
                sta ViewerEOF+1
                sta ControlIndex+TEXTVIEWBOX_TOPHI
                rts

; Needs wndParam0 filled with exit code
ViewerWndProc   jsr StdWndProc
                ;
                lda wndParam1
                beq +
                bmi +; dialog mode
                ; In menu mode
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne +
                ; Mouse btn pressed in MM
                jsr IsInCurMenu
                bcc ++; rts in viewer_update
;ViewerMenuClicked
                jsr SelectControl1
                lda CurMenuItem
                cmp #ID_MI_VIEWTEXT_UC
                bne +
                ; Clicked on "View as text upper case"
                lda ControlBits
                ora #BIT_CTRL_UPPERCASE
                sta ControlBits
                lda #1
                bne viewer_update
                ;
+               cmp #ID_MI_VIEWTEXT_LC
                bne +
                ; Clicked on "View as text lower case"
                lda ControlBits
                and #($ff-BIT_CTRL_UPPERCASE)
                sta ControlBits
                lda #1
viewer_update   sta ControlIndex+TEXTVIEWBOX_ISTEXT
                jsr UpdateControl
                jmp PaintCurWndViaBufToScreen
                ;
+               cmp #ID_MI_VIEWHEX
                bne +
                ; Clicked on "View as Hex"
                lda #0
                beq viewer_update
                ;
+               cmp #ID_MI_VIEWCLOSE
                bne ++
                ; Clicked on "Close"
                jmp KillCurWnd_RepaintAll
++              rts