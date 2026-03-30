ViewerEOF       !word 0,0

; Creates fileviewer window
CreateViewerWnd lda #WT_FILEVIEW
                sta Param
                jsr IsWndTypePresent
                bne +++
                lda #<Wnd_FileViewer
                sta $fb
                lda #>Wnd_FileViewer
                sta $fc
                jsr CreateWindow
                lda res
                beq +++
                ; Menu
                +SelectControl 0
                +ControlSetStringList <Str_ViewMenubar, >Str_ViewMenubar, 1
                +MenubarAddMenuList <ViewerMenubar, >ViewerMenubar
                ; File viewer box
                +SelectControl 1
                lda #BIT_CTRL_ISMAXIMIZED
                ora #BIT_CTRL_UPPERCASE
                sta ControlBits
                lda #CL_WHITE
                sta ControlColor
                lda #1
                sta ControlIndex+CTRLSTRUCT_ISTEXT
                lda #<FILEVIEWERBUF_START
                sta ControlIndex+CTRLSTRUCT_TOPLO
                lda #>FILEVIEWERBUF_START
                sta ControlIndex+CTRLSTRUCT_TOPHI
                +ControlSetString <FILEVIEWERBUF_START, >FILEVIEWERBUF_START, 0
+++             rts

; Needs wndParam filled with exit code
ViewerWndProc   jsr StdWndProc
                ;
                lda wndParam+1
                beq ViewerWnd_NM
                bmi +; dialog mode
                ; In menu mode
                lda wndParam
                cmp #EC_LBTNPRESS
                bne +
                ; Mouse btn pressed in MM
                jsr IsInCurMenu
                beq +
                jmp ViewerMenuClicked
+               rts
ViewerWnd_NM    ;
                rts

ViewerMenuClicked
                +SelectControl 1
                lda CurMenuItem
                cmp #ID_MI_VIEWTEXT_UC
                bne +
                ; Clicked on "View as text upper case"
                lda #1
                sta ControlIndex+CTRLSTRUCT_ISTEXT
                lda ControlBits
                ora #BIT_CTRL_UPPERCASE
                sta ControlBits
                jmp viewer_update
                ;
+               cmp #ID_MI_VIEWTEXT_LC
                bne +
                ; Clicked on "View as text lower case"
                lda #1
                sta ControlIndex+CTRLSTRUCT_ISTEXT
                lda #BIT_CTRL_UPPERCASE
                eor #%11111111
                and ControlBits
                sta ControlBits
viewer_update   jsr UpdateControl
                jsr PaintCurWindow
                jmp WindowToScreen
                ;
+               cmp #ID_MI_VIEWHEX
                bne +
                ; Clicked on "View as Hex"
                lda #0
                sta ControlIndex+CTRLSTRUCT_ISTEXT
                jmp viewer_update
                ;
+               cmp #ID_MI_VIEWCLOSE
                bne +
                ; Clicked on "Close"
                jsr KillCurWindow
                jsr RepaintAll
                jsr PaintTaskbar
+               rts