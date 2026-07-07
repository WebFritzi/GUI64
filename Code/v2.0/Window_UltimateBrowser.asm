; Creates a drive wnd with type in A
CreateUltWnd    +CreateWindowByData <Wnd_Ultimate, >Wnd_Ultimate
                lda res
                beq +++
                jsr GetCurDeviceInd
                ; Menu
                +AddControl <Ctrl_Ult_Menubar, >Ctrl_Ult_Menubar
                +ControlSetStringList <Str_UltMenubar, >Str_UltMenubar, 1
                +MenubarAddMenuList <UltMenubar, >UltMenubar
!ifdef MAC{
                jsr MenubarToScreen
}
                ; FileListBox
                +AddControl <Ctrl_Drv_FLB, >Ctrl_Drv_FLB
                lda #BIT_CTRL_ISMAXIMIZED
                sta ControlBits
                ldx #2
                lda ControlOnHeap
                sta FileListBoxesLo,x
                lda ControlOnHeap+1
                sta FileListBoxesHi,x
                ;
                lda ControlBitsEx
                ora #BIT_EX_CTRL_SHOWSIZES
                sta ControlBitsEx
                ;
                lda #<STRING_LIST_ULT
                sta ControlStrings
                lda #>STRING_LIST_ULT
                sta ControlStrings+1
                ;
                ldx #CL_WHITE
                stx ControlColor
                dex
                stx ControlNumStr
                jsr UpdateControl
+++             rts

UltWndProc      jsr StdWndProc
                lda wndParam+1
                beq UltWnd_NM
                bmi +++
                ; In menu mode
                lda wndParam
                cmp #EC_LBTNPRESS
                bne +++
                ; Mouse btn pressed in MM
                jsr IsInCurMenu
                beq +++
                jmp ImageMenuClicked
UltWnd_NM       ; In normal mode
                lda wndParam
                cmp #EC_DBLCLICK
                bne ++
                ; Double clicked in normal mode
                +SelectControl 1
                jsr IsInCurControl
                beq +++
                jsr FLSB_GetMouseArea
                cpy #1
                bne +++
                jmp UltChangeDir
++              cmp #EC_KEYPRESS
                bne +++
                ; Key pressed in normal mode
                lda actkey
                cmp #$fc
                bne +++
                ; Return pressed
                jmp UltChangeDir
+++             rts

ImageMenuClicked
                +SelectControl 1
                lda CurMenuItem
                cmp #ID_MI_IMAGEMOUNT_A
                bne +
                ; Clicked on "Mount to A"
mount_A         lda ControlHilIndex
                cmp #$ff
                beq ++
                lda CSTM_DevNumbers
-               sta U_MountDevNo
                jmp UltMountImage
+               cmp #ID_MI_IMAGEMOUNT_B
                bne +
                ; Clicked on "Mount to B"
                lda CSTM_DevNumbers+1
                jmp -
+               cmp #ID_MI_IMAGECLOSE
                bne ++
                ; Clicked on "Close"
                jsr KillCurWindow
                jsr RepaintAll
!ifdef WIN{
                jsr PaintTaskbar
}
++              rts