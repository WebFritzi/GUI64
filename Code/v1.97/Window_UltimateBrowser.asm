; Creates a drive wnd with type in A
CreateUltWnd    +CreateWindowByData <Wnd_Ultimate, >Wnd_Ultimate
                lda res
                beq +++
                jsr GetCurDeviceInd
                ; Menu
                +AddControl <Ctrl_Ult_Menubar, >Ctrl_Ult_Menubar
                +ControlSetStringList <Str_UltMenubar, >Str_UltMenubar, 3
                +MenubarAddMenuList <UltMenubar, >UltMenubar
                ; FileListBox
                +AddControl <Ctrl_Drv_FLB, >Ctrl_Drv_FLB
                lda #BIT_CTRL_ISMAXIMIZED
                sta ControlBits
                lda ControlBitsEx
                and #%01111111
                sta ControlBitsEx
                ;
                jsr UpdateWindow
                +ControlSetColorVal CL_WHITE
+++             rts

UltWndProc      jsr StdWndProc
                rts