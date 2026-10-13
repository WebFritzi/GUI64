; Creates a drive wnd with type in A
CreateUltWnd    ldx #<Wnd_Ultimate
                ldy #>Wnd_Ultimate
                jsr CreateWindow
                lda res
                beq +++
                jsr GetCurDeviceInd
                ; Menu
                ldx #<Ctrl_Ult_Menubar
                ldy #>Ctrl_Ult_Menubar
                jsr AddControl
                ldx #<Str_UltMenubar
                ldy #>Str_UltMenubar
                lda #1
                jsr SetMenu_AddMaximizedFileListBox
                ;
                lda #BIT_EX_CTRL_SHOWSIZES
                sta ControlBitsEx
                ;
                lda #<STRING_LIST_ULT
                sta ControlStrings
                lda #>STRING_LIST_ULT
                sta ControlStrings+1
                ;
                ldx #0
                stx ControlNumStr
                ldy #CL_WHITE
                jmp SetCtrlColor
+++             rts

SetMenu_AddMaximizedFileListBox
                jsr SetCtrlStringList
!ifdef MAC {
                jsr MenubarToScreen
}
                ldx #<Ctrl_Drv_FLB
                ldy #>Ctrl_Drv_FLB
                jsr AddControl
                lda #BIT_CTRL_ISMAXIMIZED
                sta ControlBits
                rts

UltWndProc      jsr StdWndProc
                lda wndParam1
                beq UltWnd_NM
                bmi +++
                ; In menu mode
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne +++
                ; Mouse btn pressed in MM
                jsr IsInCurMenu
                bcs ImageMenuClicked
                rts
UltWnd_NM       ; In normal mode
                jsr ChkBrowserAction
                beq UltChangeDir
+++             rts
                ;lda wndParam0
;                cmp #EC_DBLCLICK
;                bne ++
;                ; Double clicked in normal mode
;                jsr SelectControl1
;                jsr IsInCurControl
;                bcc +++
;                jsr FLSB_GetMouseArea
;                cpy #1
;                beq UltChangeDir
;                rts
;++              jsr Cmp_ReturnKey
;                beq UltChangeDir; Return pressed
;+++             rts

ChkBrowserAction
                lda wndParam0
                cmp #EC_DBLCLICK
                bne ++
                ; Double clicked in normal mode
                jsr SelectControl1
                jsr IsInCurControl
                bcs +
                lda #1
                rts
+               jsr FLSB_GetMouseArea
                cpy #1
                rts
++              jmp Cmp_ReturnKey

ImageMenuClicked
                jsr SelectControl1
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
                bne -; jmp -
+               cmp #ID_MI_IMAGECLOSE
                bne ++
                ; Clicked on "Close"
!ifdef WIN{
                jsr KillCurWnd_RepaintGUI
}
!ifdef MAC{
                jsr KillCurWnd_RepaintAll
}
++              rts

; Uses Ultimate target #1 for saving and reading config file in Flash folder
; Uses Ultimate target #2 for browsing and other file handling
UltPathT1       !pet "/",0,0,0,0,0,0,0; 8 bytes

UltChangeDir    ldx #2
                stx U_CurTarget
                lda #0
                sta bFileIsImage
                ldx ControlHilIndex
                bne +
                ; ".." was chosen
                jsr U_GoUpDir
                jmp UltGetPath
                ;
+               jsr UltGetSelFile
                bne ++
                ; Chosen file is not directory
                ;jsr UltIsFileImage
;                bne +
;                jsr UltIsTxtFile
;                bcs UltOpenTxtFile
;                rts
;+               jmp mount_A
                jsr UltGetFileExt
                bne mount_A
                bcs UltOpenTxtFile
                rts
;+               jmp mount_A
++              ; Chosen file is directory
                jsr U_ChangeDir
                txa
                bne +++; error
UltGetPath      jsr U_GetPath
                lda #0
                dex
                bne +
                ; Dir is Root
                sta PATH_U,x
                ldx #6
                sta Str_Title_Ult,x
                ldx #5
-               lda Str_Root,x
                sta Str_Title_Ult,x
                dex
                bpl -
                jmp ++
+               sta PATH_U,x
                dex
-               lda PATH_U,x
                cmp #47; '/'
                beq +
                dex
                bne -
+               inx
                ldy #0
-               lda PATH_U,x
                sta Str_Title_Ult+2,y
                beq ++
                inx
                iny
                cpy #20
                bcc -
++              jsr UltShowDir
!ifdef WIN{
                jsr PaintTaskbar
}
+++             rts

UltOpenTxtFile  jsr MoveFilenameBy1
                lda #FA_READ
                sta SMC_U_Open+1
                jsr U_OpenFile
                bne ++; error
                ;
                jsr CreateViewerWnd
                jsr PaintCurWndViaBufToScreen
                ;
                lda #<FILEVIEWERBUF_START
                sta DATA_ADDR
                lda #>FILEVIEWERBUF_START
                sta DATA_ADDR+1
                ;
                lda #DOS_CMD_READ_DATA
                jsr U_PrepareCmd
                lda #$fe
                sta FREEMEM+2
                +LDA_WM $17,$1b
                sta FREEMEM+3
                ldx #4
                stx cmd_len
                jsr U_SendCommand
                ;
                +LDY_WM 11, 13
-               jsr U_ReadDataPack
                txa
                bne +
                dey
                bpl -
+               ;
                clc
                adc DATA_ADDR
                sta ViewerEOF
                lda #0
                adc DATA_ADDR+1
                sta ViewerEOF+1
                ;
!ifdef WIN{
                jsr RepaintGUI
} else ifdef MAC{
                jsr RepaintAll
}
                jmp U_CloseFile
++              rts

UltShowTooltip  jsr GetMousePosInWnd
                jsr FLSB_GetMouseArea
                cpy #1
                bne ++
                tax
                beq ++
                dex
                stx U_ind
                jsr U_GetFilenameByIndex
                ldx U_len
                cpx #39
                bcs ++
                cpx #17
                bcc ++
                lda #0
                sta FREEMEM+2,x
                sta Param0
                ;
                dex
-               lda FREEMEM+2,x
                ;sec
                ;sbc #$20
                ;tay
                ;lda AsciiToPetscii,y
                jsr AsciiToPetscii
                sta FREEMEM+2,x
                dex
                bpl -
                ;
                lda #<(FREEMEM+2)
                sta $fd
                lda #>(FREEMEM+2)
                sta $fe
                ;
                +LDXV_WM MouseInfo+1,MouseInfo+5
                dex
                ldy WindowPosX
                iny
                iny
                jsr ShowTooltip
++              rts

; Retrieves time from Ultimate
UltGetTime      lda #1
                sta U_CurTarget
                jsr U_GetTime
                ; Set values in Clock
                ldy #0
-               ldx U_Time_Indices,y
                lda FREEMEM,x
                and #%00001111
                sta Clock,y
                iny
                cpy #4
                bcc -
                jmp SetTOD

Ult_GUI64_CFG_FN !pet "gui64v",VERSION,".cfg.bin"
Ult_Dir_Flash    !byte 70,108,97,115,104; = "fLASH"
CFG_FILE_LEN = Ult_Dir_Flash - Ult_GUI64_CFG_FN


; Opens config file "GUI64vx.xxy.bin" with attribute in SMC_U_Open+1
; Requires SMC_U_Open+1 filled with bitmask:
; FA_READ, FA_WRITE, FA_CREATE_NEW, FA_CREATE_ALWAYS
; Assumes Ultimate's current directory is Flash
; Output in A (0: "00,OK", 1: error, status msg in FREEMEM)
; X = Length of status msg
UltOpenCfgFile  ; Write filename "GUI64.xxx.cfg" to FREEMEM+3
                ldx #(CFG_FILE_LEN-1)
-               lda Ult_GUI64_CFG_FN,x
                sta FREEMEM+3,x
                dex
                bpl -
                ; Try to open config file
                lda #CFG_FILE_LEN
                sta U_len
                jmp U_OpenFile

; Assumes target #1
UltGotoFlash    jsr U_GotoRoot
                ; Change dir to /Flash/
                ldx #4
-               lda Ult_Dir_Flash,x
                sta FREEMEM+2,x
                dex
                bpl -
                lda #5
                sta U_len
                jmp U_ChangeDir

; Reads the settings from config file in Ultimate's Flash folder
UltReadSettings ldx #1
                stx U_CurTarget
                jsr UltGotoFlash
                beq +
                ; Error changing dir
                rts
                ;
+               lda #FA_READ
                sta SMC_U_Open+1
                jsr UltOpenCfgFile
                bne +
                ; Success opening file for reading --> Read settings
                jsr U_FREEMEM_TO_DATA_ADDR
                jsr U_ReadSmallFile
                ldx #(SIZE_OF_SETTINGS-1)
-               lda FREEMEM,x
                sta CSTM_Icons,x
                dex
                bpl -
                jsr U_CloseFile
                jmp U_GoUpDir; to root
+               ; Error opening file for reading (file doesn't exist)
                lda #(FA_CREATE_NEW+FA_WRITE)
                sta SMC_U_Open+1
                jsr UltOpenCfgFile
                beq UltWriteCfgFile
                ; Error opening file for writing
                rts
UltWriteCfgFile ; Success creating and opening file for writing --> Write std settings
                lda #<CSTM_Icons
                sta SMC_WriteFile+1
                lda #>CSTM_Icons
                sta SMC_WriteFile+2
                lda #SIZE_OF_SETTINGS
                sta U_len
                lda #0
                sta U_len+1
                jsr U_WriteSmallFile
                ;;;;;;;; ERROR HANDLING ?????? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                jsr U_CloseFile
                jmp U_GoUpDir; to root

UltSaveSettings ldx #1
                stx U_CurTarget
                jsr UltGotoFlash
                lda error_code
                beq +
                ; Error changing dir
                rts
                ; Writes to config file
+               lda #FA_WRITE
                sta SMC_U_Open+1
                jsr UltOpenCfgFile
                beq UltWriteCfgFile
                rts

UltShowDir      ; Read directory into STRING_LIST_ULT
                ldx #2
                stx U_CurTarget
                lda #16
                sta U_FileNameMaxLen
                lda #231
                sta U_AbbrevChar
                lda #<STRING_LIST_ULT
                sta DATA_ADDR
                lda #>STRING_LIST_ULT
                sta DATA_ADDR+1
                jsr U_ReadDirMaxLen
                ;
                ldx U_max_fn_len
                inx
                inx
                txa
                ldy #2
                sta Max_Fn_Len_Plus2,y
                ; Update listscrollbox and window
                jsr SelectControl1
                lda #0
                sta ControlTopIndex
                lda #$ff
                sta ControlHilIndex
                lda U_NumDirStrings
                sta ControlNumStr
                jsr UpdateControl
                jsr RepaintAll
                lda bTooManyFiles
                beq +
                ldx #<Str_UltTooManyFiles
                ldy #>Str_UltTooManyFiles
                jmp ShowMessage
+               rts

UltMountImage   lda bFileIsImage; Is current dir an image?
                beq +
                ; We are inside an image
-               rts
+               ; We are inside a directory
                jsr UltGetSelFile
                bne -
                ; No directory selected
                jsr UltGetFileExt
                beq -; Non-image selected
                ; Image selected
                jsr MoveFilenameBy1
                jmp U_MountImage

; Moves filename with length U_len from
; FREEMEM+2 to FREEMEM+3
; Required: U_len
MoveFilenameBy1 ldx U_len
                dex
-               lda FREEMEM+2,x
                sta FREEMEM+3,x
                dex
                bpl -
                rts

UltGetSelFile   ldx ControlHilIndex
                dex
                stx U_ind
                jsr U_GetFilenameByIndex
                lda FREEMEM+1; file attribute
                and #$10
                rts

bFileIsImage    !byte 0
; Return values:
; A = 1..3, Z=0 (BNE)       Image: d64/d71/d81
; A = 0, Z=1 (BEQ), C=1     txt
; A = 0, Z=1 (BEQ), C=0     unknown
UltGetFileExt   lda U_len
                sec
                sbc #5
                bmi no_ext
                clc
                adc #3
                tax
                lda FREEMEM,x
                cmp #$2e; '.' (ASCII)
                bne no_ext
                ;
                inx
                lda FREEMEM,x
                ; TXT?
                cmp #$74; 't' (ASCII)
                beq check_txt
                ; D64/D71/D81?
                ora #$20
                cmp #"d"
                bne no_ext
                ;
                inx
                lda FREEMEM,x
                sec
                sbc #"6"          ; 6 -> 0, 7 -> 1, 8 -> 2
                cmp #3
                bcs no_ext
                tay
                ;
                inx
                lda FREEMEM,x
                cmp ExtLast,y
                bne no_ext
                ;
                tya
                clc
                adc #1            ; A = 1,2,3
                rts
check_txt       inx
                lda FREEMEM,x
                cmp #$78; 't' (ASCII)
                bne no_ext
                inx
                lda FREEMEM,x
                cmp #$74; 't' (ASCII)
                bne no_ext
                eor #$74; 't' (ASCII) ; A=0; CMP equal left C=1
                rts
no_ext          lda #0
                clc
                rts

;UltImgLast      !byte "4","1","1"

;; Result in carry
;UltHasExtension ldx U_len
;                dex
;                dex
;                lda FREEMEM,x
;                cmp #$2e; '.' (ASCII)
;                beq ++
;                clc
;++              rts

;; Result in carry
;UltIsTxtFile    jsr UltHasExtension
;                bcc ++
;;                ; File has extension
;;                inx
;;                lda FREEMEM,x
;;                cmp #$74; 't' (ASCII)
;;                bne +
;;                inx
;;                lda FREEMEM,x
;;                cmp #$78; 'x' (ASCII)
;;                bne +
;;                inx
;;                lda FREEMEM,x
;;                cmp #$74; 't' (ASCII)
;;                beq ++
;;+               clc
;;++              rts
;                ; File has extension
;                ldy #2
;-               inx
;                lda FREEMEM,x
;                cmp TXT,y
;                bne +
;                dey
;                bpl -
;                sec
;                rts
;+               clc
;                rts

;bFileIsImage    !byte 0
;; Checks if filename in FREEMEM+2 has an image extension
;; Required: U_len
;; Output in A (and zero flag)
;; 0: no valid image extension
;; 1: d64
;; 2: d71
;; 3: d81
;UltIsFileImage  jsr UltHasExtension
;                bcc ++
;                ; File has extension
;                inx
;                lda FREEMEM,x
;                ora #$20
;                cmp #$64; 'd' (ASCII)
;                ;beq +
;                ;cmp #$44; 'D' (ASCII)
;                bne ++
;+               ; File has image extension
;                inx
;                lda FREEMEM,x
;                cmp #$36; '6'
;                bne +
;                inx
;                lda FREEMEM,x
;                cmp #$34; '4'
;                bne ++
;                ; It's a d64
;                lda #1
;                rts
;+               ; Not a d64
;                inx
;                lda FREEMEM,x
;                cmp #$31; '1'
;                bne ++
;                dex
;                lda FREEMEM,x
;                cmp #$37; '7'
;                bne +
;                ; It's a d71
;                lda #2
;                rts
;+               cmp #$38; '8'
;                bne ++
;                lda #3
;                rts
;++              lda #0
;                rts