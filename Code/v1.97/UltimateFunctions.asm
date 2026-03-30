!source "UltimateLib.asm"

; Retrieves time from Ultimate
UltGetTime      jsr U_GetTime
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
UltReadSettings jsr UltGotoFlash
                lda error_code
                beq +
                ; Error changing dir
                rts
                ;
+               lda #FA_READ
                sta SMC_U_Open+1
                jsr UltOpenCfgFile
                bne +
                ; Success opening file for reading --> Read settings
                lda #<FREEMEM
                sta SMC_UltimatData+1
                lda #>FREEMEM
                sta SMC_UltimatData+2
                jsr U_ReadSmallFile
                ldx #(SIZE_OF_SETTINGS-1)
-               lda FREEMEM,x
                sta CSTM_Icons,x
                dex
                bpl -
                jsr U_CloseFile
                jsr U_GoUpDir; to root
                rts
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
                jsr U_GoUpDir; to root
                rts

UltSaveSettings jsr UltGotoFlash
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