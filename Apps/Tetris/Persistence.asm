; tetris.dat: 8 palette bytes (O/S/T/L/Z/J/I/background), then ten
; records: 10 PETSCII bytes padded with $20, 24-bit little-endian score.
; No load address, terminator, header or saved sound setting: exactly 138.
; All disk operations run on the main loop, using the public disk API.

DATA_SIZE = 138
SCORE_RECORD_SIZE = 13

!zone DiskIO
; GUI64 owns $02..$8f, while KERNAL file I/O also uses this range.
; Keep loop state in app RAM. Restore GUI zero page before EndDiskIO
; (WIN redraws the taskbar), and never pass GUI pointers across disk I/O.
BeginDataIO     lda #1
                sta DiskResult
                sta DiskBusy
                lda #0
                sta DiskStarted
                lda #255
                sta DiskDOS
                jsr GUI_GetAppDevice
                sta DiskDevice
                beq .failed
                php
                sei
                jsr MusicMute
                ldx #0
.save           lda $02,x
                sta DiskZeroPage,x
                inx
                cpx #$8e
                bne .save
                plp
                jsr GUI_BeginDiskIO
                lda #1
                sta DiskStarted
                lda #0
                tax
                tay
                jsr $ffbd                       ; SETNAM: command channel, no command
                lda #15
                ldx DiskDevice
                ldy #15
                jsr $ffba                       ; SETLFS
                jsr $ffc0                       ; OPEN
                bcs .failed
                clc
                rts
.failed         sec
                rts

!zone EndDataIO
EndDataIO       lda DiskStarted
                beq .done
                jsr $ffcc                       ; CLRCHN
                lda #2
                jsr $ffc3                       ; CLOSE data channel, also on errors
                lda #15
                jsr $ffc3
                ldx #0
.restore        lda DiskZeroPage,x
                sta $02,x
                inx
                cpx #$8e
                bne .restore
                jsr GUI_EndDiskIO
.done           lda #0
                sta DiskBusy
                jsr ResetInput
                lda DiskResult
                cmp #1                          ; C=0 success, C=1 failure
                rts

; Read DOS code and consume the complete response, bounded on bad devices.
!zone DiskStatus
DiskStatus      jsr $ffcc
                ldx #15
                jsr $ffc6                       ; CHKIN
                bcs .error
                jsr $ffcf                       ; CHRIN
                and #15
                sta DiskDOS
                asl
                asl
                clc
                adc DiskDOS
                asl
                sta DiskDOS
                jsr $ffcf
                and #15
                clc
                adc DiskDOS
                sta DiskDOS
                jsr $ffb7
                bne .error
                lda #40
                sta DiskCount
.tail           jsr $ffcf
                cmp #13
                beq .done
                jsr $ffb7                       ; READST
                bne .error
                dec DiskCount
                bne .tail
.error          lda #255
                sta DiskDOS
.done           jmp $ffcc

; A = command length, X/Y = command address; return DOS code in DiskDOS.
!zone DiskCommand
DiskCommand     sta DiskLength
                stx DiskPointer
                sty DiskPointer+1
                jsr $ffcc
                ldx #15
                jsr $ffc9                       ; CHKOUT
                lda #0
                sta DiskIndex
.byte           lda DiskPointer
                sta ZP_FB
                lda DiskPointer+1
                sta ZP_FC
                ldy DiskIndex
                lda (ZP_FB),y
                jsr $ffd2                       ; CHROUT
                inc DiskIndex
                lda DiskIndex
                cmp DiskLength
                bcc .byte
                jmp DiskStatus

!zone OpenDataFile
OpenDataFile    jsr $ffbd                       ; caller supplies filename in A/X/Y
                lda #2
                ldx DiskDevice
                ldy #2
                jsr $ffba
                jsr $ffc0
                bcc .status
                lda #255
                sta DiskDOS
                bne .failed
.status
                jsr DiskStatus
                lda DiskDOS
                bne .failed
                clc
                rts
.failed         sec
                rts

!zone ReadDataFile
ReadDataFile    sta DiskLength
                lda #1
                sta DiskResult
                lda DiskLength
                jsr OpenDataFile
                bcs .close
                ldx #2
                jsr $ffc6
                lda #0
                sta DiskIndex
.read           jsr $ffcf
                ldx DiskIndex
                sta DataBuffer,x
                jsr $ffb7
                sta DiskReadStatus
                and #$bf                        ; EOF allowed only on byte 138
                bne .close
                inc DiskIndex
                lda DiskIndex
                cmp #DATA_SIZE
                beq .last
                lda DiskReadStatus
                beq .read
                bne .invalid                    ; truncated file
.last           lda DiskReadStatus
                and #$40
                beq .invalid                    ; extra bytes / wrong format
                lda #0
                sta DiskResult
                beq .close
.invalid        lda #2
                sta DiskResult
.close          jsr $ffcc
                lda #2
                jsr $ffc3
                lda DiskResult
                bne .leave
                jsr ValidateData
                bcs .leave
                lda #2
                sta DiskResult
.leave          rts

!zone LoadData
LoadData        lda #0
                sta LoadIssue
                jsr BeginDataIO
                bcs .error
                lda #ReadDataNameEnd-ReadDataName
                ldx #<ReadDataName
                ldy #>ReadDataName
                jsr ReadDataFile
                lda DiskResult
                beq .finish
                lda DiskDOS
                sta DiskFirstDOS
                lda #ReadTempNameEnd-ReadTempName
                ldx #<ReadTempName
                ldy #>ReadTempName
                jsr ReadDataFile                 ; recover after interrupted replacement
                lda DiskResult
                beq .finish
                lda DiskFirstDOS
                cmp #62
                bne .error
                lda DiskDOS
                cmp #62
                bne .error
                lda #0                          ; both absent: normal first start
                sta DiskResult
                jsr EndDataIO
                rts
.error          lda #1
                sta LoadIssue
.finish         jsr EndDataIO
                bcs .leave
                ldx #0
.copy           lda DataBuffer,x
                sta SavedColors,x
                inx
                cpx #DATA_SIZE
                bcc .copy
                ldx #6
.colors         ldy ColorPickerPieces,x
                lda SavedColors,x
                sta PieceColors,y
                dex
                bpl .colors
                lda SavedColors+7
                sta PlayfieldColor
.leave          rts

!zone ReportLoadIssue
ReportLoadIssue lda LoadIssue
                beq .leave
                ldx #<Str_LoadError
                ldy #>Str_LoadError
                jmp GUI_ShowMessage
.leave          rts

!zone ValidateData
; C=1 valid: exact length checked by reader; valid colors, printable names,
; scores 0..9999999 and descending order. Never apply a partial record.
ValidateData    ldx #7
.colors         lda DataBuffer,x
                cmp #16
                bcs .bad
                dex
                bpl .colors
                ldx #8
.record         ldy #10
.name           lda DataBuffer,x
                cmp #32
                bcc .bad
                cmp #128
                bcc .printable
                cmp #160
                bcc .bad
                cmp #255
                beq .bad
.printable      inx
                dey
                bne .name
                lda DataBuffer+2,x
                cmp #$98
                bcc .range
                bne .bad
                lda DataBuffer+1,x
                cmp #$96
                bcc .range
                bne .bad
                lda DataBuffer,x
                cmp #$80
                bcs .bad
.range          cpx #18
                beq .next
                lda DataBuffer-11,x
                cmp DataBuffer+2,x
                bcc .bad
                bne .next
                lda DataBuffer-12,x
                cmp DataBuffer+1,x
                bcc .bad
                bne .next
                lda DataBuffer-13,x
                cmp DataBuffer,x
                bcc .bad
.next           inx
                inx
                inx
                cpx #DATA_SIZE
                bcc .record
                sec
                rts
.bad            clc
                rts

!zone SaveData
SaveData        ldx #6
.colors         ldy ColorPickerPieces,x
                lda PieceColors,y
                sta SavedColors,x
                dex
                bpl .colors
                lda PlayfieldColor
                sta SavedColors+7
                jsr BeginDataIO
                bcc .begin
                jmp .finish
.begin
                lda #ScratchTempEnd-ScratchTemp
                ldx #<ScratchTemp
                ldy #>ScratchTemp
                jsr DiskCommand
                lda DiskDOS
                cmp #2                          ; 00 or 01 FILES SCRATCHED
                bcs .finish
                lda #WriteTempNameEnd-WriteTempName
                ldx #<WriteTempName
                ldy #>WriteTempName
                jsr OpenDataFile
                bcs .finish
                ldx #2
                jsr $ffc9
                lda #0
                sta DiskIndex
.write          ldx DiskIndex
                lda SavedColors,x
                jsr $ffd2
                jsr $ffb7
                bne .finish
                inc DiskIndex
                lda DiskIndex
                cmp #DATA_SIZE
                bcc .write
                jsr $ffcc
                lda #2
                jsr $ffc3
                jsr DiskStatus                  ; includes errors reported on CLOSE
                lda DiskDOS
                bne .finish
                lda #ReadTempNameEnd-ReadTempName
                ldx #<ReadTempName
                ldy #>ReadTempName
                jsr ReadDataFile
                lda DiskResult
                bne .finish
                inc DiskResult                  ; subsequent command failures are failures
                ldx #0
.verify         lda DataBuffer,x
                cmp SavedColors,x
                bne .finish
                inx
                cpx #DATA_SIZE
                bcc .verify
                lda #ScratchDataEnd-ScratchData
                ldx #<ScratchData
                ldy #>ScratchData
                jsr DiskCommand
                lda DiskDOS
                cmp #2
                bcs .finish
                lda #RenameTempEnd-RenameTemp
                ldx #<RenameTemp
                ldy #>RenameTemp
                jsr DiskCommand
                lda DiskDOS
                bne .finish
                sta DiskResult
.finish         jsr EndDataIO
                bcc .leave
                ldx #<Str_SaveError
                ldy #>Str_SaveError
                jmp GUI_ShowMessage
.leave          rts

!zone Highscores
; Use decimal digits already displayed by the game to build a binary score.
ScoreToBinary   lda #0
                sta NewScore
                sta NewScore+1
                sta NewScore+2
                ldx #0
.digit          asl NewScore
                rol NewScore+1
                rol NewScore+2
                ldy #2
.times2         lda NewScore,y
                sta ScoreWork,y
                dey
                bpl .times2
                asl NewScore
                rol NewScore+1
                rol NewScore+2
                asl NewScore
                rol NewScore+1
                rol NewScore+2
                clc
                lda NewScore
                adc ScoreWork
                sta NewScore
                lda NewScore+1
                adc ScoreWork+1
                sta NewScore+1
                lda NewScore+2
                adc ScoreWork+2
                sta NewScore+2
                lda ScoreDigits,x
                and #15
                clc
                adc NewScore
                sta NewScore
                bcc .next
                inc NewScore+1
                bne .next
                inc NewScore+2
.next           inx
                cpx #7
                bne .digit
                rts

FindHighscore   jsr ScoreToBinary
                lda NewScore
                ora NewScore+1
                ora NewScore+2
                beq .none                       ; don't prompt for zero-point games
                ldx #0
.entry          lda Highscores+12,x
                cmp NewScore+2
                bne .different
                lda Highscores+11,x
                cmp NewScore+1
                bne .different
                lda Highscores+10,x
                cmp NewScore
.different      bcc .found                      ; ties retain their existing order
                txa
                clc
                adc #SCORE_RECORD_SIZE
                tax
                cpx #130
                bcc .entry
.none           ldx #$ff
.found          stx HighscoreOffset
                rts

OfferHighscore  lda GameOver
                beq .leave
                lda HighscoreChecked
                bne .leave
                inc HighscoreChecked
                jsr FindHighscore
                cpx #$ff
                beq .leave
                jsr ResetInput
                ldx #<Wnd_Name
                ldy #>Wnd_Name
                jsr GUI_CreateWindowEx
                lda $96
                beq .leave
                jsr GUI_GetDesign               ; Z=1: WIN, Z=0: MAC
                bne +
                ; WIN
                ldy #2                          ; OK button
                lda #BIT_CTRL_DBLFRAME_TOP
                jsr GUI_SelectCtrl_AddBits
                ldy #3                          ; Cancel button
                lda #BIT_CTRL_DBLFRAME_BTM
                jsr GUI_SelectCtrl_AddBits
                jmp ++
+               ; MAC
                lda #23
                sta WindowWidth
                inc WindowPosX
                jsr GUI_UpdateWindow
                jsr GUI_SelectControl2          ; OK button
                dec ControlPosX
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_BTM + BIT_CTRL_DBLFRAME_RGT)
                sta ControlBits
                jsr GUI_UpdateControl
                jsr GUI_SelectControl3          ; Cancel button
                dec ControlPosX
                dec ControlPosY
                lda #(BIT_CTRL_DBLFRAME_TOP + BIT_CTRL_DBLFRAME_RGT)
                sta ControlBits
                jsr GUI_UpdateControl
++              jsr GUI_SelectControl1
                lda #0
                ldx #10
.clearName      sta HighscoreName,x             ; start each name entry empty
                dex
                bpl .clearName
                ldx #<HighscoreName
                ldy #>HighscoreName
                jsr GUI_SetCtrlString
                lda #0                          ; first character position (zero-based)
                ldx #10
                jsr GUI_SetEditSLInfo
                lda #1
                sta WindowFocCtrl
                jsr GUI_UpdateWindow
                jmp GUI_ShowWndAsDlg
.leave          rts

!zone NameDialog
NameDlgProc     lda wndParam0
                cmp #EC_KEYPRESS
                bne .standard
                lda actkey
                cmp #KEYCODE_RETURN
                beq .accept
                cmp #$fd                        ; GUI64 backspace
                beq .edit
                cmp #32
                bcc .leave
                cmp #127
                bcs .leave                      ; ignore cursor/function/control codes
.edit           jsr GUI_SelectControl1          ; keyboard stays with the name field
.standard       jsr GUI_StdWndProc
                lda wndParam0
                cmp #EC_KEYPRESS
                bne .mouse
                lda actkey
                cmp #KEYCODE_RETURN
                beq .accept
                rts
.mouse          cmp #EC_LBTNRELEASE
                bne .leave
                jsr GUI_IsInCurControl
                bcc .leave
                lda ControlIndex
                cmp #3
                beq .close
                cmp #2
                bne .leave
.accept         jsr InsertHighscore
                jsr GUI_KillDialog
                jmp SaveData
.close          jsr ResetInput
                jmp GUI_KillDialog
.leave          rts

!zone InsertHighscore
InsertHighscore ldx #116                        ; shift complete 13-byte records down
.shift          cpx HighscoreOffset
                bcc .name
                lda Highscores,x
                sta Highscores+13,x
                dex
                bpl .shift
.name           ldx HighscoreOffset
                ldy #0
                lda #0
                sta NameEnded
.copy           lda NameEnded
                bne .space
                lda HighscoreName,y
                bne .put
                inc NameEnded
.space          lda #32
.put            sta Highscores,x
                inx
                iny
                cpy #10
                bcc .copy
                lda NewScore
                sta Highscores,x
                lda NewScore+1
                sta Highscores+1,x
                lda NewScore+2
                sta Highscores+2,x
                jmp ResetInput

!zone HighscoreText
; Fixed-size dialog label: ten rows of ten name characters and seven digits.
ShowHighscores  jsr ResetInput
                lda #0
                sta TextIndex
                sta RecordIndex
.record         ldx RecordIndex
                ldy #10
.checkName      lda Highscores,x
                cmp #32
                bne .hasName
                inx
                dey
                bne .checkName
                ; Render empty names as ten dots, retaining the saved spaces.
                ldy #10
                lda #"."
.dots           jsr AppendScoreText
                dey
                bne .dots
                beq .separator
.hasName        ldx RecordIndex
                ldy #10
.name           lda Highscores,x
                jsr AppendScoreText
                inx
                dey
                bne .name
.separator      lda #32
                jsr AppendScoreText
                lda Highscores,x
                sta ScoreWork
                lda Highscores+1,x
                sta ScoreWork+1
                lda Highscores+2,x
                sta ScoreWork+2
                ldx #0
.digit          lda #$30
                sta DecimalDigit
.subtract       lda ScoreWork+2
                cmp DecimalHi,x
                bne .compare
                lda ScoreWork+1
                cmp DecimalMid,x
                bne .compare
                lda ScoreWork
                cmp DecimalLo,x
.compare        bcc .emit
                lda ScoreWork
                sec
                sbc DecimalLo,x
                sta ScoreWork
                lda ScoreWork+1
                sbc DecimalMid,x
                sta ScoreWork+1
                lda ScoreWork+2
                sbc DecimalHi,x
                sta ScoreWork+2
                inc DecimalDigit
                bne .subtract
.emit           lda DecimalDigit
                jsr AppendScoreText
                inx
                cpx #7
                bcc .digit
                lda RecordIndex
                clc
                adc #13
                sta RecordIndex
                cmp #130
                beq .done
                lda #$1c
                jsr AppendScoreText
                jmp .record
.done           lda #0
                jsr AppendScoreText
                ldx #<Wnd_Highscores
                ldy #>Wnd_Highscores
                jsr GUI_CreateWindowEx
                lda $96
                beq .leave
                jsr GUI_SelectControl0
                ldx #<DataBuffer
                ldy #>DataBuffer
                jsr GUI_SetCtrlString
                lda #1
                sta WindowFocCtrl
                jsr GUI_GetDesign               ; Z=1: WIN, Z=0: MAC
                beq .show
                dec WindowHeight
                inc WindowPosY
                ldy #1                          ; MAC button shares right/bottom frame
                lda #(BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_BTM)
                jsr GUI_SelectCtrl_AddBits
                inc ControlPosX
                jsr GUI_UpdateControl
.show
                jsr GUI_UpdateWindow
                jmp GUI_ShowWndAsDlg
.leave          rts

AppendScoreText sty TextSavedY
                ldy TextIndex
                sta DataBuffer,y
                inc TextIndex
                ldy TextSavedY
                rts

!zone HighscoresDialog
HighscoresDlgProc
                jsr GUI_StdWndProc
                lda wndParam0
                cmp #EC_KEYPRESS
                bne .mouse
                lda actkey
                cmp #KEYCODE_RETURN
                beq .close
                rts
.mouse          cmp #EC_LBTNRELEASE
                bne .leave
                jsr GUI_IsInCurControl
                bcc .leave
                lda ControlIndex
                cmp #1                          ; OK button
                bne .leave
.close          jsr ResetInput
                jmp GUI_KillDialog
.leave          rts

!zone PersistenceData
SavedColors     !fill 8,0
Highscores      !for .entry,1,10 {
                    !fill 10,32
                    !byte 0,0,0
                }
!if *-SavedColors != DATA_SIZE { !error "Invalid tetris.dat layout" }
DataBuffer      !fill 190,0                     ; also scratch for the highscore message
DiskZeroPage    !fill $8e,0
DiskDevice      !byte 0
DiskBusy        !byte 0
DiskStarted     !byte 0
DiskResult      !byte 0
DiskDOS         !byte 0
DiskFirstDOS    !byte 0
DiskIndex       !byte 0
DiskReadStatus  !byte 0
DiskLength      !byte 0
DiskPointer     !word 0
DiskCount       !byte 0
LoadIssue       !byte 0
HighscoreChecked !byte 0
HighscoreOffset !byte $ff
HighscoreName   !fill 11,0
NameEnded       !byte 0
NewScore        !fill 3,0
ScoreWork       !fill 3,0
RecordIndex     !byte 0
TextIndex       !byte 0
TextSavedY      !byte 0
DecimalDigit    !byte 0
DecimalLo       !byte <1000000,<100000,<10000,<1000,<100,<10,<1
DecimalMid      !byte >1000000,>100000,>10000,>1000,>100,>10,>1
DecimalHi       !byte 1000000>>16,100000>>16,0,0,0,0,0
ReadDataName    !pet "tetris.dat,s,r"
ReadDataNameEnd
ReadTempName    !pet "tetris.tmp,s,r"
ReadTempNameEnd
WriteTempName   !pet "tetris.tmp,s,w"
WriteTempNameEnd
ScratchTemp     !pet "s0:tetris.tmp"
ScratchTempEnd
ScratchData     !pet "s0:tetris.dat"
ScratchDataEnd
RenameTemp      !pet "r0:tetris.dat=tetris.tmp"
RenameTempEnd
Str_LoadError   !pet "Cannot load tetris.dat.",$1c,"Using default values.",0
Str_SaveError   !pet "Cannot save tetris.dat.",$1c,"Values remain in memory.",0
Str_NameTitle   !pet "New Highscore!",0
Str_HighscoresTitle !pet "Highscores",0
Wnd_Highscores  !byte WT_HIGHSCORES,0,10,3,20,16
                !word Str_HighscoresTitle,HighscoresDlgProc
                ; 0: fixed-size highscore list, assigned DataBuffer before painting
                !byte CT_LABEL_ML,1,1,18,10,0
                ; 1: OK button
                !byte CT_BUTTON,13,11,6,3
                !pet " OK ",0
                !byte 0
Wnd_Name        !byte WT_NAME,0,7,5,25,7
                !word Str_NameTitle,NameDlgProc
                ; 0: Label
                !byte CT_LABEL,1,1,10,1
                !pet "Your name:",0
                ; 1: Edit
                !byte CT_EDIT_SL,1,2,14,3,0
                ; 2: Button OK
                !byte CT_BUTTON,16,0,8,3
                !pet "  OK  ",0
                ; 3: Button Cancel
                !byte CT_BUTTON,16,3,8,3
                !pet "Cancel",0
                !byte 0
