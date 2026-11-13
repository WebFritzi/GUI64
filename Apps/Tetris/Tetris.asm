;!to "Tetris.d64",d64,"tetris.gui","tetris disk"
;
; Tetris for GUI64 (WIN and MAC), ACME assembler.
; 10 x 20 cells, seven tetrominoes, counterclockwise rotation without kicks.
; CRSR: move / soft drop / rotate; RETURN: hard drop; P: pause.
; All game logic and buffered drawing run in the main loop. The frame
; callback reads GUI_GetPressedKeys and advances the YAT SID music;
; no keyboard matrix or scratch zero page.
; Requires three-key snapshot and disk APIs at $5fa5..$5fae.

!source "../gui64.inc.asm"

WT_TETRIS       = 56
WT_COLORS       = 57
WT_NAME         = 58
WT_HIGHSCORES   = 59
CT_BOARD        = 50
CT_PREVIEW      = 51
CT_SEPARATOR    = 52
CT_CHARLABEL_ML = 53
CT_SIDEPANEL    = 54
ID_MENU_GAME    = 10
ID_MENU_OPTIONS = 11
ID_MENU_HELP    = 12
BOARD_W         = 10
BOARD_H         = 20
BOARD_SIZE      = BOARD_W * BOARD_H
FLASH_PHASE_TICKS = 2 ; each normal/background phase lasts 200 ms
NUM_CONTROLS    = 11
NUM_COLOR_CONTROLS = 14
COLORS_FIRST_PIECE = 3
COLORS_BACKGROUND = 11
COLORS_CANCEL   = 12
COLORS_OK       = 13
SOLID_CHAR      = 160 ; full block in both GUI64 character sets
APP_CHAR_O      = APP_CHAR_0
APP_CHAR_L      = APP_CHAR_1
APP_CHAR_J      = APP_CHAR_2
APP_CHAR_S      = APP_CHAR_3
APP_CHAR_Z      = APP_CHAR_4
APP_CHAR_I      = APP_CHAR_5
APP_CHAR_T      = APP_CHAR_6
STATUS_TEXT_LEN = 7 + 1 + 5 ; first row, GUI64 newline, second row
SOUND_MENU_LEN  = 9 ; fixed width for Sound On/Off, excluding terminator

IN_UP           = 1
IN_DOWN         = 2
IN_LEFT         = 4
IN_RIGHT        = 8
IN_DROP         = 16
IN_PAUSE        = 32

!zone Init
*=$b000
                cld
                lda #WT_TETRIS
                sta Param0
                jsr GUI_FindWndByType
                bcc .create
                stx Param0
                jmp GUI_SelectTopWindow
.create         lda #0
                sta Registered
                ; Create an empty window first, so control-pool capacity
                ; can be checked using the handles of all older windows.
                ldx #<Wnd_Tetris
                ldy #>Wnd_Tetris
                jsr GUI_CreateWindow
                lda $96                         ; CreateWindow result (res)
                bne .capacity
                rts
.capacity       lda #NUM_CONTROLS+NUM_COLOR_CONTROLS ; reserve space for the Colors dialog
                sta ControlCount
                lda CurrentWindow              ; new window is last handle
                sta InitHandle
.count          lda InitHandle
                beq .controls
                dec InitHandle
                lda InitHandle
                jsr GUI_GetWndInfo              ; returns window in FB/FC
                ldy #12                         ; WindowNumCtrls
                lda (ZP_FB),y
                clc
                adc ControlCount
                bcs .full
                cmp #97                         ; GUI64 reserves 96 controls
                bcs .full
                sta ControlCount
                jmp .count
.full           jsr GUI_KillCurWindow
                ldx #<Str_NoControls
                ldy #>Str_NoControls
                jmp GUI_ShowMessage
.controls       ldx #<CharList
                ldy #>CharList
                lda #7
                jsr GUI_RegisterChars           ; symbols 224..230 for the Colors dialog
                ldx #<CtrlAction
                ldy #>CtrlAction
                jsr GUI_SetCtrlActionsRoutine
                ldx #<PaintCtrls
                ldy #>PaintCtrls
                jsr GUI_SetPaintCtrlsRoutine
                ; Add the controls, preserving the definition pointer
                ; across API calls (the API may use FB-FE).
                lda #<Controls
                sta InitPtr
                lda #>Controls
                sta InitPtr+1
.add            lda InitPtr
                sta ZP_FB
                lda InitPtr+1
                sta ZP_FC
                ldy #0
                lda (ZP_FB),y
                beq .layout
                ldy #5
.string         lda (ZP_FB),y
                iny
                cmp #0
                bne .string
                tya
                clc
                adc InitPtr
                sta InitNext
                lda InitPtr+1
                adc #0
                sta InitNext+1
                ldx InitPtr
                ldy InitPtr+1
                jsr GUI_AddControl
                lda InitNext
                sta InitPtr
                lda InitNext+1
                sta InitPtr+1
                jmp .add
.layout         jsr GUI_GetDesign               ; Z=1: WIN, Z=0: MAC
                beq .menu
                dec WindowHeight                ; MAC menu is on desktop
                inc WindowPosY                  ; one extra row below the desktop menu
                jsr GUI_UpdateWindow
.menu           jsr LoadData
                jsr GUI_SelectControl0
                ldx #<Str_Menubar
                ldy #>Str_Menubar
                lda #3
                jsr GUI_SetCtrlStringList
                jsr GUI_SelectControl4
                lda PlayfieldColor              ; preview frame and background
                sta ControlColor
                jsr GUI_UpdateControl
                lda $dc04
                ora #1
                sta Seed
                lda $dc05
                sta Seed+1
                jsr ReadyGame
                ldx #<TimerProc
                ldy #>TimerProc
                jsr GUI_InitTimer
                ldx #<FrameHandler
                ldy #>FrameHandler
                jsr GUI_SetFrameHandler
                lda #1
                sta Registered
                jsr GUI_StartFrameHandling
                jsr GUI_StartTimer
                jmp ReportLoadIssue

!zone Events
TetrisWndProc   lda wndParam0
                cmp #EC_SHUTDOWN
                bne .normal
                lda Registered
                beq .leave
                jsr GUI_StopFrameHandling
                jsr MusicStop
                jsr GUI_StopTimer
                lda #0
                sta Registered
                ; Clear extension callbacks before this app is unloaded.
                ldx #0
                ldy #0
                jsr GUI_SetCtrlActionsRoutine
                jmp GUI_SetPaintCtrlsRoutine
.normal         jsr GUI_StdWndProc
                lda wndParam1
                cmp #PM_MENU
                bne .leave
                lda wndParam0
                cmp #EC_LBTNPRESS
                bne .leave
                jsr GUI_IsInCurMenu
                bcc .leave
                jsr GUI_GetCurMenuID
                cmp #ID_MENU_GAME
                beq .game
                cmp #ID_MENU_OPTIONS
                beq .options
                cmp #ID_MENU_HELP
                bne .leave
                jmp HelpMenuClicked
.game           jmp GameMenuClicked
.options        lda CurMenuItem
                beq .sound
                cmp #1
                bne .leave
                jmp ShowColorsDialog
.sound          jmp ToggleMusic
.leave          rts

!zone GameMenu
GameMenuClicked lda CurMenuItem
                beq .new
                cmp #1
                beq TogglePause
                cmp #2
                beq .scores
                cmp #3
                bne .leave
                jsr GUI_KillCurWindow
                jmp GUI_Repaint
.new            jsr NewGame
                jmp GUI_RepaintCurWindow
.scores         jmp ShowHighscores
.leave          rts

!zone HelpMenu
HelpMenuClicked lda CurMenuItem
                beq .info
                cmp #1
                bne .leave
                jsr ResetInput
                ldx #<Str_About
                ldy #>Str_About
                jmp GUI_ShowMessage
.info           jsr ResetInput
                ldx #<Str_SomeInfo
                ldy #>Str_SomeInfo
                jmp GUI_ShowMessage
.leave          rts

!zone Pause
TogglePause     lda Waiting
                bne .leave
                lda Paused
                eor #1
                sta Paused
                jsr ResetInput
                jsr UpdateStatus
                jmp GUI_RepaintCurWindow
.leave          rts

!zone Active
; C=1 only when the game is the active, expanded window in normal mode.
; GetTopWindow is IRQ-safe: its implementation only loads the priority
; list's first byte. No window handle is cached across heap compaction.
IsActive        lda DiskBusy
                bne .no
                lda Waiting
                bne .no
                lda ProgramMode
                bne .no
                lda CurrentWindow
                bmi .no
                lda WindowType
                cmp #WT_TETRIS
                bne .no
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne .no
                lda WindowHeight
                cmp #21
                bcc .no
                jsr GUI_GetTopWindow
                cmp CurrentWindow
                bne .no
                sec
                rts
.no             clc
                rts

; GUI64 calls this from the raster IRQ. Read the engine's three-key
; snapshot and key_shifted while the IRQ prevents another keyboard scan.
; The frame cadence captures edges between the 100 ms game timer ticks;
; a frame handler is a sampling choice, not a requirement for reading keys.
FrameHandler    jsr MusicFrame
                jsr IsActive
                bcs .sample
                lda #0
                sta InputHeld
                sta InputEdges
                lda #$ff                        ; require release on return
                sta PreviousInput
                rts
.sample         lda #0
                sta FrameKeys
                jsr GUI_GetPressedKeys
                stx FrameSecond
                sty FrameThird
                jsr DecodeKey                   ; first code in A
                lda FrameSecond
                jsr DecodeKey
                lda FrameThird
                jsr DecodeKey
                ; SHIFT is shared by both C64 cursor keys. Prefer left/down
                ; over left/rotate, and keep a held down key from becoming
                ; rotate when SHIFT or the horizontal key changes.
                lda FrameKeys
                and #IN_UP
                beq .edges
                lda FrameKeys
                and #IN_LEFT
                bne .down
                lda PreviousInput
                and #IN_DOWN
                beq .edges
.down           lda FrameKeys
                eor #(IN_UP + IN_DOWN)
                sta FrameKeys
.edges          lda PreviousInput
                eor #$ff
                and FrameKeys
                ora InputEdges
                sta InputEdges
                lda FrameKeys
                sta PreviousInput
                sta InputHeld
                rts

!zone DecodeKey
; Merge one translated key into the frame's game input bits.
; The getter preserves flags, so every code is compared explicitly.
DecodeKey       cmp #KEYCODE_CRSR_UD
                bne .horizontal
                lda #IN_DOWN
                ldx key_shifted
                beq .add
                lda #IN_UP
                bne .add
.horizontal     cmp #KEYCODE_CRSR_LR
                bne .return
                lda #IN_RIGHT
                ldx key_shifted
                beq .add
                lda #IN_LEFT
                bne .add
.return         cmp #KEYCODE_RETURN
                bne .pause
                lda #IN_DROP
                bne .add
.pause          and #$df                        ; accept P and shifted p
                cmp #$50
                bne .leave
                lda #IN_PAUSE
.add            ora FrameKeys
                sta FrameKeys
.leave          rts

!zone Timer
; 100 ms main-loop callback. Frozen ticks never advance gravity, and
; input collected during menus/inactive windows is discarded.
TimerProc       jsr IsActive
                bcs .active
                lda #0
                sta InputEdges
                sta RepeatCount
                sta LastHorizontal
                rts
.active         php
                sei                             ; atomic exchange with sampler
                lda InputEdges
                sta TickEdges
                lda #0
                sta InputEdges
                lda InputHeld
                sta TickHeld
                plp
                lda TickEdges
                and #IN_PAUSE
                beq .state
                jsr TogglePause
.state          lda Paused
                ora GameOver
                beq .animation
                rts
.animation      lda FlashTicks
                beq .play
                jsr AnimateLines
                jmp .paint
.play           lda #0
                sta Dirty
                ; Rotation and hard drop use edges, never key repeat.
                lda TickEdges
                and #IN_UP
                beq .drop
                jsr Rotate
.drop           lda TickEdges
                and #IN_DROP
                beq .horizontal
                jsr HardDrop
                jmp .paint                      ; don't move the new piece
.horizontal     lda TickHeld
                ora TickEdges                   ; retain short taps
                and #(IN_LEFT + IN_RIGHT)
                cmp #(IN_LEFT + IN_RIGHT)
                bne .one
                lda #0                          ; opposing directions cancel
.one            sta Horizontal
                cmp #0                          ; STA does not update Z
                beq .released
                cmp LastHorizontal
                bne .first
                lda TickEdges
                and #(IN_LEFT + IN_RIGHT)
                bne .first
                dec RepeatCount
                bne .gravity
                lda #2                          ; two checked cells per repeat
                sta HorizontalSteps
                lda #1                          ; repeat every 100 ms
                sta RepeatCount
                bne .move
.first          lda #2                          ; 200 ms initial repeat delay
                sta RepeatCount
                lda #1                          ; taps move exactly one cell
                sta HorizontalSteps
.move           lda Horizontal
                sta LastHorizontal
                cmp #IN_LEFT
                bne .right
                lda #$ff
                bne .step
.right          lda #1
.step           jsr MoveHorizontal
                dec HorizontalSteps
                beq .gravity
                lda MoveDX
                jmp .step
.released       sta LastHorizontal
                sta RepeatCount
.gravity        lda TickHeld
                ora TickEdges
                and #IN_DOWN
                beq .fall
                jsr StepDown                    ; soft drop: one row per tick
                jmp .paint
.fall           dec FallCount
                bne .paint
                jsr StepDown
.paint          lda Dirty
                beq .leave
                jsr ComposeBoard
                jsr GUI_RepaintCurWindow
                jmp OfferHighscore
.leave          rts

!zone Game
ReadyGame       lda #1
                sta Waiting
                bne ResetGame
NewGame         lda #0
                sta Waiting
ResetGame       jsr MusicReset
                lda #0
                sta Paused
                sta GameOver
                sta HighscoreChecked
                sta FlashTicks
                sta FlashHidden
                sta FlashDelay
                sta BagRemaining                ; a new game starts with a fresh bag
                sta LinesInLevel
                sta LastHorizontal
                sta RepeatCount
                ldx #0
.board          sta Board,x
                inx
                cpx #BOARD_SIZE
                bne .board
                lda #"0"
                ldx #6
.digits         sta ScoreDigits,x
                dex
                bpl .digits
                lda #1
                sta Level
                jsr SetSpeed
                jsr UpdateLevel
                jsr UpdateStatus
                jsr ResetInput
                lda Waiting
                bne .draw
                jsr RandomPiece
                sta NextPiece
                jsr SpawnPiece
.draw           jmp ComposeBoard

!zone ResetInput
ResetInput      php
                sei
                lda #0
                sta InputEdges
                sta InputHeld
                lda #$ff
                sta PreviousInput
                plp
                rts

; Offsets for four cells in a 4x4 rotation box are stored as x/y nibbles.
; Collision accepts cells only inside the visible 10x20 board.
; CandidateX may wrap to $ff: the unsigned bounds check rejects it.
!zone CheckPosition
CheckPosition   lda CandidateRotation
                asl
                asl
                sta ShapeIndex
                lda Piece
                asl
                asl
                asl
                asl
                clc
                adc ShapeIndex
                sta ShapeIndex
                ldx #0
.cell           stx CellNumber
                ldy ShapeIndex
                lda Shapes,y
                sta CellOffset
                and #15
                clc
                adc CandidateX
                cmp #BOARD_W
                bcs .blocked
                sta CellX
                lda CellOffset
                lsr
                lsr
                lsr
                lsr
                clc
                adc CandidateY
                cmp #BOARD_H
                bcs .blocked
                tax
                lda RowIndex,x
                clc
                adc CellX
                tax
                lda Board,x
                bne .blocked
                ldy CellNumber
                txa
                sta CandidateCells,y
                inc ShapeIndex
                ldx CellNumber
                inx
                cpx #4
                bne .cell
                sec
                rts
.blocked        clc
                rts

!zone LoadCandidate
LoadCandidate   lda PieceX
                sta CandidateX
                lda PieceY
                sta CandidateY
                lda Rotation
                sta CandidateRotation
                rts

!zone CommitCandidate
CommitCandidate lda CandidateX
                sta PieceX
                lda CandidateY
                sta PieceY
                lda CandidateRotation
                sta Rotation
                ldx #3
.cells          lda CandidateCells,x
                sta ActiveCells,x
                dex
                bpl .cells
                lda #1
                sta Dirty
                rts

!zone MoveHorizontal
MoveHorizontal  sta MoveDX
                jsr LoadCandidate
                lda CandidateX
                clc
                adc MoveDX
                sta CandidateX
                jsr CheckPosition
                bcc .leave
                jmp CommitCandidate
.leave          rts

!zone Rotate
Rotate          jsr LoadCandidate
                lda CandidateRotation
                clc
                adc #3                          ; previous orientation modulo 4
                and #3
                sta CandidateRotation
                jsr CheckPosition
                bcc .leave
                jmp CommitCandidate
.leave          rts

!zone StepDown
StepDown        lda FallDelay
                sta FallCount
                jsr LoadCandidate
                inc CandidateY
                jsr CheckPosition
                bcc LockPiece
                jmp CommitCandidate

!zone HardDrop
HardDrop        jsr LoadCandidate
.fall           inc CandidateY
                jsr CheckPosition
                bcc LockPiece
                jsr CommitCandidate
                jmp .fall

!zone LockPiece
LockPiece       lda Piece
                clc
                adc #1                          ; board stores shape IDs 1..7, not colors
                ldx #3
.lock           ldy ActiveCells,x
                sta Board,y
                dex
                bpl .lock
                jmp BeginLineClear

!zone BeginLineClear
; Keep the locked board intact until all three normal/background flashes end.
BeginLineClear  jsr MarkFullRows
                lda Cleared
                bne .flash
                jmp SpawnPiece
.flash          lda #6                          ; three normal/background pairs
                sta FlashTicks
                lda #FLASH_PHASE_TICKS
                sta FlashDelay
                lda #0
                sta FlashHidden
                sta RepeatCount
                sta LastHorizontal
                lda #1
                sta Dirty
                jmp ResetInput

!zone MarkFullRows
MarkFullRows    lda #0
                sta Cleared
                ldx #BOARD_H-1
.reset          sta FlashRows,x
                dex
                bpl .reset
                lda #BOARD_H-1
                sta ScanRow
.row            ldx ScanRow
                lda RowIndex,x
                tax
                ldy #BOARD_W
.full           lda Board,x
                beq .next
                inx
                dey
                bne .full
                inc Cleared
                ldx ScanRow
                lda #1
                sta FlashRows,x
.next           dec ScanRow
                bpl .row
                rts

!zone AnimateLines
AnimateLines    lda #0
                sta Dirty
                dec FlashDelay
                bne .leave
                lda #FLASH_PHASE_TICKS
                sta FlashDelay
                dec FlashTicks
                beq .finish
                lda FlashHidden
                eor #1
                sta FlashHidden
                lda #1
                sta Dirty
.leave          rts
.finish         lda #0
                sta FlashHidden
                jsr ClearLines
                jsr SpawnPiece
                jmp ResetInput

; Scan bottom-up. After a removal, recheck the same row, since it now
; contains the row above. Row zero is explicitly cleared after shifting.
!zone ClearLines
ClearLines      lda #0
                sta Cleared
                lda #BOARD_H-1
                sta ScanRow
.row            ldx ScanRow
                lda RowIndex,x
                tax
                ldy #BOARD_W
.full           lda Board,x
                beq .next
                inx
                dey
                bne .full
                inc Cleared
                ldx ScanRow
                lda RowIndex,x
                tax
                beq .top
.copy           dex
                lda Board,x
                sta Board+BOARD_W,x
                cpx #0
                bne .copy
.top            ldx #BOARD_W-1
                lda #0
.zero           sta Board,x
                dex
                bpl .zero
                jmp .row
.next           dec ScanRow
                bpl .row
                lda Cleared
                beq .leave
                ; Classic scoring: 40 / 100 / 300 / 1200 times level.
                tax
                lda ScoreLo-1,x
                sta Award
                lda ScoreHi-1,x
                sta Award+1
                lda Level
                sta AwardTimes
.award          lda Award
                sta Addend
                lda Award+1
                sta Addend+1
                jsr AddScore
                dec AwardTimes
                bne .award
                lda LinesInLevel
                clc
                adc Cleared
                cmp #10
                bcc .lines
                sbc #10
                ldx Level
                cpx #99
                bcs .lines
                inc Level
.lines          sta LinesInLevel
                jsr UpdateLevel
                jsr SetSpeed
.leave          rts

!zone SpawnPiece
SpawnPiece      lda NextPiece
                sta Piece
                jsr RandomPiece
                sta NextPiece
                lda #3
                sta CandidateX
                lda #0
                sta CandidateY
                sta CandidateRotation
                lda FallDelay
                sta FallCount
                jsr CheckPosition
                bcc .over
                jmp CommitCandidate
.over           lda #1
                sta GameOver
                sta Dirty
                jmp UpdateStatus

; Fisher-Yates seven-bag: each refill contains every piece exactly once.
; Rejection sampling picks a uniform index in 0..ShuffleLimit.
!zone RandomPiece
RandomPiece     ldx BagRemaining
                bne .draw
                ldx #6
.fill           txa
                sta PieceBag,x
                dex
                bpl .fill
                ldx #6
.shuffle        stx ShuffleLimit
.retry          jsr RandomByte
                and #7
                cmp ShuffleLimit
                bcc .swap
                beq .swap
                bcs .retry
.swap           tay
                lda PieceBag,x
                sta ShuffleTemp
                lda PieceBag,y
                sta PieceBag,x
                lda ShuffleTemp
                sta PieceBag,y
                dex
                bne .shuffle
                lda #7
                sta BagRemaining
.draw           dec BagRemaining
                ldx BagRemaining
                lda PieceBag,x
                rts

; Advance the 16-bit Galois LFSR eight times per byte, so successive
; shuffle indices do not reuse overlapping three-bit windows.
!zone RandomByte
RandomByte      lda Seed
                ora Seed+1
                bne .advance
                inc Seed                        ; recover from an all-zero seed
.advance        ldy #8
.bit            lsr Seed+1
                ror Seed
                bcc .next
                lda Seed+1
                eor #$b4
                sta Seed+1
.next           dey
                bne .bit
                lda Seed
                rts

!zone SetSpeed
SetSpeed        ldx Level
                cpx #10
                bcc .speed
                ldx #10
.speed          lda SpeedTable-1,x
                sta FallDelay
                sta FallCount
                rts

; Add a 16-bit Addend to seven decimal digits, saturating at 9999999.
; Convert by decimal places, then propagate carry from right to left.
; Work is bounded by the number of digits, not by the awarded points.
; Decimal-mode arithmetic is deliberately avoided (GUI64 runs with D=0).
!zone AddScore
AddScore        lda #0
                sta AddDigits
                sta AddDigits+1
                ldx #0
.place          lda #0
                sta DigitValue
.subtract       lda Addend
                sec
                sbc PlaceLo,x
                sta DigitLow
                lda Addend+1
                sbc PlaceHi,x
                bcc .digit
                sta Addend+1
                lda DigitLow
                sta Addend
                inc DigitValue
                jmp .subtract
.digit          lda DigitValue
                sta AddDigits+2,x
                inx
                cpx #5
                bne .place
                clc
                ldx #6
.sum            lda ScoreDigits,x
                adc AddDigits,x
                cmp #'9'+1
                bcc .store
                sbc #10                         ; CMP supplied C=1
.store          sta ScoreDigits,x               ; preserve decimal carry
                dex
                bpl .sum
                bcc .leave
                lda #"9"
                ldx #6
.saturate       sta ScoreDigits,x
                dex
                bpl .saturate
.leave          rts

!zone UpdateLevel
UpdateLevel     lda Level
                ldx #"0"
.tens           cmp #10
                bcc .ones
                sbc #10
                inx
                bne .tens
.ones           stx LevelDigits
                ora #"0"
                sta LevelDigits+1
                rts

!zone UpdateStatus
; Fixed-size text for the 7 x 2 multiline label, including the blank row.
UpdateStatus    ldx #STATUS_TEXT_LEN-1
.text           lda Str_Ready,x
                ldy Waiting
                bne .store
                lda Str_Playing,x
                ldy GameOver
                beq .pause
                lda Str_GameOver,x
                bne .store
.pause          ldy Paused
                beq .store
                lda Str_Paused,x
.store          sta StatusText,x
                dex
                bpl .text
                rts

!zone ColorsDialog
; Controls and raw character data are defined in Wnd_Colors below.
ShowColorsDialog
                jsr ResetInput
                ldx #<Wnd_Colors
                ldy #>Wnd_Colors
                jsr GUI_CreateWindowEx
                lda $96                         ; CreateWindow result (res)
                beq .leave
                lda #0
                sta ColorPickerIndex
.colors         lda ColorPickerIndex
                clc
                adc #COLORS_FIRST_PIECE
                jsr GUI_SelectControl
                ldx ColorPickerIndex
                lda ColorPickerPieces,x
                tax
                lda PieceColors,x
                sta ControlColor
                jsr GUI_UpdateControl
                inc ColorPickerIndex
                lda ColorPickerIndex
                cmp #7
                bcc .colors
                lda #COLORS_BACKGROUND
                jsr GUI_SelectControl
                lda PlayfieldColor
                sta ControlColor
                jsr GUI_UpdateControl
                ldy #COLORS_CANCEL
                lda #BIT_CTRL_DBLFRAME_BTM
                jsr GUI_SelectCtrl_AddBits
                ldy #COLORS_OK
                lda #BIT_CTRL_DBLFRAME_BTM
                jsr GUI_SelectCtrl_AddBits
                jsr GUI_GetDesign               ; Z=1: WIN, Z=0: MAC
                beq .show
                inc ControlPosX                 ; OK is still selected
                lda ControlBits
                ora #(BIT_CTRL_DBLFRAME_LFT + BIT_CTRL_DBLFRAME_RGT)
                sta ControlBits
                jsr GUI_UpdateControl
                ldy #COLORS_CANCEL
                lda #BIT_CTRL_DBLFRAME_RGT
                jsr GUI_SelectCtrl_AddBits
                lda ControlPosX
                clc
                adc #3
                sta ControlPosX
                jsr GUI_UpdateControl
.show
                jmp GUI_ShowWndAsDlg
.leave          rts

!zone ColorsDlgProc
ColorsDlgProc   jsr GUI_StdWndProc
                lda ProgramMode
                bne .event
                lda #PM_DIALOG                  ; color-menu selection returns to this dialog
                sta ProgramMode
.event
                lda wndParam0
                cmp #EC_KEYPRESS
                bne .mouse
                lda actkey
                cmp #KEYCODE_RETURN
                beq AcceptColorsDialog
                rts
.mouse          cmp #EC_LBTNRELEASE
                bne .leave
                jsr GUI_IsInCurControl
                bcc .leave
                lda ControlIndex
                cmp #COLORS_CANCEL
                beq CloseColorsDialog
                cmp #COLORS_OK
                beq AcceptColorsDialog
.leave          rts

AcceptColorsDialog
                lda #0
                sta ColorPickerIndex
.colors         lda ColorPickerIndex
                clc
                adc #COLORS_FIRST_PIECE
                jsr GUI_SelectControl
                ldx ColorPickerIndex
                lda ColorPickerPieces,x
                tax
                lda ControlColor
                and #15
                sta PieceColors,x
                inc ColorPickerIndex
                lda ColorPickerIndex
                cmp #7
                bcc .colors
                lda #COLORS_BACKGROUND
                jsr GUI_SelectControl
                lda ControlColor
                and #15
                sta PlayfieldColor
                jsr ComposeBoard
                jsr CloseColorsDialog
                jmp SaveData
CloseColorsDialog
                jsr ResetInput
                jmp GUI_KillDialog

!zone Painting
; Build a stable shape-ID map for repaints, including the active piece.
; Zero is empty; 1..7 identify L/J/O/S/Z/I/T, even when their color is black.
ComposeBoard    ldx #0
.copy           lda Board,x
                sta DisplayBoard,x
                inx
                cpx #BOARD_SIZE
                bne .copy
                lda GameOver
                ora Waiting
                bne .leave
                lda Piece
                clc
                adc #1
                ldx #3
.piece          ldy ActiveCells,x
                sta DisplayBoard,y
                dex
                bpl .piece
.leave          rts

CtrlAction      rts

!zone PaintCharLabelML
; ControlStrings: raw screen codes, $ff = newline, $00 = end.
; Clip long lines and extra rows to the control rectangle. Untouched
; cells retain their background; no automatic wrap or PETSCII conversion.
PaintCharLabelML lda ControlWidth
                beq .leave
                lda ControlHeight
                beq .leave
                sta CharLabelRows
                lda ControlStrings
                sta CharLabelSource
                lda ControlStrings+1
                sta CharLabelSource+1
                lda #0
                sta CharLabelColumn
.read           lda CharLabelSource
                sta ZP_FB
                lda CharLabelSource+1
                sta ZP_FC
                ldy #0
                lda (ZP_FB),y
                beq .leave
                inc CharLabelSource
                bne .code
                inc CharLabelSource+1
.code           cmp #$ff
                beq .newline
                ldy CharLabelColumn
                cpy ControlWidth
                bcs .read
                sta (ZP_FD),y
                lda ControlColor
                sta ($02),y
                inc CharLabelColumn
                jmp .read
.newline        dec CharLabelRows
                beq .leave
                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                lda #0
                sta CharLabelColumn
                jmp .read
.leave          rts

; FDFE is the control's character buffer, 0203 its color buffer.
; GUI64's buffer-stride helpers may alter scratch zero page, so data
; pointers are kept in app memory and reconstructed for each row.
PaintCtrls      cpx #CT_SIDEPANEL
                beq PaintSidePanel
                cpx #CT_CHARLABEL_ML
                beq PaintCharLabelML
                cpx #CT_BOARD
                beq PaintBoard
                cpx #CT_SEPARATOR
                beq PaintSeparator
                cpx #CT_PREVIEW
                bne .unknown
                jmp PaintPreview
.unknown        rts

!zone PaintSidePanel
; Refresh label colors before GUI64 paints them, then draw the panel.
; SelectControl, GetCSTMWindowColor and UpdateControl preserve X.
PaintSidePanel  ldx #5
.labels         txa
                jsr GUI_SelectControl
                jsr GUI_GetCSTMWindowColor
                sta ControlColor
                jsr GUI_UpdateControl
                inx
                cpx #NUM_CONTROLS
                bcc .labels
                jsr GUI_SelectControl1
                jsr GUI_GetCSTMWindowColor
                sta ControlColor
                jmp GUI_PaintBox

!zone PaintSeparator
; The board is painted afterwards and restores the shared left border.
PaintSeparator  jsr GUI_GetCSTMWindowColor
                sta ControlColor
                ldy #0
.cell           lda #96         ; clean horizontal line in both WIN and MAC
                cpy #9
                bne .put
                lda #40
.put            sta (ZP_FD),y
                lda ControlColor
                sta ($02),y
                iny
                cpy #10
                bne .cell
                rts

!zone PaintBoard
PaintBoard      lda #0
                sta PaintRow
                sta PaintIndex
.row            ldy #0
.cell           lda FlashHidden
                beq .normal
                ldx PaintRow
                lda FlashRows,x
                beq .normal
                lda PlayfieldColor              ; background phase; keep frame glyphs
                jmp .color                      ; black (0) is a valid background
.normal         ldx PaintIndex
                lda DisplayBoard,x
                beq .empty
                tax
                lda PieceColors-1,x
                jmp .color                      ; black pieces remain occupied
.empty
                lda PlayfieldColor              ; zero denotes an empty cell
.color          sta ($02),y
                ldx PaintRow
                beq .top
                cpx #BOARD_H-1
                beq .bottom
                lda #37                         ; left edge, also on a block
                cpy #0
                beq .put
                lda #41                         ; shared with the side panel
                cpy #BOARD_W-1
                beq .put
                lda #SOLID_CHAR
                bne .put
.top            lda BoardFrameTop,y
                bne .put
.bottom         lda BoardFrameBottom,y
.put            sta (ZP_FD),y
                inc PaintIndex
                iny
                cpy #BOARD_W
                bne .cell
                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                inc PaintRow
                lda PaintRow
                cmp #BOARD_H
                bne .row
                rts

!zone PaintPreview
PaintPreview    lda PlayfieldColor              ; custom control owns its frame color
                sta ControlColor
                jsr GUI_PaintBox                ; standard frame, returns FD/02
                jsr GUI_AddBufWidthToFD          ; first interior row
                jsr GUI_AddBufWidthTo02
                lda #0
                sta PaintRow
.row            ldy #4                          ; interior columns 1..4
                lda #SOLID_CHAR
.blank          sta (ZP_FD),y
                dey
                bne .blank
                ldy #4
                lda PlayfieldColor
.color          sta ($02),y
                dey
                bne .color
                lda Waiting
                bne .nextrow
                lda NextPiece
                asl
                asl
                asl
                asl
                tax
                lda NextPiece
                tay
                lda PreviewOffsets,y
                sta PreviewOffset
                lda PieceColors,y
                sta PaintColor
                lda PaintRow
                asl
                asl
                asl
                asl
                sta PaintY
                lda #4
                sta PaintCells
.piece          lda Shapes,x
                clc
                adc PreviewOffset
                and #$f0
                cmp PaintY
                bne .next
                lda Shapes,x
                clc
                adc PreviewOffset
                and #15
                clc
                adc #1                          ; leave left frame untouched
                tay
                lda PaintColor
                sta ($02),y
.next           inx
                dec PaintCells
                bne .piece
.nextrow        jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                inc PaintRow
                lda PaintRow
                cmp #4
                bne .row
                rts

!zone Data
; Edge cells keep their block colors; the right edge is also the panel's
; left border. Its corners join the panel's top/bottom horizontal lines.
BoardFrameTop   !byte 35,96,96,96,96,96,96,96,96,40
BoardFrameBottom !byte 38,39,39,39,39,39,39,39,39,42

; The boxes use fixed pivots. I has two distinct orientations, O one;
; S/Z two; J/L/T four. No wall kicks or lock delay.
; Each entry is (y * 16 + x), four entries per orientation.
Shapes          ; L
                !byte $02,$10,$11,$12, $01,$11,$21,$22
                !byte $10,$11,$12,$20, $00,$01,$11,$21
                ; J (mirrored L)
                !byte $00,$10,$11,$12, $01,$02,$11,$21
                !byte $10,$11,$12,$22, $01,$11,$20,$21
                ; O
                !byte $00,$01,$10,$11, $00,$01,$10,$11
                !byte $00,$01,$10,$11, $00,$01,$10,$11
                ; S
                !byte $01,$02,$10,$11, $01,$11,$12,$22
                !byte $01,$02,$10,$11, $01,$11,$12,$22
                ; Z
                !byte $00,$01,$11,$12, $02,$11,$12,$21
                !byte $00,$01,$11,$12, $02,$11,$12,$21
                ; I
                !byte $10,$11,$12,$13, $02,$12,$22,$32
                !byte $10,$11,$12,$13, $02,$12,$22,$32
                ; T
                !byte $01,$10,$11,$12, $01,$11,$12,$21
                !byte $10,$11,$12,$21, $01,$10,$11,$21
ShapesEnd
!if ShapesEnd-Shapes != 7*4*4 { !error "Invalid tetromino table" }

; App - Chars.bin, character indices 224..230 (eight bytes per glyph).
CharList        !byte $ff,$ff,$c3,$c3,$c3,$c3,$ff,$ff ; O
                !byte $ff,$cf,$cf,$cf,$cf,$c3,$c3,$ff ; L
                !byte $ff,$f3,$f3,$f3,$f3,$c3,$c3,$ff ; J
                !byte $ff,$ff,$e1,$e1,$87,$87,$ff,$ff ; S
                !byte $ff,$ff,$87,$87,$e1,$e1,$ff,$ff ; Z
                !byte $e7,$e7,$e7,$e7,$e7,$e7,$e7,$e7 ; I
                !byte $ff,$ff,$81,$81,$e7,$e7,$ff,$ff ; T
CharListEnd
!if CharListEnd-CharList != 7*8 { !error "Invalid app character table" }

; Colors in shape order: L, J, O, S, Z, I, T.
PieceColors     !byte CL_DARKGREEN,CL_RED,CL_LIGHTGREEN,CL_LIGHTBLUE
                !byte CL_DARKBLUE,CL_ORANGE,CL_ROSE
PlayfieldColor  !byte CL_WHITE                   ; shared board/preview background
; Dialog picker order: O, S, T, L, Z, J, I (indices into PieceColors).
ColorPickerPieces !byte 2,3,6,0,4,1,5
; Preview only: O moves right/down; S, Z and T move down one row.
PreviewOffsets  !byte 0,0,$11,$10,$10,0,$10
RowIndex        !for .row,0,BOARD_H-1 { !byte .row*BOARD_W }
SpeedTable      !byte 8,7,6,5,4,3,2,2,1,1 ; units: 100 ms
ScoreLo         !byte <40,<100,<300,<1200
ScoreHi         !byte >40,>100,>300,>1200
PlaceLo         !byte <10000,<1000,<100,<10,<1
PlaceHi         !byte >10000,>1000,>100,>10,>1

Registered      !byte 0
InitHandle      !byte 0
ControlCount    !byte 0
ColorPickerIndex !byte 0
InitPtr         !word 0
InitNext        !word 0
Seed            !word 1
PieceBag        !fill 7,0
BagRemaining    !byte 0
ShuffleLimit    !byte 0
ShuffleTemp     !byte 0
Piece           !byte 0
NextPiece       !byte 0
PieceX          !byte 0
PieceY          !byte 0
Rotation        !byte 0
CandidateX      !byte 0
CandidateY      !byte 0
CandidateRotation !byte 0
CandidateCells  !fill 4,0
ActiveCells     !fill 4,0
ShapeIndex      !byte 0
CellNumber      !byte 0
CellOffset      !byte 0
CellX           !byte 0
MoveDX          !byte 0
ScanRow         !byte 0
Cleared         !byte 0
Level           !byte 1
LinesInLevel    !byte 0
FallDelay       !byte 8
FallCount       !byte 8
Paused          !byte 0
GameOver        !byte 0
Waiting         !byte 1
FlashTicks      !byte 0
FlashHidden     !byte 0
FlashDelay      !byte 0
FlashRows       !fill BOARD_H,0
Dirty           !byte 0
Award           !word 0
AwardTimes      !byte 0
Addend          !word 0
AddDigits       !fill 7,0
DigitValue      !byte 0
DigitLow        !byte 0
InputHeld       !byte 0
InputEdges      !byte 0
PreviousInput   !byte $ff
FrameKeys       !byte 0
FrameSecond     !byte KEYCODE_NONE
FrameThird      !byte KEYCODE_NONE
TickHeld        !byte 0
TickEdges       !byte 0
Horizontal      !byte 0
HorizontalSteps !byte 0
LastHorizontal  !byte 0
RepeatCount     !byte 0
PaintRow        !byte 0
PaintIndex      !byte 0
PaintColor      !byte 0
PaintY          !byte 0
PaintCells      !byte 0
PreviewOffset   !byte 0
CharLabelSource !word 0
CharLabelColumn !byte 0
CharLabelRows   !byte 0
Board           !fill BOARD_SIZE,0
DisplayBoard    !fill BOARD_SIZE,0

Str_Title       !pet "Tetris",0
; Border glyphs share edge cells, retaining the color of any block.
; WIN occupies desktop rows 0..21; MAC occupies rows 2..22
; (desktop menu is row 0).
Wnd_Tetris      !byte WT_TETRIS,BIT_WND_CANMINIMIZE,10,0,19,22
                !word Str_Title,TetrisWndProc
Controls        ; 0: menu
                !byte CT_MENUBAR,<Menubar,>Menubar,0,0,0
                ; 1: side-panel frame using the current system window color
                !byte CT_SIDEPANEL,9,0,10,20,0
                ; 2: separator at row 16, using characters 96 and 40
                !byte CT_SEPARATOR,9,16,10,1,0
                ; 3: board (its right edge also restores the shared separator cell)
                !byte CT_BOARD,0,0,BOARD_W,BOARD_H,0
                ; 4: framed preview, 4x4 interior
                !byte CT_PREVIEW,11,8,6,6,0
                ; 5..10: information
                !byte CT_LABEL,11,1,8,1
                !pet "Score",0
                !byte CT_LABEL,11,2,7,1
ScoreDigits     !pet "0000000",0
                !byte CT_LABEL,11,4,8,1
                !pet "Level",0
                !byte CT_LABEL,11,5,2,1
LevelDigits     !pet "01",0
                !byte CT_LABEL,11,7,8,1
                !pet "Next",0
                !byte CT_LABEL_ML,11,17,7,2
StatusText      !pet "Playing",$1c,"     ",0
                !byte 0
Menubar         !word Menu_Game,Menu_Options,Menu_Help
Str_Menubar     !pet "Game",0,"Opts",0,"?",0
Menu_Game       !pet ID_MENU_GAME,10,4,"New",0,"Pause",0,"Highscores",0,"Exit",0
Menu_Options    !pet ID_MENU_OPTIONS,9,2
SoundMenuText   !pet "Sound On ",0 ; trailing space keeps both states nine chars wide
SoundMenuTextEnd
!if SoundMenuTextEnd-SoundMenuText != SOUND_MENU_LEN+1 { !error "Invalid sound menu buffer size" }
                !pet "Colors",0
Menu_Help       !pet ID_MENU_HELP,9,2,"Some Info",0,"About",0
Str_SomeInfo    !pet "Keys:",$1c
                !pet "- CRSR left/right: move",$1c
                !pet "- CRSR up: rotate",$1c
                !pet "- CRSR down: soft drop",$1c
                !pet "- RETURN: hard drop",$1c
                !pet "- P: pause",$1c,$1c
                !pet "Points & scores:",$1c
                !pet "-  1 line :   40xlevel",$1c
                !pet "-  2 lines:  100xlevel",$1c
                !pet "-  3 lines:  300xlevel",$1c
                !pet "-  4 lines: 1200xlevel",$1c
                !pet "- 10 lines: next level",0
Str_About       !pet "Tetris",$1c,"By WebFritzi",$1c,96," 2026",0
Str_Ready       !pet "Ready  ",$1c,"     "
Str_Playing     !pet "Playing",$1c,"     "
Str_Paused      !pet "Paused ",$1c,"     "
Str_GameOver    !pet " Game  ",$1c," Over"
Str_NoControls  !pet "Not enough free",$1c,"GUI64 controls.",0

; Colors dialog template: window header, controls, final zero.
Str_ColorsTitle !pet "Colors",0
Wnd_Colors      !byte WT_COLORS,0,10,2,20,19
                !word Str_ColorsTitle,ColorsDlgProc
                ; 0: Frame "Stones"
                !byte CT_FRAME,1,1,18,8
                !pet "Stones",0
                ; 1: Frame "Background"
                !byte CT_FRAME,1,10,18,5
                !pet "Background",0
                ; 2: CharLabel_ML
                !byte CT_CHARLABEL_ML,2,3,13,5
                !byte APP_CHAR_O,160,160,160,160,160
                !byte APP_CHAR_S,160,160,160,160,160
                !byte APP_CHAR_T,$ff,$ff
                !byte APP_CHAR_L,160,160,160,160,160,APP_CHAR_Z,$ff,$ff
                !byte APP_CHAR_J,160,160,160,160,160,APP_CHAR_I,0
                ; 3: ColorPicker O
                !byte CT_COLORPICKER,4,3,2,1,0
                ; 4: ColorPicker S
                !byte CT_COLORPICKER,10,3,2,1,0
                ; 5: ColorPicker T
                !byte CT_COLORPICKER,16,3,2,1,0
                ; 6: ColorPicker L
                !byte CT_COLORPICKER,4,5,2,1,0
                ; 7: ColorPicker Z
                !byte CT_COLORPICKER,10,5,2,1,0
                ; 8: ColorPicker J
                !byte CT_COLORPICKER,4,7,2,1,0
                ; 9: ColorPicker I
                !byte CT_COLORPICKER,10,7,2,1,0
                ; 10: MultilineLabel
                !byte CT_LABEL_ML,2,12,9,2
                !pet "Playfield",$1c,"& Preview",0
                ; 11: ColorPicker BKG Color
                !byte CT_COLORPICKER,16,12,2,1,0
                ; 12: Cancel button
                !byte CT_BUTTON,4,15,8,3
                !pet "Cancel",0               ; button caption fills width minus two
                ; 13: OK button (also closes with RETURN)
                !byte CT_BUTTON,13,15,6,3
                !pet " OK ",0 ; button caption fills width minus two
                !byte 0

!source "YatMusic.asm"
!source "Persistence.asm"

AppEnd
!if AppEnd > $d000 { !error "Tetris exceeds GUI64 app area ($b000-$cfff)" }
