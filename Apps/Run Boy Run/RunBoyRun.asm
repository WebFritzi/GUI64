!to "RunBoyRun.d64",d64,"runboyrun.gui","runboyrun disk"

; Run Boy Run - a one button runner for GUI64 (MAC and WIN design),
; in the style of Canabalt
;
; Buildings of random heights (3 levels) with gaps between them scroll
; from right to left, the runner runs over their roofs. Press SPACE (or
; click into the window) to jump over the gaps - the longer SPACE (or
; the mouse button) is held, the higher the jump. Running into the wall
; of a higher building ends the run. Every char the line scrolls is a step (16 bit counter). The
; game gets faster over time.
;
; How it works:
; * Every cell of the line holds 0 (gap) or the level of the building
;   (1..3). The rows of the levels and the row below level 1 are drawn
;   with 4 base app chars: sky, building, and two edge chars
;   (building->sky, sky->building), plus left/right panel-edge variants.
;   A cell is building in a row if its level is at
;   least the level of the row, so the roof is the top of the building.
;   Every frame, the chars are redefined for the fine scroll position
;   (0..7 pixels, the windows of the buildings move, too), and every 8
;   pixels, the cells are shifted left by one char.
; * GUI64 has no key release event (and sends the mouse button release
;   only after the double click time), so the engine reads SPACE and
;   the fire lines of both ports (the mouse button) directly to find out
;   how long they are held.
; * The runner is sprite 6 (free in both designs: GUI64 uses 0-1 for
;   the mouse and, in the WIN design, 2-5 for the taskbar logo).
; * Like in Roaches, the game runs in an "engine", which is GUI64's
;   frame handler (GUI_SetFrameHandler, called from GUI64's raster IRQ,
;   50 frames per second). The engine stays in the normal application
;   code; only the sprite bitmap data is copied to $c400 in GUI64's VIC
;   bank. The engine draws the line, the step counter and the status text
;   directly into the screen while the window is the current window.
;   GUI64's repaints use the same data (custom control and labels). On
;   EC_SHUTDOWN, the frame handling is stopped.

!source "../gui64.inc.asm"

!zone Constants
WT_RUNBOYRUN     = 53 ; app window types start at 50
CT_PLAYFIELD     = 50 ; app control types start at 50
ID_MENU_GAME     = 10 ; menu IDs start at 10
ID_MENU_HELP     = 11

SPRITE_BASE      = $c400 ; sprite bitmap destination in the VIC bank

SPRITE_NO        = 6     ; apps for both designs may use sprites 6 and 7
SPRITE_BIT       = %01000000
CLOUD_BIT        = %10000000
SPRITE_BITS      = %11000000
RUNNER_COLOR     = CL_BLACK

; Layout (content coordinates of the window)
PF_X             = 1  ; playfield control
PF_Y             = 3
PF_W             = 30
PF_H             = 9
NUM_LEVELS       = 3
GROUND_ROW       = 7  ; row of the roofs of the lowest buildings (level 1) in the playfield
TOP_ROW          = GROUND_ROW - NUM_LEVELS + 1 ; row of the highest roofs
RUNNER_COL       = 4  ; column of the runner in the window
NUM_CELLS        = PF_W + 1 ; one more cell for the right edge
FOOT_X           = (RUNNER_COL - PF_X) * 8 + 4 ; runner's foot in the line
; Content row 0 is the second row of the window header, so the
; controls start in row 1
SCORE_Y          = 1  ; row of steps and best
STEPS_X          = 1
BEST_X           = 19
STATUS_Y         = 2
STATUS_X         = 1
STATUS_LEN       = 26

; App chars of the buildings. Set pixels have the window color (sky and
; lit windows), the buildings are drawn with cleared (black) pixels.
; The order matters: char = CH_GAP + 2 * (building here) + (building in
; the next cell)
CH_GAP           = APP_CHAR_0
CH_GS            = APP_CHAR_1 ; sky -> building
CH_SG            = APP_CHAR_2 ; building -> sky
CH_SOLID         = APP_CHAR_3
; Variants for the two vertical CT_PANEL edges.  They keep the complete
; playfield cell and add only the edge pixel, so the houses can scroll
; through the first and last playfield columns without erasing the box.
CH_GAP_L         = APP_CHAR_4
CH_GS_L          = APP_CHAR_5
CH_SG_L          = APP_CHAR_6
CH_SOLID_L       = APP_CHAR_7
CH_GAP_R         = APP_CHAR_8
CH_GS_R          = APP_CHAR_9
CH_SG_R          = APP_CHAR_10
CH_SOLID_R       = APP_CHAR_11
EDGE_L_OFS       = CH_GAP_L - CH_GAP
EDGE_R_OFS       = CH_GAP_R - CH_GAP
; Black panel edge on light-blue character color: a cleared bit is black.
; The panel occupies the outermost pixel of the first/last playfield cell.
PANEL_L_MASK     = %01111111   ; clear leftmost pixel (bit 7)
PANEL_R_MASK     = %11111110   ; clear rightmost pixel (bit 0)
WINDOWS          = 0;%01100110  ; lit windows in the rows 3 and 4 of a char

; Parts of the window the engine has to redraw
DIRTY_LINE       = 1 ; the buildings
DIRTY_STEPS      = 2 ; step counter
DIRTY_STATUS     = 4 ; best and status text
DIRTY_ALL        = DIRTY_LINE | DIRTY_STEPS | DIRTY_STATUS

; Game states
ST_READY         = 0
ST_RUN           = 1
ST_JUMP          = 2
ST_FALL          = 3
ST_OVER          = 4

; Speed in 1/16 pixels per frame
SPEED_START      = 32
SPEED_MAX        = 96 ; 6 pixels/frame
SPEED_STEP       = 2   ; faster every 64 steps

; Jump physics. Velocities in 1/16 pixels per frame (up is positive),
; gravity in 1/16 pixels per frame^2. While the button is held, the
; runner keeps rising without gravity for up to HOLD_MAX frames.
; Jump height: about 8 pixels without holding, up to 31 pixels held.
JUMP_VEL         = 32
GRAVITY          = 4
HOLD_MAX         = 12
VEL_MAX          = 96  ; max. falling speed
STEP_UP          = 4   ; the runner climbs a wall up to 4 pixels high
; Heights are biased by HBASE, so that they are always positive:
; HBASE is the top of a level 1 building, each level is 8 pixels higher.
HBASE            = 64
FALL_OUT         = HBASE - 16 ; game over below this height

!zone Init
*=$b000
                jsr GUI_StopFrameHandling       ; no stale frame callback while copying
                ; Copy only the sprite bitmap data into the VIC bank.
                ; The engine code itself stays at its normal load address.
                lda #<SpriteData
                sta ZP_FB
                lda #>SpriteData
                sta ZP_FC
                lda #<SPRITE_BASE
                sta ZP_FD
                lda #>SPRITE_BASE
                sta ZP_FE
                ; Copy exactly SPRITE_BYTES bytes. Complete 256-byte pages
                ; first, followed by the remaining low-byte count.
                ldx #>SPRITE_BYTES
                beq .copySpriteTail
                ldy #0
.copySpritePage  lda (ZP_FB),y
                sta (ZP_FD),y
                iny
                bne .copySpritePage
                inc ZP_FC
                inc ZP_FE
                dex
                bne .copySpritePage
.copySpriteTail  ldy #0
                cpy #<SPRITE_BYTES
                beq .copySpriteDone
.copySpriteByte  lda (ZP_FB),y
                sta (ZP_FD),y
                iny
                cpy #<SPRITE_BYTES
                bne .copySpriteByte
.copySpriteDone ;
                ldx #<CharList                  ; Register the
                ldy #>CharList                  ; chars of
                lda #12                         ; the line, including the
                                                ; left/right panel-edge variants
                jsr GUI_RegisterChars           ;
                ldx #<CtrlAction                ; Behavior of the playfield
                ldy #>CtrlAction                ; (void, events are handled
                jsr GUI_SetCtrlActionsRoutine   ; in the window proc)
                ldx #<PaintCtrls                ; Look of the
                ldy #>PaintCtrls                ; playfield
                jsr GUI_SetPaintCtrlsRoutine    ;
                ;
                ldx #<Wnd_RunBoyRun             ; Create the window with
                ldy #>Wnd_RunBoyRun             ; its controls
                jsr GUI_CreateWindowEx          ;
                lda CurrentWindow
                sta GameWindow
                lda #1                          ; WIN: content row 0 is below the
                sta RowOffset                   ; menu bar of the window
                jsr GUI_GetDesign               ; Z=1: WIN, Z=0: MAC
                beq +                           ; MAC: the window table y is one row
                inc WindowPosY                  ; above the screen row, the menu bar
                dec WindowHeight                ; is at the top of the screen
                jsr GUI_UpdateWindow            ; confirm window changes
+               ; Menu
                jsr GUI_SelectControl0          ; Associate the menu bar
                ldx #<Str_RBRMenubar            ; strings with control 0
                ldy #>Str_RBRMenubar            ;
                lda #2                          ; 2 strings
                jsr GUI_SetCtrlStringList       ;
                ; Labels show the text buffers of the engine
                jsr GUI_SelectControl1
                ldx #<StepsText
                ldy #>StepsText
                jsr GUI_SetCtrlString
                jsr GUI_SelectControl2
                ldx #<BestText
                ldy #>BestText
                jsr GUI_SetCtrlString
                jsr GUI_SelectControl3
                ldx #<StatusText
                ldy #>StatusText
                jsr GUI_SetCtrlString
                ; Panel
                lda #4
                jsr GUI_SelectControl
                lda #CL_LIGHTBLUE
                sta ControlColor
                jsr GUI_UpdateControl
                ; Do not force WindowFocCtrl here. CT_PANEL is not an input
                ; control, and changing the window focus/control state here can
                ; interfere with GUI64 menu and dialog handling.
                jmp EngineStart

; Window Proc (event handler for window)
RunBoyRunWndProc lda wndParam0                  ; app shuts down (window closed)?
                cmp #EC_SHUTDOWN
                bne +
                jmp StopEngine                  ; then stop the frame handler
+               jsr GUI_StdWndProc              ; MUST always be called
                lda wndParam1                   ; ProgramMode
                bmi .leave                      ; leave in dialog mode
                beq .normal                     ; normal mode
                ; menu mode
                lda wndParam0                   ; event code
                cmp #EC_LBTNPRESS               ;
                bne .leave                      ;
                jsr GUI_IsInCurMenu             ; mouse in current menu?
                bcc .leave                      ;
                jsr GUI_GetCurMenuID            ;
                cmp #ID_MENU_GAME               ;
                beq GameMenuClicked             ;
                jmp HelpMenuClicked             ;
.normal         lda wndParam0                   ; event code
                cmp #EC_KEYPRESS
                bne +
                lda actkey
                cmp #" "                        ; SPACE?
                beq .jump
                rts
+               cmp #EC_DBLCLICK                ; (a quick second click)
                beq .mouse
                cmp #EC_LBTNPRESS               ; click into panel/playfield?
                bne .leave
.mouse          ; Do not select the panel here: GUI_SelectControl changes
                ; GUI64's global current-control state and can disturb menu/
                ; dialog processing. Test the panel rectangle directly.
                jsr GUI_GetMousePosInWnd
                lda MousePosInWndX
                cmp #PF_X
                bcc .leave
                cmp #PF_X + PF_W
                bcs .leave
                lda MousePosInWndY
                cmp #PF_Y
                bcc .leave
                cmp #PF_Y + PF_H
                bcs .leave
.jump           lda #1                          ; the engine does the rest
                sta JumpReq
.leave          rts

; Invoked when an item in the game menu was clicked
GameMenuClicked lda CurMenuItem                 ; 0: New
                bne +
                php                             ; back to the start screen
                sei                             ; (the engine must not run
                jsr ResetLine                   ; in between)
                lda #ST_READY
                sta State
                lda #213
                sta CloudPosX
                plp
                rts
+               jsr StopEngine                  ; 1: Quit - stop callback before killing wnd
                jsr GUI_KillCurWindow
                jmp GUI_Repaint

; Invoked when an item in the help menu was clicked
HelpMenuClicked lda CurMenuItem
                bne +
                ldx #<Str_Mess_Help             ; 0: Help
                ldy #>Str_Mess_Help
                jmp GUI_ShowMessage
+               ldx #<Str_Mess_About            ; 1: About
                ldy #>Str_Mess_About
                jmp GUI_ShowMessage

; Starts the game and the engine (GUI64's frame handler)
EngineStart     lda $dc04                       ; seed random generator
                ora #1                          ; (must not be 0)
                sta Seed
                lda $dc05
                sta Seed+1
                jsr GUI_GetScreenMem            ; high byte of the screen
                sta ScreenHi                    ; (MAC: $e0, WIN: $e4)
                lda WindowPosX                  ; SetRow needs the current
                sta WinX                        ; window position already here
                lda WindowPosY
                sta WinY
                ; Init cloud strpite pointer
                ldx ScreenHi
                inx
                inx
                inx
                stx ptr+2
                lda #CLOUD_SPR_PTR
ptr             sta $03ff
                lda #114                        ; cloud y pos
                sta $d00f
                lda #CL_WHITE                   ; cloud color = white
                sta $d000+46
                ;
                jsr ResetLine                   ; (State is ST_READY)
                ldx #<FrameHandler              ; GameFrame runs once
                ldy #>FrameHandler              ; per frame from now on
                jsr GUI_SetFrameHandler
                jmp GUI_StartFrameHandling

; X is ControlType
CtrlAction      rts                             ; Must be provided if new controls are registered

; X is ControlType
; FDFE points to position of control in paint buffer
; 0203 points to position of control in color buffer
PaintCtrls      cpx #CT_PLAYFIELD
                beq +
                rts
+               ldx #TOP_ROW                    ; go down to the highest level
-               jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                dex
                bne -
                jsr GUI_GetCSTMWindowColor      ; (not in the zero page, the
                sta PaintColor                  ; GUI64 routines below use it)
                ldx #NUM_LEVELS                 ; one row per level, and X=0:
                                                ; the row below level 1
.row            ldy #PF_W-1
-               jsr CellChar
                sta ($fd),y
                ;lda PaintColor
                ;sta ($02),y
                dey
                bpl -
                dex
                bmi +
                jsr GUI_AddBufWidthToFD
                jsr GUI_AddBufWidthTo02
                jmp .row
+               rts

; Y = playfield column, X = level of the row (0: row below level 1)
; -> A = char there. X and Y are preserved. Must not use any variables
; of the engine, as the engine's IRQ can interrupt GUI64's repaint.
; (The engine's Render uses the tables Level3Chars etc. instead.)
CellChar        stx PaintLevel
                txa
                bne +
                inc PaintLevel                  ; row below level 1: like level 1
+               lda Cells,y
                cmp PaintLevel                  ; C=1: building in this cell
                lda #0
                rol
                asl
                sta PaintChar
                lda Cells+1,y
                cmp PaintLevel
                lda #CH_GAP
                adc PaintChar                   ; normal playfield char
                ; PaintCtrls uses Y = 0..PF_W-1.
                ; At the two sides use the panel-edge variants.
                cpy #0
                beq .leftEdge
                cpy #PF_W-1
                beq .rightEdge
                rts
.leftEdge       clc
                adc #EDGE_L_OFS
                rts
.rightEdge      clc
                adc #EDGE_R_OFS
                rts
!zone Data
GameWindow      !byte 0
PaintColor      !byte 0
PaintLevel      !byte 0
PaintChar       !byte 0
Str_Title_App   !pet "Run Boy Run",0

; Definition of app window
; type, bits, xpos, ypos, width, height, address of string in title bar, address of wnd proc
Wnd_RunBoyRun   !byte WT_RUNBOYRUN, %00100001, 4, 3, 32, 15, <Str_Title_App, >Str_Title_App
                !byte <RunBoyRunWndProc, >RunBoyRunWndProc
; Followed by control definitions (necessary for call CreateWindowEx)
; type, xpos, ypos, width, height, control string (null terminated)
                ;0
                !byte CT_MENUBAR, <RBRMenubar, >RBRMenubar, 0, 0
                !pet 0
                ;1
                !byte CT_LABEL, STEPS_X, SCORE_Y, 12, 1
                !pet 0
                ;2
                !byte CT_LABEL, BEST_X, SCORE_Y, 11, 1
                !pet 0
                ;3
                !byte CT_LABEL, STATUS_X, STATUS_Y, STATUS_LEN, 1
                !pet 0
                ;4
                !byte CT_PANEL, PF_X, PF_Y, PF_W, PF_H
                !pet 0
                ;5
                !byte CT_PLAYFIELD, PF_X, PF_Y, PF_W, PF_H
                !pet 0
                ; closing zero byte
                !byte 0

; Strings
Str_Mess_Help   !pet "SPACE or click: jump\Hold it: jump higher\Don't hit the walls",0
Str_Mess_About  !pet "Run Boy Run\A one button runner\for GUI64",0

; Definition of menu bar
RBRMenubar      !word Menu_RBR_Game, Menu_RBR_Help
Str_RBRMenubar  !pet "Game",0,"?",0
; Definition of menus
; Format: ID, max_str_len, item_count, StringList
Menu_RBR_Game   !pet ID_MENU_GAME,4,2,"New",0,"Quit",0
Menu_RBR_Help   !pet ID_MENU_HELP,5,2,"Help",0,"About",0

; Chars of the buildings. EdgeChars redefines the scrolling variants
; every frame. The L/R sets combine the playfield with the black panel
; edge: the edge is visible only in sky pixels; buildings are in front.
CharList        !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; sky
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; sky -> building
                !byte $00,$00,$00,WINDOWS,WINDOWS,$00,$00,$00 ; building -> sky
                !byte $00,$00,$00,WINDOWS,WINDOWS,$00,$00,$00 ; building
                ; left panel-edge variants (redefined by EdgeChars)
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
                !byte $00,$00,$00,WINDOWS,WINDOWS,$00,$00,$00
                !byte $00,$00,$00,WINDOWS,WINDOWS,$00,$00,$00
                ; right panel-edge variants (redefined by EdgeChars)
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
                !byte $00,$00,$00,WINDOWS,WINDOWS,$00,$00,$00
                !byte $00,$00,$00,WINDOWS,WINDOWS,$00,$00,$00

;======================================================================
; Engine - stays at its normal application load address.
; Only SpriteData is copied to SPRITE_BASE ($c400).
;======================================================================
!zone Engine
; Sprite bitmap source. The VIC uses the copy at SPRITE_BASE.
SpriteData      ; 0: start
                !byte %00001100,0,0, %00001100,0,0, %00011000,0,0, %00011000,0,0
                !byte %00011100,0,0, %00011000,0,0, %00011000,0,0, %00011000,0,0
                !fill 64-24,0
                ; 1: run 1
                !byte %00011000,0,0, %00011000,0,0, %00110000,0,0, %01111100,0,0
                !byte %10110000,0,0, %01010000,0,0, %00001000,0,0, %00000100,0,0
                !fill 64-24,0
                ; 2: run 2
                !byte %00011000,0,0, %00011000,0,0, %01110100,0,0, %01111000,0,0
                !byte %00110000,0,0, %11010000,0,0, %00001000,0,0, %00001000,0,0
                !fill 64-24,0
                ; 3: run 3
                !byte %00011000,0,0, %00011000,0,0, %00110000,0,0, %00111000,0,0
                !byte %00110100,0,0, %00110000,0,0, %01100000,0,0, %00110000,0,0
                !fill 64-24,0
                ; 4: run 4
                !byte %00011000,0,0, %00011000,0,0, %00110000,0,0, %00110000,0,0
                !byte %00111100,0,0, %00110000,0,0, %01010000,0,0, %10100000,0,0
                !fill 64-24,0
                ; 5: run 5
                !byte %00011000,0,0, %00011000,0,0, %00110000,0,0, %00110000,0,0
                !byte %00011100,0,0, %00111000,0,0, %00100100,0,0, %01000100,0,0
                !fill 64-24,0
                ; 6: run 6
                !byte %00011000,0,0, %00011000,0,0, %01110000,0,0, %10110000,0,0
                !byte %00111000,0,0, %01110000,0,0, %11001000,0,0, %00000100,0,0
                !fill 64-24,0
                ; 7: jump: take-off
                !byte %00011000,0,0, %00011000,0,0, %00110000,0,0, %00110000,0,0
                !byte %00111000,0,0, %00100100,0,0, %01000000,0,0, %01000000,0,0
                !fill 64-24,0
                ; 8: jump: rising
                !byte %00011000,0,0, %00011000,0,0, %00110000,0,0, %00110000,0,0
                !byte %00111100,0,0, %00110010,0,0, %00100000,0,0, %01000000,0,0
                !fill 64-24,0
                ; 9: jump: top
                !byte %00001100,0,0, %00011100,0,0, %00110000,0,0, %00111000,0,0
                !byte %00110100,0,0, %00011010,0,0
                !fill 64-18,0
                ; 10: jump: falling
                !byte %00011010,0,0, %00011100,0,0, %00111000,0,0, %01011000,0,0
                !byte %00011000,0,0, %00001100,0,0, %00001100,0,0, %00000100,0,0
                !fill 64-24,0
                ; 11: jump: landing
                !byte %00011000,0,0, %00011010,0,0, %00111100,0,0, %01011000,0,0
                !byte %01011000,0,0, %00011000,0,0, %00001000,0,0, %00001000,0,0
                !fill 64-24,0
                ; 12: manipulated cloud
                !byte $00,$00,$00,$00,$00,$00,$01,$c0
                !byte $00,$07,$f0,$00,$0c,$f8,$00,$1b
                !byte $fd,$e0,$17,$ff,$f8,$17,$ff,$fc
                !byte $3f,$ff,$fc,$7f,$ff,$fe,$7f,$ff
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
                !byte $ff,$ff,$7f,$ff,$f6,$7f,$ff,$f4
                !byte $1d,$ff,$ec,$01,$ff,$98,$00,$ff
                !byte $f0,$00,$7c,$e0,$00,$38,$00,$01
                ; 13: original cloud
                !byte $00,$00,$00,$00,$00,$00,$01,$c0
                !byte $00,$07,$f0,$00,$0c,$f8,$00,$1b
                !byte $fd,$e0,$17,$ff,$f8,$17,$ff,$fc
                !byte $3f,$ff,$fc,$7f,$ff,$fe,$7f,$ff
                !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
                !byte $ff,$ff,$7f,$ff,$f6,$7f,$ff,$f4
                !byte $1d,$ff,$ec,$01,$ff,$98,$00,$ff
                !byte $f0,$00,$7c,$e0,$00,$38,$00,$01
SpriteDataEnd
SPRITE_BYTES    = SpriteDataEnd - SpriteData
SPRITE_PTR0     = (SPRITE_BASE - $c000) / 64
CLOUD_SPR_PTR   = ($c700 - $c000) / 64
FRAME_START     = 0   ; standing (before the game starts)
FRAME_RUN       = 1   ; run: 6 frames, the next one every RUN_STEP
RUN_FRAMES      = 6   ; pixels scrolled (so the feet match the ground)
RUN_STEP        = 5
; AnimSub is kept below RUN_STEP*16 (=80). With SPEED_MAX=160,
; the largest addition is 79+160=239, so the 8-bit accumulator is safe.
FRAME_JUMP      = 7   ; jump: 5 frames, selected by the velocity
JUMP_FRAMES     = 5
; Lowest velocity of the jump frames 7-10 (+$80, so that they compare
; unsigned; below the last one: frame 11)
JumpVels        !byte 128+24, 128+8, 128-8, 128-32

; Chars of the rows of the levels, index = cell * 4 + next cell
; (a cell is building in the row of level L if its level is >= L)
Level3Chars     !byte CH_GAP, CH_GAP, CH_GAP, CH_GS,    CH_GAP, CH_GAP, CH_GAP, CH_GS
                !byte CH_GAP, CH_GAP, CH_GAP, CH_GS,    CH_SG,  CH_SG,  CH_SG,  CH_SOLID
Level2Chars     !byte CH_GAP, CH_GAP, CH_GS,  CH_GS,    CH_GAP, CH_GAP, CH_GS,  CH_GS
                !byte CH_SG,  CH_SG,  CH_SOLID, CH_SOLID, CH_SG, CH_SG, CH_SOLID, CH_SOLID
Level1Chars     !byte CH_GAP, CH_GS,  CH_GS,  CH_GS,    CH_SG,  CH_SOLID, CH_SOLID, CH_SOLID
                !byte CH_SG,  CH_SOLID, CH_SOLID, CH_SOLID, CH_SG, CH_SOLID, CH_SOLID, CH_SOLID

; Status label shows the current speed relative to the start speed:
; "Speed: 0/128" .. "Speed: 128/128".
SpeedPrefix     !pet "Speed: "
; Text buffers - all in one page (see PutText)
!align 255, 0, 0                       ; start at a page boundary
StepsText       !pet "Steps: 00000",0
BestText        !pet "Best: 00000",0
STEPS_DIGITS    = 7 ; offset of the digits in the texts
BEST_DIGITS     = 6
StatusText      !fill STATUS_LEN,$20
                !byte 0
TextsEnd
!if >StepsText != >(TextsEnd - 1) {
!error "The text buffers must be in one page"
}

; Frame handler: called by GUI64 once per frame from its raster IRQ
; (line 0, I/O visible, GUI64 saves the registers). It must not call
; GUI64 routines except the IRQ-safe ones.
FrameHandler    jsr GameFrame
                lda $d015                       ; the runner on or off
                and #$ff - SPRITE_BITS
                ldy SprOn
                beq +
                ora #SPRITE_BIT
+               ldy CloudOn
                beq +
                ora #CLOUD_BIT
+               sta $d015
                rts
; Stops the frame handler and switches the sprite off. Called on
; EC_SHUTDOWN and before an explicit Quit.
StopEngine      jsr GUI_StopFrameHandling
                lda #0
                sta SprOn
                sta CloudOn
                lda $d015
                and #$ff - SPRITE_BITS
                sta $d015
                rts

;----------------------------------------------------------------------
; One frame
GameFrame       lda CurrentWindow
                cmp GameWindow
                bne .pause
                ; additional consistency check
                lda WindowProc
                cmp #<RunBoyRunWndProc
                bne .pause
                lda WindowProc+1
                cmp #>RunBoyRunWndProc
                bne .pause
                lda WindowPosX
                sta WinX
                lda WindowPosY
                sta WinY
                lda WindowBits
                and #BIT_WND_ISMINIMIZED
                bne .pause
                lda ProgramMode
                bne .pause
                jsr Update
                jmp Render

.pause          lda #0                          ; hide the runner
                sta SprOn
                sta CloudOn
                sta JumpReq
                rts

;----------------------------------------------------------------------
; Game logic
Update          lda State
                cmp #ST_READY
                bne +
                ; GUI64 may repaint the panel after startup. Keep the initial
                ; building line dirty while READY so it is restored on top.
                lda Dirty
                ora #DIRTY_LINE
                sta Dirty
                jmp .waiting
+               cmp #ST_OVER
                bne .playing
.waiting        lda JumpReq                     ; SPACE starts a new game
                beq +
                jsr NewGame
+               lda #0
                sta JumpReq
                rts
.playing        jsr Scroll
                lda AnimSub                     ; run animation: advance once
                clc                             ; per RUN_STEP pixels scrolled
                adc Speed
.animNext       cmp #RUN_STEP * 16
                bcc .animDone
                sbc #RUN_STEP * 16
                ldx Anim
                inx
                cpx #RUN_FRAMES
                bcc +
                ldx #0
+               stx Anim
                jmp .animNext                   ; high speeds may advance twice
.animDone       sta AnimSub
                jsr ReadHold
                lda State
                cmp #ST_RUN
                bne .air
                ; running
                lda JumpReq
                beq +
                lda #0                          ; jump
                sta JumpReq
                sta HoldT
                lda #JUMP_VEL
                sta Vel
                lda #ST_JUMP
                sta State
                rts
+               jsr FootSurface                 ; still ground under the foot?
                bcc .off
                cmp Height
                beq .onGround
.off            lda #0                          ; ran over the edge
                sta Vel
                lda #ST_JUMP
                sta State
.onGround       rts
                ; in the air (ST_JUMP: can land, ST_FALL: hit a wall)
.air            lda #0                          ; no jumping in the air
                sta JumpReq
                lda Height
                sta OldH
                ldx #GRAVITY
                lda Vel                         ; while rising and the
                beq .accel                      ; button is held, the
                bmi .accel                      ; runner flies without
                lda Held                        ; gravity for a while
                beq .release
                lda HoldT
                cmp #HOLD_MAX
                bcs .accel
                inc HoldT
                ldx #0
                beq .accel                      ; jmp
.release        lda #HOLD_MAX                   ; released: the jump can't
                sta HoldT                       ; be extended any more
.accel          stx Tmp
                lda Vel
                sec
                sbc Tmp
                bpl +
                cmp #256-VEL_MAX
                bcs +
                lda #256-VEL_MAX
+               sta Vel
                jsr Move
                lda State
                cmp #ST_FALL
                beq .out
                jsr FootSurface                 ; roof under the foot?
                bcc .out
                sta Tmp                         ; its surface
                lda Height
                cmp Tmp
                beq .onTop
                bcs .above
                lda OldH                        ; below the surface: came
                cmp Tmp                         ; from above it?
                bcs .land
                lda Height                      ; almost high enough?
                adc #STEP_UP                    ; (C=0)
                cmp Tmp
                bcs .land
                lda #ST_FALL                    ; no: hit the wall
                sta State
                lda Vel
                bmi +
                lda #0
                sta Vel
+               rts
.onTop          lda Vel                         ; not while rising
                beq .land
                bpl .above
.land           lda Tmp
                jsr SetHeight
                lda #ST_RUN
                sta State
.above          rts
.out            lda Height                      ; fell down?
                cmp #FALL_OUT
                bcs +
                jmp GameOver
+               rts

; Returns C=1 and A = height of the surface, if the cell under the
; runner's foot is solid, C=0 if it is a gap.
FootSurface     lda Fine
                clc
                adc #FOOT_X
                lsr
                lsr
                lsr
                tax
                lda Cells,x
                clc
                beq +                           ; gap: C=0
                sec                             ; (level - 1) * 8 + HBASE
                sbc #1
                asl
                asl
                asl
                adc #HBASE
                sec
+               rts

; Moves the runner by Vel (signed, 1/16 pixels)
Move            ldx #0
                lda Vel
                bpl +
                dex
+               clc
                adc PosLo
                sta PosLo
                txa
                adc PosHi
                sta PosHi
                asl                             ; Height = Pos / 16
                asl
                asl
                asl
                sta Tmp
                lda PosLo
                lsr
                lsr
                lsr
                lsr
                ora Tmp
                sta Height
                rts

; A = height in pixels -> Height and Pos
SetHeight       sta Height
                ldx #0
                stx PosHi
                asl
                rol PosHi
                asl
                rol PosHi
                asl
                rol PosHi
                asl
                rol PosHi
                sta PosLo
                rts

; Held <> 0 while SPACE or the mouse button is held. SPACE is read from
; the keyboard matrix (column 7, row 4) - the same line as the fire
; button (left mouse button) in port 1. The fire line of port 2 is
; bit 4 of $dc00.
;ReadHold        ;lda $dc00
;                tax
;                lda #$7f
;                sta $dc00
;                lda $dc01                       ; SPACE or port 1
;                and $dc00                       ; port 2
;                stx $dc00
;                and #$10
;                eor #$10                        ; $10 if pressed
;                sta Held
;                rts
KEY_SPACE       = $3b

ReadHold        ;jsr GUI_GetKeyCode
                ;cmp #KEY_SPACE
                lda actkey
                cmp #$20
                beq held
                jsr GUI_GetLBtnFirePressed
                bne held
                lda #0
                sta Held
                rts

held            lda #1
                sta Held
                rts

NewGame         jsr ResetLine
                lda #ST_RUN
                sta State
                lda #213
                sta CloudPosX
                rts

; Solid line on level 1, start speed, 0 steps
ResetLine       lda Dirty
                ora #DIRTY_LINE
                sta Dirty
                ldx #NUM_CELLS-1
                lda #1
-               sta Cells,x
                dex
                bpl -
                sta SegSolid
                sta SegLevel
                lda #8                          ; 8 more solid cells
                sta SegLeft
                lda #HBASE
                jsr SetHeight
                lda #0
                sta Fine
                sta SubPix
                sta Vel
                sta Steps
                sta Steps+1
                sta JumpReq
                lda #SPEED_START
                sta Speed
                ldx #4                          ; "00000" steps
                lda #"0"
-               sta StepsText+STEPS_DIGITS,x
                dex
                bpl -
                lda Dirty
                ora #DIRTY_STEPS
                sta Dirty
                jmp UpdateSpeedStatus

GameOver        lda #ST_OVER
                sta State
                lda Steps                       ; new best?
                cmp Best
                lda Steps+1
                sbc Best+1
                bcc +
                lda Steps
                sta Best
                lda Steps+1
                sta Best+1
                ldx #4                          ; and its digits
-               lda StepsText+STEPS_DIGITS,x
                sta BestText+BEST_DIGITS,x
                dex
                bpl -
+               jmp UpdateSpeedStatus           ; also redraws BestText

; Scrolls the line by Speed/16 pixels
Scroll          lda SubPix
                clc
                adc Speed
                pha
                and #15
                sta SubPix
                pla
                lsr
                lsr
                lsr
                lsr
                clc
                adc Fine
-               cmp #8
                bcc +
                sbc #8
                pha
                jsr ShiftCells
                pla
                jmp -
+               sta Fine
                rts

; Shifts the cells one char to the left and adds a new one
ShiftCells      lda Dirty
                ora #DIRTY_LINE
                sta Dirty
                ldx #0
-               lda Cells+1,x
                sta Cells,x
                inx
                cpx #NUM_CELLS-1
                bne -
                jsr NextCell
                sta Cells+NUM_CELLS-1
                lda State                       ; count the steps
                cmp #ST_FALL                    ; (not while falling)
                beq .done
                inc Steps
                bne +
                inc Steps+1
+               lda Steps                       ; faster every 64 steps
                and #63
                bne +
                lda Speed
                cmp #SPEED_MAX
                bcs +
                adc #SPEED_STEP
                sta Speed
                jsr UpdateSpeedStatus
+               ldx #4                          ; count the digits of the
-               inc StepsText+STEPS_DIGITS,x    ; steps text, too
                lda StepsText+STEPS_DIGITS,x
                cmp #"9"+1
                bcc +
                lda #"0"
                sta StepsText+STEPS_DIGITS,x
                dex
                bpl -
+               lda Dirty
                ora #DIRTY_STEPS
                sta Dirty
.done           rts

; Returns the next cell of the line: 0 = gap, 1..3 = level of the building
NextCell        lda SegLeft
                bne .same
                lda SegSolid                    ; new segment
                eor #1
                sta SegSolid
                beq .gap
                lda Speed                       ; solid: 5 + speed / 16 + 0..7
                lsr
                lsr
                lsr
                lsr
                clc
                adc #5
                sta Tmp
                jsr Random
                and #7
                clc
                adc Tmp
                sta SegLeft
                lda #NUM_LEVELS                 ; random level 1..3
                sta ModVal
                jsr Random
                jsr Mod
                clc
                adc #1
                sta SegLevel
                bne .same                       ; jmp
.gap            lda Speed                       ; gap: 2 .. speed / 8
                lsr
                lsr
                lsr
                sec
                sbc #1
                sta ModVal
                jsr Random
                jsr Mod
                clc
                adc #2
                sta SegLeft
.same           dec SegLeft
                lda SegSolid
                beq +
                lda SegLevel
+               rts

; Updates StatusText to "Speed: N/128", where N = Speed-SPEED_START.
; N is 0..128 for the current constants. The rest of the label is blanked.
UpdateSpeedStatus
                lda Dirty
                ora #DIRTY_STATUS
                sta Dirty

                ; Blank the whole fixed-width label first.
                ldx #STATUS_LEN-1
                lda #" "
-               sta StatusText,x
                dex
                bpl -

                ; "Speed: "
                ldx #6
-               lda SpeedPrefix,x
                sta StatusText,x
                dex
                bpl -

                ; Current value = Speed - SPEED_START, decimal without
                ; leading zeroes. With SPEED_MAX=160 this is 0..128.
                lda Speed
                sec
                sbc #SPEED_START
                sta Tmp
                ldy #7                          ; after "Speed: "

                cmp #100
                bcc .under100
                sec
                sbc #100
                sta Tmp
                lda #"1"
                sta StatusText,y
                iny

.under100       lda Tmp
                ldx #0
.tens           cmp #10
                bcc .tensDone
                sbc #10
                inx
                bne .tens
.tensDone       sta Tmp                         ; ones digit
                cpx #0
                bne .writeTens
                cpy #8                          ; after a hundreds digit,
                bne .ones                       ; write the zero tens digit
.writeTens      txa
                clc
                adc #"0"
                sta StatusText,y
                iny
.ones           lda Tmp
                clc
                adc #"0"
                sta StatusText,y
                iny

                ; Maximum relative speed: SPEED_MAX-SPEED_START = 64.
                lda #"/"
                sta StatusText,y
                iny
                lda #"6"
                sta StatusText,y
                iny
                lda #"4"
                sta StatusText,y
                rts

;----------------------------------------------------------------------
; Drawing (directly into the screen, while the window is on top)
Render          jsr EdgeChars
                jsr Cloud
                ; Only redraw what changed (the whole window was
                ; redrawn by GUI64 anyway, if something else changed)
                lda WinX                        ; everything, if the window
                cmp DrawnX                      ; was moved
                bne .all
                lda WinY
                cmp DrawnY
                beq .parts
.all            lda WinX
                sta DrawnX
                lda WinY
                sta DrawnY
                lda #DIRTY_ALL
                sta Dirty
.parts          lsr Dirty                       ; DIRTY_LINE
                bcc .noLine
                ; the buildings: all rows (levels 3, 2, 1 and the row
                ; below level 1) at once
                lda #PF_Y + TOP_ROW
                jsr SetRow
                ldx #0                          ; patch the addresses of
-               lda PutChar+1                   ; the rows (one row down
                sta .lv3+1,x                    ; per level)
                clc
                adc #40
                sta PutChar+1
                lda PutChar+2
                sta .lv3+2,x
                adc #0
                sta PutChar+2
                txa
                clc
                adc #.lv2 - .lv3
                tax
                cpx #3 * (.lv2 - .lv3)
                bne -
                lda PutChar+1                   ; the row below level 1
                sta .lv0+1
                lda PutChar+2
                sta .lv0+2
                ldy #PF_X + PF_W - 1            ; Y = content column
.cell           lda Cells - PF_X,y              ; cell * 4 + next cell
                asl
                asl
                ora Cells - PF_X + 1,y
                tax
                ; The panel uses the same character cells as the playfield.
                ; Keep the full 30-column playfield, but use special app chars
                ; in the first/last column whose bitmap also contains the
                ; vertical panel-edge pixel.
                lda #0
                cpy #PF_X
                bne .notLeftEdge
                lda #EDGE_L_OFS
                bne .edgeOfsReady               ; EDGE_L_OFS is non-zero
.notLeftEdge    cpy #PF_X + PF_W - 1
                bne .edgeOfsReady               ; A is still 0
                lda #EDGE_R_OFS
.edgeOfsReady   sta Tmp
                lda Level3Chars,x
                clc
                adc Tmp
.lv3            sta $ffff,y                     ; addresses are patched
                lda Level2Chars,x
                clc
                adc Tmp
.lv2            sta $ffff,y
                lda Level1Chars,x
                clc
                adc Tmp
                sta $ffff,y
.lv0            sta $ffff,y                     ; (same chars as level 1)
                dey
                cpy #PF_X - 1
                bne .cell
.noLine         lsr Dirty                       ; DIRTY_STEPS
                bcc .noSteps
                lda #SCORE_Y
                jsr SetRow
                ldx #STEPS_X
                lda #<StepsText
                ldy #12
                jsr PutText
.noSteps        lsr Dirty                       ; DIRTY_STATUS
                bcc .noStatus
                lda #SCORE_Y
                jsr SetRow
                ldx #BEST_X
                lda #<BestText
                ldy #11
                jsr PutText
                lda #STATUS_Y
                jsr SetRow
                ldx #STATUS_X
                lda #<StatusText
                ldy #STATUS_LEN
                jsr PutText
.noStatus       ; the runner
                lda WinX                        ; x = 24 + column * 8
                clc
                adc #RUNNER_COL
                ldx #0
                stx Tmp
                asl
                rol Tmp
                asl
                rol Tmp
                asl
                rol Tmp
                clc
                adc #24
                sta $d000+2*SPRITE_NO
                lda Tmp
                adc #0
                beq +
                lda $d010
                ora #SPRITE_BIT
                bne ++
+               lda $d010
                and #$ff - SPRITE_BIT
++              sta $d010
                lda Height                      ; y = 50 + row * 8 - 8 - height
                sec                             ; (height above level 1,
                sbc #HBASE                      ; signed)
                sta Tmp
                lda WinY
                clc
                adc RowOffset
                adc #1 + PF_Y + GROUND_ROW
                asl
                asl
                asl
                clc
                adc #50 - 8                     ; (the feet are in sprite
                sec                             ; row 7)
                sbc Tmp
                sta $d001+2*SPRITE_NO
                lda #RUNNER_COLOR
                sta $d027+SPRITE_NO
                lda State                       ; sprite frame
                cmp #ST_RUN
                beq .runFrame
                cmp #ST_READY
                bne .jumpFrame
                lda #FRAME_START
                beq .frame                      ; jmp
.runFrame       lda Anim
                clc
                adc #FRAME_RUN
                bpl .frame                      ; jmp
.jumpFrame      lda Vel                         ; in the air: by velocity
                eor #$80                        ; (signed -> unsigned)
                ldx #0
-               cmp JumpVels,x
                bcs +
                inx
                cpx #JUMP_FRAMES - 1
                bcc -
+               txa
                adc #FRAME_JUMP - 1             ; (C = 1 here)
.frame          clc
                adc #SPRITE_PTR0
                ldx ScreenHi                    ; pointer = screen + $3f8
                inx
                inx
                inx
                stx .ptr+2
.ptr            sta $03f8+SPRITE_NO             ; high byte is patched
                ldx #3                          ; in front, single color,
-               ldy SprRegs,x                   ; not expanded
                lda $d000,y
                and #$ff - SPRITE_BIT
                sta $d000,y
                dex
                bpl -
                ldx #0                          ; visible unless the game is over
                lda State
                cmp #ST_OVER
                beq +
                inx
+               stx SprOn
                lda #1
                sta CloudOn
                rts

MAX_CLOUD_CNT = 10
CloudCounter    !byte MAX_CLOUD_CNT
CloudPosX       !byte 213
Cloud           lda State
                beq ++
                cmp #ST_OVER
                beq ++
                dec CloudCounter
                bne ++
                dec CloudPosX
                bne +
                lda #213
                sta CloudPosX
+               lda #MAX_CLOUD_CNT
                sta CloudCounter
++              ; x coord
                lda WinX
                asl
                asl
                asl
                clc
                adc #24+8
                clc
                adc CloudPosX
                sta $d00e
                bcc +
                lda #%10000000
                ora $d010
                bne ++
+               lda #%01111111
                and $d010
++              sta $d010
                ; y coord
                lda WinY
                asl
                asl
                asl
                clc
                adc #90
                sta $d00f
                rts

; Redefines the building chars for the fine scroll position. Building
; pixels are cleared, sky and lit windows are set:
; building = windows (scrolled), sky -> building = building | left
; part, building -> sky = building | right part.  The L/R variants put
; the GUI64 panel side BEHIND the playfield: the edge is visible in sky
; pixels, while building pixels (including windows) cover it.
EdgeChars       ldx Fine
                lda ShlTab,x                    ; left part (8 - Fine pixels)
                sta Tmp
                eor #$ff                        ; right part
                sta ModVal                      ; (free here)
                txa
                and #3                          ; (windows repeat every 4 pixels)
                tax
                lda WinTab,x
                sta .win+1
                lda #$34                        ; charset is under the I/O area
                sta $01                         ; (not GUI_MapOutIO: not IRQ-safe)
                ldx #7
-               lda #0                          ; rows 3 and 4: windows
                cpx #3
                bcc +
                cpx #5
                bcs +
.win            lda #0                          ; windows (patched)
+               sta APP_CHARSET+(CH_SOLID-APP_CHAR_0)*8,x
                tay                             ; Y = unframed building byte

                ; The panel is BEHIND the playfield:
                ; - in sky pixels the black panel edge must be visible
                ; - in building pixels the building (including lit windows)
                ;   must completely cover the panel edge.
                ; PANEL_L_MASK/PANEL_R_MASK are 0 only at the black frame pixel.

                ; GAP: everything is sky, so the black panel edge is visible.
                lda #PANEL_L_MASK
                sta APP_CHARSET+(CH_GAP_L-APP_CHAR_0)*8,x
                lda #PANEL_R_MASK
                sta APP_CHARSET+(CH_GAP_R-APP_CHAR_0)*8,x

                ; SOLID: everything is building.  Do NOT apply the panel mask;
                ; the house is in front of the frame, including its windows.
                tya
                sta APP_CHARSET+(CH_SOLID_L-APP_CHAR_0)*8,x
                sta APP_CHARSET+(CH_SOLID_R-APP_CHAR_0)*8,x

                ; sky -> building. Tmp is 1 in the sky part and 0 in the
                ; building part.  Apply the panel mask only where Tmp says sky:
                ; edge mask = (~Tmp) | PanelMask.
                tya
                ora Tmp
                sta APP_CHARSET+(CH_GS-APP_CHAR_0)*8,x
                pha                             ; save normal GS bitmap byte
                lda Tmp
                eor #$ff
                ora #PANEL_L_MASK
                sta EdgeMask
                pla
                pha
                and EdgeMask
                sta APP_CHARSET+(CH_GS_L-APP_CHAR_0)*8,x
                lda Tmp
                eor #$ff
                ora #PANEL_R_MASK
                sta EdgeMask
                pla
                and EdgeMask
                sta APP_CHARSET+(CH_GS_R-APP_CHAR_0)*8,x

                ; building -> sky. ModVal is the corresponding sky mask.
                tya
                ora ModVal
                sta APP_CHARSET+(CH_SG-APP_CHAR_0)*8,x
                pha                             ; save normal SG bitmap byte
                lda ModVal
                eor #$ff
                ora #PANEL_L_MASK
                sta EdgeMask
                pla
                pha
                and EdgeMask
                sta APP_CHARSET+(CH_SG_L-APP_CHAR_0)*8,x
                lda ModVal
                eor #$ff
                ora #PANEL_R_MASK
                sta EdgeMask
                pla
                and EdgeMask
                sta APP_CHARSET+(CH_SG_R-APP_CHAR_0)*8,x
                dex
                bpl -
                lda #$35
                sta $01
                rts
SprRegs         !byte $1b, $1c, $17, $1d
ShlTab          !byte $ff,$fe,$fc,$f8,$f0,$e0,$c0,$80
WinTab          !byte WINDOWS, ((WINDOWS << 1) & $ff) | (WINDOWS >> 7) ; rotated left by 0..3 pixels
                !byte ((WINDOWS << 2) & $ff) | (WINDOWS >> 6), ((WINDOWS << 3) & $ff) | (WINDOWS >> 5)

; A = content row of the window -> PutChar (in PutText) writes to
; this row
SetRow          clc
                adc WinY
                adc RowOffset
                adc #1                          ; title bar
                sta Tmp                         ; screen row * 40:
                asl                             ; row * 5 (< 128) ...
                asl
                adc Tmp
                ldx #0
                stx PutChar+2
                asl                             ; ... * 8
                rol PutChar+2
                asl
                rol PutChar+2
                asl
                rol PutChar+2                   ; (C=0)
                adc WinX
                sta PutChar+1
                lda PutChar+2
                adc ScreenHi
                sta PutChar+2
                rts
; Writes the PETSCII text at A (low byte, page of StepsText) with
; length Y to content column X of the current row
PutText         sta .txt+1
                lda #>StepsText
                sta .txt+2
                sty Tmp
                ldy #0
.txt            lda $ffff,y
                cmp #$c0                        ; PETSCII to GUI64 screen code
                bcc +
                sbc #$40                        ; $c0-$df -> $80-$9f
                bcs ++                          ; jmp
+               ora #$80                        ; $20-$5f -> $a0-$df
++
PutChar         sta $ffff,x                     ; address is patched (SetRow)
                inx
                iny
                cpy Tmp
                bne .txt
                rts

;----------------------------------------------------------------------
; 16 bit Galois LFSR, returns random byte in A. X and Y are preserved.
Random          lsr Seed+1
                ror Seed
                bcc +
                lda Seed+1
                eor #$b4
                sta Seed+1
+               lda Seed
                rts

; A <- A mod [ModVal]
Mod             sec
-               sbc ModVal
                bcs -
                adc ModVal
                rts

;----------------------------------------------------------------------


;----------------------------------------------------------------------
; Engine variables
RowOffset       !byte 0 ; content row 0 = window table y + RowOffset + 1
Seed            !word 1
WinX            !byte 0
WinY            !byte 0
ScreenHi        !byte 0 ; high byte of the screen (GUI_GetScreenMem)
SprOn           !byte 0
CloudOn         !byte 0
JumpReq         !byte 0 ; set by the window proc
Held            !byte 0 ; SPACE or mouse button held
State           !byte 0
Cells           !fill NUM_CELLS,0 ; 0 = gap, 1..3 = level
SegSolid        !byte 0
SegLevel        !byte 0
SegLeft         !byte 0
Dirty           !byte DIRTY_ALL
DrawnX          !byte $ff ; window position of the last drawing
DrawnY          !byte $ff
Fine            !byte 0
SubPix          !byte 0
Speed           !byte 0
Steps           !word 0
Best            !word 0
Height          !byte 0 ; of the runner's foot in pixels (biased by HBASE)
OldH            !byte 0
PosLo           !byte 0 ; Height in 1/16 pixels
PosHi           !byte 0
Vel             !byte 0 ; signed, 1/16 pixels per frame, up is positive
HoldT           !byte 0 ; frames the jump was extended
Anim            !byte 0 ; run frame 0..RUN_FRAMES-1
AnimSub         !byte 0 ; 1/16 pixels scrolled since the last run frame
Tmp             !byte 0
ModVal          !byte 0
EdgeMask        !byte $ff   ; scratch mask used while composing edge chars
EngineEnd
!if EngineEnd > SPRITE_BASE {
!error "Application code overlaps sprite destination at $c400"
}
