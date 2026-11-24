; One-time startup may overlap later loader state; permanent code follows above it.
autostart       ; No kernal messages ("SEARCHING FOR ..." etc)
                lda #0
                sta $9d
                ; Disable system interrupt
                jsr Disable_CIA_IRQ
                ; Init zero page
                lda #0
                ldx #$7f
-               sta $10,x
                dex
                bpl -
                lda #>WND_HEAP
                sta WindowOnHeap+1
                lda #$ff
                sta CurrentWindow
                sta CurMenuItem
                ; Initialize CIA timer
                ldx #0
                jsr Startup_ROMCall
Startup_AfterClock

                ; Bank out BASIC, I/O, and Kernal
                lda #$34 ; RAM / RAM / RAM
                sta $01
                ; Copy desktop chars and sprites to CHARBASE
                lda #<GraphicsData
                sta $fb
                lda #>GraphicsData
                sta $fc
                lda #<CHARBASE
                sta $fd
                lda #>CHARBASE
                sta $fe
                ;
                ldx #13
                jsr CopyBlockFBtoFD

                ; Bank in I/O
                lda #$35 ; RAM / IO / RAM
                sta  $01

!ifdef WIN{
                ; Copy taskbar chars to TASKCHARBASE
                lda #<TaskCharsData
                sta $fb
                lda #>TaskCharsData
                sta $fc
                lda #<TASKCHARBASE
                sta $fd
                lda #>TASKCHARBASE
                sta $fe
                ;
                ldx #4
                jsr CopyBlockFBtoFD
}

                ; Check for Ultimate + CMD Interface
                lda #0
                sta IconAvailable+2
                jsr U_IsUltimateCMD

                ;lda #1

                sta bIsUltimate
                beq +
                inc IconAvailable+2
                ; Get Ultimate's time and retrieve settings values
                jsr UltGetTime
                jsr UltReadSettings
                ldx #2
                stx U_CurTarget
                jsr U_GotoRoot

+               ldx #1
                jsr Startup_ROMCall
Startup_AfterGlobals
                jsr RepaintGUI
!ifdef WIN{
                jsr SetBkgPattern

                ; Set graphics environment
                ; Tell CIA that data comes in at bits 0,1
                lda $dd02
                ora #%00000011
                sta $dd02
                ; Choose VIC bank at VIC_BANK
                lda $dd00
                and #%11111100
!if (3-((>VICBANK)/64)) {
                ora #(3-((>VICBANK)/64))
}
                sta $dd00
                ; Choose char set at CHAR_BASE
                lda #(((>(SCRMEM-VICBANK))*4)+((>(CHARBASE-VICBANK))/4))
                sta $d018
                
                sta desktop_d018
                lda #(((>(SCRMEM-VICBANK))*4)+TASKCHARSHI)
                sta taskbar_d018
}
!ifdef MAC{
                ; Set graphics environment
                ; Tell CIA that data comes in at bits 0,1
                lda $dd02
                ora #%00000011
                sta $dd02
                ; Choose VIC bank at VIC_BANK
                lda $dd00
                and #%11111100
!if (3-((>VICBANK)/64)) {
                ora #(3-((>VICBANK)/64))
}
                sta $dd00
                ; Choose char set at CHAR_BASE
                lda #(((>(SCRMEM-VICBANK))*4)+((>(CHARBASE-VICBANK))/4))
                sta $d018
}
                
!ifdef WIN{     
                ; Initialize color ram for taskbar
                lda CSTM_WindowClr
                ora #8
                ldx #39
-               sta CLRMEM+880,x
                sta CLRMEM+920,x
                sta CLRMEM+960,x
                dex
                bpl -
}
                jsr Krill_Init
                ; Init clears the mode byte at $07FE. The following IRQ call
                ; and possible jump must not still need that startup byte.
                !if * <= KRILL_MDAPPMODE {
                    !if *+6 > KRILL_MDAPPMODE { !error "Startup return code overlaps Krill mode" }
                }
                jsr InstallIRQ
                jmp MainLoop

; Used only before Krill_Init/InstallIRQ. No permanent scratch RAM.
; X=0: WiC64/TOD clock init; X=1: SetGlobals. A/flags are not returned.
Startup_ROMCall lda $01
                pha
                lda #$37
                sta $01
                lda #KRILL_GAME_BANK
                sta $de00
                jsr Startup_ROMDispatch
                lda #$80
                sta $de00
                pla
                sta $01
                rts
Startup_ROMCallEnd = *
    !if * < $0801 {
                * = $0801
    }

MainLoop        ldx exit_code
                beq +
                lda #0
                sta exit_code
                dex
                lda EventsLo,x
                sta call_event+1
                lda EventsHi,x
                sta call_event+2
call_event      jsr $ffff
                
+               ldx exit_code2
                beq MainLoop
                lda #0
                sta exit_code2
                cpx #EC2_TIMER
                bne MainLoop
                jsr App_TimerHandler
                jmp MainLoop

EventsLo        !byte <OnRBtnPress, <OnRBtnRelease, <OnLBtnPress, <OnLBtnRelease, <OnMouseMove
                !byte <Exit, <OnDblClick, <OnScrollWheel, <OnScrollWheel, <OnKeyPress, <RunFile
                !byte <BootFile, <OnTooltip, <OnApp, <OnLBtnRepeat
EventsHi        !byte >OnRBtnPress, >OnRBtnRelease, >OnLBtnPress, >OnLBtnRelease, >OnMouseMove
                !byte >Exit, >OnDblClick, >OnScrollWheel, >OnScrollWheel, >OnKeyPress, >RunFile
                !byte >BootFile, >OnTooltip, >OnApp, >OnLBtnRepeat
                
;dumm            !byte 0; Uncomment if indirect jumps occur

Exit            pla
                pla
                ; Number of chars in keyboard buffer
                jsr DeinstallIRQ
                jsr Enable_CIA_IRQ
                jsr SetC64Defaults
                ; Reset to BASIC
                jmp ($a000)

OnApp           lda App_Windows
                beq +
                ldx #<Str_Msg_AppRunning
                ldy #>Str_Msg_AppRunning
                jmp ShowMessage
+               lda #1
                sta LOAD_TYPE
                jsr RunBootFile
                beq +
                jmp load_error
+               ; No error
                lda CurDeviceNo
                sta App_AppDeviceNo
                jsr BackToNormal
jmp_app         jsr $FFFF
                jmp RepaintGUI
