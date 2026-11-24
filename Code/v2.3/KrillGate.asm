; Shared FREEMEM state and layout assertions; no emitted code.

!if KRILL_CORE_INSTALL < $8000 { !error "Krill installer must execute in ROM" }
!if KRILL_CORE_INSTALL+KRILL_INSTALL_SIZE > $a000 { !error "Krill installer exceeds ROM window" }
; Shared scratch space: only needed while main-thread installation owns FREEMEM.
!if KRILL_INSTALL_STATE_RAM < FREEMEM+$100 { !error "Krill state must use upper FREEMEM page" }
!if KRILL_INSTALL_STATE_RAM+KRILL_INSTALL_STATE_SIZE != FREEMEM+$200 { !error "Krill state must end at FREEMEM boundary" }
!if KRILL_INSTALL_STATE_SIZE > $2d { !error "Krill state exceeds reserved FREEMEM tail" }


; Six saved ZP bytes plus eleven status/register bytes in FREEMEM.
Krill_RuntimeData = KRILL_INSTALL_STATE_RAM-17
Krill_NativeActive = Krill_RuntimeData
Krill_LastStatus = Krill_RuntimeData+1
Krill_LastWarning = Krill_RuntimeData+2
Krill_DriveType = Krill_RuntimeData+3
Krill_Device = Krill_RuntimeData+4
Krill_SavedP = Krill_RuntimeData+5
Krill_ResultCarry = Krill_RuntimeData+6
Krill_Saved01 = Krill_RuntimeData+7
Krill_GameInstalling = Krill_RuntimeData+8 ; Enable progress during game installation.
Krill_SavedDD00 = Krill_RuntimeData+9
Krill_SavedDD02 = Krill_RuntimeData+10
Krill_SavedZP = Krill_RuntimeData+11
!if Krill_RuntimeData != FREEMEM+$1c2 { !error "Krill runtime must use $64C2-$64D2" }
!if Krill_SavedZP+6 != KRILL_INSTALL_STATE_RAM { !error "Krill saved state overlaps installer" }
!if Krill_LastStatus != $64c3 { !error "Krill installer progress vector differs from loaderconfig.inc" }
!if Krill_LastWarning != Krill_LastStatus+1 { !error "Krill progress vector must use consecutive status/warning bytes" }

; Low-RAM entry points. App/installer control uses bank 5, game control bank 6.
; Main-thread only; no nesting. The common gate preserves flags except result carry.
; Entries that can install a loader request GUI64's normal IRQ/FakeTaskbar stop.
Krill_Init      php
                ldx #<Krill_ROMInit
                ldy #>Krill_ROMInit
                clc
                bcc Krill_EnterROM
Krill_Install   sec
; C=0: caller already used GUI64's IRQ/FakeTaskbar stop (Viewer only).
Krill_InstallStopped
                php
                ldx #<Krill_ROMInstall
                ldy #>Krill_ROMInstall
                jmp Krill_EnterROM
Krill_LoadApp   php
                ldx #<Krill_ROMLoadApp
                ldy #>Krill_ROMLoadApp
                sec
                bcs Krill_EnterROM
; Raw internal transfers also need the ROM fallback bank mapped.
Krill_LoadRaw   stx KRILL_BLOCKDESTLO ; filename pointer $E4/$E5
                sty KRILL_BLOCKDESTLO+1
                php
                ldx #<Krill_ROMLoadRaw
                ldy #>Krill_ROMLoadRaw
                clc
                bcc Krill_EnterROM
Krill_Uninstall php
                ldx #<Krill_ROMUninstall
                ldy #>Krill_ROMUninstall
                clc
Krill_EnterROM  stx Krill_ROMCall+1
                sty Krill_ROMCall+2
                sta Krill_ROMArgument+1
                pla
                sta Krill_SavedP
                lda $01
                sta Krill_Saved01
                bcc Krill_MapROM
!ifdef WIN {
                jsr UninstallIRQ_FakeTB
} else {
                jsr UninstallIRQ
}
Krill_MapROM    sei
                lda #$37
                sta $01
                lda #KRILL_ROM_BANK
                sta $de00
Krill_ROMArgument
                lda #0
Krill_ROMCall   jsr $ffff
                sta Krill_ROMResult+1
                php
                pla
                and #1
                sta Krill_ResultCarry
                lda #$80
                sta $de00
                lda Krill_Saved01
                sta $01
                lda Krill_SavedP
                and #$fe
                ora Krill_ResultCarry
                pha
Krill_ROMResult lda #0
                plp
                rts

; Same low-RAM transfer core for Apps and games. Install before any PRG writes.
Krill_LoadGame  lda #0
                sta KRILL_MDGAMEWRITTEN
                lda #$ff
                sta MOTION_COUNTER
                ldx #1
                stx Krill_GameInstalling
                lda CurDeviceNo
                jsr Krill_Install
                ldx #0
                stx Krill_GameInstalling
                bcs Krill_GameInstallError
                lda #$37
                sta $01
                lda Krill_NativeActive
                beq KernalLoadFile
                lda #KRILL_GAME_BANK
                sta $de00
                jsr Krill_ROMLoadGame
Krill_GameAfterLoad
                ldx KRILL_MDGAMEWRITTEN
                lda error_code
                beq Krill_GameReturn
                txa
                beq Krill_GameReturn
                ; A partial PRG can destroy GUI64. Restart it from cartridge ROM.
                lda #0
                sta $de00
                jmp ($8000)
Krill_GameReturn
                lda #$80
                sta $de00
                jsr Enable_CIA_IRQ
                cli
                rts
Krill_GameInstallError
                lda #5
                sta error_code
                rts

; The fallback reader must survive overwriting GUI64 RAM, including FREEMEM.
; The game entry already selected $01=$37 and stopped GUI IRQ/sprites.
KernalLoadFile  lda #KRILL_ROM_BANK
                sta $de00
                jsr KernalLoadFileBody
                jmp Krill_GameAfterLoad
; KERNAL byte reader calls this from ROM; PRG data under I/O must reach RAM.
Krill_FileStore pha
                lda LOAD_TYPE
                bne Krill_FileStoreChecked
                lda $af
                cmp #8
                bcc Krill_FileStoreBad
                bne +
                lda $ae
                beq Krill_FileStoreBad
+               lda #1
                sta KRILL_MDGAMEWRITTEN
Krill_FileStoreChecked
                lda $af
                cmp #$d0
                bcc +
                cmp #$e0
                bcs +
                pla
                php
                sei
                ldx #$34
                stx $01
                sta ($ae),y
                ldx #$37
                stx $01
                plp
                clc
                rts
+               pla
                sta ($ae),y
                clc
                rts
Krill_FileStoreBad
                pla
                sec
                rts
!if * > $0800 { !error "Cartridge entry gates must stay below game RAM at $0801" }
