; CRT Viewer entry; replaces the old RAM reader in both designs.
; Installation uses the existing IRQ/FakeTaskbar gate. Viewer control is in bank 6.
ReadFileToViewerBuf
!ifdef WIN {
                jsr UninstallIRQ_FakeTB
} else {
                jsr UninstallIRQ
}
                lda #1
                jsr InitHourglass
                lda #2          ; installer checkpoints animate the Viewer hourglass
                sta LOAD_TYPE
                lda CurDeviceNo
                clc
                jsr Krill_InstallStopped
                bcc +
                lda #5
                sta error_code
                jmp InstallIRQ
+               lda #$37
                sta $01
                lda #KRILL_GAME_BANK
                sta $de00
                jsr Krill_ROMLoadViewer
Krill_ViewerAfterLoad
                lda #$80
                sta $de00
                lda #$35
                sta $01
                jmp InstallIRQ

; Called only from Viewer bank 6. Reuse ROM bank 5's serial-port restoration.
Krill_ViewerUninstall
                lda #KRILL_ROM_BANK
                sta $de00
                jsr Krill_ROMUninstall
                lda #KRILL_GAME_BANK
                sta $de00
                rts
