; Copy-only 1541 overlay. Normal drive images and the C64 core stay unchanged.
; Records are bank offset, byte count, payload. Build-Krill applies them to bank 7.
; The serial/GCR timing loops are not patched. No C64 RAM contains this code.
!source "build/krill/copy-drive-symbols.asm"
!cpu 6510
* = 0
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyHash)
 !word PatchEnd0-PatchData0
PatchData0
 !pseudopc mdCopyHash {
                ldy $ef
                jmp mdCopyLoad
EOFEntry
                lda #1
                sta EndFlag+1
                jmp SourcePark
 !if * > mdCopyHashEnd { !error "Copy hash overlay too large" }
 }
PatchEnd0
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyFind)
 !word PatchEnd1-PatchData1
PatchData1
 !pseudopc mdCopyFind {
SourceResume
EndFlag         lda #0
                beq SourcePort
                jmp mdCopyUninstall
SourcePort      lda #$13
                sta $1800
SourceX         ldx #0
SourceY         ldy #0
                jmp mdCopyWait+3
SourcePark      sei
                stx SourceX+1
                lda $1800
                sta SourcePort+1
                sty SourceY+1
                lda #2
                sta ParkData+1
                lda #0
                sta EdgeCount+1
                lda $1800
                and #$84
                sta Last+1
ParkLoop        lda $1800
                tax
                and #$80
                beq +
                lda #$10
ParkData
+               ora #2
                sta $1800
                txa
                and #$84
Last            cmp #0
                beq ParkStable
                sta Sample+1
                eor Last+1
                cmp #$80
                bne ParkReset
                inc EdgeCount+1
EdgeCount       lda #0
                cmp #4
                bcc ParkNext
Sample          lda #0
                and #4
                beq ParkRelease
                jmp SourceResume
ParkRelease     lda #0
                sta ParkData+1
                sta EdgeCount+1
                lda Sample+1
                jmp ParkNextNoPop
ParkReset       lda #0
                sta EdgeCount+1
ParkNext        lda Sample+1
ParkNextNoPop   sta Last+1
                jmp ParkLoop
ParkStable      beq ParkLoop
!fill mdCopyFindEnd-*,0
LoadStart
                ldx $f0
                dex
                stx $76
                jmp mdCopyHash
 !if * != mdCopyLoad { !error "Copy open overlay size differs" }
 }
PatchEnd1
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyWait)
 !word PatchEnd2-PatchData2
PatchData2
 !pseudopc mdCopyWait {
 jmp BatchWait
NativeWait
 lda #4
 bit $1800
 bmi NativeWait
 bne mdCopyClose
 stx $1c05
 jmp mdCopyWaitEnd
 !if * > mdCopyWaitEnd { !error "Copy wait overlay too large" }
 }
PatchEnd2
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyOpen)
 !word PatchEnd3-PatchData3
PatchData3
 !pseudopc mdCopyOpen {
 jmp mdCopyFindEnd
 nop
 }
PatchEnd3
 !word mdCopyImage-$8000-mdCopyBase+(mdCopySpec)
 !word PatchEnd4-PatchData4
PatchData4
 !pseudopc mdCopySpec {
 rts
Ordered
 ldx $5c
 lda $ef,x
 and #$1f
 sta $54
 clc
 adc $5e
 cmp $70
 beq Accept
 jmp mdCopyBlockLoop
Accept
 sec
 ldx $5c
 jmp mdCopyServe+3
InitCount
 sta $29
 sta $70
 lda #1
 sta $74
 sta CopyCancelEnabled+1
 lda #0
 ldy $ef
 jmp mdCopyLoad+4
NextTrackCount
 lda $70
 ldy $ee
 jmp mdCopyLoad+4
; Empty raw files have no block bytes. Send the regular EOF status instead
; of the stock last-block size $FF, which aliases FILE_NOT_FOUND in the core.
CopyControl
 ldy <mdCopyLinkTrack
 bne CopyNormalControl
 lda <mdCopyLinkSector
 cmp #1
 bne CopyNormalControl
 lda #0
 sta $ee
 tay
 jmp mdCopyEOF
CopyNormalControl
 lda #1
 ldx #1
 ldy #0
 jmp mdCopyControl+4
CopyReadCheck
CopyCancelEnabled
 lda #0
 beq CopyReadContinue
 bit $1800
 bmi CopyReadContinue
 jmp mdCopyUninstall ; host released ATN to abort a stalled disk read
CopyReadContinue
 lax $30
 sta $5c
 jmp mdCopyScan+4
BatchWait
                dec BatchLeft
                beq BatchPause
                jmp NativeWait
BatchPause      lda #20
                sta BatchLeft
                jmp SourcePark
BatchLeft       !byte 1 ; first packet parks, then one pause per 20 packets
 !if * > mdCopyHash { !error "Copy ordered overlay too large" }
 }
PatchEnd4
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyServe)
 !word PatchEnd5-PatchData5
PatchData5
 !pseudopc mdCopyServe {
 jmp Ordered
 }
PatchEnd5
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyLoad)
 !word PatchEnd6-PatchData6
PatchData6
 !pseudopc mdCopyLoad {
 jmp InitCount
 nop
 }
PatchEnd6
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyInc)
 !word PatchEnd7-PatchData7
PatchData7
 !pseudopc mdCopyInc {
 inc $70
 }
PatchEnd7
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyControl)
 !word PatchEnd8-PatchData8
PatchData8
 !pseudopc mdCopyControl {
 jmp CopyControl
 nop
 }
PatchEnd8
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyFirst)
 !word PatchEnd9-PatchData9
PatchData9
 !pseudopc mdCopyFirst {
 nop
 nop
 nop
 nop
 }
PatchEnd9
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyNext)
 !word PatchEnd10-PatchData10
PatchData10
 !pseudopc mdCopyNext {
 jmp NextTrackCount
 nop
 nop
 }
PatchEnd10
 !word mdCopyImage-$8000-mdCopyBase+(mdCopyCache)
 !word PatchEnd11-PatchData11
PatchData11
 !pseudopc mdCopyCache {
 jmp EOFEntry
 }
PatchEnd11
 !word mdCopyImage-$8000-mdCopyBase+mdCopyScan
 !word CopyScanEnd-CopyScanData
CopyScanData
 !pseudopc mdCopyScan {
 jmp CopyReadCheck
 nop
 }
CopyScanEnd
 !word $ffff,0
