; Copy-only 1541 receiver and raw-write helper. RawBuffer is 2 or 4.
.FastTargetCode
!pseudopc $0300 {

.CopyWriterEntry           sei ; M-E runs from the DOS main loop with IRQs enabled
                jsr .CopyRawReserve
                tsx
                stx .CopyWriterSP+1
                lda $180e
                ora #$80 ; IER readback bit 7 is not set on all emulations
                sta .CopyWriterIER+1
                lda #$7f
                sta $180e
                lda #8
                sta $1800
.CopyWriterNext
                lda #2
                sta $83
                sta $84
                sta $180d
                cli
                jsr $d107
                jsr $df93
                asl
                tax
                sei
                ; Ready is CLK high / DATA low, distinct from idle DOS.
                lda #2
                sta $1800
-               bit $1800
                bpl -
                lda #$10
                sta $1800
                ; A parked source needs time to mirror ATN. Wait until
                ; every drive has released DATA before taking the baseline.
-               lda $1800
                and #1
                bne -
.CopyWriterReceive         jsr .CopyWriterGetByte
                sta ($99,x)
                inc $99,x
                lda $0205
                bne +
                dec $0206
+               dec $0205
                lda $0205
                ora $0206
                beq .CopyWriterBlockReceived
                lda $99,x
                bne .CopyWriterReceive
.CopyWriterBlockReceived
                lda #$18
                sta $1800
-               bit $1800
                bmi -
                lda #8
                sta $1800
                lda #2
                sta $180d
                cli
                lda $99,x
                bne .CopyWriterAfterBlock
                jsr .CopyRawWrt0
                ldx $43 ; controller sector count for the track just written
                dex
                dex
                stx $69 ; adapt allocation spacing to the track density
.CopyWriterAfterBlock
                lda $0205
                ora $0206
                bne .CopyWriterNext
                ; Finish physical jobs before the C64 overwrites this
                ; receiver with the target silence routine for source reads.
                cli
                ldx #4
.CopyWriterLastJob     lda $00,x
                bmi .CopyWriterLastJob
                dex
                bpl .CopyWriterLastJob
                sei
                lda #2
                sta $1800
-               bit $1800
                bpl -
                lda #$10
                sta $1800
-               bit $1800
                bmi -
                lda #<.CopyRawKeep
                sta .CopyWriterRestore+1 ; normal return keeps the helper allocated
                lda #$fe
.CopyWriterAbort
.CopyWriterSP          ldx #0
                txs
                jmp .CopyWriterRestore
.CopyWriterRestore
                jsr .CopyRawRelease
                lda #0
                sta $1800
                lda #2
                sta $180d
.CopyWriterIER         lda #$82
                sta $180e
                lda #$60
                sta .CopyWriterRestore ; idempotent after DOS reuses the helper buffer
                cli
                rts
; Two inverted input bits per sample: DATA at bit 0, CLK at bit 2.
; C64 starts each byte by pulling CLK low; ATN stays asserted throughout.
.CopyWriterGetByte
.CopyWriterIdle lda $1800
                and #4
                bne .CopyWriterIdle
                lda #$12
                sta $1800 ; DATA low: ready for the next byte
.CopyWriterStart lda $1800
                bpl .CopyWriterAbort
                and #4
                beq .CopyWriterStart
                lda #$10
                sta $1800 ; release DATA before sampling the four pairs
                pha
                pla
                bit $00 ; ten drive cycles, without changing the sampled timing
                ldy #4
.CopyWriterPair lda $1800
                lsr
                ror $85
                lsr
                lsr
                ror $85
                dey
                bne .CopyWriterPair ; samples are 25 drive cycles apart
                lda $85
                rts

}
.FastTargetEnd
!if .FastTargetEnd-.FastTargetCode > 256 { !error "Copy writer exceeds drive buffer zero" }
                !fill 256-(.FastTargetEnd-.FastTargetCode),0
.RawHelperCode
; Selected helper buffer: reserved along with buffer 0 while receiving.
; Only normal DOS channels are active here; RawCheck confirmed both free.
!pseudopc $0300+.RawBuffer*$100 {

                jmp .CopyRawJob
.CopyRawReserve
                lda $69
                sta .CopyRawSkewRestore+1
                lda #18
                sta $69
                lda $024f
                ora #1+(1<<.RawBuffer)
                sta $024f
                rts
; Clear just our reservations: a non-local DOS error may already have
; released file buffers, so restoring an old allocation bitmap is unsafe.
.CopyRawRelease
                lda #$ff-(1+(1<<.RawBuffer)) ; abort/final cleanup releases both
.CopyRawKeep
                pha
.CopyRawSkewRestore  lda #18
                sta $69
                pla
                and $024f
                sta $024f
.CopyRawReleased
                rts
!if <.CopyRawRelease > $fd { !error "Copy helper entry crosses page" }
; WRT0 with the physical $90 write replaced by an $E0 execute job.
; The selected helper supplies code; the data buffer keeps DOS accounting.
.CopyRawWrt0
                jsr $d1d3
                jsr $f11e
                lda #0
                jsr $d4c8
                lda $80
                jsr $cff1
                lda $81
                jsr $cff1
                sei
                jsr $d0c7
                stx .CopyRawData+1
                txa
                clc
                adc #3
                sta .CopyRawPage+1
                txa
                asl
                tax
                lda $06,x
                sta $06+.RawBuffer*2
                lda $07,x
                sta $07+.RawBuffer*2
                ldx .CopyRawData+1
                lda #1
                sta $00,x
                lda #$e0
                sta .RawBuffer
                cli
-               lda .RawBuffer
                bmi -
                ldx .CopyRawData+1
                jsr $d599
                jsr $cf1e
                jsr $d6d0
                lda #2
                jmp $d4c8
.CopyRawJob
                lda #1
                sta .RawBuffer
.CopyRawData         lda #2
                sta $3f
                tax
                lda #$90
                sta $00,x
                txa
                asl
                clc
                adc #6
                sta $32
                lda #0
                sta $33
                sta $30
.CopyRawPage         lda #5
                sta $31
                jmp .CopyRawWrite
.CopyRawWrite
                ; Physical 1541 write sequence ($F575-$F5DB), relocated.
                ; Preserve write-protect/sync checks and GCR conversion. Only
                ; the DOS tail that schedules a verify job is replaced below.
                ; Reference: https://github.com/mist64/dos1541, lcc.wrt.s.
                jsr $f5e9             ; CHKBLK
                sta $3a               ; CHKSUM
                lda $1c00
                and #$10
                bne .CopyRawWritable
                lda #8
                jmp $f969             ; controller error: write protected
.CopyRawWritable
                jsr $f78f             ; BINGCR
                jsr $f510             ; SRCH, bounded sector/header search
                ldx #9                ; header gap
.CopyRawGap      bvc .CopyRawGap
                clv
                dex
                bne .CopyRawGap
                lda #$ff
                sta $1c03             ; data port output
                lda $1c0c
                and #$1f
                ora #$c0
                sta $1c0c             ; write mode
                lda #$ff
                ldx #5
                sta $1c01
                clv
.CopyRawSync     bvc .CopyRawSync
                clv
                dex
                bne .CopyRawSync
                ldy #187
.CopyRawOverflow lda $0100,y
-               bvc -
                clv
                sta $1c01
                iny
                bne .CopyRawOverflow
.CopyRawBuffer   lda ($30),y
-               bvc -
                clv
                sta $1c01
                iny
                bne .CopyRawBuffer
-               bvc -
                lda $1c0c
                ora #$e0
                sta $1c0c             ; read mode
                lda #0
                sta $1c03
                jsr $f5f2             ; WTOBIN: restore DOS file buffer
.CopyRawPhysicalEnd
                lda #1                ; done; no $A0 read-back verify job
                jmp $f969
.CopyRawEnd = *
                !if * > $0400+.RawBuffer*$100 { !error "Raw helper exceeds selected drive buffer" }
                !fill $0400+.RawBuffer*$100-*,0
}
.RawHelperEnd
