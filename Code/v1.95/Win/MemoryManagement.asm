; Structure of FREMEM:
; FREMEM_HEAD ($xxff): number of entries (1 byte)
; FREMEM_HEAD+1: 16 memory addresses
; FREMEM: data

EofFrememHeap   !byte 0,0

; Allocates a new memory block
; Param: how many bytes
AllocMemory     ldy FREMEM_HEAD
                cpy #16
                bcc +
                rts
+               tya
                asl
                tay
                lda #<(FREMEM_HEAD+1)
                sta $fb
                lda #>(FREMEM_HEAD+1)
                sta $fc
                lda EofFrememHeap
                sta ($fb),y
                iny
                lda EofFrememHeap+1
                sta ($fb),y
                ;
                inc FREMEM_HEAD
                lda EofFrememHeap
                clc
                adc Param
                sta EofFrememHeap
                lda EofFrememHeap+1
                adc #0
                sta EofFrememHeap+1
                rts

; 
FreeMemory      ;
                rts