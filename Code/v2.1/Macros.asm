!macro EOR_WM .w, .m {
!ifdef WIN {
                eor #.w
}
!ifdef MAC {
                eor #.m
}
}

!macro LDA_WM .w, .m {
!ifdef WIN {
                lda #.w
}
!ifdef MAC {
                lda #.m
}
}

!macro LDAV_WM .w, .m {
!ifdef WIN {
                lda .w
}
!ifdef MAC {
                lda .m
}
}

!macro LDX_WM .w, .m {
!ifdef WIN {
                ldx #.w
}
!ifdef MAC {
                ldx #.m
}
}

!macro LDY_WM .w, .m {
!ifdef WIN {
                ldy #.w
}
!ifdef MAC {
                ldy #.m
}
}

!macro LDXV_WM .w, .m {
!ifdef WIN {
                ldx .w
}
!ifdef MAC {
                ldx .m
}
}

!macro CMPV_WM .w, .m {
!ifdef WIN {
                cmp .w
}
!ifdef MAC {
                cmp .m
}
}

!macro adc_WM .w, .m {
!ifdef WIN {
                adc #.w
}
!ifdef MAC {
                adc #.m
}
}