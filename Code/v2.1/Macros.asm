; Distinguish between WIN and MAC version (WM)
!macro EOR_WM w, m
!ifdef WIN{
                eor #w
} else ifdef MAC{
                eor #m
}
!end

!macro LDA_WM w, m
!ifdef WIN{
                lda #w
} else ifdef MAC{
                lda #m
}
!end

!macro LDAV_WM w, m
!ifdef WIN{
                lda w
} else ifdef MAC{
                lda m
}
!end

!macro LDX_WM w, m
!ifdef WIN{
                ldx #w
} else ifdef MAC{
                ldx #m
}
!end

!macro LDY_WM w, m
!ifdef WIN{
                ldy #w
} else ifdef MAC{
                ldy #m
}
!end

!macro LDXV_WM w, m
!ifdef WIN{
                ldx w
} else ifdef MAC{
                ldx m
}
!end

!macro CMPV_WM w, m
!ifdef WIN{
                cmp w
} else ifdef MAC{
                cmp m
}
!end

!macro adc_WM w, m
!ifdef WIN{
                adc #w
} else ifdef MAC{
                adc #m
}
!end