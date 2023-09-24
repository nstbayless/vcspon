   
    MAC DBLDA
        BYTE $A9
    ENDM

    MAC CMPSKIP1
        BYTE $C9
    ENDM

    MAC BITSKIP2
        BYTE $2C
    ENDM
    
    MAC IFEQ_LDA
        bne .skip
        lda #{1}
.skip
    ENDM
    
    MAC asln
        REPEAT {1}
            asl
        REPEND
    ENDM

    MAC LOADPTR
        lda #<{2}
        sta {1}
        lda #>{2}
        sta {1}+1
    ENDM
    
    MAC INCCARRY
    SUBROUTINE
        bcc .nextinc
        inc {1}
.nextinc
    ENDM
    
    MAC INCEQ
    SUBROUTINE
        bne .nextinc
        inc {1}
.nextinc
    ENDM
    
    MAC bge 
        bcs {1}
    ENDM
    
    MAC blt
        bcc {1}
    ENDM