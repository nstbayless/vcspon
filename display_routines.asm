    
jmpworda_sleep15:
    sta WSYNC
    jsr sleep15
    
jmpworda:
    jmp (WORD_A)
    
JSR_CalcExplosion
    ldy VAR1            ; 3
    lda BLOCKS_R+1,y    ; 4 (not +)
    cmp #$8             ; 2
    ror VAR2            ; 5
    ror VAR2            ; 5
    lda BLOCKS_R,y      ; 4 (not +)
    cmp #$8             ; 2
    ror VAR2            ; 5
    lda VAR2            ; 3
    sta GRP1            ; 3
    
                        ; = 31
    rts
    

    
