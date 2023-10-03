    MAC CALC_EXPLOSION
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
    ENDM

kernel_cursor_pre:
    lda #$0
    dec TCURY0
    sta WSYNC
    bne ._skipdraw
    
    lda #$FF - $7*THICCURSOR
    sta GRP0
    
    CALC_EXPLOSION
    
    sleep 2 ; could use this!
    
    lda #$81 + ($7*THICCURSOR)
    
    sta GRP0
    rts
    
._skipdraw
    sta GRP0
    
    CALC_EXPLOSION
    
    rts
    
kernel_cursor_post:
    lda #$0
    sta VAR2
    sta GRP1
    cmp TCURY0
    IFEQ_LDA #$FF - $7*THICCURSOR
    sta WSYNC
    sta GRP0
    
    ; next row
    lda VAR1
    clc
    adc #$8
    sta VAR1
    
    rts
    
jmpworda_sleep15:
    sta WSYNC
    jsr sleep15
    
jmpworda:
    jmp (WORD_A)
    
sleep15
    SLEEP 3
    rts
    
jmpwordb_sleep15:
    sta WSYNC
    jsr sleep15
    
jmpwordb:
    jmp (WORD_B)