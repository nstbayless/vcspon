; inline routine causing blocks to fall.

DoGravity
    dec GRAVROW
    bne _DontWrapGravRow
    lda #ROWS-1
    sta GRAVROW
_DontWrapGravRow

    lda #WIDTH
    sta VAR1
GravLoop
    dec VAR1
    
    ldx VAR1
    lda GRAVROW
    jsr JSR_GetBlockValue_XA
    bne SkipGravDrop
    
    ldx GRAVROW
    dex
    txa
    sta VAR2
        ldx VAR1
        jsr JSR_GetBlockValue_XA
        beq SkipGravDrop
        cmp #$8
        bge SkipGravDrop
        
        pha
            lda GRAVROW
            ldx VAR1
            jsr JSR_AddBlockToQueue
        pla
        
        tay
        lda GRAVROW
        ldx VAR1
        jsr JSR_SetBlockValue_XA_Y
    lda VAR2
    ldx VAR1
    jsr JSR_SetBlockValue_XA_0
    
SkipGravDrop:

GravLoopBottom:
    lda #$FF
    and VAR1
    bne GravLoop
GravEnd: