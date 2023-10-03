ShiftDownDuringNewRow:
    dex
    bmi RenderLoopTop
    stx WSYNC
    bpl ShiftDownDuringNewRow

RenderLoopTop:
    lda #$FF - $7*THICCURSOR
    dec TCURY0
    sta WSYNC
    bne ._skipdraw
    
    sta GRP0
    
    jsr JSR_CalcExplosion
    
    lda #$81 + ($7*THICCURSOR)
    
    sta GRP0
    
donecursor:
    lda #SL_PER_SUBROW
    
    sta ITERATOR
        
.rowloop
    IF FLICKER == 1
        bit FLICKER_FRAME
        bpl .kernrow1
    ENDIF
.kernrow0
        jsr jmpworda_sleep15
        jsr jmpwordb_sleep15
.endkernrow
        dec ITERATOR
        bne .rowloop
    
    IF FLICKER == 1
        beq post
        
.kernrow1
        jsr jmpwordb_sleep15
        jsr jmpworda_sleep15
        dec ITERATOR
        bne .rowloop
    ENDIF
        
post:
    ; cursor
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
    ADD_WORD_IMM WORD_A, #64
    ADD_WORD_IMM WORD_B, #64
    
    dec VAR3
    bpl RenderLoopTop
    bmi RenderLoopDone
    
._skipdraw
    jsr JSR_CalcExplosion
    lda #$0
    sta GRP0
    
    jmp donecursor
    
RenderLoopDone:
    lda #$0
    sta GRP0
    sta GRP1