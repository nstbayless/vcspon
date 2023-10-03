ShiftDownDuringNewRow:
    dex
    bmi RenderLoopTop
    stx WSYNC
    bpl ShiftDownDuringNewRow

RenderLoopTop:
    jsr kernel_cursor_pre
    
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
    jsr kernel_cursor_post
    ADD_WORD_IMM WORD_A, #64
    ADD_WORD_IMM WORD_B, #64
    
    dec VAR3
    bpl RenderLoopTop
    
RenderLoopDone:
    lda #$0 
    sta GRP0
    sta GRP1