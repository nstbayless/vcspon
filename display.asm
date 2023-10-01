RenderLoopTop:
    jsr kernel_cursor_pre
    
    lda #SL_PER_SUBROW
    
    sta ITERATOR
        
.rowloop
    IF FLICKER == 1
        lda TIMER
        lsr
    ELSE
        sec
    ENDIF
        bcc .kernrow1
.kernrow0
        sta WSYNC
        lda #>(.endEven-1)
        pha
        lda #<(.endEven-1)
        pha
        SLEEP 5
        jmp (WORD_A)
.endEven:
        sta WSYNC
        lda #>(.endkernrow-1)
        pha
        lda #<(.endkernrow-1)
        pha
        SLEEP 5
        jmp (WORD_B)
.kernrow1
        sta WSYNC
        lda #>(.endOdd-1)
        pha
        lda #<(.endOdd-1)
        pha
        SLEEP 5
        jmp (WORD_B)
.endOdd:
        sta WSYNC
        lda #>(.endkernrow-1)
        pha
        lda #<(.endkernrow-1)
        pha
        SLEEP 5
        jmp (WORD_A)
.endkernrow
        dec ITERATOR
        bne .rowloop
        
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