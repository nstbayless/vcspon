; routines for getting/setting blocks


; input: x is x position, a is y position
; output: a is index of block
; clobbers: ITERATOR
; preserved: x, y
    MAC GetBlockAddr_XA
        asln 3
        stx ITERATOR
        ora ITERATOR
    ENDM
    
; input: x is x position, a is y position
; output: a is value of block, y is address of block
    MAC GetBlockValue_XA_y0
        GetBlockAddr_XA
        tay
        lda BLOCKS_R,y
    ENDM

; input: x is x position, a is y position, y is value
; output: a is value, y is x position, x is y position
    MAC SetBlockValue_XA_Y
        pha
        GetBlockAddr_XA
        tax
        tya
        sta BLOCKS_W,X
        
        ; Y &= 7
        and #$7
        tay
        
        ; write LINE opcode
        asl ITERATOR ; multiply x position by 2, because the opcodes are length 2
        pla
        asl ; double y position, for A/B rows.
        tax
        
        ; row A
        lda LINES_CORE_W_ADDR_LOOKUP_HI,x
        sta WORD_A+1
        lda LINES_CORE_W_ADDR_LOOKUP_LO,x
        sta WORD_A
        lda LINE_OPCODE_LOOKUP_B,y
        pha
            
            if LINEOP_WRITE_INDIRECT_ACCESS
                ; [py] all((syms['LINES_CORE_W'] + syms['ROWINSTRC']*i) % 0x100 <= 0x100 - syms['ROWINSTRC'] for i in range(syms['ROWS'] * syms['ROWSUB']))
                lda LINE_OPCODE_LOOKUP_A,y
                ldy ITERATOR ; y <- x position
                sta (WORD_A),y
            else
                lda ITERATOR ; y <- x position
                tay 
                clc
                adc WORD_A
                sta WORD_A
                lda #$0
                adc WORD_A+1
                sta WORD_A+1
                lda LINE_OPCODE_LOOKUP_A,y
                ldy #$0
                sta (WORD_A),y
            endif
            
            ; row B
            inx
            ;lda LINES_CORE_W_ADDR_LOOKUP_HI,x   ; FIXME: necessary? Maybe this value is the same?
            ;sta WORD_A+1                        ; FIXME: ^
            ; [py] all((syms["LINES_CORE_W"] + syms["ROWINSTRC"]*i*2) // 0x100 == (syms["LINES_CORE_W"] + syms["ROWINSTRC"]*(i*2 + 1)) // 0x100 for i in range(syms["ROWS"]))
            lda LINES_CORE_W_ADDR_LOOKUP_LO,x
            sta WORD_A
        if LINEOP_WRITE_INDIRECT_ACCESS
                pla
            sta (WORD_A),y
        else
                lda ITERATOR
                clc
                adc WORD_A
                sta WORD_A
                lda #$0
                adc WORD_A+1
                sta WORD_A+1
                ldy #$0
            pla
            sta (WORD_A),y
        endif
    ENDM

; input: x is x position, a is y position
; output: a is value of block, y is address of block
; flags: z if block is 0
; clobbers: ITERATOR
JSR_GetBlockValue_XA
    ldy #$0
JSR_GetBlockValue_XA_y0
    GetBlockValue_XA_y0
    ora #$0
    rts

; clobbers: ITERATOR
JSR_SetBlockValue_XA_0
    ldy #$0
JSR_SetBlockValue_XA_Y
    SetBlockValue_XA_Y
_addQueueRts:
    rts
    
JSR_SetBlockValueDirect_XA_Y
    GetBlockAddr_XA
    sta BLOCKS_W,Y
    rts
    
JSR_ror4iterator:
    REPEAT 4
    lsr
    rol ITERATOR
    REPEND
    rts
    