; routines for accessing and processing the "check" queue
; this is a queue of [a few dozen] block coordinates to check
; for possible matches (and then mark them to explode).

; input: x = x coordinate, a = y coordinate
; clobbers: ITERATOR, a, x, y
JSR_AddBlockToQueue
    stx ITERATOR
    jsr JSR_ror4iterator
    lda CHECK_QUEUE_C
    cmp #CHECK_QUEUE_MAX
    bpl _addQueueRts
    ldy CHECK_QUEUE_C
    inc CHECK_QUEUE_C
    lda ITERATOR
    sta CHECK_QUEUE_W,y
    rts

; clobbers: a, x, y, ITERATOR, WORD_B, VAR1, VAR2, VAR3
JSR_ProcessQueue:
BLOCK_T = VAR2+1
BLOCK_B = WORD_B
BLOCK_START_X = WORD_B+1
BLOCK_START_Y = VAR4
BLOCK_L = VAR1
BLOCK_R = VAR2
BLOCK_CMP = VAR3
    lda CHECK_QUEUE_C
    beq _addQueueRts
    dec CHECK_QUEUE_C
    ldy CHECK_QUEUE_C
    lda CHECK_QUEUE_R,y
    
    ; x <- x position, a <- y position
    jsr JSR_ror4iterator
    tax
    lda ITERATOR
    stx BLOCK_L
    stx BLOCK_R
    stx BLOCK_START_X
    sta BLOCK_T
    sta BLOCK_B
    sta BLOCK_START_Y
    inc BLOCK_B
    
    ; BLOCK_CMP <- block at x,y
    ; (return if block is 0)
    jsr JSR_GetBlockValue_XA
    beq _addQueueRts
    sta BLOCK_CMP
    
    ; verify there's a block underneath (not falling)
    lda BLOCK_B
    cmp #ROWS
    bpl ScanLeft
    ldx BLOCK_L
    jsr JSR_GetBlockValue_XA
    beq _addQueueRts
    
    
ScanLeft:    
    ldx BLOCK_L
    beq ScanRight
    dex
    stx BLOCK_L
    beq ScanLeft2

ScanLeft1:
    ldx BLOCK_L
    jsr ScanHelper
    bne ScanRight
    dec BLOCK_L
    
ScanLeft2:
    ldx BLOCK_L
    jsr ScanHelper
    bne ScanRight
    dec BLOCK_L
    
ScanRight:
    inc BLOCK_L
    inc BLOCK_R
    ldx BLOCK_R
    cpx #WIDTH-1
    beq ScanRight2
    bpl ScanUp
    
ScanRight1:
    ;ldx BLOCK_R
    jsr ScanHelper
    bne ScanUp
    inc BLOCK_R
    
ScanRight2:
    ldx BLOCK_R
    jsr ScanHelper
    bne ScanUp
    inc BLOCK_R
    
ScanUp:
    dec BLOCK_T
    bmi ScanDown
    beq ScanUp2

ScanUp1:
    ldx BLOCK_START_X
    jsr ScanHelper
    bne ScanDown
    dec BLOCK_T
    
ScanUp2:
    ; lda BLOCK_T
    ldx BLOCK_START_X
    jsr ScanHelper
    bne ScanDown
    dec BLOCK_T

ScanDown:
    inc BLOCK_T
    lda BLOCK_B
    cmp #ROWS-1
    beq ScanDown2
    bpl ScanDone

ScanDown1:
    ldx BLOCK_START_X
    ; lda BLOCK_B
    jsr ScanHelper2
    bne ScanDone
    inc BLOCK_B

ScanDown2:
    lda BLOCK_B
    ldx BLOCK_START_X
    jsr ScanHelper2
    bne ScanDone
    inc BLOCK_B

ScanDone:
    ; postcondition:
    ; BLOCK_L: points to leftmost of same colour
    ; BLOCK_R: points to one right of rightmost of same colour
    ; BLOCK_T: points to topmost of same colour
    ; BLOCK_B: points to one below bottommost of same colour
CLEAR_TIMER = 3

    lda #(CLEAR_TIMER << 3)
    sta BLOCK_CMP
    
CheckHor:
    sec
    lda BLOCK_R
    sbc BLOCK_L
    cmp #3
    bmi CheckVer
    jsr RowTimer_OnExplosion
ClearHor:
    ldx BLOCK_L
    cpx BLOCK_R
    beq CheckVer
    lda BLOCK_START_Y
    ldy BLOCK_CMP
    jsr JSR_SetBlockValue_XA_Y
    inc BLOCK_L
    bpl ClearHor ; gauranteed
    
CheckVer:
    sec
    lda BLOCK_B
    sbc BLOCK_T
    cmp #3
    bmi CheckDone
    
    jsr RowTimer_OnExplosion
ClearVer:
    ldx BLOCK_START_X
    lda BLOCK_T
    cmp BLOCK_B
    beq CheckDone
    ldy BLOCK_CMP
    jsr JSR_SetBlockValue_XA_Y
    inc BLOCK_T
    bpl ClearVer ; gauranteed
    
CheckDone:
    rts
    
ScanHelper:
    lda BLOCK_T
ScanHelper2:
    jsr JSR_GetBlockValue_XA
    cmp BLOCK_CMP
    rts