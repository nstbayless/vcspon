; clobbers ITERATOR
; resulting A is random 0-7, but never 4; biased toward 0
rng6
    jsr rng
    and #7
    cmp #4
    bne _rtsrng6
    and #$0
_rtsrng6
    rts
    
; clobbers ITERATOR
; resulting A is random
rng:
    eor RNGSEED+0
    sta RNGSEED+0
    pha
    lda #$8
    sta ITERATOR
    pla
    
.rngloop0
    asl
    rol RNGSEED+1
    bcc .rngloop1
    
    eor #$6B

.rngloop1:
    
    dec ITERATOR
    bne .rngloop0
    sta RNGSEED+0
    rts