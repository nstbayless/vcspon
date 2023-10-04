; this file is an inline routine which checks player input during the kernel.

DoInput
    
    ; read input: bit 7 is button pressed, bits 3, 4, 5, 6 are direction
    lda INPT4 ; read button
    asl
    lda SWCHA ; read directional inputs
    ror
    tay
    eor #$FF
    tax
        
ProcessButtonPress
    and PREVINPUT
    ;sta WSYNC  ; ---------------------------------
    bpl DontSwap

SwapBlocks:
    ; set player colour
    lda #$C
    sta PLAYER_COLOUR
    if BASIC_SOUND
        ldx SOUNDVOL
        bne .noswapsound
        sta AUDF0
        sta AUDC0
        lsr
        sta SOUNDVOL
.noswapsound
    endif
    ; SWAP HERE
    ldx CURX0
    stx PREVINPUT ; this actually just sets the relevant bits of previnput to 0
    lda CURY0
                        jsr JSR_GetBlockValue_XA
                        sta VAR2
                        cmp #$8
                        bge DontSwapPLA4
                        
                        ldx CURX0
                        inx
                    lda CURY0
                    jsr JSR_GetBlockValue_XA
                    
                    cmp #$8
                    bge DontSwapPLA3
                    
                    tay
                lda CURY0
                ldx CURX0
                jsr JSR_SetBlockValue_XA_Y
                
                ldx CURX0
                ldy VAR2
                inx
            lda CURY0
            jsr JSR_SetBlockValue_XA_Y
        lda CURY0
        ldx CURX0
        jsr JSR_AddBlockToQueue
    lda CURY0
    ldx CURX0
    inx
    jsr JSR_AddBlockToQueue
    jmp InputEnd
    
DontSwapPLA4
DontSwapPLA3
    lda #$10
    ; warning yellow if player tries to swap invalid
    adc PLAYER_COLOUR
    sta PLAYER_COLOUR
    bne InputEnd ; guaranteed
    
ExecuteDas
    stx VAR1
    ldy #(DAS_INITIAL - DAS_INTERVAL)
    bne DoneDasSty ; guaranteed
    
DontSwap
DoDAS
    txa ; a <- raw input
    and #$78
    tax
    and PREVINPUT
    sta VAR1 ; pressed this frame
    sty PREVINPUT
    
AdjustDASTimer
    ldy DAS
    txa
    bne dontResetDAS
    ldy #0
dontResetDAS
    cpy #DAS_INITIAL
    bge ExecuteDas
    iny
DoneDasSty
    sty DAS
DoneDas
    
    
LeftMovement
    
    lda #$20
    ldy #$30 ; +3 clocks direction 
    and VAR1
    sta WSYNC  ; ---------------------------------
    beq ._skip_moveleft
    lda CURX0
    eor #$00
    beq ._skip_moveleft
    dec CURX0
    sty HMP0
    
._skip_moveleft

RightMovement

    ldy #$D0 ; -3 clocks
    lda #$40
    and VAR1
    sta WSYNC ; ----------------------------------
    beq ._skip_moveright
    lda CURX0
    cmp #WIDTH-2
    bcs ._skip_moveright
    sty HMP0
    inc CURX0
._skip_moveright
    
UpMovement
    lda #$08
    and VAR1
    
    sta WSYNC  ; ---------------------------------
    sta HMOVE ; 1/3
    
    beq ._skip_moveup
    dec CURY0
    lda CURY0
    cmp #$80
    bcc ._skip_moveup
    inc CURY0
._skip_moveup
    
DownMovement
    lda #$10
    and VAR1
    sta WSYNC  ; ---------------------------------
    sta HMOVE  ; 2/3
    
    beq ._skip_movedown
    inc CURY0
    lda CURY0
    cmp #ROWS
    bcc ._skip_movedown
    dec CURY0
._skip_movedown

    if CURSOR_SLIDE == 0
        stx WSYNC
        stx HMOVE ; 3/3
    endif

InputEnd: