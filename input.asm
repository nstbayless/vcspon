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

HorizontalMovement
    ldx CURX0

RightMovement
    asl VAR1
    bpl ._skip_moveright
    cpx #WIDTH-2
    bcs ._skip_moveright
    inx
    asl VAR1
    ldy #$D0 ; -3 clocks
    bne .endmoveleft ; guaranteed
._skip_moveright
    
LeftMovement
    
    asl VAR1
    bpl ._skip_moveleft
    txa
    beq ._skip_moveleft
    dex
    ldy #$30 ; +3 clocks direction 
.endmoveleft
    sty HMP0
    
._skip_moveleft
    stx CURX0

VerticalMovement
    ldx CURY0
    
DownMovement
    asl VAR1
    sta WSYNC  ; ---------------------------------
    sta HMOVE  ; 1/3
    
    bpl _skip_movedown
    if CURSOR_ALLOWED_IN_INCOMING_ROW
        cpx #ROWS-1
        beq _skip_movedown
        inx
    else
        cpx #ROWS-2
        blt _yes_movedown
        cpx #ROWS-1
        bge _skip_movedown
        
        lda SHIFTY
        bne _skip_movedown
        
_yes_movedown
        inx
    endif
    
_skip_movedown

UpMovement
    asl VAR1
    
    sta WSYNC  ; ---------------------------------
    sta HMOVE ; 2/3
    
    bpl ._skip_moveup
    dex
    bpl ._skip_moveup
    inx
._skip_moveup

    stx CURY0

    stx WSYNC
    stx HMOVE ; 3/3

InputEnd: