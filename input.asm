; this file is an inline routine which checks player input during the kernel.

DoInput
    MAC ifneq_incvar2
    beq .skip_inc
    inc VAR2
.skip_inc
    ENDM
    
    ; read input
    lda SWCHA
    eor #$FF
    
    if READ_BOTH_INPUTS
    sta VAR1
    endif 
    
    lsr
    lsr
    lsr
    lsr
    
    if READ_BOTH_INPUTS
    ora VAR1
    endif 
    
    and #$0F
    sta VAR1
    lda INPT4
    if READ_BOTH_INPUTS
    and INPT5
    endif
    eor #$FF
    and #$80
    ora VAR1
    sta VAR1
    
ProcessButtonPress
    lda #$80
    and VAR1
    and PREVINPUT
    ;sta WSYNC  ; ---------------------------------
    beq DontSwap

SwapBlocks:
    ; set player colour
    lda #$C
    sta PLAYER_COLOUR
    ; SWAP HERE
    ldx CURX0
    lda CURY0
    pha
        pha
            pha
                pha
                    pha
                        jsr JSR_GetBlockValue_XA
                        sta VAR2
                        cmp #$8
                        bge DontSwapPLA4
                        
                        ldx CURX0
                        inx
                    pla
                    jsr JSR_GetBlockValue_XA
                    
                    cmp #$8
                    bge DontSwapPLA3
                    
                    tay
                pla
                ldx CURX0
                jsr JSR_SetBlockValue_XA_Y
                
                ldx CURX0
                ldy VAR2
                inx
            pla
            jsr JSR_SetBlockValue_XA_Y
        pla
        ldx CURX0
        jsr JSR_AddBlockToQueue
    pla
    ldx CURX0
    inx
    jsr JSR_AddBlockToQueue
    jmp ProcessDAS
    
DontSwapPLA4
    pla
DontSwapPLA3
    pla
    pla
    pla
    pla
    lda #$10
    ; warning yellow if player tries to swap invalid
    adc PLAYER_COLOUR
    sta PLAYER_COLOUR
    bne ProcessDAS ; guaranteed
    
DontSwap
    
LeftMovement
    
    lda #$04
    ldy #$30 ; +3 clocks
    and VAR1
    ifneq_incvar2
    and PREVINPUT
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
    lda #$08
    and VAR1
    ifneq_incvar2
    and PREVINPUT
    sta WSYNC ; ----------------------------------
    beq ._skip_moveright
    lda CURX0
    cmp #WIDTH-2
    bcs ._skip_moveright
    sty HMP0
    inc CURX0
._skip_moveright
    
UpMovement
    lda #$01
    and VAR1
    
    sta WSYNC  ; ---------------------------------
    sta HMOVE ; 1/3
    
    ifneq_incvar2
    
    and PREVINPUT
    beq ._skip_moveup
    dec CURY0
    lda CURY0
    cmp #$80
    bcc ._skip_moveup
    inc CURY0
._skip_moveup
    
DownMovement
    lda #$02
    and VAR1
    sta WSYNC  ; ---------------------------------
    sta HMOVE  ; 2/3
    
    ifneq_incvar2
    
    and PREVINPUT
    
    beq ._skip_movedown
    inc CURY0
    lda CURY0
    cmp #ROWS
    bcc ._skip_movedown
    dec CURY0
._skip_movedown

ProcessDAS
    lda VAR1
    eor #$FF
    sta PREVINPUT
    lda VAR2
    EOR #$0
    sta WSYNC  ; ---------------------------------
    sta HMOVE  ; 3/3
    
    bne ._dontresetdas
    sta DAS
._dontresetdas
    inc DAS
    bne ._dontcapdas
    dec DAS
._dontcapdas

    lda DAS
    cmp #$10
    ;sta WSYNC ; ----------------------------------
    
    bcc ._dontdas
    
    lda TIMER
    lsr
    lsr
    bcc ._dontdas
    lda #$7F
    ora PREVINPUT
    sta PREVINPUT
._dontdas

InputEnd: