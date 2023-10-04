DrawDigits
    ; display digits
    ldx LEVEL
    lda DigitsAlt,X
    pha
        jsr DisplayDigitElement
    pla
    lsr
    lsr
    lsr
    pha
        jsr DisplayDigitElement
    pla
    lsr
    lsr
    lsr
    tax
    jsr DisplayDigitPart
    jmp VBlankWaitEnd
        
DisplayDigitElement
    and #$7
    tax
    jsr DisplayDigitPart
    inx
    jsr DisplayDigitPart
    jsr DisplayDigitPart
    
DisplayDigitPart
    sta WSYNC
    lda DigitElements,X
    sta PF1
    jsr sleep15
    jsr _DisplayRTS
    sta PF1 ; disable
_DisplayRTS
    rts