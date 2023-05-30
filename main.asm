    processor 6502
    include "vcs.h"
    include "macro.h"

    include "wram.asm"
    include "xram.asm"

    MAC DBLDA
        BYTE $A9
    ENDM

    MAC CMPSKIP1
        BYTE $C9
    ENDM

    MAC BITSKIP2
        BYTE $2C
    ENDM

    SEG ROM
    ORG $1800
    
    ALIGN $100

Reset
    CLEAN_START

VISIBLE_ROWS = 192

StartOfFrame
    lda #0
    sta VBLANK

    lda #2
    sta VSYNC
    
    sta WSYNC
    sta WSYNC
    sta WSYNC
    
    lda #$0
    sta VSYNC
    
    REPEAT 37
    sta WSYNC
    REPEND
    
    REPEAT VISIBLE_ROWS/2
        sta WSYNC
        jsr RoutineA
        
        sta WSYNC
        jsr RoutineB
    REPEND
    
    lda #%01000010
    sta VBLANK
    
    REPEAT 30
    STA WSYNC
    REPEND

    jmp StartOfFrame

RoutineA:
    ldx #$54
    lda ~#$54
    ldy #$86
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sty COLUBK
    sax COLUBK
    sta COLUBK
    sax COLUBK
    stx COLUBK
    sax COLUBK
    rts
    
RoutineB:
    ldx #$6F
    lda ~#$6F
    ldy #$33
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sta COLUBK
    stx COLUBK
    sty COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    rts
        
ZBankEnd

    ORG $1FFA
    
    .word Reset ; NMI
    .word Reset ; NMI
    .word Reset ; NMI