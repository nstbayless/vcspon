    processor 6502
    include "vcs.h"
    include "macro.h"
    
ROWINSTRC = 2*16
ROWSUB = 2
ROWS = 10
WIDTH = 6
SL_PER_SUBROW = 8
    
    include "cmac.h"

    include "wram.asm"
    include "xram.asm"

    SEG ROM
    ORG $1800
    
    ALIGN $100

COLX = $54
COLA = $FF^COLX
COLY = $86

COLX2 = $4B
COLA2 = $FF^COLX2
COLY2 = $33

; configuration: set this to 0 or 1 for different style.
THICCURSOR = 1

DISPMARGIN = 14

; bitwise pointer arithmetic may in the future happen here
; so this table musn't move.

LINE_OPCODE_LOOKUP_B
    hex 87 ; SAX
    hex 87 ; SAX
    hex 87 ; SAX
    hex 87 ; SAX
    
LINE_OPCODE_LOOKUP_A
    hex 87 ; SAX
    hex 86 ; STX
    hex 85 ; STA
    hex 84 ; STY
    
    hex 87 ; SAX
    hex 87 ; SAX
    hex 87 ; SAX
    hex 87 ; SAX

LINES_CORE_W_ADDR_LOOKUP_HI
i SET 0
    REPEAT ROWS*2
        dc.b (LINES_CORE_W+$20*i) >> 8
i SET i + 1
    REPEND
    
LINES_CORE_W_ADDR_LOOKUP_LO
i SET 0
    REPEAT ROWS*2
        dc.b (LINES_CORE_W+$20*i) & $FF
i SET i + 1
    REPEND

RoutineA
    ldx #COLX
    lda #COLA
    ldy #COLY
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK; one extra, to clear
    rts
    
    REPEAT 32-21
        hex 00
    REPEND
    
RoutineB:
    lda #COLA2
    ldx #COLX2
    ldy #COLY2
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK
    sax COLUBK; one extra, to clear
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
    
    dey
    bne .rngloop0
    sta RNGSEED+0
    rts
    

RoutineBPost:
    sta (#$00),y
    rts
    lda (#$00),y
    rts

    ; input: y, WORD_A, WORD_B
    ; clobbers: a,x
    ; y <- 0
    ; if y is 0, copy 256 bytes.
memcpy
_memcpy_loop:
    ldx #$0
    lda (WORD_A,x)
    sta (WORD_B,x)
    inc WORD_A
    INCEQ WORD_A+1
    inc WORD_B
    INCEQ WORD_B+1
    dey
    bne _memcpy_loop
    rts

VISIBLE_ROWS = 192

; input: x is x position, a is y position
; output: a is index of block
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
        lda (PTR_TO_BLOCKS_R),y
    ENDM

; input: x is x position, a is y position, y is value
; output: a is value, y is x position, x is y position
    MAC SetBlockValue_XA_Y
        pha
        GetBlockAddr_XA
        
        ; write BLOCK
        sta PTR_TO_BLOCKS_W
        tya
        ldx #$0
        sta (PTR_TO_BLOCKS_R,X)
        stx PTR_TO_BLOCKS_W
        
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
            lda LINE_OPCODE_LOOKUP_A,y
            ldy ITERATOR
            sta (WORD_A),y
            
            ; row B
            inx
            lda LINES_CORE_W_ADDR_LOOKUP_HI,x
            sta WORD_A+1
            lda LINES_CORE_W_ADDR_LOOKUP_LO,x
            sta WORD_A
        pla
        sta (WORD_A),y
    ENDM

JSR_GetBlockValue_XA
    ldy #$0
JSR_GetBlockValue_XA_y0
    GetBlockValue_XA_y0
    rts

JSR_SetBlockValue_XA_Y
    SetBlockValue_XA_Y
    rts

ResetRows
    lda #ROWS
    sta ITERATOR
    
.ResetRowsLoop
    LOADPTR WORD_A, RoutineA
    
    lda ITERATOR
    asl
    tax
    lda LINES_CORE_W_ADDR_LOOKUP_LO,x
    sec
    sbc #$6
    sta WORD_B
    lda LINES_CORE_W_ADDR_LOOKUP_HI,x
    sbc #$0
    sta WORD_B+1
    
    ldy #64
    jsr memcpy
    
    dec ITERATOR
    bpl .ResetRowsLoop
    
    rts

Reset
    CLEAN_START
    lda $F0
    eor $F1
    eor $F2
    sta RNGSEED
    eor $F3
    eor $F4
    sta RNGSEED+1
    ldx #$00
    lda #$02
clean_loop
    sta $1400,x
    sta $1500,x
    sta $1600,x
    sta $1700,x
    inx
    bne clean_loop
    
    LOADPTR PTR_TO_LINES_CORE_R, LINES_CORE_R
    LOADPTR PTR_TO_LINES_CORE_W, LINES_CORE_W
    LOADPTR PTR_TO_BLOCKS_R, BLOCKS_R
    LOADPTR PTR_TO_BLOCKS_W, BLOCKS_W
    
    lda #$0F
    sta COLUPF
    sta COLUP0
    sta COLUP1
    
    lda #$05 + 2*THICCURSOR
    sta NUSIZ0
    sta NUSIZ1
    
    sta WSYNC
    sleep DISPMARGIN+10
    sta RESP0
    
    sta WSYNC
    sleep DISPMARGIN+10
    sta RESP1
    
    sta WSYNC
    lda #$F0 - THICCURSOR*$80
    sta HMP0
    sta HMP1
    sta HMOVE
    
    jsr ResetRows
    
InitBlockValues
    lda #ROWS-1
    sta VAR2
.nexty
    lda #WIDTH
    sta VAR1
.nextx
    ldx VAR1

    lda VAR1
    ora VAR2
    
    jsr rng
    and #$7
    cmp #$4
    IFEQ_LDA #$0
    tay
    
    lda VAR2
    dex
    jsr JSR_SetBlockValue_XA_Y
    
    dec VAR1
    bne .nextx
    dec VAR2
    bne .nexty
    
    ldx #$0
    ldy #$3
    lda #$0
    jsr JSR_SetBlockValue_XA_Y
    

StartOfFrame
    lda #0
    sta VBLANK
    inc TIMER

    lda #2
    sta VSYNC
    sta WSYNC
    sta WSYNC
    sta WSYNC
    
    lda #$0
    sta VSYNC
    
    tax
    ; var2 ends up nonzero if das timer shouldn't be reset
    sta VAR2
    sta HMP0
    
    MAC ifneq_incvar2
    beq .skip_inc
    inc VAR2
.skip_inc
    ENDM
    
    ; read input
    lda SWCHA
    eor #$FF
    sta VAR1
    
LeftMovement
    
    lda #$44
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
    lda #$88
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
    lda #$11
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
    lda #$22
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
    sta WSYNC ; ----------------------------------
    
    bcc ._dontdas
    
    lda TIMER
    lsr
    lsr
    bcc ._dontdas
    lda #$FF
    sta PREVINPUT
._dontdas

    sta WSYNC ; ---------------------------------
    
    REPEAT 37-7
    sta WSYNC ; ---------------------------------
    REPEND
    
    inc CURY0
    
i SET 0
    REPEAT ROWS
        SUBROUTINE
        
        jsr kernel_cursor_pre
        
        lda #SL_PER_SUBROW
        
        sta ITERATOR
        
.rowloop
        sta WSYNC
        SLEEP DISPMARGIN
        jsr LINES_R+64*i
        
        sta WSYNC
        SLEEP DISPMARGIN
        jsr LINES_R+32+64*i
        dec ITERATOR
        bne .rowloop
        
        ; cursor
        jsr kernel_cursor_post
        
i SET i+1
    REPEND
    
    ; undo the 'damage' done to CURY during render loop
    clc
    lda #ROWS-1
    adc CURY0
    sta CURY0
    
    sta WSYNC
    lda #$0
    sta GRP0
    
    REPEAT VISIBLE_ROWS-ROWS*(SL_PER_SUBROW*2+2)-1
    sta WSYNC
    REPEND
    
    lda #%01000010
    sta VBLANK
    
    REPEAT 30
    STA WSYNC
    REPEND

    jmp StartOfFrame

kernel_cursor_pre:
    lda #$0
    dec CURY0
    sta WSYNC
    bne ._skipdraw
    
    lda #$FF - $7*THICCURSOR
    sta GRP0
    
    sleep 24+DISPMARGIN ; could use this!
    
    lda #$81 + ($7*THICCURSOR)
    
    ;sta GRP0
    
._skipdraw
    sta GRP0
    
    rts
    
kernel_cursor_post:
    lda #$0
    cmp CURY0
    IFEQ_LDA #$FF - $7*THICCURSOR
    sta WSYNC
    sta GRP0
    rts
    

ZBankEnd

    ORG $1FFA
    
    .word Reset ; NMI
    .word Reset ; NMI
    .word Reset ; NMI