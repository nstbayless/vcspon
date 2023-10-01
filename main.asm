    processor 6502
    include "vcs.h"
    include "macro.h"
    
ROWINSTRC = 2*16
ROWSUB = 2
ROWS = 10
ROW_STRIDE = 8
ROW_INTERVAL = 250
WIDTH = 6
SL_PER_SUBROW = 8
CHECK_QUEUE_MAX = 41
    
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
FLICKER = 0
LINEOP_WRITE_INDIRECT_ACCESS = 1
GRAVITY = 1
STROBE_P1 = 1

DROP_TOP = 0

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
    REPEAT ROWS*ROWSUB
        dc.b (LINES_CORE_W+ROWINSTRC*i) >> 8
i SET i + 1
    REPEND
    
LINES_CORE_W_ADDR_LOOKUP_LO
i SET 0
    REPEAT ROWS*ROWSUB
        dc.b (LINES_CORE_W+ROWINSTRC*i) & $FF
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
    
    include "rng.asm"

RoutineBPost:
    sta (#$00),y
    rts
    lda (#$00),y
    rts

    ; input: y, WORD_A, WORD_B
    ; clobbers: a,x
    ; if y is 0, copy 256 bytes.
    ; y <- 0, x <- 0
memcpy
    ldx #$0
_memcpy_loop:
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

    MAC ADD_WORD_IMM
        lda {1}   ; 3
        adc {2}      ; 2
        sta {1}   ; 3
        lda #$0      ; 2
        adc {1}+1 ; 3
        sta {1}+1 ; 3
    ENDM

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
        
        ; write BLOCK
        ; [py] syms["BLOCKS_W"] % 0x100 == 0
        sta PTR_TO_BLOCKS_W
        tya
        ldx #$0
        sta (PTR_TO_BLOCKS_W,X)
        stx PTR_TO_BLOCKS_W
        
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

ResetRows
    lda #ROWS-1
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

; [py] syms["NextP1StrobeTable"] % 0x100 <= 0x100 - 3
NextP1StrobeTable:
    hex 06
    hex 00
    hex 01
    hex 00 ; --
    hex 02
    hex 04
    hex 05

; main / Entrypoint
Reset
    inc $F1
    dec $F2
    inc $F3
    dec $F4
    CLEAN_START
    lda $F0
    eor $F1
    eor $F2
    sta RNGSEED
    eor $F3
    eor $F4
    sta RNGSEED+1
    lda #$00
    tax
clean_loop
    sta $1400,x
    sta $1500,x
    sta $1600,x
    sta $1700,x
    inx
    bne clean_loop
    
    LOADPTR PTR_TO_LINES_CORE_R, LINES_CORE_R
    LOADPTR PTR_TO_LINES_CORE_W, LINES_CORE_W
    LOADPTR PTR_TO_BLOCKS_W, BLOCKS_W
    
    lda #$0F
    sta COLUPF
    sta COLUP1
    
    lda #$05 + 2*THICCURSOR
    sta NUSIZ0
    sta GRAVROW ; just need to set this to any value between 1 and ROWS inclusive
    lda #$7
    sta NUSIZ1
    
    sta WSYNC
    sleep DISPMARGIN+10
    sta RESP0
    
    sta WSYNC
    sleep DISPMARGIN+10
    sta RESP1
    
    lda #$70
    sta WSYNC
    sta HMP1
    lda #$F0 - THICCURSOR*$80
    sta HMP0
    sta HMOVE
    
    lda #$0
    sta HMP1
    
    jsr ResetRows
    
InitBlockValues
    lda #ROWS
    sta VAR2
.nexty
    dec VAR2
    lda #WIDTH
    sta VAR1
.nextx
    ldx VAR1

    lda VAR1
    ora VAR2
    
    jsr rng6
    cmp #$4
    IFEQ_LDA #$0
    tay
    
    lda VAR2
    dex
    jsr JSR_SetBlockValue_XA_Y
    
    dec VAR1
    bne .nextx
    inc VAR2
    dec VAR2
    bne .nexty
    
    ldx #$0
    ldy #$3
    lda #$0
    jsr JSR_SetBlockValue_XA_Y

Kernel:
StartOfFrame:
    inc TIMER
    sta WSYNC
    lda #0
    sta VBLANK
    lda #2
    sta VSYNC

    ; set timer for end of vblank -- we'll check on this later
    lda #55
    sta TIM64T
    
    ; set player colour
    ldx PLAYER_COLOUR_R
    beq _noDecPlayerColour
    dex
    stx PLAYER_COLOUR_W
    
_noDecPlayerColour:
    txa
    eor #$FF
    adc #$0F
    sta COLUP0
    
    lda #$FE
    ldx P1_STROBE_POSITION
    sta VAR1
    lda NextP1StrobeTable,X
    sta WSYNC
    ; Timing sensitive -- strobe p1 position
    SLEEP 3
    sta P1_STROBE_POSITION
    and #$3
    cmp P1_STROBE_POSITION
    beq _strobelradjust
_strobelradjust
    sec
    adc #$0
    sec
    ; [py] ${_strobelooptop} % 0x100 != 0xFF
_strobelooptop
    adc VAR1
    bne _strobelooptop
    sta RESP1
    
    sta WSYNC
    
    ; TODO -- use this
    
    sta WSYNC
    lda #$0
    sta VSYNC
    
    tax ; x <- 0
    ; var2 ends up nonzero if das timer shouldn't be reset
    sta VAR2
    sta HMP0
    
    ; this is very important;
    ; we'll get glitches if the queue fills up.
ThrottleIfQueueNearlyFull
    lda CHECK_QUEUE_C
    cmp #CHECK_QUEUE_MAX-WIDTH-3
    blt DoNotThrottle
    
    ; FIXME: why does this glitch?
    ;jsr JSR_ProcessQueue
    
    jmp PreRender
DoNotThrottle
    
DoInput
    MAC ifneq_incvar2
    beq .skip_inc
    inc VAR2
.skip_inc
    ENDM
    
    ; read input
    lda SWCHA
    eor #$FF
    sta VAR1
    lsr
    lsr
    lsr
    lsr
    ora VAR1
    and #$0F
    sta VAR1
    lda INPT4
    and INPT5
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
    sta PLAYER_COLOUR_W
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
    adc PLAYER_COLOUR_R
    sta PLAYER_COLOUR_W
    jmp ProcessDAS
    
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

    .IF GRAVITY
DoGravity
    dec GRAVROW
    bne _DontWrapGravRow
    lda #ROWS-1
    sta GRAVROW
_DontWrapGravRow

    lda #WIDTH
    sta VAR1
GravLoop
    dec VAR1
    
    ldx VAR1
    lda GRAVROW
    jsr JSR_GetBlockValue_XA
    bne SkipGravDrop
    
    ldx GRAVROW
    dex
    txa
    sta VAR2
        ldx VAR1
        jsr JSR_GetBlockValue_XA
        beq SkipGravDrop
        cmp #$8
        bge SkipGravDrop
        
        pha
            lda GRAVROW
            ldx VAR1
            jsr JSR_AddBlockToQueue
        pla
        
        tay
        lda GRAVROW
        ldx VAR1
        jsr JSR_SetBlockValue_XA_Y
    lda VAR2
    ldx VAR1
    jsr JSR_SetBlockValue_XA_0
    
SkipGravDrop:

GravLoopBottom:
    lda #$FF
    and VAR1
    bne GravLoop
GravEnd:
    ENDIF
    
PreRender
    inc CURY0    
    lda P1_STROBE_POSITION
    and #$3
    asl
    sta VAR1
    ldx #227
VBlankWaitEnd:
    lda INTIM
    bne VBlankWaitEnd
    stx WSYNC
    ; set timer to wait for start of overscan
    stx TIM64T
    sta VAR2 ; clears grp1 bits later
    LOADPTR WORD_A, LINES_R
    LOADPTR WORD_B, (LINES_R+32)
    lda #ROWS-1
    sta VAR3
    
VBlankEnd:

    include "display.asm"
    
WaitForOverscan:
    ldx #29
    lda INTIM
    bne WaitForOverscan
    stx WSYNC ; ----------
    stx TIM64T
    
OverscanBegin:
    lda #%01000010
    sta VBLANK

    ; undo the 'damage' done to CURY during render loop
    clc
    lda #ROWS-1
    adc CURY0
    sta CURY0
    lda #$0
    sta GRP0
    sta VAR2
    
CheckShiftUp
    ; check if we need to shift up
    
    if DROP_TOP == 0
    bit SHIFT_UP
    bpl NoShiftUp
    dec SHIFT_UP
    jmp ShiftUp
    endif
    
NoShiftUp
    ; add new rows
    dec ROW_TIMER
    bne DecrementExplosionTimers
    lda #ROW_INTERVAL
    sta ROW_TIMER
    
    jsr JSR_AddRow
    jmp WaitForVblank

DecrementExplosionTimers
    ; decrement explosion timers
    ldy DECTIMERROW
    cpy #ROWS-1
    bne _stxDecTimerRow
    ldy #$FF
_stxDecTimerRow:
    iny
    sty DECTIMERROW
    
    ldx #WIDTH-1
    stx VAR1
_decloop:
    lda DECTIMERROW
    ldx VAR1
    jsr JSR_GetBlockValue_XA
    cmp #$8
    blt _next
    
    inc VAR2; mark that a decrement occurred
    ;sec
    sbc #8
    sta BLOCKS_W,y
    
_next
    dec VAR1
    bpl _decloop
_decloopend:
    
    ; process check-queue, unless we decremented an explosion this frame
    ; (gotta choose our battles!)
    lda VAR2
    bne _noProcessQueue
    jsr JSR_ProcessQueue
_noProcessQueue

WaitForVblank:
    lda INTIM
    bne WaitForVblank
    jmp StartOfFrame
    
JSR_AddRow:
    ; first check for topping out
    ldy #WIDTH-1
    lda #$0
check_topout_loop:
    cmp BLOCKS_R,y
    bne TopOut
    dey
    bpl check_topout_loop

    if DROP_TOP == 1
shift_rows_up:
    lda #WIDTH-1
    sta VAR3
    lda #ROWS
    sta VAR4
    
add_new_rows:
    lda #WIDTH-1
    sta VAR1
    
add_new_rows_loop:
    jsr rng6
    tay
    lda #$0
    ldx VAR1
    jsr JSR_SetBlockValue_XA_Y
    
    dec VAR1
    bpl add_new_rows_loop
    
    rts
    endif
    
    if DROP_TOP == 0
    lda #$80
    sta SHIFT_UP ; will 
    rts
    endif
    
RowTimer_OnExplosion:
    lda #ROW_INTERVAL
    sta ROW_TIMER
    rts
    
TopOut:
    ; TODO
    jmp Reset
    
    include "display_routines.asm"
    
ShiftUp
    ldy #ROW_STRIDE
    ldx #0
    
ShiftLoop
    lda BLOCKS_R,y
    sta BLOCKS_W,x
    iny
    inx
    cpy #(ROW_STRIDE*ROWS)
    blt ShiftLoop
    
    ldx #$0    
    
    LOADPTR WORD_A, LINES_CORE_W
    LOADPTR WORD_B, (LINES_CORE_R+ROWSUB*ROWINSTRC)
    ldx #ROWS*ROWSUB-2
ShiftRow
    ldy #(WIDTH-1)*2
    
ShiftRowLoop
    lda (WORD_B),y
    sta (WORD_A),y
    dey
    dey
    bpl ShiftRowLoop
    
NextRow:
    dex
    beq _nextrowrts
    
    cpx #$10
    bne EndPseudoKernelWait
    lda #$0
PseudoKernelWait
    cmp INTIM
    bne PseudoKernelWait
    sta WSYNC
    sta WSYNC
    sta VBLANK
    lda #2
    sta VSYNC
    lda #54
    sta TIM64T
    sta WSYNC
    lda #$0
    sta WSYNC
    sta VSYNC
EndPseudoKernelWait

    
    clc
    ADD_WORD_IMM WORD_A, #ROWINSTRC
    clc
    ADD_WORD_IMM WORD_B, #ROWINSTRC
    jmp ShiftRow
    
_nextrowrts
    ; randomize bottom row now
    lda #WIDTH-1
    sta VAR3
    
newrowrngloop:
    dec VAR3
    bpl newrowrngloop
    
    jmp PreRender
    
ZBankEnd

    ORG $1FFA
    
    .word Reset ; NMI
    .word Reset ; NMI
    .word Reset ; NMI