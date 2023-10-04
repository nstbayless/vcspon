    processor 6502
    include "vcs.h"
    include "macro.h"
    
    include "config.asm"
    include "cmac.h"

    include "wram.asm"
    include "xram.asm"

    SEG ROM
    ORG $1800
    
    ALIGN $100

    include "data.asm"
    include "rng.asm"

    MAC ADD_WORD_IMM
        lda {1}   ; 3
        adc {2}      ; 2
        sta {1}   ; 3
        lda #$0      ; 2
        adc {1}+1 ; 3
        sta {1}+1 ; 3
    ENDM

    include "blocks.asm"
    include "queue.asm"

RandomizeBottomRowHelper
    and #$7
    tax
    ldy RNG8Table,x
    ldx VAR3
    lda #ROWS-1
    jsr JSR_SetBlockValue_XA_Y
    dec VAR3
    rts

    include "display_routines.asm"

TopOut:
    if TOPDELAY
    bit TOPOUT_DELAYED
    bpl Reset
    clc
    ror TOPOUT_DELAYED
    ldx LEVEL
    lda TopOutGracePeriod,X
    sta ROW_TIMER
    
    if SOUNDVOL
    lda #34
    sta AUDC0
    sta AUDF0
    lda #22
    sta SOUNDVOL
    endif
    
    rts
    endif

; main / Entrypoint
Reset
    inc $F1
    bne .cleanstart
    dec $F2
.cleanstart
    CLEAN_START
    
    ; red background while loading
    lda #$30
    sta COLUBK
    
    eor $F0
    eor $F1
    eor $F2
    sta RNGSEED
    eor $F3
    ;eor $F4
    sta RNGSEED+1
    
    if 0
        lda #$00
        tax
clean_loop
        sta $1400,x
        sta $1500,x
        sta $1600,x
        sta $1700,x
        inx
        bne clean_loop
    endif
    
    sta WSYNC
    lda #$0F
    sta COLUPF
    sta COLUP1
    lda #$05 + 2*THICCURSOR
    sta NUSIZ0
    sta GRAVROW ; just need to set this to any value between 1 and ROWS inclusive
    lda #$7
    sta NUSIZ1
    sleep DISPMARGIN-11 ; [UNUSED]
    sta RESP0
    
    ;lda #$70
    sta WSYNC
    SLEEP 3; [UNUSED]
    lda #$F0 - THICCURSOR*$80
    sta HMP0
    sta HMOVE
    
    ;lda #$0
    ;sta HMP1
    
    if RESET_WAIT
    ; wait
    lda #255
    sta T1024T
ResetWait
    lda INTIM
    bne ResetWait
    endif
    
    
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
    
    ldx #$0
_memcpy_loop:
    lda (WORD_A,x)
    sta (WORD_B,x)
    inc WORD_A
    ;INCEQ WORD_A+1 ;
    inc WORD_B
    INCEQ WORD_B+1
    dey
    bne _memcpy_loop
    
    dec ITERATOR
    bpl .ResetRowsLoop
    
InitBlockValues
    lda #ROWS
    sta VAR2
.nexty
    dec VAR2
    lda #WIDTH
    sta VAR1
.nextx
    
    ; spawn blocks on even rows only
    ; this forces gravity to fix any problems
    ; (e.g. too many in a row) -- we check for such things
    ; when blocks fall.
    ldy #$0
    lda VAR2
    ror
    bcs _norng
    jsr rng6
    tay
_norng
    
    ldx VAR1
    lda VAR2
    dex
    jsr JSR_SetBlockValue_XA_Y
    
    dec VAR1
    bne .nextx
    inc VAR2
    dec VAR2
    bne .nexty
    
Kernel:
StartOfFrame:
  
    sta WSYNC
    lda #0
    sta VBLANK
    lda #2
    sta VSYNC

    ; set timer for end of vblank -- we'll check on this later
    lda #55
    sta TIM64T
    
    ; update timer
    if FLICKER
        inc TIMER
        lda SHIFTY
        adc TIMER
        ror
        ror
        sta FLICKER_FRAME
    else
        inc TIMER
    endif
    
    ; set player colour
    ldx PLAYER_COLOUR
    beq _noDecPlayerColour
    dex
    stx PLAYER_COLOUR
    
_noDecPlayerColour:
    txa
    eor #$FF
    adc #$0F
    sta COLUP0
    
StrobeExplosion:
    lda #$FE
    ldx P1_STROBE_POSITION
    sta VAR1
    lda NextP1StrobeTable,X
    sta WSYNC
    ; Timing sensitive -- strobe p1 position
    SLEEP 3 ; UNUSED
    sta P1_STROBE_POSITION
    and #$3
    cmp P1_STROBE_POSITION
    ; [py] ${_strobelradjust} % 0x100 != 0
    beq _strobelradjust
_strobelradjust
    sec
    adc #$0
    sec
    ; [py] ${_strobelooptop} % 0x100 < 0xFB
_strobelooptop
    adc VAR1
    bne _strobelooptop
    sta RESP1
    
    if BASIC_SOUND
        lda TIMER
        ror
        bcs nosound
        ldx SOUNDVOL
        dex
        bpl noincsoundvol
        inx
noincsoundvol
        stx SOUNDVOL
        stx AUDV0
nosound
    endif
    
    sta WSYNC
    
    ; mostly unused!
    if CAN_RESET
    ror SWCHB
    bcs noreset
    jmp Reset
noreset
    endif
    
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
    
    ; (read input here.)
    include "input.asm"

    .IF GRAVITY
    include "gravity.asm"
    ENDIF
    
PreRender
    ldx CURY0
    inx
    stx TCURY0
    lda P1_STROBE_POSITION
    and #$3
    asl
    sta VAR1
    
    ldx LEVEL
    lda LevelColours,X
    asl
    asl
    asl
    tay
    
    ldx #228
VBlankWaitEnd:
    lda INTIM
    bne VBlankWaitEnd
    
    stx WSYNC
    stx TIM64T
    ; set timer to wait for start of overscan
    sta VAR2 ; VAR2 <- 0 / clears grp1 bits later
    
    LOADPTR WORD_A, LINES_R
    sta WORD_B+1
    lda #<(LINES_R+32)
    sta WORD_B
    ; [py] ${LINES_R} // 0x100 == (${LINES_R} + 32) // 0x100
    ;LOADPTR WORD_B, (LINES_R+32)
    
    sty COLUBK ; draw line at top
    
    ldy #ROWS-1
    ldx SHIFTY
    beq _nodecshifty ; 2/3
    dey ; 2
_nodecshifty
    beq _here ; correct timing
_here
    sty VAR3
_adjusttime
    lda #$0
    sta COLUBK
    
VBlankEnd:

    include "display.asm"
    
CheckLevelUp
    ; check for level up before overscan, why not
    ldx LEVEL
    lda LVL_CLEARS
    bit SWCHB ; check difficulty
    bpl nodouble
    asl
nodouble
    cmp LevelExplosionsRequiredToAdvance,X
    blt WaitForOverscan
    beq WaitForOverscan
    
LevelUp
    inc LEVEL
    stx LVL_CLEARS
    
    if BASIC_SOUND
        lda #38
        sta SOUNDVOL
        sta AUDC0
        sta AUDF0
    endif
    
WaitForOverscan:
    ldx #29
    lda INTIM
    bne WaitForOverscan
    stx WSYNC ; ----------
    stx TIM64T
    
OverscanBegin:
    lda #%01000010
    sta VBLANK
    
    lda #$0
    sta GRP0
    sta VAR2
    
CheckShiftUp
    ; check if we need to shift up
    
    if DROP_TOP == 0
    bit SHIFT_UP
    bpl NoShiftUp
    lda CHECK_QUEUE_C
    bne NoShiftUp2
    dec SHIFT_UP
    jmp ShiftUp
NoShiftUp
    bvc NoShiftUp2
    lda #$0
    sta SHIFT_UP
RandomizeBottomRow:
    lda #WIDTH-1
    sta VAR3
    
newrowrngloop:
    jsr rng
    pha
        jsr RandomizeBottomRowHelper
    pla
    ror
    ror
    ror
    ror
    jsr RandomizeBottomRowHelper
    bpl newrowrngloop
    jmp WaitForVblank
NoShiftUp2
    endif
    
    ; decrement shifty
    dec SHIFTY
    bpl _noincshifty
    inc SHIFTY
_noincshifty
    
    ; add new rows
    dec ROW_TIMER
    bne DecrementExplosionTimers
    ldx LEVEL
    lda LevelRowInterval,X
    
    bit SWCHB ; difficulty
    bvc nohalve
    lsr
    adc #$3
nohalve
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
    if BASIC_SOUND
        lda #10
        sta AUDC0
        sta AUDF0
        asl
        sta SOUNDVOL
    endif
check_topout_loop:
    lda BLOCKS_R,y
    bne JumpToTopOut
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
    if BASIC_SOUND
        lda TIMER
        ora #4
        sta AUDF0
        ldx #$83
        stx AUDC0
        lda #15
        sta SOUNDVOL
    else if TOPDELAY
        ldx #$80
    endif
    
    if TOPDELAY
        stx TOPOUT_DELAYED
    endif
    inc LVL_CLEARS
    ldx LEVEL
    bit SWCHB ; difficulty
    bvc notfaster
    inx
notfaster
    lda LevelExplosionAddTime,X
    adc ROW_TIMER
    bcs _rowmax
    
    hex 2C ; bit trick
    
_rowmax
    lda #$FF
    sta ROW_TIMER
    rts

_nextrowrts
    ; mark bottom row to check
    
    ; [py] ${ROWS-1} == 9 # (palindrome)
    lda #(ROWS-1)
    ldx #WIDTH
    stx CHECK_QUEUE_C
    clc
    
_markloop
    sta CHECK_QUEUE_W-1,x
    adc #$10
    dex
    bne _markloop
    
    jmp PreRender

JumpToTopOut
    jmp TopOut

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
    
    if TOPDELAY
    ror TOPOUT_DELAYED
    endif
    
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
PseudoKernelWait
    lda INTIM
    bne PseudoKernelWait
    sta WSYNC
    lda #(SL_PER_SUBROW*ROWSUB)
    sta SHIFTY
    sta WSYNC
    sta VBLANK
    lda #2
    sta VSYNC
    lda #54
    sta TIM64T
    sta WSYNC
_DecCursorY
    dec CURY0
    bpl _NoIncCursorY
    inc CURY0
_NoIncCursorY
    lda #$0
    sta WSYNC
    sta VSYNC
EndPseudoKernelWait

    clc
    ADD_WORD_IMM WORD_A, #ROWINSTRC
    ;clc
    ADD_WORD_IMM WORD_B, #ROWINSTRC
    jmp ShiftRow
    
ZBankEnd

    ORG $1FFA
    
    .word Reset ; NMI
    .word Reset ; NMI
    .word Reset ; NMI