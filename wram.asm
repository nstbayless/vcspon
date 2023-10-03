    SEG.U wram
    ORG $80

TIMER ds 1
ITERATOR ds 1
WORD_A ds 2
WORD_B ds 2
PTR_TO_LINES_CORE_R ds 2
PTR_TO_LINES_CORE_W ds 2
PTR_TO_BLOCKS_R ds 2
PTR_TO_BLOCKS_W ds 2
P1_STROBE_POSITION ds 1
VAR1 ds 1
VAR2 ds 2
VAR3 ds 1
VAR4 ds 1
RNGSEED ds 2

; this is an absolute value
CURY0 ds 1

; during main rendering, this decrements once per row
TCURY0 ds 1

; board column position
CURX0 ds 1

; input
DAS ds 1
PREVINPUT ds 1
GRAVROW ds 1 ; ranges 1-(ROWS-1)
DECTIMERROW ds 1
CHECK_QUEUE_C ds 1
ROW_TIMER ds 1
SHIFT_UP ds 1 ; used when DROP_TOP == 0
SHIFTY ds 1
PLAYER_COLOUR ds 1
FLICKER_FRAME ; used when FLICKER == 1