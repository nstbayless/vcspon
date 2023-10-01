    SEG.U xramr
    ORG $f000

BLOCKS_R ds ROWS*ROW_STRIDE
CHECK_QUEUE_R ds CHECK_QUEUE_MAX
PLAYER_COLOUR_R ds 1
LINES_R ds ROWS*ROWSUB*ROWINSTRC

; NOTE: xramw and xramr must mirror each other.

    SEG.U xramw
    ORG $1400
    
BLOCKS_W ds ROWS*ROW_STRIDE
CHECK_QUEUE_W ds CHECK_QUEUE_MAX
PLAYER_COLOUR_W ds 1
LINES_W ds ROWS*ROWSUB*ROWINSTRC

LINES_CORE_R = LINES_R+6
LINES_CORE_W = LINES_W+6

XRAM_END ds 0

; [py] (syms["BLOCKS_R"] % 0x2000) + 0x400 == syms["BLOCKS_W"]
; [py] (syms["LINES_R"] % 0x2000) + 0x400 == syms["LINES_W"]
; [py] (syms["LINES_CORE_R"] % 0x2000) + 0x400 == syms["LINES_CORE_W"]
; [py] (syms["XRAM_END"] % 0x2000) <= 0x1800
; [py] (syms["CHECK_QUEUE_W"] % 0x100 <= (0x100 - CHECK_QUEUE_MAX))