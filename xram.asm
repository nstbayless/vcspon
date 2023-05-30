    SEG.U xramr
    ORG $f000

BLOCKS_R ds ROWS*8
LINES_R ds ROWS*ROWSUB*ROWINSTRC

; NOTE: xramw and xramr must mirror each other.

    SEG.U xramw
    ORG $1400
    
BLOCKS_W ds ROWS*8
LINES_W ds ROWS*ROWSUB*ROWINSTRC

LINES_CORE_R = LINES_R+6
LINES_CORE_W = LINES_W+6