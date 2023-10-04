; enable/disable these with 0s and 1s.
THICCURSOR = 1
FLICKER = 0
LINEOP_WRITE_INDIRECT_ACCESS = 1
GRAVITY = 1
STROBE_P1 = 1
DROP_TOP = 0
READ_BOTH_INPUTS = 0 ; read both left and right ports -- saves about 9 bytes
CAN_RESET = 1 ; respect the reset latch
TOPDELAY = 1 ; grace period before topping out, gives the player extra time to recover.
DISPLAY_DIGITS = 1

DISPMARGIN = 14 ; may be a little baked in actually. Don't change this?

COLX = $54
COLA = $FF^COLX
COLY = $86

COLX2 = $4B
COLA2 = $FF^COLX2
COLY2 = $33

ROWINSTRC = 2*16
ROWSUB = 2
ROWS = 10
ROW_STRIDE = 8
WIDTH = 6
SL_PER_SUBROW = 8
CHECK_QUEUE_MAX = 42
TOPROW = 5
BASIC_SOUND = 1