; bitwise pointer arithmetic may in the future happen here
; so this table musn't move.
; [py] ${LINE_OPCODE_LOOKUP_B} % 0x100 == 0
; [py] ${LINE_OPCODE_LOOKUP_A} == ${LINE_OPCODE_LOOKUP_B} + 4

LINE_OPCODE_LOOKUP_B
    hex 87 ; SAX
    hex 87 ; SAX
    hex 87 ; SAX
    hex 87 ; SAX [unused]
    
LINE_OPCODE_LOOKUP_A
    hex 87 ; SAX
    hex 86 ; STX
    hex 85 ; STA
    hex 84 ; STY
    
    hex 87 ; SAX [unused]
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

; these routines are copied wholesale to XRAM on init.
; [py] ${RoutineB} == ${RoutineA} + 32
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
    
    ; UNUSED: 11 bytes
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
    

; [py] syms["NextP1StrobeTable"] % 0x100 <= 0x100 - 3
NextP1StrobeTable:
    hex 06
    hex 00
    hex 01
    hex 00 ; [unused]
    hex 02
    hex 04
    hex 05