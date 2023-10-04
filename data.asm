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
; [py] ${RoutineA} // 0x100 == ${RoutineEnd} // 0x100
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
    
; otherwise unused 11 bytes; hide some routines in here!
jmpwordb_sleep15:
    sta WSYNC
    jsr sleep15
    
jmpwordb:
    jmp (WORD_B)
    
; loads A <- 0
; takes 15 cycles to execute
sleep15 ; this is actually a useful routine rarely
    lda RESERVED_ZERO ; 3
    rts
    
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
TopOutGracePeriod
    rts
    
RoutineEnd

    ; more grace period
    hex 40
    hex 38
    hex 30
    hex 2C
    hex 16

LevelExplosionAddTime:
    hex 6a
    hex 40
    hex 3a
    hex 34
    hex 30
    hex 20
    
; [py] syms["NextP1StrobeTable"] % 0x100 <= 0x100 - 3
NextP1StrobeTable: ; length 7, overlaps with RNG8Table
    hex 04
    hex 00
RNG8Table
    ; oops, we're biased a bit toward some blocks over others. Alas...
    hex 01
LevelColours
    hex 01
    hex 05
    hex 06
    hex 02
    hex 03
    hex 07
    hex 05

    MAC DigAlt
    dv.b #(({1} << 0) | ({2} << 2) | ({3} << 4))
    ENDM

DigitsAlt
    hex 49 ;DigAlt 1, 1, 1 ; 1
    hex 18 ;DigAlt 0, 3, 0 ; 2
    hex 00 ;DigAlt 0, 0, 0 ; 3
    hex 46 ;DigAlt 3, 0, 1 ; 4
    hex 03 ;DigAlt 3, 0, 0 ; 5
LevelExplosionsRequiredToAdvance:
    hex 2B ;DigAlt 2, 5, 0 ; F
    
    ; actually, we start each level with LEVEL clears already.

    hex 2f
    hex 39
    hex 2b
    hex F1

LevelRowInterval:
    hex FF
    hex 90
    hex 60
    hex 30
    hex 20
    
DigitElements
    hex 07
    hex 01
    hex 01
    
    hex 07
    hex 04
    
    hex 07
    hex 05
    hex 05