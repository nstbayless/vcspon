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
    
sleep15 ; this is actually a useful routine rarely
    SLEEP 3
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
    rts
    
RoutineEnd

LevelExplosionAddTime:
    hex 5a
    hex 40
    hex 3a
    hex 34
    hex 30
    hex 20

; actually, we start each level with LEVEL clears already.
LevelExplosionsRequiredToAdvance:
    hex 15
    hex 25
    hex 35
    hex 2b
    hex F1

LevelRowInterval:
    hex FF
    hex 90
    hex 60
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