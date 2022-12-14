.SEGMENT "CODE_MAIN"

SCREEN_ADDR = $00
STRING_PTR = $03
IS_SCROLLING = $05
SCROLL_VALUE = $06
CURRENT_KB_COL = $07
CURRENT_KB_ROW = $08
KEY_WAS_FOUND = $09
LAST_KB_BIT = $0A
LAST_PRESSED_KEY = $0B
MOTOR_STATUS = $0C

HELLO_STR:
.ASCIIZ "        (To the left)"
HERE_STR:
.ASCIIZ "LOOK AROUND YOU "

ENTRY:
;Turn off interrupts and decimal mode
    SEI
    CLD
    LDX #$FF
    TXS

;Initialize PPU state (disable interrupts and rendering)
    LDA #$00
    STA $2000
    STA $2001

;Wait for three vblanks (let PPU state settle?)
    BIT $2002
ppu_vblank_wait1:
    BIT $2002
    BPL ppu_vblank_wait1

    BIT $2002
ppu_vblank_wait2:
    BIT $2002
    BPL ppu_vblank_wait2

    BIT $2002
ppu_vblank_wait3:
    BIT $2002
    BPL ppu_vblank_wait3

;Wait for a vblank
:
    BIT $2002
    BPL :-

;Set background palette values
;Universal background = black
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006
    LDA #$20
    STA $2007

    LDA #$3F
    STA $2006
    LDA #$01
    STA $2006
    LDA #$20
    STA $2007

    LDA #$3F
    STA $2006
    LDA #$02
    STA $2006
    LDA #$1D
    STA $2007
    
    LDA #$3F
    STA $2006
    LDA #$03
    STA $2006
    LDA #$1D
    STA $2007

;Fill first attribute table to all be palette zero
    LDX #64
WRITE_ATTR_TOP:
    LDA #$23
    STA $2006
    TXA
    CLC
    ADC #$BF
    STA $2006
    LDA #$00
    STA $2007
    DEX
    BNE WRITE_ATTR_TOP

;Set MMC3 to map text characters into the first bank
    LDA #$00
    STA $8000
    LDA #$5E
    STA $8001

;Set MMC3 to map the second bank to some usable garbage
    LDA #$01
    STA $8000
    LDA #$31
    STA $8001

;Set MMC3 to map the third bank to some even more garbage garbage
    LDA #$02
    STA $8000
    LDA #$15
    STA $8001

;Wait for VBLANK
:
    BIT $2002
    BPL :-

;Fill the screen with blank tiles
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006
    LDX #$3C
    LDA #$7E
:
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    STA $2007
    DEX
    BNE :-

;Set scroll to 0, 0
    LDA #$00
    STA $2005
    STA $2005
    STA SCROLL_VALUE

;Initialize print location to 1,1 on the playfield
    LDA #$21
    STA SCREEN_ADDR
    LDA #$20
    STA SCREEN_ADDR+1

;When we first start, we do not yet need to scroll
    LDA #$00
    STA IS_SCROLLING

;Indicate that we haven't yet clocked in any bits from the keyboard
    LDA #$FF
    STA LAST_KB_BIT
    LDA #$00
    STA CURRENT_KB_COL
    STA CURRENT_KB_ROW

;Enable rendering
    LDA #$0E
    STA $2001
 
;Set MMC3 to vertical mirroring
    LDA #$00
    STA $A000

    LDA #<HELLO_STR
    STA STRING_PTR
    LDA #>HELLO_STR
    STA STRING_PTR+1
    JSR PRINTSTR
 
    LDA #$0A
    JSR PRNTCHR

    LDA #$0D
    JSR PRNTCHR

;Map MMC3 A000 bank to mask rom area 0
    LDA #$07
    STA $8000
    LDA #$00
    STA $8001

;Map MMC3 C000 bank to FDC reset register memory area
    LDA #$46
    STA $8000
    LDA #$20
    STA $8001

;Make sure MMC3 SRAM protection is off
    LDA #$80
    STA $A001

    JSR INITKEYBOARD

;Use 1-bit register to reset the FDC
    STA $6000 ;Turn off reset signal
    STA $6000 ;Turn reset signal back on
    LDX #$10
FDC_RESET_WAIT_LOOP:
    DEX
    BNE FDC_RESET_WAIT_LOOP
    STA $6000 ;Turn reset signal back off

LDA #4
STA MOTOR_STATUS

HLTLOOP:
    JSR GETKEY
    JSR PRNTCHR
    LDA MOTOR_STATUS
    STA $6802
    EOR #$10
    STA MOTOR_STATUS

;TEST LOOP WRITING TO FDC ADDRESS area
    JSR WAIT_FDC_CMD_READY
    LDA #$80 ;Send 'get version command'
    JSR WAIT_FDC_CMD_READY
    LDA $6804
    PHA
    LDX #8
BIT_SHIFT_LOOP:
    PLA
    TAY
    AND #$80
    BEQ BIT_IS_ZERO
    TYA
    PHA
    TXA
    PHA
    LDA #'1'
    CLC
    BCC PRINT_BIT
BIT_IS_ZERO:
    TYA
    PHA
    TXA
    PHA
    LDA #'0'
PRINT_BIT:
    JSR PRNTCHR
    PLA
    TAX
    DEX
    BEQ PRINT_FDC_REG_DONE
    PLA
    ASL
    PHA
    CLC
    BCC BIT_SHIFT_LOOP

PRINT_FDC_REG_DONE:
    LDA #$0D
    JSR PRNTCHR
    LDA #$0A
    JSR PRNTCHR

    CLC
    BCC HLTLOOP

WAIT_FDC_CMD_READY:
    LDA $6804
    AND #80
    BEQ WAIT_FDC_CMD_READY
    RTS

INITKEYBOARD:
    ;Loop over each column and check the 7th row to see if we are at column 1
KB_INIT_COLUMN_LOOP:
    LDA #$01
    STA $4016
    LDA $4016
    LDA #$00
    STA $4016
    LDY #$06
KB_ROW_SKIP_LOOP:
    LDA $4016
    DEY
    BNE KB_ROW_SKIP_LOOP
    AND #$01
    BNE KB_INIT_COLUMN_LOOP
KB_COL_ONE_FOUND:
    RTS

GETKEY:
    LDA #0
    STA KEY_WAS_FOUND
    LDX #0
NEXT_KB_COLUMN_LOOP:
    LDY #6
    LDA #01
CLK_KB_BIT_LOOP:
    STA $4016
    LDA $4016
    STA LAST_KB_BIT
    LDA KEY_WAS_FOUND
    BNE CONTINUE_KB_BIT_LOOP
    LDA LAST_KB_BIT
    AND #$01
    BNE CONTINUE_KB_BIT_LOOP
    LDA KEY_LUT,X
    PHA 
    LDA #$01
    STA KEY_WAS_FOUND
    CLC
    BCC CONTINUE_KB_BIT_LOOP
DO_ANOTHER_INNER_KB_LOOP:
    LDA #$00
    CLC
    BCC CLK_KB_BIT_LOOP
CONTINUE_KB_BIT_LOOP:
    INX
    DEY
    BNE DO_ANOTHER_INNER_KB_LOOP
    TXA
    CMP #48
    BNE NEXT_KB_COLUMN_LOOP
    LDA KEY_WAS_FOUND
    BNE RETURN_STACK_KEY_VALUE
    LDA #$FF
    STA LAST_PRESSED_KEY
    CLC
    BCC GETKEY
RETURN_STACK_KEY_VALUE:
    PLA
    CMP LAST_PRESSED_KEY
    BEQ GETKEY
    STA LAST_PRESSED_KEY
    RTS

;PRINT CHARACTER SUBROUTINE
;Wait for VBLANK
PRNTCHR:
:

    BIT $2002
    BPL :-

    ;Look up the tile number of this character, print nothing if it was zero
    TAX
    LDA CHR_LUT,X
    BEQ CHECK_CTL

    ;Non-zero tile number found, write it to the PPU and increment the cursor
    LDY SCREEN_ADDR+1
    STY $2006
    LDX SCREEN_ADDR
    STX $2006
    STA $2007

    ;Character was successfully printed, so advance the cursor
STEP_CHARACTER:
    INX
    STX SCREEN_ADDR
    BNE CHECK_END_OF_SREEN
    INY
    STY SCREEN_ADDR+1
CHECK_END_OF_SREEN:
    TXA
    AND #$1F
    CMP #$1F
    BNE PRNTCHR_END
STEP_OVER_MARGIN:
    INX
    BNE KEEP_ON_STEPPIN
    INY
    STY SCREEN_ADDR+1
KEEP_ON_STEPPIN:   
    INX
    STX SCREEN_ADDR
DO_WRAP_SCROLL:
    TXA
    AND #$E0
    JSR CHECK_AND_SCROLL
    CLC
    BCC PRNTCHR_END

CHECK_CTL:
    TXA
    CMP #$0D
    BNE CHECK_LINEFEED
    LDA SCREEN_ADDR
    AND #$E0
    TAX
    INX
    TXA
    STA SCREEN_ADDR
    CLC
    BCC PRNTCHR_END

CHECK_LINEFEED:
    CMP #$0A
    BNE PRNTCHR_END
    LDA SCREEN_ADDR
    CLC
    ADC #$20
    STA SCREEN_ADDR
    AND #$E0
    BNE DO_LF_SCROLL
    LDX SCREEN_ADDR+1
    INX
    STX SCREEN_ADDR+1

DO_LF_SCROLL:
    JSR CHECK_AND_SCROLL

PRNTCHR_END:
    LDA #$00
    STA $2005
    LDA SCROLL_VALUE
    STA $2005

    RTS

;PRINT STRING SUBROUTINE
PRINTSTR:
    LDY #$00
:
    LDA (STRING_PTR),Y
    BEQ PRINTSTR_END
    TAX
    TYA
    PHA
    TXA
    JSR PRNTCHR
    PLA
    TAY
    INY
    CLC
    BCC :-
PRINTSTR_END:
    RTS

;SUBROUTINE FOR SOFTWARE LINE SCROLLING
CHECK_AND_SCROLL:
    TAX
    LDA SCREEN_ADDR+1
    CMP #$23
    BNE CHECK_SCROLL_FLAG
    TXA
    CMP #$C0
    BNE CHECK_AT_BOTTOM

    ;If we are at the end of the nametable, wrap to the beginning
    LDA SCREEN_ADDR
    AND #$1F
    STA SCREEN_ADDR
    LDA #$20
    STA SCREEN_ADDR+1
    LDA #$01
    CLC
    BCC CHECK_SCROLL_FLAG

CHECK_AT_BOTTOM:
    CMP #$A0
    BNE CHECK_SCROLL_FLAG
    STA IS_SCROLLING
CHECK_SCROLL_FLAG:
    LDA IS_SCROLLING
    BEQ PRNTCHR_END

DO_SCREEN_SCROLL:
    LDA SCREEN_ADDR+1
    STA $2006
    LDA SCREEN_ADDR
    AND #$E0
    STA $2006
    LDX #$20
    LDA #$7D
LINE_CLEAR_TOP:
    STA $2007
    DEX
    BNE LINE_CLEAR_TOP

    LDA SCROLL_VALUE
    CLC
    ADC #$08
    CMP #$F0
    BEQ WRAP_SCROLL
    STA SCROLL_VALUE

    RTS

WRAP_SCROLL:
    LDA #$00
    STA SCROLL_VALUE

    RTS

.SEGMENT "KEY_LUT"
KEY_LUT:
.BYTE ';'
.BYTE 'L'
.BYTE 'K'
.BYTE 'J'
.BYTE 'H'
.BYTE $20
.BYTE 'P'
.BYTE 'O'
.BYTE 'I'
.BYTE 'U'
.BYTE 'Y'
.BYTE $0D
.BYTE '0'
.BYTE '9'
.BYTE '8'
.BYTE '7'
.BYTE '6'
.BYTE $00
.BYTE 'Z'
.BYTE 'X'
.BYTE 'C'
.BYTE 'V'
.BYTE 'B'
.BYTE $00
.BYTE 'A'
.BYTE 'S'
.BYTE 'D'
.BYTE 'F'
.BYTE 'G'
.BYTE $00
.BYTE 'Q'
.BYTE 'W'
.BYTE 'E'
.BYTE 'R'
.BYTE 'T'
.BYTE $00
.BYTE '1'
.BYTE '2'
.BYTE '3'
.BYTE '4'
.BYTE '5'
.BYTE $00
.BYTE '/'
.BYTE '>'
.BYTE '<'
.BYTE 'M'
.BYTE 'N'
.BYTE '+'

.SEGMENT "CHR_LUT"
CHR_LUT:
.BYTE $00 ; 0x0 
.BYTE $00 ; 0x1
.BYTE $00 ; 0x2
.BYTE $00 ; 0x3
.BYTE $00 ; 0x4
.BYTE $00 ; 0x5
.BYTE $00 ; 0x6
.BYTE $00 ; 0x7
.BYTE $00 ; 0x8
.BYTE $00 ; 0x9
.BYTE $00 ; 0xa
.BYTE $00 ; 0xb
.BYTE $00 ; 0xc
.BYTE $00 ; 0xd
.BYTE $00 ; 0xe
.BYTE $00 ; 0xf
.BYTE $00 ; 0x10
.BYTE $00 ; 0x11
.BYTE $00 ; 0x12
.BYTE $00 ; 0x13
.BYTE $00 ; 0x14
.BYTE $00 ; 0x15
.BYTE $00 ; 0x16
.BYTE $00 ; 0x17
.BYTE $00 ; 0x18
.BYTE $00 ; 0x19
.BYTE $00 ; 0x1a
.BYTE $00 ; 0x1b
.BYTE $00 ; 0x1c
.BYTE $00 ; 0x1d
.BYTE $00 ; 0x1e
.BYTE $00 ; 0x1f
.BYTE $7D ; 0x20
.BYTE $6A ; 0x21
.BYTE $BD ; 0x22
.BYTE $27 ; 0x23
.BYTE $6C ; 0x24
.BYTE $E8 ; 0x25
.BYTE $67 ; 0x26
.BYTE $2B ; 0x27
.BYTE $64 ; 0x28
.BYTE $66 ; 0x29
.BYTE $7B ; 0x2a
.BYTE $8F ; 0x2b
.BYTE $1A ; 0x2c
.BYTE $65 ; 0x2d
.BYTE $69 ; 0x2e
.BYTE $7A ; 0x2f
.BYTE $70 ; 0x30
.BYTE $71 ; 0x31
.BYTE $72 ; 0x32
.BYTE $73 ; 0x33
.BYTE $74 ; 0x34
.BYTE $75 ; 0x35
.BYTE $76 ; 0x36
.BYTE $77 ; 0x37
.BYTE $78 ; 0x38
.BYTE $79 ; 0x39
.BYTE $F7 ; 0x3a
.BYTE $DC ; 0x3b
.BYTE $2E ; 0x3c
.BYTE $C7 ; 0x3d
.BYTE $2F ; 0x3e
.BYTE $6B ; 0x3f
.BYTE $6D ; 0x40
.BYTE $30 ; 0x41
.BYTE $31 ; 0x42
.BYTE $32 ; 0x43
.BYTE $33 ; 0x44
.BYTE $34 ; 0x45
.BYTE $35 ; 0x46
.BYTE $36 ; 0x47
.BYTE $37 ; 0x48
.BYTE $38 ; 0x49
.BYTE $39 ; 0x4a
.BYTE $3A ; 0x4b
.BYTE $3B ; 0x4c
.BYTE $3C ; 0x4d
.BYTE $3D ; 0x4e
.BYTE $3E ; 0x4f
.BYTE $3F ; 0x50
.BYTE $40 ; 0x51
.BYTE $41 ; 0x52
.BYTE $42 ; 0x53
.BYTE $43 ; 0x54
.BYTE $44 ; 0x55
.BYTE $45 ; 0x56
.BYTE $46 ; 0x57
.BYTE $47 ; 0x58
.BYTE $48 ; 0x59
.BYTE $49 ; 0x5a
.BYTE $98 ; 0x5b
.BYTE $E7 ; 0x5c
.BYTE $9A ; 0x5d
.BYTE $6E ; 0x5e
.BYTE $8C ; 0x5f
.BYTE $81 ; 0x60
.BYTE $50 ; 0x61
.BYTE $51 ; 0x62
.BYTE $52 ; 0x63
.BYTE $53 ; 0x64
.BYTE $54 ; 0x65
.BYTE $55 ; 0x66
.BYTE $56 ; 0x67
.BYTE $57 ; 0x68
.BYTE $58 ; 0x69
.BYTE $59 ; 0x6a
.BYTE $5A ; 0x6b
.BYTE $5B ; 0x6c
.BYTE $5C ; 0x6d
.BYTE $5D ; 0x6e
.BYTE $5E ; 0x6f
.BYTE $5F ; 0x70
.BYTE $4A ; 0x71
.BYTE $4B ; 0x72
.BYTE $4C ; 0x73
.BYTE $4D ; 0x74
.BYTE $4E ; 0x75
.BYTE $4F ; 0x76
.BYTE $01 ; 0x77
.BYTE $08 ; 0x78
.BYTE $0C ; 0x79
.BYTE $0F ; 0x7a
.BYTE $A0 ; 0x7b
.BYTE $5B ; 0x7c
.BYTE $A1 ; 0x7d
.BYTE $1C ; 0x7e

.SEGMENT "BLANK_AREA"
.REPEAT $E000
.BYTE $00
.ENDREP

.SEGMENT "VECTORS"
.WORD $0000
.WORD ENTRY
.WORD $0000