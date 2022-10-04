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
FDC_VER = $0C

; cmon zero page usage
ADRESS = $0D
NUMBER = $0F

FDC_LO_BUFFER_ADDRESS = $10
FDC_HI_BUFFER_ADDRESS = $12
ST0_RESULT = $14
ST1_RESULT = $15
ST2_RESULT = $16
C_RESULT = $17
H_RESULT = $18
R_RESULT = $19
N_RESULT = $1A
SP_VALUE = $1B

SECTOR_READ_ATTEMPT_MSG: .ASCIIZ "Attempting to read a sector..."
FDC_INIT_FAIL_MSG: .ASCIIZ "Failed to init\r\n"
FDC_READ_FAIL_MSG: .ASCIIZ "Failed to read sector\r\n"
FDC_READ_SUCCESS_MSG: .ASCIIZ "Read sector!!\r\n"

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
    LDX #$20
FDC_RESET_WAIT_LOOP:
    DEX
    BNE FDC_RESET_WAIT_LOOP
    STA $6000 ;Turn reset signal back off

    LDA #$03
    PHA

ATTEMPT_READ_SECTOR:
    PLA
    TAX
    DEX
    BEQ READ_SECTOR_FAIL
    TXA
    PHA

    LDA #<SECTOR_READ_ATTEMPT_MSG
    STA STRING_PTR
    LDA #>SECTOR_READ_ATTEMPT_MSG
    STA STRING_PTR+1
    JSR PRINTSTR

    ;Read a sector
    JSR FDC_INIT
    CMP #$00
    BNE FDC_INIT_FAILURE

    JSR FDC_SELECT_DRIVE

    ;LDA #$00
    ;LDX #$00
    ;LDY #$00
    JSR FDC_READ_SECTOR
    CMP #$00
    BNE ATTEMPT_READ_SECTOR

    LDA #<FDC_READ_SUCCESS_MSG
    STA STRING_PTR
    LDA #>FDC_READ_SUCCESS_MSG
    STA STRING_PTR+1
    JSR PRINTSTR

    JMP init
    
READ_SECTOR_FAIL:
    LDA #<FDC_READ_FAIL_MSG
    STA STRING_PTR
    LDA #>FDC_READ_FAIL_MSG
    STA STRING_PTR+1
    JSR PRINTSTR

    JMP init

FDC_INIT_FAILURE:
    LDA #<FDC_INIT_FAIL_MSG
    STA STRING_PTR
    LDA #>FDC_INIT_FAIL_MSG
    STA STRING_PTR+1
    JSR PRINTSTR

    JMP init

FDC_INIT:
;INITIALIZE THE FDC
    LDA #$00
    STA $6802
    LDA #$0C
    STA $6802

    JSR WAIT_FDC_IRQ

    LDX #$04
SEND_NEXT_SENSE_INT_CMD:
    TXA
    PHA
    JSR FDC_SENSE_INTERRUPT
    PLA
    TAX
    DEX
    BNE SEND_NEXT_SENSE_INT_CMD

    ;Configure the floppy controller
    LDA #$13
    JSR SEND_FDC_COMMAND
    LDA #$00    ;Param 1 
    JSR SEND_FDC_COMMAND
    LDA #$47    ;Param 2 - implied seek en/fifo en/drive poll en/thresh
    JSR SEND_FDC_COMMAND
    LDA #$00    ;Param 3 - Precomp (use default)
    JSR SEND_FDC_COMMAND

    ;Set data rate to 500kbs
    LDA #$00
    STA $6807

    ;Use the SPECIFY command to configure the FDC operating mode
    LDA #$03
    JSR SEND_FDC_COMMAND
    LDA #$85
    JSR SEND_FDC_COMMAND
    LDA #$21
    JSR SEND_FDC_COMMAND

    JSR FDC_RECALIBRATE

    RTS

FDC_RECALIBRATE:
    JSR FDC_SELECT_DRIVE

    LDA #$10
    PHA
RECALIBRATE_LOOP:
    PLA
    TAX
    DEX
    BEQ RECALIBRATE_FAILED
    TXA
    PHA

    ;Do a recalibrate command
    LDA #$07
    JSR SEND_FDC_COMMAND
    LDA #$00
    JSR SEND_FDC_COMMAND

    JSR WAIT_FDC_IRQ

    ;Sense interrupt
    LDA #$08
    JSR SEND_FDC_COMMAND
    JSR READ_FDC_DATA
    JSR READ_FDC_DATA

    ;Check to see if we're at track 0
    LDA $6800
    AND #$10
    BNE RECALIBRATE_LOOP

    ;At track zero, return success
    PLA
    LDA #$00
    RTS

RECALIBRATE_FAILED:
    PLA
    LDA #$01
    RTS

FDC_SEEK_DRIVE:
    LDA #$0F
    JSR SEND_FDC_COMMAND
    LDA #$00
    JSR SEND_FDC_COMMAND
    LDA #00
    JSR SEND_FDC_COMMAND

    JSR WAIT_FDC_IRQ

    ;Sense interrupt
    LDA #$08
    JSR SEND_FDC_COMMAND
    JSR READ_FDC_DATA
    JSR READ_FDC_DATA

FDC_SELECT_DRIVE:
    ;FOR SHITS AND GIGGLES DO AN SEEK
    ;Select drive zero, motor zero on
    LDA #$14
    STA $6802

    ;Wait for drive motor to spin up
    LDX #$FF
WAIT_FDC_MOTOR:
    DEX
    BNE WAIT_FDC_MOTOR

    RTS

FDC_READ_SECTOR:
    ;Set up the low buffer address
    LDA #$00
    STA FDC_LO_BUFFER_ADDRESS
    LDA #$02
    STA FDC_LO_BUFFER_ADDRESS+1

    ;Set up the high buffer address
    LDA #$00
    STA FDC_HI_BUFFER_ADDRESS
    LDA #$03
    STA FDC_HI_BUFFER_ADDRESS+1

    ;READ A SECTOR MEBBE
    LDA #$46 ;Read sector command, MFM mode
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$00 ;Head 1, drive 0
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$00 ;Cylinder 0
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$00 ;Head 0
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$01 ;Sector 1
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$02 ;512 bytes/sector
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$02 ;Final sector of track
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$1B ;Gap3 number from the manual
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    LDA #$FF ;DTL (only used if sector address is 0)
    JSR SEND_FDC_COMMAND
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL_A

    JMP PROCEED_READ_SECTOR_DATA

FDC_READ_SECTOR_FAIL_A:
    JMP FDC_READ_SECTOR_FAIL

PROCEED_READ_SECTOR_DATA:

    LDY #$00

READ_SECTOR_BYTE_LOOP_LO:
    JSR READ_FDC_DATA
    DEX 
    BEQ FDC_READ_SECTOR_FAIL

    STA (FDC_LO_BUFFER_ADDRESS),Y
    INY
    BNE READ_SECTOR_BYTE_LOOP_LO

 READ_SECTOR_BYTE_LOOP_HI:
    JSR READ_FDC_DATA
    DEX 
    BEQ FDC_READ_SECTOR_FAIL

    STA (FDC_HI_BUFFER_ADDRESS),Y
    INY
    BNE READ_SECTOR_BYTE_LOOP_HI

    JSR READ_FDC_DATA ;ST0
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA ST0_RESULT

    JSR READ_FDC_DATA ;ST1
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA ST1_RESULT

    JSR READ_FDC_DATA ;ST2
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA ST2_RESULT

    JSR READ_FDC_DATA ;C
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA C_RESULT

    JSR READ_FDC_DATA ;H
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA H_RESULT

    JSR READ_FDC_DATA ;R
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA R_RESULT

    JSR READ_FDC_DATA ;N
    CMP #$00
    BNE FDC_READ_SECTOR_FAIL
    STA N_RESULT

    LDA #$00
    RTS

FDC_READ_SECTOR_FAIL:
    LDA #$01
    RTS

FDC_SENSE_INTERRUPT:
    LDA #$08
    JSR SEND_FDC_COMMAND
    JSR READ_FDC_DATA
    JSR READ_FDC_DATA
    RTS

;SEND COMMAND TO FLOPPY CONTROLLER SUBROUTINE
SEND_FDC_COMMAND:
    PHA
    TXA
    PHA
    LDA #$10
    PHA
NEXT_FDC_WRITE_COUNTDOWN_LOOP:
    PLA
    TAX
    DEX
    BEQ SEND_FDC_COMMAND_FAIL
    TXA
    PHA
    LDX #$FF
SEND_FDC_CHECK:
    DEX
    BEQ NEXT_FDC_WRITE_COUNTDOWN_LOOP

    LDA $6804
    AND #$C0
    CMP #$80
    BNE SEND_FDC_CHECK
    PLA ;Outer counter value
    PLA
    TAX
    PLA
    STA $6805
    LDA #$00
    RTS

SEND_FDC_COMMAND_FAIL:
    PLA 
    TAX
    PLA 
    TSX ;TEMP
    STX SP_VALUE ;TEMP
    JMP init ;TEMP
    LDA #$01
    RTS

READ_FDC_DATA:
    LDA #$10
    PHA
NEXT_FDC_READ_COUNTDOWN_LOOP:
    PLA
    TAX
    DEX
    BEQ READ_FDC_DATA_FAIL
    TXA
    PHA
    LDX #$FF
READ_FDC_CHECK:
    DEX
    BEQ NEXT_FDC_READ_COUNTDOWN_LOOP

    LDA $6804
    AND #$C0
    CMP #$C0

    BNE READ_FDC_CHECK
    PLA
    LDA $6805
    LDX #$00
    RTS

READ_FDC_DATA_FAIL:
    TSX ;TEMP
    STX SP_VALUE ;TEMP
    JMP init ;TEMP
    LDX #$01
    RTS

WAIT_FDC_IRQ:
    ;;TEMPORARY LOBOTOMIZATION
    ;;LDA #$80
    ;;RTS
    ;;END TEMPORARY LOBOTOMIZATION
WAIT_FDC_IRQ_CHECK:
    LDA $6800
    AND #$80
    BEQ WAIT_FDC_IRQ_CHECK
    RTS

; C'mon, the Compact MONitor
; written by Bruce Clark and placed in the public domain
;
; minor tweaks and porting by Ed Spittles
;
; To the extent possible under law, the owners have waived all
; copyright and related or neighboring rights to this work. 
;
; retrieved from http://www.lowkey.comuf.com/cmon.htm
; archived documentation at http://biged.github.io/6502-website-archives/lowkey.comuf.com/cmon.htm
;
; ported to ca65 from dev65 assembler
; /opt/cc65/bin/ca65 --listing -DSINGLESTEP cmon.a65 
; /opt/cc65/bin/ld65 -t none -o cmon.bin cmon.o
;
; define SINGLESTEP to include the single-stepping plugin
;     (modified to display registers in AXYS order)
;
; ported to 6502 from 65Org16
; ported to a6502 emulator

.feature labels_without_colons

WIDTH  = 4         ;must be a power of 2
HEIGHT = 20

.macro putc
       TAX
       TYA
       PHA
       PHP
       TXA
       PHA
       JSR PRNTCHR
       PLA
       TAX
       PLP 
       PLA
       TAY
       TXA
.endmacro

.macro getc
       TYA
       PHA
       PHP
       JSR GETKEY
       TAX
       PLP
       PLA
       TAY
       TXA
.endmacro

.ifdef SINGLESTEP
AREG   = 4
PREG   = 5
SREG   = 6
XREG   = 7
YREG   = 8
STBUF  = 9 ;uses 9 bytes
.endif

DUMP_BYTE:
       LDY #$00
       LDA (ADRESS),Y
       JSR OUTHEX
       INC ADRESS
       BNE M2
       INC ADRESS+1
       JMP M2

init:
.ifdef SINGLESTEP
       TSX
       STX SREG
       PHP
       PLA
       STA PREG
.endif

MON    CLD
M1     JSR OUTCR
       LDA #$2D       ;output dash prompt
       putc
M2     LDA #0
       STA NUMBER+1
       STA NUMBER
M3     AND #$0F
M4     LDY #4         ;accumulate digit
M5     ASL NUMBER
       ROL NUMBER+1
       DEY
       BNE M5
       ORA NUMBER
       STA NUMBER
M6     getc
       CMP #$0D
       BEQ M1         ;branch if cr
;
; Insert additional commands for characters (e.g. control characters)
; outside the range $20 (space) to $7E (tilde) here
;

       CMP #$20       ;don't output if outside $20-$7E
       BCC M6
       CMP #$7F
       BCS M6
       putc
       CMP #$2C
       BEQ COMMA
       CMP #'+'
       BEQ AT
       CMP #'/'
       BEQ DUMP_BYTE
;
; Insert additional commands for non-letter characters (or case-sensitive
; letters) here
;
.ifdef SINGLESTEP
       CMP #$24		; $ is single step
       BNE NSSTEP
       JMP SSTEP
NSSTEP
.endif

; now dealing with letters
       EOR #$30
       CMP #$0A
       BCC M4         ;branch if digit
       ORA #$20       ;convert to upper case
       SBC #$77
;
; mapping:
;   A-F -> $FFFA-$FFFF
;   G-O -> $0000-$0008
;   P-Z -> $FFE9-$FFF3
;
       BEQ GO
       CMP #$FA ; #$FA or #$FFFA
       BCS M3
;
; Insert additional commands for (case-insensitive) letters here
;

       CMP #$F1 ; #$F1 or  #$FFF1
       BNE M6
DUMP   JSR OUTCR
       TYA
       PHA
       CLC            ;output address
       ADC NUMBER
       PHA
       LDA #0
       ADC NUMBER+1
       JSR OUTHEX
       PLA
       JSR OUTHSP
D1     LDA (NUMBER),Y ;output hex bytes
       JSR OUTHSP
       INY
       TYA
       AND #WIDTH-1
       BNE D1
       PLA
       TAY
D2     LDA (NUMBER),Y ;output characters
       AND #$7F
       CMP #$20
       BCC D3
       CMP #$7F
       BCC D4
D3     EOR #$40
D4     putc
       INY
       TYA
       AND #WIDTH-1
       BNE D2
       CPY #WIDTH*HEIGHT
       BCC DUMP
M2J
       JMP M2		; branches out of range for 6502 when putc is 3 bytes
COMMA  LDA NUMBER
       STA (ADRESS),Y
       INC ADRESS
       BNE M2J
       INC ADRESS+1
       BCS M2J
AT     LDA NUMBER
       STA ADRESS
       LDA NUMBER+1
       STA ADRESS+1
       BCS M2J
GO     JSR G1
       JMP M2		; returning after a 'go'
G1     JMP (NUMBER)
OUTHEX ;JSR OH1		; for 16-bit bytes
OH1    JSR OH2
OH2    ASL
       ADC #0
       ASL
       ADC #0
       ASL
       ADC #0
       ASL
       ADC #0
       PHA
       AND #$0F
       CMP #$0A
       BCC OH3
       ADC #$66
OH3    EOR #$30
       putc
       PLA
       RTS
OUTHSP JSR OUTHEX
       LDA #$20
OA1    putc
       RTS
OUTCR  LDA #$0D
       putc
       LDA #$0A
       BNE OA1        ;always


.ifdef SINGLESTEP
SSTEP  LDX #7
STEP1  LDA STEP4,X
       STA STBUF+1,X
       DEX
       BPL STEP1
       LDX SREG
       TXS
       LDA (ADRESS),Y
       BEQ STBRK
       JSR GETLEN
       TYA
       PHA
STEP2  LDA (ADRESS),Y
       STA STBUF,Y
       DEY
       BPL STEP2
       EOR #$20
       CMP #1
       PLA
       JSR STADR
       LDA STBUF
       CMP #$20
       BEQ STJSR
       CMP #$4C
       BEQ STJMP
       CMP #$40
       BEQ STRTI
       CMP #$60
       BEQ STRTS
       CMP #$6C
       BEQ STJMPI
       AND #$1F
       CMP #$10
       BNE STEP3
       LDA #4
       STA STBUF+1
STEP3  LDA PREG
       PHA
       LDA AREG
       LDX XREG
       LDY YREG
       PLP
       JMP STBUF
STEP4  NOP
       NOP
       JMP STNB
       JMP STBR
STJSR  LDA ADRESS+1
       PHA
       LDA ADRESS
       PHA        ;fall thru
STJMP  LDY STBUF+1
       LDA STBUF+2
STJMP1 STY ADRESS
STJMP2 STA ADRESS+1
       JMP STNB1
STJMPI INY
       LDA (STBUF+1),Y
       STA ADRESS
       INY
       LDA (STBUF+1),Y
       JMP STJMP2
STRTI  PLA
       STA PREG
       PLA
       STA ADRESS
       PLA
       JMP STJMP2
STRTS  PLA
       STA ADRESS
       PLA
       STA ADRESS+1
       LDA #0
       JSR STADR
       JMP STNB1
STBRK  LDA ADRESS+1
       PHA
       LDA ADRESS
       PHA
       LDA PREG
       PHA
       ORA #$04 ; set i flag
       AND #$F7 ; clear d flag
       STA PREG
       LDY a:-2 ; $FFFFFFFE
       LDA a:-1 ; $FFFFFFFF
       JMP STJMP1
STNB   PHP
       STA AREG
       STX XREG
       STY YREG
       PLA
       STA PREG
       CLD
STNB1  TSX
       STX SREG
STNB2  JSR STOUT
       JMP M2
STBR   DEC ADRESS+1
       LDY #-1  ; #$FFFF
       LDA (ADRESS),Y
       BMI STBR1
       INC ADRESS+1
STBR1  CLC
       JSR STADR
       JMP STNB2
STADR  ADC ADRESS
       STA ADRESS
       BCC STADR1
       INC ADRESS+1
STADR1 RTS
OUTPC  LDA ADRESS+1
       JSR OUTHEX
       LDA ADRESS
       JMP OUTHSP
STOUT  JSR OUTCR
       JSR OUTPC ; fall thru
OUTREG LDA AREG
       JSR OUTHSP
       LDA XREG
       JSR OUTHSP
       LDA YREG
       JSR OUTHSP
       LDA SREG
       JSR OUTHSP
       LDA PREG
       JSR OUTHSP
       LDA PREG   ;fall thru
OUTBIN SEC
       ROL
OUTB1  PHA
       LDA #$18
       ROL
       putc
       PLA
       ASL
       BNE OUTB1
       RTS
;
;    0123456789ABCDEF
;
; 00 22...22.121..33.
; 10 22...22.13...33.
; 20 32..222.121.333.
; 30 22...22.13...33.
; 40 12...22.121.333.
; 50 22...22.13...33.
; 60 12...22.121.333.
; 70 22...22.13...33.
; 80 .2..222.1.1.333.
; 90 22..222.131..3..
; A0 222.222.121.333.
; B0 22..222.131.333.
; C0 22..222.121.333.
; D0 22...22.13...33.
; E0 22..222.121.333.
; F0 22...22.13...33.
;
; Return instruction length - 1 (note that BRK is considered to be a 2 byte
; instruction and returns 1)
;
GETLEN LDY #1
       CMP #$20  ; if opcode = $20, then length = 3
       BEQ GETL3
       AND #$DF
       CMP #$40
       BEQ GETL1 ; if (opcode & $DF) = $40, then length = 1
       AND #$1F
       CMP #$19
       BEQ GETL3 ; if (opcode & $1F) = $19, then length = 3
       AND #$0D
       CMP #$08
       BNE GETL2 ; if (opcode & $0D) = $08, then length = 1
GETL1  DEY
GETL2  CMP #$0C
       BCC GETL4 ; if (opcode & $0D) >= $0C, then length = 3
GETL3  INY
GETL4  RTS

BREAK  STA AREG
       STX XREG
       STY YREG
       PLA
       STA PREG
       PLA
       STA ADRESS
       PLA
       STA ADRESS+1
       TSX
       STX SREG
       CLD
       JSR STOUT
       JMP M1
.endif

Lnmi:
        .byte 1,2
 
Lreset:
        .word init

Lirqbrk:
.ifdef SINGLESTEP
        .word BREAK
.else
        .byte 5,6
.endif

Lend:


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

PRINTSTR:
    TXA
    PHA
    TYA
    PHA
    LDY #$00
PRINTSTR_LOOP:
    LDA (STRING_PTR),Y
    TAX
    BEQ PRINTSTR_DONE
    INY
    BEQ PRINTSTR_DONE
    TYA
    PHA
    TXA
    JSR PRNTCHR
    PLA
    TAY
    CLC
    BCC PRINTSTR_LOOP
PRINTSTR_DONE:
    PLA
    TAY
    PLA
    TAX
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
.BYTE '.'
.BYTE ','
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

.SEGMENT "VECTORS"
.WORD $0000
.WORD ENTRY
.WORD $0000