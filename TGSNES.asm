
;macros.. DMA
;copying from ROM to Work RAM quickly:

.MACRO LoadRAM ARGS SOURCEPTR, DESTPTR, SIZE
   LDX #SOURCEPTR                   ;Get lower 16-bits of source ptr
   STX $4302                        ;Set source offset
   LDA #:SOURCEPTR                  ;Get upper 8-bits of source ptr
   STA $4304                        ;Set source bank
   LDX #SIZE                        ;
   STX $4305                        ;Set transfer size in bytes
   LDX #DESTPTR                     ;Get lower 16-bits of destination ptr
   STX $2181                        ;Set WRAM offset
   LDA #:DESTPTR                    ;Get upper 8-bits of dest ptr 
   STA $2183                        ;Set WRAM bank (only LSB is significant)
   LDA #$80
   STA $4301                        ;DMA destination is $2180
   LDA #$01                         ;DMA transfer mode=auto increment
   STA $4300                        ;  Write mode=1 byte to $2180
   STA $420B                        ;Initiate transfer using channel 0
   .endm

   
   

; SNES Initialization Tutorial code
; This code is in the public domain.

.include "TGHeader.inc"
.include "TGSnes_Init.asm"

; Needed to satisfy interrupt definition in "Header.inc".

.bank 0 slot 0
.org 0
.section "Vblank"
;--------------------------------------
VBlank:
rti ; RTI 	Return from interrupt
;--------------------------------------
.ends




.bank 0  slot 0
.org 0
.section "MainCode"

Start:
 ; Initialize the SNES.
 Snes_Init

 
 rep #%00010000  ;16 bit xy  - clears the x flag - 16 bit index register size.      REP 	Reset status bits (for example REP #%00100000 clears the M flag)
sep #%00100000  ;8 bit ab - sets the m flag - 8 bit accumulator register size.        SEP 	Set status bits (for example SEP #%00010000 sets the X flag)


;accumulator 8 or 16 bit - operand or result - A
;index registers 8 or 16 bit - reference memory, pass data to mem, loop counters -  X, Y

;stack pointer ? bit - points to next unused loc on stack - S
;data bank register - default bank for mem transfer - DBR
;d
;pbr
;p - status flags


; LDA #4096 loads value 4096 into A. won't recognise odd digit values, so 05 instead of 5
; LDA #$01 loads value 0x01 into A
; LDA $018000 loads value of address $018000
; 4096 = $1000
; sep #%00100000 binary



;See this? We take every byte from the palette, and put it to CGRAM
;there are 8 bytes in UntitledPalette = 4 colours. 15 bit colour in snes. 2 bytes per colour.
;If an address is used as the operand, then we refer to it as direct page addressing(2 hex digit), absolute addressing(4), or long addressing(6),
;Also, .b, .w, and .l at the end of the opcode can indicate direct page, absolute, and long addressing

ldx #$0000 ; puts value 0 in x?                    #$ = ????                              LDX 	Load register X from memory
- lda UntitledPalette.l,x ; puts value found in pallette.l+x into a?                       LDA 	Load accumulator from memory
sta $2122 ; puts a into 2122, which adds to ppu and auto increments                                                               STA 	Store accumulator in memory
inx ; INX 	Increment X register
cpx #8 ; CPX 	Compare register X with memory - the n, z, c flags are set by this - negative, zero==, carry = less than (so are these less than and more than?)
bne - ; BNE 	Branch if not equal (Z=0) , The bne - branches to the nearest "-" backwards. 

; $2122 is an 8-bit memory-mapped register for colour data, the data of which is 16-bits;
; each write to it is handled internally by the PPU unit on the system. There's no "direct" 16-bit equivalent.
;If you were to write a 16-bit value to $2122, the LSB would end up going into $2122, and the MSB would end up going into $2123 which is a completely different register (window mask setting).


;I'll explain this later
;We'll have two palettes, only one color is needed for the second:
;lda #33		;The color we need is the 33rd
;sta $2121
;lda.l Palette2
;sta $2122
;lda.l Palette2+1
;sta $2122





;example that copies tile data into VRAM at whatever address is currently set in $2116:

;LDX #TILEOFFSET ;Source Offset into source bank
;STX $4302       ;Set Source address lower 16-bits
;LDA #TILEBANK   ;Source bank
;STA $4304       ;Set Source address upper 8-bits
;LDX #TILESIZE   ;# of bytes to copy (16k)
;STX $4305       ;Set DMA transfer size
;LDA #$18        ;$2118 is the destination, so - Note that only 8-bits of the destination can be set; thus the destination MUST be in the $2100-$21FF range.
;STA $4301       ;  set lower 8-bits of destination to $18
;LDA #$01        ;Set DMA transfer mode: auto address increment
;STA $4300       ;  using write mode 1 (meaning write a word to $2118/$2119)
;LDA #$01        ;The registers we've been setting are for channel 0 - start the transfer by writing a 1 to the bit in $420B that corresponds to the channel. 
;STA $420B       ;  so Start DMA transfer on channel 0 (LSB of $420B) - 

;$2100-$21FF 	PPU1, APU, hardware registers
;https://en.wikibooks.org/wiki/Super_NES_Programming/SNES_memory_map


ldx #UntitledData   ; LOWER 16 bits     Address - this is a value?? not address??
lda #:UntitledData  ; UPPER 8 bits .....of UntitledData
ldy #(15*16*30)      ; length of data 15 * 16 * 2 ???
stx $4302           ; write. i guess 4303 = other 8 bits, as 16 bit number in X
sta $4304           ; address
sty $4305           ; and length

lda #%00000001      ; set this mode (transferring words)
sta $4300;

lda #$18            ; $211[89]: VRAM data write
sta $4301           ; set destination

ldy #$0000          ; Write to VRAM from $0000
sty $2116

lda #%00000001      ; start DMA, channel 0
sta $420B




lda #%10000000	; VRAM writing mode
sta $2115
ldx #$4000	    ; write to vram
stx $2116       ; from $4000


;2105 - SCREEN mode register

;The main data area is the VRAM, a 64 KB memory space that can be accessed with registers $2115, $2116, $2118, and $2139. 
;This area is used for storing all the tiles used in your game, as well as the tile maps. 
;2115 = 
;2116 = 
;stx $2118; = adding to tile map. but where? and how do i delete?
;2139 = 



;ugly code starts here - it writes the # shape I mentioned before.
.rept 2
   ;X|X|X

     ldx #$0000 ; tile 0 ( )
     stx $2118
     ldx #$0083 ; tile 1 (|)
     stx $2118

   ldx #$0084
   stx $2118
   ;first line finished, add BG's
   .rept 27
     stx $2118  ; X=0
   .endr
   ;beginning of 2nd line
   ;-+-+-
   .rept 2
     ldx #$0004 ; tile 4 (-)
     stx $2118
     ldx #$0006 ; tile 6 (+)
     stx $2118
   .endr
   ldx #$0004   ; tile 4 (-)
   stx $2118
   ldx #$0000
   .rept 27
     stx $2118
   .endr
.endr
.rept 2
  ldx #$0000    ; tile 0 ( )
  stx $2118
  ldx #$0002    ; tile 2 (|)
  stx $2118
.endr



;set up the screen
lda #%00110000  ; 16x16 tiles, mode 0
sta $2105       ; screen mode register
lda #%01000000  ; data starts from $4000
sta $2107       ; for BG1
lda #%01100000  ; and $6000
sta $2108       ; for BG2

stz $210B	    ; BG1 and BG2 use the $0000 tiles

lda #%00000011  ; enable bg1 and 2
sta $212C

;The PPU doesn't process the top line, so we scroll down 1 line.
rep #$20        ; 16bit a
lda #$07FF      ; this is -1 for BG1
sep #$20        ; 8bit a
sta $210E       ; BG1 vert scroll
xba
sta $210E

rep #$20        ; 16bit a
lda #$FFFF      ; this is -1 for BG2
sep #$20        ; 8bit a
sta $2110       ; BG2 vert scroll
xba
sta $2110

lda #%00001111  ; enable screen, set brightness to 15
sta $2100

lda #%10000001  ; enable NMI and joypads
sta $4200



 ; Loop forever.
Forever:
 jmp Forever

.ends
 
 
 
 .bank 1 slot 0       ; We'll use bank 1
.org 0
.section "Tiledata"
.include "tgspritesheetmaster2bit.inc"
.ends

