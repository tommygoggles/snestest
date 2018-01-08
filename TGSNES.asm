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

; #$ - 
; #
; $



;See this? We take every byte from the palette, and put it to CGRAM
ldx #$0000 ; #$ = ????       LDX 	Load register X from memory
- lda UntitledPalette.l,x ; LDA 	Load accumulator from memory
sta $2122 ; STA 	Store accumulator in memory
inx ; INX 	Increment X register
cpx #8 ; CPX 	Compare register X with memory, (are we comparing register x with 8? i guess so?, then branching? what is z??)
bne - ; BNE 	Branch if not equal (Z=0) , The bne - branches to the nearest "-" backwards. 

;I'll explain this later
;We'll have two palettes, only one color is needed for the second:
;lda #33		;The color we need is the 33rd
;sta $2121
;lda.l Palette2
;sta $2122
;lda.l Palette2+1
;sta $2122



ldx #UntitledData   ; Address
lda #:UntitledData  ; of UntitledData
ldy #(15*16*30)      ; length of data 15 * 16 * 2 ???
stx $4302           ; write
sta $4304           ; address
sty $4305           ; and length
lda #%00000001      ; set this mode (transferring words)
sta $4300
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

;ugly code starts here - it writes the # shape I mentioned before.
.rept 2
   ;X|X|X

     ldx #$00f0 ; tile 0 ( )
     stx $2118
     ldx #$00f1 ; tile 1 (|)
     stx $2118

   ldx #$00f2
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
.include "tgspritesheetmaster2.pcx.inc"
.ends

