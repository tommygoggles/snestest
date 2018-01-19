;https://en.wikibooks.org/wiki/Super_NES_Programming/SNES_Hardware_Registers

;Register $2105: Screen mode register (1b/W) - sets screen mode, and 8*8 or 16*16 pixel tile size for each of the up to 4 BGs. also priority.
;but how does 16x16 pixel tile mode work? how are they stored? like sprites?


;VRAM, 64 KB, accessed with registers $2115, $2116, $2118, and $2139.
;	Stores tiles AND tile maps.
;OAM, 544 Bytes, accessed with 
;	sprite properties. (4 bytes per sprite + 2 bits) * 128 sprites = 512 bytes + 32 bytes
;CGRAM, 512 bytes, accessed with 2121, 2122
;	palettes = 256 colours (15-bit) - other bit unused??


;TILE
One2bit4colour8by8tile: ;a T - in colour 01, with background colour 10 at the top, and 00 at the bottom. underlined with colour 11.
    	    .db ;#%01111110, ;(=7E)line 1, least significant bit
		;#%10000001, ;(=81) line 1, most significant bit
		;#%00011000, ;(=18) line 2, least
		;#%11100111,
		;#%00011000,
		;#%11100111,
		;#%00011000,
		;#%11100111,
		;#%00011000,
		;#%00000000,
		;#%00011000,
		;#%00000000,
		;#%00011000,
		;#%00000000,
		;#%11111111,
		;#%11111111
		; - if 4 or 8 bit colour palette, follow this with
		;line 1, even more significant bit in a 4 or 8 bit colour tile
		;line 2, etc.

		; =
		$7e, $81, $18, $e7, $18, $e7, $18, $e7, $18, $00, $18, $00, $18, $00, $FF, $FF

One4bit16colour8by8tile:;colour ramp part 1
	.db ;00000000 - $00 - for ramp part 2, change this to all 1's ($FF)
		;00001111 - $0F
		;repeat x 7
		
		;00110011 - $33
		;01010101 - $55
		;repeat * 7
		$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F, $33,$55,$33,$55,$33,$55,$33,$55,$33,$55,$33,$55,$33,$55,$33,$55
;and the next tile.. ramp part 2.. why not...?
		$FF,$0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF,$0F, $33,$55,$33,$55,$33,$55,$33,$55,$33,$55,$33,$55,$33,$55,$33,$55

RedRampPalette16colour:
	.db $00,$00, $00,$02, $00,$04, $00,$06, $00,$08, $00,$0A, $00,$0C, $00,$0E, 
	    $00,$10, $00,$12, $00,$14, $00,$16, $00,$18, $00,$1A, $00,$1C, $00,$1F, 

		
;To put tile in VRAM, use Register $2118/$2119: VRAM data write (2b/W). auto incremented if register $2115 says so
;To read it back, use register 2139
;To set where to put it in VRAM, or where to read VRAM back, use register 2116 - takes word address, *2 for byte address (remember to dummy read)

;can use DMA to do this


;Register $2107-$210A: Tile map location (4*1B/W) - tile map address and size (32*32 = 1024, 32*64 = 2048, 64*32 = 2048, 64*64 = 4096)
;each of these 1024, 2048, or 4096 tiles has 2 bytes in the tile map (hflip, vflip, palette, priority, tile number relative to that given in Register $210B/$210C for that BG?)
; guess this is one for each BG? ..... address_of_character = (base_location_bits << 13) + (8 * color_depth * character_number);
;. 8*2-bit = 16bytes , * character number
;2k, 4k, or 8k memory (/64k) used for a tile map then?

;The number formed by the 3 palette bits is multiplied by the number of colors in the BG to get the starting index in the color palette.
; palette 010 (2) in 4 colour mode = starts at 8?

;PALETTE
;  To upload palette entries to CGRAM, select the color with register $2121 (word addr), and then begin sending the bytes to register $2122. 

RedGreenBlueWhitePalette: ;least significant byte first
    .db $1F,$00,     $E0,$03,     $00,$7c,    $FF,$7F
	;transparent colour?????
	

;SPRITES
;All SNES sprites are 16 colors. (4 bit) 32 bytes per 8x8 tile.
;bits 5-7 of register 2101 set which 2 sizes of sprite are on. 8,16,32,64
; base selection bits of 2101 are bits 0-2 (there are name bits too?)
;bytes 0-511 (enough for 16 8*8 tiles) = a row of the full sprite, made of 1,2,4, or 8 8x8 tiles. meaning you can fit 16,8,4, or 2 of these in a row.
;the other rows follow this

;in OAM- which size, x coord(9bit), y coord(8), start tile num, palette num, vflip, hflip, priority
;in 2101, bits 0-2. 011 shift left by 14. so 011 = 0 11000000 00000000, or 00C000. so choices are 0000 4000 8000 C000. added = TILE NUMBER 000-1FF, * 32 = 
; 1 11111111 11111 = 111111 11111111 = 3FFF, so 0-3FFF, 4000-7FFF, 8000 - BFFF, C000-FFFF etc. everything covered in 64k vram.

;do a visual representation of memory? start points, what goes to what, possible sizes?
 
