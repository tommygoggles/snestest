
;VRAM, 64 KB, accessed with registers $2115, $2116, $2118, and $2139.
;	Stores tiles, tile maps.
;OAM, 544 Bytes, accessed with 
;	sprite properties. 4 bytes per sprite * 128 sprites = 512 bytes. +32 bytes for ??? 2 more bits info??
;CGRAM, 512 bytes, accessed with 
;	= 256 colours (15-bit) - other bit unused??


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


;To put tile in VRAM, use Register $2118/$2119: VRAM data write (2b/W). auto incremented if register $2115 says so
;To read it back, use register 2139
;To set where to put it in VRAM, or where to read VRAM back, use register 2116 (remember to dummy read)





