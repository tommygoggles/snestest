

One2bit4colour8by8tile: ;a T - in colour 01, with background colour 10 at the top, and 00 at the bottom. underlined with colour 11.
    .db #%01111110, ;line 1, least significant bit
		#%10000001, ;line 1, most significant bit
		#%00011000, ;line 2, least
		#%11100111,
		#%00011000,
		#%11100111,
		#%00011000,
		#%11100111,
		#%00011000,
		#%00000000,
		#%00011000,
		#%00000000,
		#%00011000,
		#%00000000,
		#%11111111,
		#%11111111
		;line 1, even more significant bit in a 4 or 8 bit colour tile
		;line 2, etc.
	;which = $7e, $81, $18, $e7, $18, $e7, $18, $e7, $18, $00, $18, $00, $18, $00, $FF, $FF
	