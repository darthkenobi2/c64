// My version implementing the same functionality
// Pontus "Bacchus" Berg
// 2026-JAN-12

// Quirks: Logo scrolls out of the window
// but if allowed to turn inside, the turn is jerky

// Memory map
// 0800 - Code (ends around 0b00)
// 1000 - music (relocated to be more memory efficient)
// 3000 - Logo - Paded with spaces to make code easier
// 4400 - Screen for char and bitmap (ends at 47ff)
// 5000 - sprite chars (ends around 5700)
// 5c00 - copy of the colourmap
// 6000 - font (ends around 6250)
// 6f18 - bitmap (ends at 7f3f)

// Should be possible to fit ino the low bank
// Basic bootstrap $0800 - $0900
// Sprites $0900 - $1000
// Another music $1000 - $2000
// Font $2000 - $2200
// Code $2200 -
// Screen $2400 - $27ff
// Logo (space padded) $2800 - $2c00
// Bitmap $2f18 - $3f40
// Colourmap $4000 - $43e8

// Code, logo and music doesn't need to be in this bank
// Placed such only to be compact

#undef test   		//#undef if you want to shut off the border indication
#define playmusic 	//#undef to mute music

.var music = LoadSid("MSX/Armalyte1000.sid")
.var screen1 = $4400
.var logoheight = 12  	      // Define the height of the top logo
.var ScrollCharStart = $e9    // Leftmost position ($01e9)
.var ScrollCharOffset = $35   // Distance between sprites


*=$3000 "Depacked LOGO"  // Pad the logo with spaces
#import "gfx/logodepacker.asm"

*=$4400 + (logoheight * 40) "Screen"
.import c64 "gfx/background5.koa",($1f40+(logoheight*40)),($03e8-(logoheight*40))

*=$5000 "Sprites"
#import "gfx/sprites.asm"

*=$5c00 "ColMem copy - LOGO"
.fill logoheight*40,$09   	// Set white + multicolour mode

*=$5c00 + (logoheight * 40) "Colmem copy - Background"
.import c64 "gfx/background5.koa",($2328+(logoheight*40)),($03e8-(logoheight*40))

*=$6000 "Charset"
.import binary "gfx/darth kenobi - Chars2.bin" // Trimmed to only needed

*=$6000 + (logoheight*40*8) "Koala bitmap" 
.import c64 "gfx/background5.koa",$0000+(logoheight*40*8),$1f40-(logoheight*40*8)

*=$0801 "Main code"

BasicUpstart2(start)
start:
	
	sei
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda #$01
	sta $d01a

	lda $dc0d
	lda $dd0d
	lsr $d019

	// Init irq
	jsr setirq1
	
	// Setup the gfx stuff
	jsr setgfx
	jsr copygfx

	// Init the music
	lda #music.startSong-1
  	jsr music.init

	cli
endloop:	
	#if test
	inc $d020
	#endif
	jmp endloop

// ========================================================================
// GFX init routines
// Mind: We don't need to set the values that are set inside the IRQ
// ========================================================================

setgfx:	
	// Set VIC bank
	lda #%10
	sta $dd00

	// Prepare sprite parameters
	lda #$ff
	sta $d015  //00011111 for first 7 sprites turned on
	sta $d017 //sprite expand vertically
	sta $d01d //sprite expand horizontally
	sta $d01c //multicolour

	// Set the Y positions for the sprites
	ldx #$10
!: 	lda #$b1
	sta $d001,x
	dex
	dex
	bpl !-

	ldx #$0a
!:	lda coltab,x 
	sta $d025,x
	dex 
	bpl !-
	rts

coltab: 	.byte $00,$01,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d 	// Colours

// ========================================================================
// Copy the logo to the screen
// Copy the colourmap to the Colour memory

copygfx:	ldx #$00
!: 	lda $4000,x
	sta $4400,x

	lda $40e0,x
	sta $44e0,x

	inx
	bne !-

	// Colour memory for both Logo and Bitmap

	ldx #$00
!:	lda $5c00,x 
	sta $d800,x 
	lda $5d00,x 
	sta $d900,x 
	lda $5e00,x 
	sta $da00,x 
	lda $5f00,x 
	sta $db00,x
	dex 
	bne !-

	// Clear the spritepointers

	ldx #$07
	lda #$40
!:	sta $47f8,x
	dex 
	bpl !-	

	rts

// ========================================================================
// Setup the IRQ
// Also the IRQ routines

setirq1:	lda #<irq1
	sta $0314
	lda #>irq1
	sta $0315

	// create raster interrupt at line 0
	lda #$00
	sta $d012
	rts

setirq2:	lda #<irq2
	sta $0314
	lda #>irq2
	sta $0315

	// create raster interrupt at line 146
	lda #$92
	sta $d012
	rts

irq1:	

	lda #$d0
	ora softscroll  	
 	sta $d016
	lda #$1b //char mode
	sta $d011
	lda #$18
	sta $d018
	lda #$00
	sta $d020
	sta $d021

	#if test
	  inc $d020
	#endif 

	#if playmusic
	jsr music.play
	#endif

	#if test
	  inc $d020
	#endif 

	jsr scroll

	#if test
	  lda #$00
	  sta $d020
	#endif 

	jsr setirq2
	lsr $d019
	jmp $ea81

irq2:	nop
	nop
	nop
	nop
	nop
	lda #$3b //bitmap mode on (bottom half)
	ldx #$df
	ldy #$18
	sta $d011 
	stx $d016 //set scroll to 7
	sty $d018
	lda #$06
	sta $d020
	sta $d021

	jsr movelogo

	jsr setirq1

	lsr $d019
	jmp $ea81

// ========================================================================
// Routines for the sprite scroller

SpriteX:	.byte ScrollCharStart+ScrollCharOffset 	// The "soft scroll"

.const scrolltx = screen1+$03f8  	// The current chars on screen

hibit:	.byte $00

textcnt:	.byte 0

scroll:	dec SpriteX
	lda SpriteX
	cmp #ScrollCharStart
	bne plotsprites

	// Big move - "coarse scroll"

	lda #ScrollCharOffset+ScrollCharStart
	sta SpriteX

	// Update the sprite pointers / current chars on screen
	ldx #$00
!:	lda scrolltx+1,x
	sta scrolltx,x 
	inx
	cpx #$07
	bne !-

	// Get next character in the scroller
	// Max 256 bytes scroll text "lda (textpoint),y" for longer text
	inc textcnt
	ldx textcnt
!:	lda scrolltext,x 
	bne !+

	// End of scrolltext - reset pointer and read again
	ldx #$00
	stx textcnt
	jmp !-

!:	cmp #$20 	// If space then use $1b
	bne !+
	lda #$40
!:	sta scrolltx+7

	// Set the Sprite X positions - including the MSB in $D010

plotsprites:	ldx #$00
	ldy #$00
	lda SpriteX
NextSprite:	sta $d000,x
	clc
	adc #ScrollCharOffset
	bcc !+

	sty savey+1

!:	inx
	inx 
	iny
	cpy #$08
	bne NextSprite

savey:	ldy #$00
	iny
	lda hibittab,y
	sta $d010

	// Position 0 on the left still leaves the expanded sprite visible
	// so we must have MSB set and a high value
	// Also, positions $f9 to $ff doesn't exist.
	// So if MSB is set for sprite 0, one should deduct 8 from the position

	lda $d000
	bpl !+
	lda $d010
	ora #$01
	sta $d010

	lda $d000
	sec 
	sbc #$08
	sta $d000

!:  rts

hibittab:	.byte %11111111
	.byte %11111110
	.byte %11111100
	.byte %11111000
	.byte %11110000
	.byte %11100000
	.byte %11000000
	.byte %10000000
	.byte %00000000
	.byte %00000000 	// Padding

.encoding "petscii_mixed"

scrolltext:	.text " this is the first ever crack by darth kenobi  "
	.text "an amateur effort but i had to start somewhere  this is my first ever assembly coding attempt  "
	.text "thanks to bacchus of fairlight for helping me with tips   i have used the music from the game armalyte "
	.byte 0	

// ========================================================================
// Routnes relate to shifting the screen

.const LogoRowDest00 = $4400+0*$28
.const LogoRowDest01 = $4400+1*$28
.const LogoRowDest02 = $4400+2*$28
.const LogoRowDest03 = $4400+3*$28
.const LogoRowDest04 = $4400+4*$28
.const LogoRowDest05 = $4400+5*$28
.const LogoRowDest06 = $4400+6*$28
.const LogoRowDest07 = $4400+7*$28
.const LogoRowDest08 = $4400+8*$28
.const LogoRowDest09 = $4400+9*$28
.const LogoRowDest10 = $4400+10*$28
.const LogoRowDest11 = $4400+11*$28

softscroll:
	.byte $3

logodirection: //0 is left, i is right
	.byte 1

currentchar: 	
	.byte 40

movelogo:
	lda logodirection
	beq decdirection

incdirection: 	// Direction 1

	dec softscroll
	beq !+
	rts

!:	lda #$07
	sta softscroll
	inc currentchar
	lda currentchar
	cmp #80
	beq !+

	jmp populaterow

!:	lda #$00 
	sta softscroll
	lda #$00
	sta logodirection
	rts

decdirection: // Direction 0	

	inc softscroll
	lda softscroll
	cmp #$08
	beq !+
	rts

!:	lda #$00 
	sta softscroll
	dec currentchar
	lda currentchar
	cmp #$01
	beq !+

	jmp populaterow

!:	lda #$07 		// It was the last char so, restore change direction
	sta softscroll
	lda #$01
	sta logodirection
	rts

populaterow:
	ldx currentchar
	ldy #$00
!:	lda LogoRowSource00,x
	sta LogoRowDest00,y
	lda LogoRowSource01,x
	sta LogoRowDest01,y
	lda LogoRowSource02,x
	sta LogoRowDest02,y
	lda LogoRowSource03,x
	sta LogoRowDest03,y
	lda LogoRowSource04,x
	sta LogoRowDest04,y
	lda LogoRowSource05,x
	sta LogoRowDest05,y
	lda LogoRowSource06,x
	sta LogoRowDest06,y
	lda LogoRowSource07,x
	sta LogoRowDest07,y
	lda LogoRowSource08,x
	sta LogoRowDest08,y
	lda LogoRowSource09,x
	sta LogoRowDest09,y
	lda LogoRowSource10,x
	sta LogoRowDest10,y
	lda LogoRowSource11,x
	sta LogoRowDest11,y
	inx 
	iny
	cpy #$28
	bne !-
	rts

// ========================================================================
// Music: Armalyte relocated to $1000

*=music.location "Music"
  .fill music.size, music.getData(i)

