.const KOALA_TEMPLATE = "C64FILE, Bitmap=$0000, ScreenRam=$1f40, ColorRam=$2328, BackgroundColor = $2710"
.var picture = LoadBinary("background5.koa", KOALA_TEMPLATE) //better make sure its 320x100 in size



*=$4c00;            .fill picture.getScreenRamSize(), picture.getScreenRam(i) //was $0c00 but we set to $0400
*=$5c00 "bitmap colour" ; colorRam:  .fill picture.getColorRamSize(), picture.getColorRam(i) //was $1c00
*=$6000 "bitmap";            .fill picture.getBitmapSize(), picture.getBitmap(i) //was $2000
*=$6000 "Charset"
.import binary "darth kenobi - Chars.bin" 
*=$d800 "Charset Attrib" 
.import binary "darth kenobi - CharAttribs_L1.bin"
*=$4000 "LOGO" 
.import binary "darth kenobi - Map (8bpc, 40x12).bin"
*=$b000 "main program"
BasicUpstart2(start)
.var music = LoadSid("Armalyte.sid")
start:
	
	lda #music.startSong-1
  jsr music.init
	jsr $e544
	txs
	// disable interrupts
	sei
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda #$01
	sta $d01a
	lda #%10
	sta $dd00

	lda #$3b
	ldx #$d7
	ldy #$38 //$18 for $4c00 screen or $18 for $4400 screen
	sta $d011 //00011011 loaded in bits 3,4,5

	stx $d016 //00001111 loaded in bit 3 plus 7 for scroll


	sty $d018 //00010100 loaded in bits 2,4
ldx #0

.for (var scrn=$4000; scrn<$41e1; scrn++) { //copy $4c00 onto $4400
           lda scrn
           sta scrn+$400
           sta scrn+$c00
       }

.for (var scr=$4de1; scr<$4fe7; scr++) { //copy bitmap colours from bottom half of screen $4c00 to screen $4400
           lda scr
           sta scr-$800
        }


jsr setup_sprites

lda #$7f
sta $d015  //00011111 for first 7 sprites turned on
sta $d017 //sprite expand vertically
sta $d01d //sprite expand horizontally
sta $d01c //multicolour

lda #$30
sta $d000 //D000 Sprite 0 X Pos
ldy #$b1
sty $d001 //D001 Sprite 0 Y Pos
lda #$62
sta $d002  //1
sty $d003
lda #$94
sta $d004 //2
sty $d005
lda #$c6
sta $d006 //3
sty $d007
lda #%01100000
sta $d010
lda #$f8
sta $d008 //4
sty $d009

lda #$2b
sta $d00a //5
sty $d00b
lda #$5d
sta $d00c //6
sty $d00d
//sta $d00e //7
//sta $d00f

lda #$0d //colour
sta $d027
sta $d028
sta $d029
sta $d02a
sta $d02b
sta $d02c
sta $d02d
//sta $d02e
lda #$00
sta $d025 //multicolour 1
lda #$01
sta $d026 //multicolour 2
lda #$40
sta $20 //hi byte for logo
lda #$44
sta $21 //hi byte for screen 1 at $4400
lda #$4c
sta $22 //hi byte for screen 2 at $4c00
lda #$14
sta $fd //logo_offset
lda #$00
sta $fe //logo_row
lda #$28
sta $fb //multiply by $28 (40) in multiplication routine
lda #$00
sta $bf //raster counter
lda #$00
sta $2a
lda #$d7
sta $bf //bounce
lda #$00
sta $23 //screen_one flag
lda #$07
sta $24 //store d016 scroll value






	// init irq
	lda #<irq
	sta $0314
	lda #>irq
	sta $0315


	// create raster interrupt at line 0
	lda #$00
	sta $d012

	// clear interrupts
	lda $dc0d
	lda $dd0d
	lda #$01
	sta $d019
	cli



loopy:
	jsr scroller
        .for (var l=0; l<750; l++) {
           nop
        }

   !:jmp loopy


scrolltext:
	.text "this is the first ever crack by darth kenobi  "
	.text "an amateur effort but i had to start somewhere  this is my first ever assembly coding attempt  "
	.text "thanks to bacchus of fairlight for helping me with tips   i have used the music from the game armalyte "
	.byte 0


multiply:
lda #$02
cmp #$00
beq !+++++++++
dec $02
lda $02
sta $25
lda #$00
ror $fb
bcc !+
adc $25
!: ror //b1
ror $fb
bcc !+
adc $25
!: ror //b2
ror $fb
bcc !+
adc $25
!: ror //b3
ror $fb
bcc !+
adc $25
!: ror //b4
ror $fb
bcc !+
adc $25
!: ror //b5
ror $fb
bcc !+
adc $25
!: ror //b6
ror $fb
bcc !+
adc $25
!: ror //b7
ror $fb
bcc !+
adc $25
!: ror
sta $fc
ror $fb
inc $02
rts
!: lda $02
sta $fb //hi byte
sta $fc //lo byte
rts


shift_screen:

			lda $d018
			and #%00100000
			cmp #$00
			bne !+
			lda #$38
			sta $d018
			jmp !++
			!:lda #$18
			sta $d018
			

!: rts


setup_sprites:
ldx #00
.for (var setup=0; setup<7; setup++) {
lda scrolltext,x
cmp #00 //is it @
bne !+ //if not @
ldx #00 //if it is @
lda scrolltext,x
clc
adc #63
jmp !+++
!: cmp #32 //is it space
bne !+ //not space
lda #90 //it is space //#(((26)*64)+4096)/64
jmp !++
clc
!: adc #63 //lda #(((char code-1)*64)+4096)/64
!: sta $47f8+setup
 sta $4ff8+setup
 inx
}
dex
rts            

scroller:
.for (var pickle=0; pickle<7; pickle++) { 
lda $d010
and #pow(2,pickle) //just sprite 0
cmp #$00 //is it left side?
beq !+++
!: lda $d000+pickle*2 //hit 255 or $ff ?
cmp #$00
bne !++++++
!: lda #$ff
sta $d000+pickle*2
lda $d010
and #(255-pow(2,pickle)) //clear right side flag
sta $d010
jmp !+++++
!: lda $d000+pickle*2 //left side
cmp #$00 //hit zero?
bne !++++
lda $d010 //add right side flag
ora #pow(2,pickle)
sta $d010
clc
lda $d000+mod((pickle*2)+12,14)
adc #$32
sta $d000+pickle*2
inx
lda scrolltext,x
cmp #00 //is it @
bne !+ //if not @
ldx #00 //if it is @
lda scrolltext,x
clc
adc #63
jmp !+++
!: cmp #32 //is it space
bne !+ //not space
lda #90 //it is space //#(((26)*64)+4096)/64
jmp !++
clc
!: adc #63 //#(((char code-1)*64)+4096)/64
!: sta $47f8+pickle
 sta $4ff8+pickle
!: dec $d000+pickle*2
}
rts




irq:
	lda $bf
sta $d016
	lda #$1b //bitmap mode off
	sta $d011
	lda #$00
	sta $d020
	sta $d021

				//LEFT BORDER
				lda $fd
				cmp #$00 //left border
				bne !++++
				lda $2a
				cmp #$00 //moving left
				bne !+
				lda $d016 //scroll = 0
				cmp #$d0
				bne !+++
				lda #$01 //change to moving right
				sta $2a
				jmp !++++++++++++
				!: lda $d016 //moving right
				cmp #$d7 //scroll = 7
				bne !+
				jsr shift_screen
				inc $fd
				inc $fd
				inc $fd
				lda #$d0
				sta $d016
				jmp !+++++++++++
				!: inc $d016 //scroll not 0
				jmp !++++++++++
				!: dec $d016 //scroll
				jmp !+++++++++
				//RIGHT BORDER
				!: lda $fd
				cmp #$28 //right border
				bne !++++
				lda $2a
				cmp #$01 //moving right
				bne !+
				lda $d016 //scroll = 7
				cmp #$d7
				bne !+++
				lda #$00 //change to moving left
				sta $2a
				jmp !++++++++
				!: lda $d016 //moving left
				cmp #$d0 //scroll = 0
				bne !+
				jsr shift_screen
				dec $fd
				dec $fd
				dec $fd
				lda #$d7
				sta $d016
				jmp !+++++++
				!: dec $d016 //scroll not 0
				jmp !++++++
				!: inc $d016 //scroll
				jmp !+++++
				//REGULAR
				!: lda $2a
				cmp #$00
				bne !+
				lda $d016
				and #%00000111
				cmp #%00000000
				bne !++
				lda #$d7 //if moving left and scroll = 0, THEN scroll = 7, shift screen
				sta $d016
				jsr shift_screen
				dec $fd
				jmp !++++
				!: lda $d016
				and #%00000111
				cmp #%00000111
				bne !+
				lda #$d0
				sta $d016
				jsr shift_screen
				inc $fd
				jmp !+++
				!: lda $2a
				cmp #$00
				bne !+
				dec $d016 //moving left and scroll not 0 THEN screen one, dec $d016
				lda #$01
				sta $23
				lda $d016
				sta $24
				jmp !++
				!: inc $d016
				lda #$01
				sta $23
				lda $d016
				sta $24

		

        
!: //EXIT
jsr music.play
lda $d016
sta $bf
!:

lda $d012
cmp #146
bne !-
.for (var i=0; i<16; i++) nop //16 good
lda #$01
sta $d019
lda #$df
sta $d016
lda #$3b //bitmap mode on
sta $d011
lda #$06
	sta $d020
   sta $d021 
   lda $23
   cmp #$01
   bne !+
jsr screen_one
lda #$00
sta $23
!: lda $24
cmp #$00
bne !+
.for (var i=0; i<8; i++) nop
cmp #$07
bne !+
.for (var i=0; i<8; i++) nop
!: jmp $ea81







plus44:
//plus
						!: ldy #$00
						lda #$28
						sta $02
						lda $fe
						sta $fb
						jsr multiply
						lda $20
						clc
						adc $fc
						sta $24
						lda $fb
						sta $23
						lda $21
						clc
						adc $fc
						sta $b1
						lda $fb
						sec
						sbc #$14
						sta $b0
						lda $b1
						sbc #$00 //if carry flag cleared
						sta $b1 //update hi byte
						clc
						lda $b0
						adc $fd
						sta $b0
						lda $b1
						adc #$00 //if carry flag set
						sta $b1 //update hi byte
						!: lda ($23),y
						sta ($b0),y
						iny
						lda #$28 //determine logo length - only copy visible part of logo
						clc
						adc #$14
						sec
						sbc $fd
						sta $25
						cpy $25
						bne !-
						ldy #$00
						lda $21
						clc
						adc $fc
						sta $b1
						lda $fb
						sta $b0
						lda $fd //determine logo length - only copy visible part of logo
						sec
						sbc #$14
						sta $25
						!: lda #32
						sta ($b0),y
						iny
						cpy $25
						bne !-
						ldy #00
						inc $fe
						lda $fe
						cmp $52 //compare against logo_row +1
						bne !---
						lda #00
						sta $fe
			      !: rts

plus4c:
						//plus
						
						!: ldy #$00
						lda #$28
						sta $02
						lda $fe
						sta $fb
						jsr multiply
						lda $20
						clc
						adc $fc
						sta $24
						lda $fb
						sta $23
						lda $22
						clc
						adc $fc
						sta $b1
						lda $fb
						sec
						sbc #$14
						sta $b0
						lda $b1
						sbc #$00 //if carry flag cleared
						sta $b1 //update hi byte
						clc
						lda $b0
						adc $fd
						sta $b0
						lda $b1
						adc #$00 //if carry flag set
						sta $b1 //update hi byte
						!: lda ($23),y
						sta ($b0),y
						iny
						lda #$28 //determine logo length - only copy visible part of logo
						clc
						adc #$14
						sec
						sbc $fd
						sta $25
						cpy $25
						bne !-
						ldy #$00
						lda $22
						clc
						adc $fc
						sta $b1
						lda $fb
						sta $b0
						lda $fd //determine logo length - only copy visible part of logo
						sec
						sbc #$14
						sta $25
						!: lda #32
						sta ($b0),y
						iny
						cpy $25
						bne !-
						ldy #00
						inc $fe
						lda $fe
						cmp $52
						bne !---
						lda #00
						sta $fe
			      !: rts

screen_one:
						lda $24
						and #%00000111 //last 3 bits
						asl //multiply by 2
						sta $52
						lda $2a
						cmp #$00
						beq !+
						lda $52
						sec
						sbc #$02
						sta $52
						!: lda $52
						sec
						sbc #$02 //subtract 2
						sta $fe //now we have logo_row
						

						!: lda $d018
						and #%00100000
						cmp #%00100000
						beq !+
						jmp screen2 //next screen $4c00
						!: ldy #00
						!: lda $fd
						sec
						sbc #$14 //logo_offset-$14
						cmp #$00
						beq !++
						bmi screen1minus //less than zero
						bpl !+ //more than zero
						!: jsr plus44
						rts
						!: lda #$28
						sta $02
						lda $fe
						sta $fb
						jsr multiply
						lda $20
						clc
						adc $fc
						sta $24
						lda $fb
						sta $23
						lda $21
						clc
						adc $fc
						sta $b1
						lda $fb
						sta $b0
						!:lda ($23),y						
						sta ($b0),y
						iny
						cpy #$28
						bne !-
						ldy #00
						inc $fe
						lda $fe
						cmp $52
						bne !--
						lda #$00
						sta $fe
						rts
						
screen1minus:						//minus
						!: ldy #$00
						lda #$28
						sta $02
						lda $fe
						sta $fb
						jsr multiply
						lda $20
						clc
						adc $fc
						sta $24
						lda $fb
						sta $23
						clc
						adc #$14
						sta $23
						lda $24
						adc #$00 //add zero in case carry flag is set
						sta $24 //now we have updated the hi byte
						lda $23
						sec
						sbc $fd
						sta $23
						lda $24
						sbc #$00 //subtract zero in case carry flag is cleared
						sta $24 //update hi byte again
						lda $21
						clc
						adc $fc
						sta $b1
						lda $fb
						sta $b0
						!: lda ($23),y
						sta ($b0),y
						iny
						lda #$28 //determine logo length - only copy visible part of logo
						clc
						adc $fd
						sec
						sbc #$14
						sta $25
						cpy $25
						bne !- //repeat until at the end of the row
						!: lda #32 //set blank char
						sta ($b0),y //set screen not taken up by logo as blank
						iny
						cpy #$28 //end of row
						bne !-
						ldy #00
						inc $fe //next row
						lda $fe
						cmp $52
						bne !--- //repeat
						lda #00
						sta $fe
						//jmp !++++
						
						!: rts
						
					

screen2:						//screen at $4c00 below this is the bit that flashes the bitmap
						!:ldy #00
						!: lda $fd
						sec
						sbc #$14 //logo_offset-$14
						cmp #$00
						beq !++
						bmi screen2minus //less than zero
						bpl !+ //more than zero
						!: jsr plus4c
						rts
						!: lda #$28
						sta $02
						lda $fe
						sta $fb
						jsr multiply
						lda $20
						clc
						adc $fc
						sta $24
						lda $fb
						sta $23
						lda $22
						clc
						adc $fc
						sta $b1
						lda $fb
						sta $b0
						!:lda ($23),y				
						sta ($b0),y
						iny
						cpy #$28
						bne !-
						ldy #00
						inc $fe
						lda $fe
						cmp $52
						bne !--
						lda #00
						sta $fe
						!: rts
						
screen2minus:						//minus
						!: ldy #$00
						lda #$28
						sta $02
						lda $fe
						sta $fb
						jsr multiply
						lda $20
						clc
						adc $fc
						sta $24
						lda $fb
						clc
						adc #$14
						sta $23
						lda $24
						adc #$00 //add zero in case carry flag is set
						sta $24 //now we have updated the hi byte
						lda $23
						sec
						sbc $fd
						sta $23
						lda $24
						sbc #$00 //subtract zero in case carry flag is cleared
						sta $24 //update hi byte again
						lda $22
						clc
						adc $fc
						sta $b1
						lda $fb
						sta $b0
						!: lda ($23),y
						sta ($b0),y
						iny
						lda #$28 //determine logo length - only copy visible part of logo
						clc
						adc $fd
						sec
						sbc #$14
						sta $25
						cpy $25
						bne !- //repeat until at the end of the row
						!: lda #32 //set blank char
						sta ($b0),y //set screen not taken up by logo as blank
						iny
						cpy #$28 //end of row
						bne !-
						ldy #00
						inc $fe //next row
						lda $fe
						cmp $52
						bne !--- //repeat
						lda #00
						sta $fe
						//jmp !++++
						
						!: rts






irq2:

	lda #$3b //bitmap mode on (bottom half)
	sta $d011 
	//lda #$df
	//sta $d016 //set scroll to 7
	lda #$06
	sta $d020
   sta $d021



	lda #<irq
	sta $0314
	lda #>irq
	sta $0315

	lda #$00
	sta $d012




lda #$01
	sta $d019
	jmp $ea81

*=music.location "Music"
  .fill music.size, music.getData(i)

*=$5000 "sprites" //(64) $40 * 27 = (1728) $06c0 so end is $56c0
//spritea:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$ff
.byte $e4,$0a,$92,$a4,$0a,$96,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$92,$a4,$01,$50,$54,$01,$50,$54,$00,$00,$00,$87

//spriteb:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$d4,$0b,$ff
.byte $54,$0a,$92,$90,$0a,$96,$a0,$0a,$92,$a0,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$aa,$94,$01,$50,$54,$01,$55,$50,$00,$00,$00,$87

spritec:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d0,$54,$0f,$d0,$54,$0b,$d0
.byte $00,$0a,$90,$00,$0a,$90,$00,$0a,$92,$a0,$0a,$92,$a0,$0a,$92,$a4
.byte $0a,$92,$a4,$02,$aa,$94,$01,$50,$54,$00,$55,$50,$00,$00,$00,$87

sprited:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$d3
.byte $e4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$aa,$94,$01,$50,$54,$01,$55,$50,$00,$00,$00,$87

spritee:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$f0,$0f,$c0,$00,$0f
.byte $d5,$54,$0f,$d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0b,$ff
.byte $00,$0a,$90,$00,$0a,$95,$40,$0a,$90,$00,$0a,$90,$00,$0a,$90,$00
.byte $0a,$90,$00,$0a,$aa,$a0,$01,$50,$00,$01,$55,$54,$00,$00,$00,$87

spritef:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$f0,$0f,$c0,$00,$0f
.byte $d5,$54,$0f,$d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0b,$ff
.byte $00,$0a,$90,$00,$0a,$95,$40,$0a,$90,$00,$0a,$90,$00,$0a,$90,$00
.byte $0a,$90,$00,$0a,$90,$00,$01,$50,$00,$01,$50,$00,$00,$00,$00,$87

spriteg:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d0,$54,$0f,$d0,$54,$0b,$d0
.byte $00,$0a,$9a,$a0,$0a,$92,$a0,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$02,$aa,$94,$01,$50,$54,$00,$55,$50,$00,$00,$00,$87

spriteh:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c3,$f0,$0f,$c3,$f0,$0f
.byte $d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$ff
.byte $e4,$0a,$92,$a4,$0a,$96,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$92,$a4,$01,$50,$54,$01,$50,$54,$00,$00,$00,$87

spritei:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$00,$00,$fc,$00,$00
.byte $fd,$40,$00,$fd,$00,$00,$fd,$00,$00,$fd,$00,$00,$fd,$00,$00,$bd
.byte $00,$00,$a9,$00,$00,$a9,$00,$00,$a9,$00,$00,$a9,$00,$00,$a9,$00
.byte $00,$a9,$00,$02,$aa,$00,$00,$15,$00,$00,$55,$40,$00,$00,$00,$87

spritej:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$f0,$00,$03,$f0,$00
.byte $03,$f4,$00,$03,$f4,$00,$03,$f4,$00,$03,$f4,$00,$03,$f4,$00,$03
.byte $e4,$0a,$82,$a4,$0a,$82,$a4,$0a,$92,$a4,$0a,$92,$a4,$02,$92,$94
.byte $02,$92,$94,$00,$aa,$50,$00,$50,$50,$00,$15,$40,$00,$00,$00,$87

spritek:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c3,$f0,$0f,$c3,$f0,$0f
.byte $d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$db,$d4,$0b,$ff
.byte $54,$0a,$a2,$90,$0a,$96,$a0,$0a,$96,$a0,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$92,$a4,$01,$50,$54,$01,$50,$54,$00,$00,$00,$87

spritel:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c0,$00,$0f,$c0,$00,$0f
.byte $d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0f,$d0,$00,$0b,$d0
.byte $00,$0a,$90,$00,$0a,$90,$00,$0a,$90,$00,$0a,$90,$00,$0a,$90,$00
.byte $0a,$90,$00,$0a,$aa,$a0,$01,$50,$00,$01,$55,$54,$00,$00,$00,$87

spritem:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$3f,$03,$f0,$3f,$cf,$f0,$3f
.byte $ff,$f4,$3f,$33,$f4,$3f,$57,$f4,$3f,$47,$f4,$3f,$43,$f4,$2f,$43
.byte $e4,$2a,$42,$a4,$2a,$42,$a4,$2a,$42,$a4,$2a,$42,$a4,$2a,$42,$a4
.byte $2a,$42,$a4,$2a,$42,$a4,$05,$40,$54,$05,$40,$54,$00,$00,$00,$87

spriten:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c3,$f0,$0f,$f3,$f0,$0f
.byte $ff,$f4,$0f,$cf,$f4,$0f,$d3,$f4,$0f,$d7,$f4,$0f,$d3,$f4,$0b,$d3
.byte $e4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$92,$a4,$01,$50,$54,$01,$50,$54,$00,$00,$00,$87

spriteo:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$d3
.byte $e4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$02,$aa,$94,$01,$50,$54,$00,$55,$50,$00,$00,$00,$87

spritep:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$ff
.byte $d4,$0a,$90,$14,$0a,$95,$50,$0a,$90,$00,$0a,$90,$00,$0a,$90,$00
.byte $0a,$90,$00,$0a,$90,$00,$01,$50,$00,$01,$50,$00,$00,$00,$00,$87

spriteq:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$d3
.byte $e4,$0a,$92,$a4,$0a,$92,$84,$0a,$92,$94,$0a,$92,$a4,$0a,$9a,$a0
.byte $0a,$9a,$a4,$02,$a2,$a4,$01,$51,$54,$00,$54,$54,$00,$00,$00,$87

spriter:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$ff
.byte $d4,$0a,$92,$54,$0a,$96,$90,$0a,$92,$a0,$0a,$92,$a0,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$92,$a4,$01,$50,$54,$01,$50,$54,$00,$00,$00,$87

sprites:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$ff,$c0,$0f,$c3,$f0,$0f
.byte $d7,$f0,$0f,$d3,$f4,$0f,$d0,$54,$0f,$d0,$54,$0f,$d0,$00,$03,$ff
.byte $c0,$01,$52,$a0,$00,$56,$a0,$00,$02,$a4,$0a,$82,$a4,$0a,$82,$a4
.byte $0a,$92,$a4,$02,$aa,$94,$01,$50,$54,$00,$55,$50,$00,$00,$00,$87

spritet:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$3f,$ff,$f0,$00,$fc,$00,$05
.byte $fd,$54,$00,$fd,$00,$00,$fd,$00,$00,$fd,$00,$00,$fd,$00,$00,$bd
.byte $00,$00,$a9,$00,$00,$a9,$00,$00,$a9,$00,$00,$a9,$00,$00,$a9,$00
.byte $00,$a9,$00,$00,$a9,$00,$00,$15,$00,$00,$15,$00,$00,$00,$00,$87

spriteu:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c3,$f0,$0f,$c3,$f0,$0f
.byte $d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$d3
.byte $e4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$02,$aa,$94,$01,$50,$54,$00,$55,$50,$00,$00,$00,$87

spritev:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c3,$f0,$0f,$c3,$f0,$0f
.byte $d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0b,$d3
.byte $e4,$0a,$92,$a4,$0a,$92,$a4,$0a,$92,$a4,$02,$92,$94,$02,$92,$94
.byte $00,$92,$50,$00,$aa,$50,$00,$10,$40,$00,$15,$40,$00,$00,$00,$87

spritew:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$3f,$03,$f0,$3f,$03,$f0,$3f
.byte $43,$f4,$3f,$43,$f4,$3f,$43,$f4,$3f,$43,$f4,$3f,$43,$f4,$2f,$43
.byte $e4,$2a,$42,$a4,$2a,$42,$a4,$2a,$42,$a4,$2a,$62,$a4,$2a,$aa,$a4
.byte $2a,$9a,$a4,$2a,$56,$a4,$05,$51,$54,$05,$40,$54,$00,$00,$00,$87

spritex:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c3,$f0,$0f,$c3,$f0,$0f
.byte $d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$0f,$d3,$f4,$03,$d3,$d4,$01,$ff
.byte $54,$02,$92,$90,$0a,$96,$a0,$0a,$92,$a0,$0a,$92,$a4,$0a,$92,$a4
.byte $0a,$92,$a4,$0a,$92,$a4,$01,$50,$54,$01,$50,$54,$00,$00,$00,$87

spritey:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$c0,$fc,$0f,$c0,$fc,$0f
.byte $d0,$fd,$0f,$d0,$fd,$0f,$d0,$fd,$0f,$d0,$fd,$0f,$d0,$fd,$03,$ff
.byte $f5,$01,$6a,$55,$00,$6a,$54,$00,$2a,$40,$00,$2a,$40,$00,$2a,$40
.byte $00,$2a,$40,$00,$2a,$40,$00,$05,$40,$00,$05,$40,$00,$00,$00,$87

spritez:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$3f,$ff,$f0,$00,$0f,$f0,$05
.byte $5f,$d4,$00,$0f,$d4,$00,$3f,$d0,$00,$3f,$50,$00,$3f,$50,$00,$ff
.byte $40,$00,$a9,$40,$00,$a9,$40,$02,$a9,$00,$02,$a5,$00,$02,$a5,$00
.byte $0a,$a4,$00,$0a,$aa,$a0,$01,$54,$00,$01,$55,$54,$00,$00,$00,$87

spritespace:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
