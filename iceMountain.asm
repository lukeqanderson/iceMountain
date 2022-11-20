	processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with the VCS memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	include "vcs.h"
	include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables from address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg.u Variables
	org $80
	
SnowmanX	byte		; Player 0 (snowman) X axis position
SnowmanY	byte		; Player 0 (snowman) Y axis position
IcicleX		byte		; Player 1 (icicle) X axis position
IcicleY		byte		; Player 1 (icicle) Y axis position
SnowmanOffsetL	byte		; Player 0 (snowman) offset for left sprite
SnowmanOffsetR	byte		; Player 0 (snowman) offset for right sprite
Random		byte		; random number for setting player 1 X pos
Score 		byte		; stores the value for the score
Temp		byte		; variable to store temporary time values
TimerSprite	byte		; stores the current time sprite
OnesDigit	word		; stores offset of ones digit
TensDigit	word		; stores offset of tens digit
SnowmanColPtr	word		; Player 0 (nowman) color pointer
SnowmanPtr	word		; Player 0 (snowman) pointer
IcicleColPtr	word		; player 1 (icicle) color pointer
IciclePtr	word		; Player 1 (icicle) pointer 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SNOWMAN_H = 10 			; Height for snowman sprite
SNOWMAN_H_X2 = 20		; Height for two snowmen for right animation
ICICLE_H = 10			; Height for icicle sprite
DIGITS_H = 5			; Height for timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the ROM at address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg Code
	org $F000

Reset:
	CLEAN_START		; Calls macro to clear memory / registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM vars and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	lda #1
	sta SnowmanY		; SnowmanY = 1

	lda #40
	sta SnowmanX		; SnowmanX = 40

	lda #73
	sta IcicleY		; IcicleY = 73 

	lda #50
	sta IcicleX		; IcicleX = 50 

	lda #%11010100
	sta Random		; Sets Random seed 

	lda #4
	sta Score


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initilize sprite and color pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #<SnowmanColor
	sta SnowmanColPtr	; Low-byte pointer for snowman color bitmap
	lda #>SnowmanColor
	sta SnowmanColPtr+1	; High-byte pointer for snowman color bitmap

	lda #<IcicleColor
	sta IcicleColPtr	; Low-byte pointer for icicle color bitmap
	lda #>IcicleColor
	sta IcicleColPtr+1	; High-byte pointer for icicle color bitmap

	lda #<Snowman
	sta SnowmanPtr		; Low-byte pointer for snowman bitmap
	lda #>Snowman		
	sta SnowmanPtr+1	; High-byte pointer for snowman bitmap

	lda #<Icicle
	sta IciclePtr		; Low-byte pointer for icicle bitmap
	lda #>Icicle			
	sta IciclePtr+1		; High-byte pointer for icicle bitmap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main game display and render frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #2;
	sta VBLANK		; turns on VBLANK
	sta VSYNC		; turns on VSYNC
	REPEAT 3
		sta WSYNC	; display 3 lines of VSYNC
	REPEND
	lda #0
	sta VSYNC		; turns off VSYNC

	REPEAT 33 
		sta WSYNC	; display 33 lines of VBLANK
	REPEND
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations performed in Pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda SnowmanX
	ldy #0
	jsr SetObjectX		; set horizontal position for snowman and jump to subroutine

	lda IcicleX 
	ldy #1
	jsr SetObjectX		; set horizontal position for icicle and jump to subroutine

	jsr CalculateDigitOffset	; calculates the digit offset for lookup table
	
	sta WSYNC		; wait 1 scan line	
	sta HMOVE		; apply the offsets set by subroutine	

	lda #0
	sta VBLANK		; turns off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render 192 visible scan lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta GRP0
	sta GRP1		; resets registers before displaying the score	
	sta CTRLPF
	lda #$0e
	sta COLUBK
	lda #$9c
	sta COLUPF
	ldx #DIGITS_H		; X = Digits height
	 
.ScoreDigitLoop:
    	sta WSYNC               ; wait for the end of scanline
    	ldy TensDigit 		; get the left digit offset for the Timer
    	lda Digits,Y            ; load the digit pattern from lookup table
 	and #%11110000          ; mask the graphics for the ones digit
    	sta TimerSprite         ; save the timer tens digit pattern in a variable

    	ldy OnesDigit		; get the ones digit offset for the Timer
    	lda Digits,Y            ; load digit pattern from the lookup table
    	and #%00001111          ; mask the graphics for the tens digit
   	ora TimerSprite         ; merge with the saved tens digit graphics
    	sta TimerSprite         ; and save it

    	jsr Sleep12Cycles       ; wastes some cycles

    	sta PF1                  ; update the playfield for Timer display

    	sta WSYNC                ; wait for next scanline
    	inc TensDigit
    	inc OnesDigit    ; increment all digits for the next line of data

    	jsr Sleep12Cycles        ; waste some cycles

    	dex                      ; X--
    	sta PF1                  ; update the playfield for the Timer display
    	bne .ScoreDigitLoop      ; if dex != 0, then branch to ScoreDigitLoop
	
    	sta WSYNC

	lda #0
	sta PF0
	sta PF1
	sta PF2
	
    	sta WSYNC
    	sta WSYNC
    	sta WSYNC
	

VisibleLines:
	lda #$9e
	sta COLUPF		; sets color of playfield to ice blue white
	lda #%00000001		
	sta CTRLPF		; reflects playfield
	lda #$F0
	sta PF0			; sets PF0 bit pattern
	lda #$FC	
	sta PF1			; sets PF1 bit pattern lda #0 sta PF2					
	lda #$ae		
	sta COLUBK		; sets color to background light blue

	ldx #85			; runs loop 79 times
.MainLineLoop:
.InsideSnowman:
	txa			; transfer X to accumulator
	sec			; set carry 
	sbc	SnowmanY	; subtrack left body Y position from accumulator		 
	cmp	SNOWMAN_H	; inside sprite height bounds?
	bcc	.DrawSnowman	; if inside bounds, draw snowman sprite
	lda	#0		; else set lookup to 0

.DrawSnowman:
	clc			; clear carry flag
	adc SnowmanOffsetL	; go to left sprite frame in memory
	clc			; clear carry flag
	adc SnowmanOffsetR	; go to right sprite frame in memory
	tay			; load Y 
	lda #%00000101
	sta NUSIZ0
	lda (SnowmanPtr),Y  	; load snowman from lookup table
	sta WSYNC		; wait for scan line
	sta GRP0		; set graphics for player 0 (snowman)
	lda (SnowmanColPtr),Y	; load snowman color from lookup table
	sta COLUP0		; set color of player 0

.InsideIcicle:
	txa			; transfer X to accumulator
	sec			; set carry 
	sbc	IcicleY		; subtrack left body Y position from accumulator		 
	cmp	ICICLE_H	; inside sprite height bounds?
	bcc	.DrawIcicle	; if inside bounds, draw icicle sprite
	lda	#0		; else set lookup to 0

.DrawIcicle:
	tay			; load Y 
	lda (IciclePtr),Y  	; load icicle from lookup table
	sta WSYNC		; wait for scan line
	sta GRP1		; set graphics for player 1 (icicle)
	lda (IcicleColPtr),Y	; load icicle color from lookup table
	sta COLUP1		; set color of player 1 (icicle) 
	
	dex			; X--
	bne .MainLineLoop	; While (X != 0)
	
	lda #0
	sta SnowmanOffsetL
	sta SnowmanOffsetR	; reset left and right offsets


	lda #$0c
	sta COLUBK		; sets color to background snow grey
	
	REPEAT 1
		sta WSYNC	
		sta WSYNC	
	REPEND
	
	lda #$0e
	sta COLUBK		; sets color to background snow white
	
	REPEAT 6 
		sta WSYNC	
		sta WSYNC	
	REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan of 30 lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #2
	sta VBLANK		; turns on VBLANK
	REPEAT 30
		sta WSYNC	; display 30 overscan lines
	REPEND

	lda #0
	sta VBLANK		; turns off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joystick input for Player 0 (snowman)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckLeft:
	lda #%01000000
	bit SWCHA		; check for left joystick input 
	bne CheckRight		; go to check right joystick input
	lda SNOWMAN_H		
	sta SnowmanOffsetL	; adds height to go to left sprite frame
	REPEAT 2
		lda SnowmanX		 
		clc			; clears carry
		cmp #31			; compares Snowman X with value 31 
		bmi CheckRight		; goes to check right if less that 31
		dec SnowmanX		; moves snowman left two pixels per frame
	REPEND

CheckRight:
	lda #%10000000
	bit SWCHA		; check for right joystick input
	bne EndJoystickTest	; ends test if not right either
	lda SNOWMAN_H_X2			
	sta SnowmanOffsetL	; adds height x 2 to go to right sprite frame
	REPEAT 2
		lda SnowmanX		 
		clc			; clears carry
		cmp #94		; compares Snowman X with value 94 
		bpl EndJoystickTest		; goes to ends test if value > 94  
		inc SnowmanX		; moves snowman right two pixels per frame
	REPEND

EndJoystickTest:		; if no joystick input is detected, do nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updates positions for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateIciclePos:
	lda IcicleY
	clc			; clears carry
	cmp #2			; compares with value 2 
	bmi .ResetIciclePos	; if icicle is less than 2 (icicle hit floor) reset position
	REPEAT 3	
		dec IcicleY ; moves icicle down at speed of 3px per frame
	REPEND
	jmp EndIciclePosUpdate

.ResetIciclePos:
	jsr GetRandomIciclePos		; cals subrouting for random pos for icicle

EndIciclePosUpdate: 		; does nothing if it does not reset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for collision between player 0 (snowman) and player 1 (
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckCollision:
	lda #%10000000		; CXPPMM bit 7 (detects P0 and P1 colission)
	bit CXPPMM		; check for collision
	bne .Collision		; branches if collision occured
	jmp EndCollisionCheck	; skip if no collision

.Collision:
	jsr GameOver		; calls GameOver subroutine

EndCollisionCheck:		; fallback for no collision
	sta CXCLR		; clears collision flags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to the Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	jmp StartFrame		; continue to next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle horizontal positioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetObjectX subroutine
	sta WSYNC		; start a fresh scanline
	sec			; set carry flag
.Div15Loop
	sbc #15			; subtract 15 from accumulator
	bcs .Div15Loop		; loop until flag is clear
	eor #%00000111		; XOR with 4 bits for (-8 to 7) offset 
	REPEAT 4
		asl		; shift left 4 time to correct 4 bits
	REPEND
	sta HMP0,Y		; store the fine offset 
	sta RESP0,Y		; fix object in 15-step increment
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to get random X position for icicle 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetRandomIciclePos subroutine
	lda Random
	asl
	eor Random
	asl
	eor Random
	asl
	asl
	eor Random
	asl
	rol Random 	; Performs bit operations for random number

	lsr
	lsr 		; divides value by 4 
	sta IcicleX	; stores value in IcicleX
	lda #30
	clc		; clears the carry
	adc IcicleX	 
	sta IcicleX	; IcicleX += 30 
	lda #73		
	sta IcicleY	; IcicleY = 73
	inc Score	; Increments score
	rts		; return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to get time digit offsets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CalculateDigitOffset subroutine
	lda Score
	and #%00001111	; masks first 4 bits
	sta Temp	; save A in Temp 
	asl
	asl		; shift left twice for n * 4
	adc Temp	; adds original A for n * 5
	sta OnesDigit	; stores ones digit

	lda Score
	and #%11110000	; masks last 4 bits
	lsr
	lsr		; shift right twice for n / 4
	sta Temp	; save A in Temp 
	lsr
	lsr		; shift right twice for n / 16
	adc Temp	; adds value stored in temp for n / 16 + n / 4 
	sta TensDigit	; stores tens digit
	rts		; return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine when game is over (collision occured)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameOver subroutine
	
	lda #0
	sta Score		; score gets reset 
	rts		; return 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #


SnowmanColor			; color for snowman
	.byte #$00
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e

SnowmanColorLeft		; color for snowman when moving left
	.byte #$00
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e

SnowmanColor3			; color for snowman when moving right
	.byte #$00
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e
	.byte #$0e

IcicleColor			; color for icicle
	.byte #%00
	.byte #$9c
	.byte #$9c
	.byte #$9c
	.byte #$9c
	.byte #$9c
	.byte #$9c
	.byte #$9c
	.byte #$9c
	.byte #$9c

Snowman
        .byte #%00000000
        .byte #%11111111
        .byte #%11110111
        .byte #%11111111
        .byte #%01110110
        .byte #%11111111
        .byte #%10100101
        .byte #%10100101
        .byte #%01011010
        .byte #%01111110	

SnowmanLeft
        .byte #%00000000
        .byte #%00111111
        .byte #%00111100
        .byte #%00111100
        .byte #%00111101
        .byte #%11111111
        .byte #%10111100
        .byte #%00111100
        .byte #%00101100
        .byte #%00111100

SnowmanRight
        .byte #%00000000
	.byte #%11111100
        .byte #%00111100
        .byte #%00111100
        .byte #%10111100
        .byte #%11111111
        .byte #%00111101
        .byte #%00111100
        .byte #%00110100
        .byte #%00111100

Icicle				; outline of icicle 
        .byte #%00000000
        .byte #%00001000
        .byte #%00001000
        .byte #%00011000
        .byte #%00011000
        .byte #%00011000
        .byte #%00011100
        .byte #%00011100
        .byte #%00011100
        .byte #%00111100

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM with 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org $FFFC		; moves to $FFFC
	word Reset		; write 2 bytes with reset
	word Reset		; write 2 bytes with interuption vector
