$NOLIST
$MODLP51RC2
$LIST

CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER1_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER1_RELOAD  EQU 0x10000-(CLK/TIMER1_RATE)
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

; The pins used for SPI
FLASH_CE  EQU  P2.5
PIN_TWO_FOUR   EQU  P2.4 
PIN_TWO_ONE   EQU  P2.1
PIN_TWO_ZERO   EQU  P2.0 

BOOT_BUTTON   equ P4.5
SPEAKER  EQU P2.6 ; Used with a MOSFET to turn off speaker when not in use
UP            equ P0.7
DOWN		  equ P0.5
; Input 3 bit binary state from TIME/FSM MCU
STATE_bit0      equ P1.2
STATE_bit1      equ P1.3
STATE_bit2      equ P1.4
STATE_STABLE    equ P1.5
; Outputs to Time/FSM MCU
 TEMP_OK        equ P1.0
 TEMP_50        equ P1.1
 OVEN_CTL_PIN   equ P1.6

 ; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

org 0000H
   ljmp MainProgram

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector (not used in this code)
org 0x000B
	reti

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector
org 0x001B
	ljmp Timer1_ISR

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

DSEG at 30H
Count1ms:           ds 2
Count5s:            ds 1 
x:                  ds 4 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
y:                  ds 4 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
bcd:                ds 5 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file 
soaktemp:           ds 1
reflowtemp:         ds 1
volt_reading:       ds 2 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
temp_reading:       ds 1
fsm_state:          ds 1 
w:   				ds 3 ; 24-bit play counter.  Decremented in Timer 1 ISR.
temp_sound_state:     ds 1 


BSEG
seconds_flag:       dbit 1
five_seconds_flag:  dbit 1
mf:                 dbit 1 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
hold_button:        dbit 1
sound_flag:         dbit 1
playstart_flag:      dbit 1 ;used in play temp
PLAYDONE: dbit 1

CSEG
; These ’EQU’ must match the wiring between the microcontroller and ADC (used in the INC file)
CE_ADC  EQU P0.4
MY_MOSI EQU P0.3
MY_MISO EQU P0.2
MY_SCLK EQU P0.1
; These 'equ' must match the hardware wiring
; They are used by 'LCD_4bit.inc'
LCD_RS equ P3.2
; LCD_RW equ Px.x ; Always grounded
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc)
$include(math32.inc)
$include(Temp.inc)
$include(Sound.inc)
$LIST

;                   1234567890123456
SOAK_TEMP:      db 'Soak:   xxx', 0xDF, 'C   ', 0
REFLOW_TEMP:    db 'Reflow: xxx', 0xDF, 'C   ', 0
CURRENT_TEMP:   db 'Temp:   xxx', 0xDF, 'C   ', 0
TARGET_TEMP:    db 'Target: xxx', 0xDF, 'C   ', 0
OVEN_OFF:       db 'OVEN OFF        ', 0


; Approximate index of sounds in file 'nsound.wav'
sound_index:       ; index | number
    db 0x00, 0x39, 0x37 ; 0 1
    db 0x00, 0xe4, 0x5d ; 1 2
    db 0x01, 0x94, 0x44 ; 2 3
    db 0x02, 0x4b, 0x4a ; 3 4
    db 0x02, 0xf6, 0x71 ; 4 5
    db 0x03, 0x9c, 0xd6 ; 5 6
    db 0x04, 0x43, 0x3c ; 6 7
    db 0x04, 0xfc, 0xa1 ; 7 8
    db 0x05, 0xa7, 0xc8 ; 8 9
    db 0x06, 0x61, 0x32 ; 9 10
    db 0x07, 0x02, 0xd5 ; 10 11
    db 0x07, 0xad, 0xf9 ; 11 12
    db 0x08, 0x5d, 0xe1 ; 12 13
    db 0x09, 0x17, 0x4a ; 13 14
    db 0x09, 0xb6, 0x8a ; 14 15
    db 0x0a, 0x5f, 0x50 ; 15 16
    db 0x0b, 0x0c, 0xd6 ; 16 17
    db 0x0b, 0xba, 0x5c ; 17 18
    db 0x0c, 0x6c, 0xa5 ; 18 19
    db 0x0c, 0xfd, 0xa3 ; 19 20
    db 0x0d, 0xbb, 0xcc ; 20 30
    db 0x0e, 0x69, 0x53 ; 21 40
    db 0x0f, 0x03, 0xd2 ; 22 50
    db 0x0f, 0xc1, 0xfb ; 23 60
    db 0x10, 0x6a, 0xc2 ; 24 70
    db 0x11, 0x15, 0xe8 ; 25 80
    db 0x11, 0xca, 0x8e ; 26 90
    db 0x12, 0x78, 0x12 ; 27 100
    db 0x13, 0x27, 0xfb ; 28 200
    db 0x13, 0xd0, 0xc0 ; 29 idle
    db 0x14, 0x77, 0x25 ; 30 heating to soak
    db 0x15, 0x35, 0x51 ; 31 soaking
    db 0x15, 0xcd, 0x72 ; 32 heating to reflow
    db 0x16, 0x8b, 0x8b ; 33 reflowing
    db 0x17, 0x2a, 0xde ; 34 cooling 
    db 0x17, 0xd6, 0x05 ; 35 error
    db 0x18, 0x7e, 0xcc 

; Size of each sound in 'sound_index'
Size_sound:
    db 0x00, 0xab, 0x26 ; 0 
    db 0x00, 0xaf, 0xe7 ; 1 
    db 0x00, 0xb7, 0x06 ; 2 
    db 0x00, 0xab, 0x27 ; 3 
    db 0x00, 0xa6, 0x65 ; 4 
    db 0x00, 0xa6, 0x66 ; 5 
    db 0x00, 0xb9, 0x65 ; 6 
    db 0x00, 0xab, 0x27 ; 7 
    db 0x00, 0xb9, 0x6a ; 8 
    db 0x00, 0xa1, 0xa3 ; 9 
    db 0x00, 0xab, 0x24 ; 10 
    db 0x00, 0xaf, 0xe8 ; 11 
    db 0x00, 0xb9, 0x69 ; 12 
    db 0x00, 0x9f, 0x40 ; 13 
    db 0x00, 0xa8, 0xc6 ; 14 
    db 0x00, 0xad, 0x86 ; 15 
    db 0x00, 0xad, 0x86 ; 16 
    db 0x00, 0xb2, 0x49 ; 17 
    db 0x00, 0x90, 0xfe ; 18 
    db 0x00, 0xbe, 0x29 ; 19 
    db 0x00, 0xad, 0x87 ; 20 
    db 0x00, 0x9a, 0x7f ; 21 
    db 0x00, 0xbe, 0x29 ; 22 
    db 0x00, 0xa8, 0xc7 ; 23 
    db 0x00, 0xab, 0x26 ; 24 
    db 0x00, 0xb4, 0xa6 ; 25 
    db 0x00, 0xad, 0x84 ; 26 
    db 0x00, 0xaf, 0xe9 ; 27 
    db 0x00, 0xa8, 0xc5 ; 28 
    db 0x00, 0xa6, 0x65 ; 29 
    db 0x00, 0xbe, 0x2c ; 30 
    db 0x00, 0x98, 0x21 ; 31 
    db 0x00, 0xbe, 0x19 ; 32 
    db 0x00, 0x9f, 0x53 ; 33 
    db 0x00, 0xab, 0x27 ; 34 
    db 0x00, 0xa8, 0xc7 ; 35 








;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret
	
;---------------------------------;
; ISR for timer 0                 ;
;---------------------------------;
Timer0_ISR:
	jb sound_flag, Start_Chirping
	reti

Start_Chirping:
	cpl SPEAKER 
	reti

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	SPIBIT MAC
	    ; Send/Receive bit %0
		rlc a
		mov PIN_TWO_FOUR, c
		setb PIN_TWO_ZERO
		mov c, PIN_TWO_ONE
		clr PIN_TWO_ZERO
		mov acc.0, c
	ENDMAC
	
	SPIBIT(7)
	SPIBIT(6)
	SPIBIT(5)
	SPIBIT(4)
	SPIBIT(3)
	SPIBIT(2)
	SPIBIT(1)
	SPIBIT(0)

	ret

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 1                     ;
;---------------------------------;
Timer1_Init:
	; Configure P2.0, P2.4, P2.5 as open drain outputs
	orl P2M0, #0b_0011_0001
	orl P2M1, #0b_0011_0001
	setb PIN_TWO_ONE  ; Configured as input
	setb FLASH_CE ; CS=1 for SPI flash memory
	clr PIN_TWO_ZERO   ; Rest state of SCLK=0
	clr SPEAKER   ; Turn off speaker.
	
	; Configure timer 1
	anl	TMOD, #0x0F ; Clear the bits of timer 1 in TMOD
	orl	TMOD, #0x10 ; Set timer 1 in 16-bit timer mode.  Don't change the bits of timer 0
	mov TH1, #high(TIMER1_RELOAD)
	mov TL1, #low(TIMER1_RELOAD)
	; Set autoreload value
	mov RH1, #high(TIMER1_RELOAD)
	mov RL1, #low(TIMER1_RELOAD)

	; Enable the timer and interrupts
    setb ET1  ; Enable timer 1 interrupt
	clr TR1 ; Timer 1 is only enabled to play stored sound

	; Configure the DAC.  The DAC output we are using is P2.3, but P2.2 is also reserved.
	mov DADI, #0b_1010_0000 ; ACON=1
	mov DADC, #0b_0011_1010 ; Enabled, DAC mode, Left adjusted, CLK/4
	mov DADH, #0x80 ; Middle of scale
	mov DADL, #0
	orl DADC, #0b_0100_0000 ; Start DAC by GO/BSY=1

check_DAC_init:
	mov a, DADC
	jb acc.6, check_DAC_init ; Wait for DAC to finish
	setb EA ; Enable interrupts

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init one millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
    ; Init five second interrupt counter
    mov Count5s, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret

INIT_SPI:
    setb MY_MISO    ; Make MISO an input pin
    clr MY_SCLK     ; For mode (0,0) SCLK is zero
    ret

;-------------------------------------;
; ISR for Timer 1.  Used to playback  ;
; the WAV file stored in the SPI      ;
; flash memory.                       ;
;-------------------------------------;
Timer1_ISR:
	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
keep_playing:
	setb SPEAKER
	lcall Send_SPI ; Read the next byte from the SPI Flash...
	mov P0, a ; WARNING: Remove this if not using an external DAC to use the pins of P0 as GPIO
	add a, #0x80
	mov DADH, a ; Output to DAC. DAC output is pin P2.3
	orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1
	sjmp Timer1_ISR_Done

stop_playing:
	clr TR1 ; Stop timer 1
	setb FLASH_CE  ; Disable SPI Flash
	clr SPEAKER ; Turn off speaker.  Removes hissing noise when not playing sound.
	mov DADH, #0x80 ; middle of range
	orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1

Timer1_ISR_Done:	
	pop psw
	pop acc
	reti

;---------------------------------;
; ISR for timer 2                 ;
; Used to count Time              ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	;cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if 1 second has passed
	mov a, Count1ms+0
	cjne a, #low(250), Timer2_ISR_done ; Warning: this instruction high_low_flags the carry flag!
	mov a, Count1ms+1
	cjne a, #high(250), Timer2_ISR_done
	
	; 1 second has passed. Set a flag so the main program knows
	setb seconds_flag ; Let the main program know 1 second has passed
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
    inc Count5s

Inc_Done_a:
    ; Check if 5 seconds has passed
    mov a, Count5s
    cjne a, #5, Timer2_ISR_done
    
    ; 5 seconds have passed. Set a flag so the main program knows
    setb five_seconds_flag
    mov Count5s, #0

Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main Program                    ;
;---------------------------------;
MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    setb EA   ; Enable Global interrupts
    mov P0M0, #0
    mov P0M1, #0
    mov P1M0, #0
    mov P1M1, #0
    mov P2M0, #0
    mov P2M1, #0
    
    lcall InitSerialPort
    lcall INIT_SPI
    lcall Timer0_Init
    lcall Timer1_Init

    lcall LCD_4BIT
    lcall Timer2_Init

    setb STATE_bit0
    setb STATE_bit1
    setb STATE_bit2
    setb STATE_STABLE

    clr TEMP_OK
    clr TEMP_50
    clr OVEN_CTL_PIN

    clr seconds_flag
    clr five_seconds_flag
    clr hold_button

    mov count1ms+0, #0
    mov count1ms+0, #0
    ; defualt soaktemp = 150
    mov soaktemp, #0x96
    ; default reflowtemp = 240
    mov reflowtemp, #0xF0

    ljmp setup ; jump to setup after reset

;-------------------------------------------------- STATE 0 --------------------------------------------------
; idle state, reflow oven is off
State_0:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state 
    cjne a, #0, State_1

    ; turn off the oven
    clr OVEN_CTL_PIN

    ; temperature is set, TEMP_OK = 1
    setb TEMP_OK

    ; display "OVEN OFF" message
    Set_Cursor(2,1)
    Send_Constant_String(#OVEN_OFF)

    ; play sound "Idle"
    mov r0, #29
    lcall Play_Sound_Using_Index
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
    
Idle:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #0, State_1
    ; Read tempurature every second
    jnb seconds_flag, Idle_a
    clr seconds_flag
    lcall Read_ADC
    lcall Volt_To_Temp ; convert the voltage reading into temperature and store in temp_reading
    lcall Send_temp_BCD ; display/send temperature to LCD/PuTTY
Idle_a:
    ; if BOOT_BUTTON is pressed, jump to setup
    jb BOOT_BUTTON, Idle
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, Idle
    ljmp setup

;-------------------------------------------------- STATE 1 --------------------------------------------------
; heating to soak temperature
State_1:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #1, Jump_State_2 ; offset was too large for cjne to jump to State_2, branching to a ljmp

    ; turn on the oven
    setb OVEN_CTL_PIN

    ; display target temperature
    Set_Cursor(2,1)
    Send_Constant_String(#TARGET_TEMP)
    Load_X(0)
    mov x+0, soaktemp
    lcall hex2bcd
    Display_temp_BCD(2,8)

    ; play sound "Heating to soak"
    mov r0, #30
    lcall Play_Sound_Using_Index
    sjmp Heating_To_Soak

Jump_State_2:   ; ljmp to state 2
    ljmp State_2

Heating_To_Soak:
    ; read temperature every second
    jnb seconds_flag, Heating_To_Soak_a
    clr seconds_flag
    lcall Read_ADC
    lcall Volt_To_Temp ; convert the voltage reading into temperature and store in temp_reading
    lcall Send_temp_BCD ; display/send temperature to LCD/PuTTY
Heating_To_Soak_a:
    ; play sound every five seconds
    jnb five_seconds_flag, Heating_To_Soak_b
    clr five_seconds_flag
    ;lcall Play_Temp_Sound; [function to play sound here]
Heating_To_Soak_b:
    ; if temperature >= reflow temperature, TEMP_OK = 0
    ; else 1
    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, soaktemp
    lcall x_gteq_y
    jnb mf, Heating_To_Soak_c
    clr TEMP_OK
Heating_To_Soak_c:
    ; if temperature >= 50, TEMP_50 = 1
    ; else, TEMP_50 = 0
    lcall Check_50
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #1, State_2
    ljmp Heating_To_Soak

;-------------------------------------------------- STATE 2 --------------------------------------------------
; soak temperature has been reached, temperature is held for [soaktime]
State_2:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #2, State_3

    ; play sound "Soaking"
    mov r0, #31
    lcall Play_Sound_Using_Index

Soaking:
    ; read temperature every second
    jnb seconds_flag, Soaking_a
    clr seconds_flag
    lcall Read_ADC
    lcall Volt_To_Temp ; convert the voltage reading into temperature and store in temp_reading
    lcall Send_temp_BCD ; display/send temperature to LCD/PuTTY
Soaking_a:
    ; play sound every five seconds
    jnb five_seconds_flag, Soaking_b
    clr five_seconds_flag
    ;lcall Play_Temp_Sound; [function to play sound here]
Soaking_b:
    ; compare temperature to soaktemp
    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, soaktemp
    lcall x_gteq_y
    ; if temperature >= soaktemp, turn off the oven
    jb mf, Soaking_too_high
    ; else, turn on the oven
    setb OVEN_CTL_PIN
Soaking_d:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #2, State_3
    sjmp Soaking

Soaking_too_high:
    clr OVEN_CTL_PIN ; turn off the oven
    sjmp Soaking_d

;-------------------------------------------------- STATE 3 --------------------------------------------------
; heating to reflow temperature
State_3:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #3, Jump_State_4

    ; turn on the oven
    setb OVEN_CTL_PIN

    ; display target temperature
    Load_X(0)
    mov x+0, reflowtemp
    lcall hex2bcd
    Display_temp_BCD(2,8)

    ; play sound "Heating to reflow"
    mov r0, #32
    lcall Play_Sound_Using_Index
    sjmp Heating_To_Reflow

Jump_State_4:   ; ljmp to state 4
    ljmp State_4

Heating_To_Reflow:
    ; read temperature every second
    jnb seconds_flag, Heating_To_Reflow_a
    clr seconds_flag
    lcall Read_ADC
    lcall Volt_To_Temp ; convert the voltage reading into temperature and store in temp_reading
    lcall Send_temp_BCD ; display/send temperature to LCD/PuTTY
Heating_To_Reflow_a:
    ; play sound every five seconds
    jnb five_seconds_flag, Heating_To_Reflow_b
    clr five_seconds_flag
    ;lcall Play_Temp_Sound ; [function to play sound here]
Heating_To_Reflow_b:
    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, reflowtemp
    lcall x_gteq_y
    jnb mf, Heating_To_Reflow_c
    setb TEMP_OK
Heating_To_Reflow_c:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #3, State_4
    sjmp Heating_To_Reflow

;-------------------------------------------------- STATE 4 --------------------------------------------------
; reflow temperature has been reached, temperature is held for [reflowtime]
State_4:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #4, State_5

    ; play sound "Reflowing"
    mov r0, #33
    ljmp Play_Sound_Using_Index 

Reflowing:
    ; read temperature every second
    jnb seconds_flag, Reflowing_a
    clr seconds_flag
    lcall Read_ADC
    lcall Volt_To_Temp ; convert the voltage reading into temperature and store in temp_reading
    lcall Send_temp_BCD ; display/send temperature to LCD/PuTTY
Reflowing_a:
    ; play sound every five seconds
    jnb five_seconds_flag, Reflowing_b
    clr five_seconds_flag
    ;lcall Play_Temp_Sound ; [function to play sound here]
Reflowing_b:
    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, reflowtemp
    lcall x_gteq_y
    jb mf, Reflowing_too_high
    ; if temperature >= soaktemp, turn off the oven
    jb mf, Reflowing_too_high
    ; else, turn on the oven
    setb OVEN_CTL_PIN
Reflowing_d:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #4, State_5
    sjmp Reflowing

Reflowing_too_high:
    clr OVEN_CTL_PIN ; turn off the oven
    sjmp Reflowing_d

;------------------------------------------------- STATE 5/6 -------------------------------------------------
; cooldown/error
State_5:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #5, State_6

    ; turn off the oven
    clr OVEN_CTL_PIN

    ; display "OVEN OFF" message
    Set_Cursor(2,1)
    Send_Constant_String(#OVEN_OFF)

    ; play sound "Cooldown"
    mov r0, #34 ; moves the index for cooling into r0
    lcall Play_Sound_Using_Index
    sjmp Cooldown

State_6:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #6, Jump_State_0

    ; turn off the oven
    clr OVEN_CTL_PIN

    ; display "OVEN OFF" message
    Set_Cursor(2,1)
    Send_Constant_String(#OVEN_OFF)

    ; play sound "Error"
    mov r0, #35 ; moves the index for error into r0
    lcall Play_Sound_Using_Index
    sjmp Cooldown

Jump_State_0:
    ljmp State_0

Cooldown:
    ; read temperature every second
    jnb seconds_flag, Cooldown_a
    clr seconds_flag
    lcall Read_ADC
    lcall Volt_To_Temp ; convert the voltage reading into temperature and store in temp_reading
    lcall Send_temp_BCD ; display/send temperature to LCD/PuTTY
Cooldown_a:
    ; play sound every five seconds
    jnb five_seconds_flag, Cooldown_b
    clr five_seconds_flag
    ;lcall Play_Temp_Sound ; [function to play sound here] NOT FUNCTIONAL
Cooldown_b:
    Load_X(0)
    Load_Y(50)
    mov x+0, temp_reading
    lcall x_gteq_y
    jb mf, Cooldown_c
    clr TEMP_50
Cooldown_c:
    ; if temperature >= 50, TEMP_50 = 1
    ; else, TEMP_50 = 0
    lcall Check_50
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #5, Cooldown_d
    sjmp Cooldown
Cooldown_d:
    cjne a, #6, Jump_State_0
    sjmp Cooldown

;-------------------------------------------------- SETUP ----------------------------------------------------
setup:
    ; temperature not set, TEMP_OK = 0
    clr TEMP_OK
    ; prints "SOAK" left aligned in the top row
    Set_Cursor(1,1)
    Send_Constant_String(#SOAK_TEMP)
    ; prints "REFLOW" left aligned in the bottom row
    Set_Cursor(2,1)
    Send_Constant_String(#REFLOW_TEMP)
    ; display soak temperature
    Load_x(0)
    mov x+0, soaktemp
    lcall hex2bcd
    Display_temp_BCD(1,8)
    ; display reflow temperature
    mov x+0, reflowtemp+0
    lcall hex2bcd
    Display_temp_BCD(2,8)

; set soak temperature
; MAX: 240
; MIN: 120
set_soak_temp:
    mov x+0, soaktemp
    Set_Cursor(1,11)
    Cursor_On
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
set_soak_temp_a:
    ; if UP is pressed, increment temperature
    jb UP, set_soak_temp_b
    mov a, x+0
    ; if temperature < 240, increment temperature
    cjne a, #0xF0, set_soak_temp_d
set_soak_temp_b:
    ; if DOWN button is pressed, increment temperature  
    jb DOWN, set_soak_temp_c
    mov a, x+0
    ; if temperature > 120, decrement temperature
    cjne a, #0x78, set_soak_temp_e
set_soak_temp_c:  
    clr hold_button
    Set_Cursor(1,11)
    Cursor_On
    ; if BOOT_BUTTON is pressed, set reflow time
    jb BOOT_BUTTON, set_soak_temp_a
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, set_soak_temp_a
    mov soaktemp, x+0
    ljmp set_reflow_temp
set_soak_temp_d:
    ; increment soak temperature
    Cursor_Off
    inc x+0
    ; if UP is held, increment temperature rapidly
    jb hold_button, set_soak_temp_f
    sjmp set_soak_temp_g
set_soak_temp_e:
    ; decrement soak temperature
    Cursor_Off
    dec x+0
    ; if DOWN button is held, decrement temperature rapidly
    jb hold_button, set_soak_temp_f
    sjmp set_soak_temp_g
set_soak_temp_f:
    ; update display and wait 25 ms
    lcall hex2bcd
    Display_temp_BCD(1,8)
    Wait_Milli_Seconds(#25)
    ; if UP is held, increment temperature
    jnb UP, set_soak_temp_h
    ; if DOWN button is held, decrement temperature
    jnb DOWN, set_soak_temp_i
    clr hold_button
set_soak_temp_g:
    ; update display and wait 250 ms
    lcall hex2bcd
    Display_temp_BCD(1,8)
    Set_Cursor(1,11)
    Cursor_On
    Wait_Milli_Seconds(#250)
    ; if UP is held, set a flag so the program knows
    jnb UP, set_soak_temp_h
    ; if DOWN button is held, set a flag so the program knows 
    jnb DOWN, set_soak_temp_i
    ljmp set_soak_temp_a
set_soak_temp_h:
    setb hold_button
    ljmp set_soak_temp_a
set_soak_temp_i:
    setb hold_button
    ljmp set_soak_temp_b

; set reflow temp
; MAX: 240
; MIN: 120
set_reflow_temp:
    mov x+0, reflowtemp
    Set_Cursor(2,11)
    Cursor_On
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
set_reflow_temp_a:
    ; if UP is pressed, increment temperature
    jb UP, set_reflow_temp_b
    mov a, x+0
    ; if temperature < 240, increment temperature
    cjne a, #0xF0, set_reflow_temp_d
set_reflow_temp_b:
    ; if DOWN button is pressed, increment temperature  
    jb DOWN, set_reflow_temp_c
    mov a, x+0
    ; if temperature > 0, decrement temperature
    cjne a, #0x78, set_reflow_temp_e
set_reflow_temp_c:  
    clr hold_button
    Set_Cursor(2,11)
    Cursor_On
    ; if BOOT_BUTTON is pressed, set reflow time
    jb BOOT_BUTTON, set_reflow_temp_a
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, set_reflow_temp_a
    mov reflowtemp, x+0
    ljmp setup_done
set_reflow_temp_d:
    ; increment reflow temperature
    Cursor_Off
    inc x+0
    ; if UP is held, increment temperature rapidly
    jb hold_button, set_reflow_temp_f
    sjmp set_reflow_temp_g
set_reflow_temp_e:
    ; decrement reflow temperature
    Cursor_Off
    dec x+0
    ; if DOWN button is held, decrement temperature rapidly
    jb hold_button, set_reflow_temp_f
    sjmp set_reflow_temp_g
set_reflow_temp_f:
    ; update display and wait 25 ms
    lcall hex2bcd
    Display_temp_BCD(2,8)
    Wait_Milli_Seconds(#25)
    ; if UP is held, increment temperature
    jnb UP, set_reflow_temp_h
    ; if DOWN button is held, decrement temperature
    jnb DOWN, set_reflow_temp_i
    clr hold_button
set_reflow_temp_g:
    ; update display and wait 250 ms
    lcall hex2bcd
    Display_temp_BCD(2,8)
    Set_Cursor(2,11)
    Cursor_On
    Wait_Milli_Seconds(#250)
    ; if UP is held, set a flag so the program knows
    jnb UP, set_reflow_temp_h
    ; if DOWN button is held, set a flag so the program knows 
    jnb DOWN, set_reflow_temp_i
    ljmp set_reflow_temp_a
set_reflow_temp_h:
    setb hold_button
    ljmp set_reflow_temp_a
set_reflow_temp_i:
    setb hold_button
    ljmp set_reflow_temp_b

setup_done:
    Cursor_Off
    ; display current temperature
    Set_Cursor(1,1)
    Send_Constant_String(#CURRENT_TEMP)
    lcall Read_ADC
    Display_temp_BCD(1,8)
    ljmp State_0
