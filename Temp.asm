$NOLIST
$MODLP51RC2
$LIST

CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
UP            equ P0.5
DOWN		  equ P0.7
; Input 3 bit binary state from TIME/FSM MCU
STATE_bit0      equ P2.1
STATE_bit1      equ P2.2
STATE_bit2      equ P2.3
STATE_STABLE    equ P2.4
; Outputs to Time/FSM MCU
 TEMP_OK        equ P2.3
 TEMP_50        equ P2.4
 OVEN_CTL_PIN   equ P1.5

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

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

DSEG at 30H
Count1ms:           ds 2 
x:                  ds 4 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
y:                  ds 4 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
bcd:                ds 5 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file 
soaktemp:           ds 1
reflowtemp:         ds 1
volt_reading:       ds 2 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
temp_reading:       ds 1
fsm_state:          ds 1 

BSEG
seconds_flag:       dbit 1
mf:                 dbit 1 ; this dseg is used in the INC file, any changes to name need to also be updated in INC file
hold_button:        dbit 1

CSEG
; These ’EQU’ must match the wiring between the microcontroller and ADC (used in the INC file)
CE_ADC EQU P2.0 
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3
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
$LIST

;                   1234567890123456
SOAK_TEMP:      db 'Soak:   xxx', 0xDF, 'C   ', 0
REFLOW_TEMP:    db 'Reflow: xxx', 0xDF, 'C   ', 0
CURRENT_TEMP:   db 'Temp:   xxx', 0xDF, 'C   ', 0
TARGET_TEMP:    db 'Target: xxx', 0xDF, 'C   ', 0
OVEN_OFF:       db 'OVEN OFF        ', 0

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
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    CLR TR2  ; timer 2 is initially disabled
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

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
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
	
	; 1 second has passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know 1 second has passed
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

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
    mov P2M0, #0
    mov P2M1, #0
    
    lcall InitSerialPort
    lcall INIT_SPI

    lcall LCD_4BIT
    lcall Timer2_Init

    clr seconds_flag

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
    ; temperature is set, TEMP_OK = 1
    setb TEMP_OK
    ; display current temperature
    Set_Cursor(1,1)
    Send_Constant_String(#CURRENT_TEMP)
    Set_Cursor(2,1)
    Send_Constant_String(#OVEN_OFF)
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
    
Idle:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #0, State_1
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
    cjne a, #1, State_2
    ; diplay target temperature
    Set_Cursor(2,1)
    Send_Constant_String(#TARGET_TEMP)
    Load_X(0)
    mov x+0, soaktemp
    lcall hex2bcd
    Display_temp_BCD(2,8)
    ; turns on oven
    setb OVEN_CTL_PIN

Heating_To_Soak:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #1, State_2
    ; read temperature
    lcall Read_ADC

    ; convert the voltage reading into temperature and store in temp_reading
    ; display/send temp to PuTTY and LCD every second
    ; play sound

    ; if temperature >= reflow temperature, TEMP_OK = 0
    ; else 1
    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, soaktemp
    lcall x_gteq_y
    jnb mf, Heating_To_Soak
    clr TEMP_OK
    sjmp Heating_To_Soak

;-------------------------------------------------- STATE 2 --------------------------------------------------
; soak temperature has been reached, temperature is held for [soaktime]
State_2:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #2, State_3
    
    lcall Read_ADC

    ; convert the voltage reading into temperature and store in temp_reading
    ; display/send temp to PuTTY and LCD every second
    ; play sound every 5 seconds

    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, soaktemp
    lcall x_gteq_y
    jb mf, Soaking_too_high
    
Soaking_too_low:
    setb OVEN_CTL_PIN ; turn on the oven

    ljmp State_2

Soaking_too_high:
    clr OVEN_CTL_PIN ; turn off the oven

    ljmp State_2

;-------------------------------------------------- STATE 3 --------------------------------------------------
; heating to reflow temperature
State_3:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #3, State_4
    ; display target temperature
    Load_X(0)
    mov x+0, reflowtemp
    lcall hex2bcd
    Display_sr_temp_BCD(2,8)
    setb OVEN_CTL_PIN ; turn on oven

Heating_To_Reflow:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #3, State_4
    
    lcall Read_ADC

    ; convert the voltage reading into temperature and store in temp_reading
    ; display/send temp to PuTTY and LCD every second
    ; play sound every 5 seconds
    
    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, soaktemp
    lcall x_gteq_y
    jnb mf, Heating_To_Reflow
    setb TEMP_OK
    sjmp Heating_To_Reflow



 
;-------------------------------------------------- STATE 4 --------------------------------------------------
; reflow temperature has been reached, temperature is held for [reflowtime]
State_4:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #4, State_5

    lcall Read_ADC

    ; convert the voltage reading into temperature and store in temp_reading
    ; display/send temp to PuTTY and LCD every second
    ; play sound every 5 seconds

    Load_X(0)
    Load_Y(0)
    mov x+0, temp_reading
    mov y+0, soaktemp
    lcall x_gteq_y
    jb mf, Reflow_too_high

Reflow_too_low:
    setb OVEN_CTL_PIN ; turn on oven

    ljmp State_4

Reflow_too_high:
    clr OVEN_CTL_PIN ; turn off oven

    ljmp State_4


;-------------------------------------------------- STATE 5 --------------------------------------------------
; cooldown
State_5:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #5, State_6
    clr OVEN_CTL_PIN ; turn off oven

    lcall Read_ADC

    ; convert the voltage reading into temperature and store in temp_reading
    ; display/send temp to PuTTY and LCD every second
    ; play sound every 5 seconds

    Load_X(0)
    Load_Y(50)
    mov x+0, temp_reading
    lcall x_gteq_y
    jb mf, State_5
    clr TEMP_50
    ljmp State_5
    


;-------------------------------------------------- STATE 6 --------------------------------------------------
; error
State_6:
    ; check state
    jnb STATE_STABLE, $ ; wait for state to be stable
    lcall read_state
    cjne a, #6, return_state_0
    clr OVEN_CTL_PIN

    Load_X(0)
    Load_Y(50)
    mov x+0, temp_reading
    lcall x_gteq_y
    jb mf, State_5
    clr TEMP_50
    ljmp State_6

return_state_0
    ljmp State_0
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
    jnb UP, set_soak_temp_a
    ; if DOWN button is held, decrement temperature
    jnb DOWN, set_soak_temp_b
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
    jnb UP, set_reflow_temp_a
    ; if DOWN button is held, decrement temperature
    jnb DOWN, set_reflow_temp_b
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
    ljmp State_0
