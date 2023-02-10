$NOLIST
$MODLP51RC2
$LIST

CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
UP            equ P0.2
DOWN		  equ P0.6
; Input 3 bit binary state from TIME/FSM MCU
; STATE_bit0
; STATE_bit1
; STATE_bit2
; Outputs to Time/FSM MCU
 TEMP_OK     equ P2.3
 TEMP_50     equ P2.4

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
x:                  ds 4
y:                  ds 4
bcd:                ds 5
soaktemp:           ds 1
reflowtemp:         ds 1

BSEG
seconds_flag:       dbit 1
mf:                 dbit 1
hold_button:        dbit 1

CSEG
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

SOAK_TEMP:      db 'Soak:   xxx', 0xDF, 'C   ', 0
REFLOW_TEMP:    db 'Reflow: xxx', 0xDF, 'C   ', 0
CURRENT_TEMP:   db 'Temp:   xxxx', 0xDF, 'C  ', 0
TARGET_TEMP:    db 'Target: xxxx', 0xDF, 'C  ', 0

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
    CLR TR2  ; Enable timer 2
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
    ; temperature is set, TEMP_OK = 1
    setb TEMP_OK
    ; display current temperature
    Set_Cursor(1,1)
    Send_Constant_String(#CURRENT_TEMP)
    Set_Cursor(2,1)
    Send_Constant_String(#TARGET_TEMP)
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
    
Idle:
    ; if BOOT_BUTTON is pressed, jump to setup
    jb BOOT_BUTTON, Idle
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, Idle
    ljmp setup

;-------------------------------------------------- STATE 1 --------------------------------------------------
; heating to soak temperature
State_1:

;-------------------------------------------------- STATE 2 --------------------------------------------------
; soak temperature has been reached, temperature is held for [soaktime]
State_2:

;-------------------------------------------------- STATE 3 --------------------------------------------------
; heating to reflow temperature
State_3:

;-------------------------------------------------- STATE 4 --------------------------------------------------
; reflow temperature has been reached, temperature is held for [reflowtime]
State_4:

;------------------------------------------------- STATE 5/6 -------------------------------------------------
; cooldown/error
State_5_6:

;-------------------------------------------------- SETUP --------------------------------------------------
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
    lcall display_soaktemp_BCD
    ; display soak temperature
    mov x+0, reflowtemp+0
    lcall hex2bcd
    lcall display_reflowtemp_BCD

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
    lcall display_soaktemp_BCD
    Wait_Milli_Seconds(#25)
    ; if UP is held, increment temperature
    jnb UP, set_soak_temp_a
    ; if DOWN button is held, decrement temperature
    jnb DOWN, set_soak_temp_b
    clr hold_button
set_soak_temp_g:
    ; update display and wait 200 ms
    lcall hex2bcd
    lcall display_soaktemp_BCD
    Set_Cursor(1,11)
    Cursor_On
    Wait_Milli_Seconds(#200)
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
    lcall display_reflowtemp_BCD
    Wait_Milli_Seconds(#25)
    ; if UP is held, increment temperature
    jnb UP, set_reflow_temp_a
    ; if DOWN button is held, decrement temperature
    jnb DOWN, set_reflow_temp_b
    clr hold_button
set_reflow_temp_g:
    ; update display and wait 200 ms
    lcall hex2bcd
    lcall display_reflowtemp_BCD
    Set_Cursor(2,11)
    Cursor_On
    Wait_Milli_Seconds(#200)
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
