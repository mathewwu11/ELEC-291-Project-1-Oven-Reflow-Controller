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
; Output 3 bit binary state to temperature MCU
; STATE_bit0
; STATE_bit1
; STATE_bit2
; Inputs from temperature MCU
 TEMP_OK     equ P0.6
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
time:               ds 2
time_BCD:           ds 2
uptime:             ds 2
uptime_BCD:         ds 2
soaktime:           ds 2
soaktime_BCD:       ds 2
reflowtime:         ds 2
reflowtime_BCD:     ds 2
calc_time:          ds 2
calc_time_BCD:      ds 2
max_heat_time:      ds 2

BSEG
seconds_flag:       dbit 1
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
$include(Time.inc)
$LIST

SOAK_TIME:      db 'Soak:   xx:xx   ', 0
REFLOW_TIME:    db 'Reflow: xx:xx   ', 0
RUN_TIME:       db 'xx:xx           ', 0
READY:          db 'READY           ', 0
SET_TEMP:       db 'SET temperature ', 0 
HEAT_SOAK:      db 'HEAT TO SOAK    ', 0
SOAK:           db 'SOAKING         ', 0
HEAT_REFLOW:    db 'HEAT TO REFLOW  ', 0
REFLOW:         db 'REFLOWING       ', 0
COOLING:        db 'COOLING DOWN    ', 0
ABORT:          db 'PROCESS ABORTED ', 0
ERR_CD:         db 'ERR:COOLING DOWN', 0
CLR_TIME:       db '     ', 0

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
    CLR TR2  ; Timer 2 is initally disabled
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
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction high_low_flags the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
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
    mov uptime+0, #0
    mov uptime+1, #0
    mov uptime_BCD+0, #0
    mov uptime_BCD+1, #0
    ; default soaktime = 45 seconds
    mov soaktime+0, #0x2D
    mov soaktime+1, #0
    mov soaktime_BCD+0, #0x45
    mov soaktime_BCD+1, #0
    ; default reflowtime = 30 seconds
    mov reflowtime+0, #0x1E
    mov reflowtime+1, #0
    mov reflowtime_BCD+0, #0x30
    mov reflowtime_BCD+1, #0
    ; max_heat_time = 1 minutes
    mov max_heat_time+0, #0
    mov max_heat_time+1, #1

    ljmp setup ; jump to setup after reset

;-------------------------------------------------- STATE 0 --------------------------------------------------
; idle state, reflow oven is off
State_0:
    ; display uptime
    Set_Cursor (1,1)
    Send_Constant_String(#RUN_TIME)
    lcall update_uptime
    ; if soak/reflow temperatures are not set, wait for temperatures to be set
    jnb TEMP_OK, Temp_not_set
    ; if soak/reflow temperatures are set, controller is ready to start
    sjmp Idle
    
; TEMPERATURE NOT SET
Temp_not_set:
    ; prints "SET temperature" message
    Set_Cursor (2,1)
    Send_Constant_String(#SET_TEMP)
    ; if BOOT_BUTTON or UP are being pressed, wait for release
    jnb BOOT_BUTTON, $
    jnb UP, $
Temp_not_set_a:
    ; if BOOT_BUTTON is pressed, jump to setup
    jb BOOT_BUTTON, Temp_not_set_b
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, Temp_not_set_b
    ljmp setup
Temp_not_set_b:
    ; if temperature is set, controller is ready to start
    jnb TEMP_OK, Temp_not_set_a
    
; READY
Idle:
    ; prints "READY" message
    Set_Cursor (2,1)
    Send_Constant_String(#READY)
    ; if BOOT_BUTTON or UP are being pressed, wait for release
    jnb BOOT_BUTTON, $
    jnb UP, $
Idle_a:
    ; if BOOT_BUTTON is pressed, jump to setup
    jb BOOT_BUTTON, Idle_b
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, Idle_b
    ljmp setup
Idle_b:
    ; if soak/reflow temperatures are not set, wait for temperatures to be set
    jb TEMP_OK, Idle_c
    ljmp Temp_not_set
Idle_c:
    ; if UP is pressed, jump to State_1 (heating to soak temperature)
    jb UP, Idle_a
    Wait_Milli_Seconds(#50) ; debounce time
    jb UP, Idle_a
    sjmp State_1

;-------------------------------------------------- STATE 1 --------------------------------------------------
; heating to soak temperature
State_1:
    ; reset uptime
    mov uptime+0, #0
    mov uptime+1, #0
    mov uptime_BCD+0, #0
    mov uptime_BCD+1, #0
    lcall update_uptime
    ; prints "HEAT TO SOAK" message
    Set_Cursor(2,1)
    Send_Constant_String(#HEAT_SOAK)
    ; start timer 2
    setb TR2

Heating_To_Soak:
    ; if 1 second has passed, increment uptime
    lcall count_uptime
    ; if uptime >= 1 minutes && temperature < 50, jump to State_6 (Error)
    mov a, uptime+1
    jz Heating_To_Soak_a
    jb TEMP_50, Heating_To_Soak_a
    ljmp State_6
Heating_To_Soak_a:
    ; if temperature != soak temperature after 3 minutes, jump to State_6 (Error)
    mov a, uptime+1
    cjne a, #0x3, Heating_To_Soak_b
    ljmp State_6
Heating_To_Soak_b:
    ; if temperature == soak temperature, jump to State_2 (soaking)
    jnb TEMP_OK, State_2
    sjmp Heating_To_Soak

;-------------------------------------------------- STATE 2 --------------------------------------------------
; soak temperature has been reached, temperature is held for [soaktime]
State_2:
    ; prints "SOAKING" message
    Set_Cursor(2,1)
    Send_Constant_String(#SOAK)
    ; calculate when to stop soaking
    load_time(uptime, uptime_BCD)
    add_time(soaktime)
    unload_time(calc_time, calc_time_BCD)
    ; display calculated time
    lcall update_calc_time

Soaking:
    ; if 1 second has passed, increment uptime
    lcall count_uptime
    ; if temperature is incorrect, jump to State_6 (Error)
    jnb TEMP_OK, Soaking_a
    ljmp State_6
Soaking_a:
    ; if uptime == calculated time, jump to State_3 (heating to reflow temperature)
    mov a, uptime+1
    mov b, calc_time+1
    cjne a, b, Soaking
    mov a, uptime+0
    mov b, calc_time+0
    cjne a, b, Soaking
    sjmp State_3
    
;-------------------------------------------------- STATE 3 --------------------------------------------------
; heating to reflow temperature
State_3:
    ; clear calculated time from display
    Set_Cursor(1,12)
    Send_Constant_String(#CLR_TIME)
    ; prints "HEAT TO REFLOW" message
    Set_Cursor(2,1)
    Send_Constant_String(#HEAT_REFLOW)
    ; calculate max time to heat to reflow
    load_time(uptime, uptime_BCD)
    add_time(max_heat_time)
    unload_time(calc_time, calc_time_BCD)

Heating_To_Reflow:
    ; if 1 second has passed, increment uptime
    lcall count_uptime
    ; if temperature != Reflow temperature after [max_heat_time], jump to State_6 (Error)
    mov a, uptime+1
    mov b, calc_time+1
    cjne a, b, Heating_To_Reflow_a
    mov a, uptime+0
    mov b, calc_time+0
    cjne a, b, Heating_To_Reflow_a
    ljmp State_6
Heating_To_Reflow_a:
    ; if temperature == Reflow temperature, jump to State_4 (Reflowing)
    jb TEMP_OK, State_4
    sjmp Heating_To_Reflow

;-------------------------------------------------- STATE 4 --------------------------------------------------
; reflow temperature has been reached, temperature is held for [reflowtime]
State_4:
    ; prints "REFLOWING" message
    Set_Cursor(2,1)
    Send_Constant_String(#REFLOW)
    ; calculate when to stop reflowing
    load_time(uptime, uptime_BCD)
    add_time(reflowtime)
    unload_time(calc_time, calc_time_BCD)
    ; display calculated time
    lcall update_calc_time

Reflowing:
    ; if 1 second has passed, increment uptime
    lcall count_uptime
    ; if temperature is incorrect, jump to State_6 (Error)
    jb TEMP_OK, Reflowing_a
    ljmp State_6
Reflowing_a:
    ; if uptime == calculated time, jump to State_3 (heating to reflow temperature)
    mov a, uptime+1
    mov b, calc_time+1
    cjne a, b, Reflowing
    mov a, uptime+0
    mov b, calc_time+0
    cjne a, b, Reflowing
    sjmp State_5

;-------------------------------------------------- STATE 5 --------------------------------------------------
; cooldown
State_5:
    ; clear calculated time from display
    Set_Cursor(1,12)
    Send_Constant_String(#CLR_TIME)
    ; print "COOLING DOWN" message
    Set_Cursor(2,1)
    Send_Constant_String(#COOLING)

Cooldown:
    ; if 1 second has passed, increment uptime
    lcall count_uptime
    ; loop while temperature >= 50
    jb TEMP_50, Cooldown
    ; stop timer 2 and reset 1ms counter
    clr TR2
    mov count1ms+0, #0
    mov count1ms+0, #0
    ljmp State_0

;-------------------------------------------------- STATE 6 --------------------------------------------------
; error occured
State_6:
    ; stop timer 2 and reset 1ms counter
    clr TR2
    mov count1ms+0, #0
    mov count1ms+0, #0
    ; clear calculated time from display
    Set_Cursor(1,12)
    Send_Constant_String(#CLR_TIME)
    ; if temperature >= 50, cooldown
    jb TEMP_50, Error_Cooldown
    ; print "ABORT" message
    Set_Cursor(2,1)
    Send_Constant_String(#ABORT)

Error:
    ; if UP is pressed, jump to State_0 (idle)
    jb UP, Error
    Wait_Milli_Seconds(#50) ; debounce time
    jb UP, Error
    ljmp State_0

Error_Cooldown:
    ; print "ERR:COOLING DOWN" message
    Set_Cursor(2,1)
    Send_Constant_String(#ERR_CD)
Error_Cooldown_a:
    ; if 1 second has passed, increment uptime
    lcall count_uptime
    ; loop while temperature >= 50
    jb TEMP_50, Error_Cooldown_a
    sjmp State_6

;-------------------------------------------------- SETUP --------------------------------------------------
setup:
    ; prints "SOAK" left aligned in the top row
    Set_Cursor(1,1)
    Send_Constant_String(#SOAK_TIME)
    ; prints "REFLOW" left aligned in the bottom row
    Set_Cursor(2,1)
    Send_Constant_String(#REFLOW_TIME)

    ; update display with soak and reflow time
    lcall update_soak_time
    lcall update_reflow_time

; set soak time
; MAX: 2 minutes
; MIN: 30 seconds
set_soak_seconds:
    load_time(soaktime, soaktime_BCD)
    Set_Cursor(1,13)
    Cursor_On
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
set_soak_seconds_a:
    ; if UP is pressed, increment seconds
    jb UP, set_soak_seconds_b
    mov a, time+1
    ; if minutes < 2, increment seconds
    cjne a, #0x2, set_soak_seconds_d
set_soak_seconds_b:
    ; if DOWN button is pressed, increment seconds  
    jb DOWN, set_soak_seconds_c
    mov a, time+1
    ; if minutes > 0, decrement seconds
    jnz set_soak_seconds_e
    mov a, time+0
    ; if seconds > 30, decrement seconds
    cjne a, #0x1E, set_soak_seconds_e
set_soak_seconds_c:  
    clr hold_button
    Set_Cursor(1,13)
    Cursor_On
    ; if BOOT_BUTTON is pressed, set reflow time
    jb BOOT_BUTTON, set_soak_seconds_a
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, set_soak_seconds_a
    ljmp set_reflow_seconds
set_soak_seconds_d:
    ; increment soak seconds
    Cursor_Off
    lcall increment_seconds
    ; if UP is held, increment seconds rapidly
    jb hold_button, set_soak_seconds_f
    sjmp set_soak_seconds_g
set_soak_seconds_e:
    ; decrement soak seconds
    Cursor_Off
    lcall decrement_seconds
    ; if DOWN button is held, decrement seconds rapidly
    jb hold_button, set_soak_seconds_f
    sjmp set_soak_seconds_g
set_soak_seconds_f:
    ; update display and wait 25 ms
    unload_time(soaktime, soaktime_BCD)
    lcall update_soak_time
    Wait_Milli_Seconds(#25)
    ; if UP is held, increment seconds
    jnb UP, set_soak_seconds_a
    ; if DOWN button is held, decrement seconds
    jnb DOWN, set_soak_seconds_b
    clr hold_button
set_soak_seconds_g:
    ; update display and wait 250 ms
    unload_time(soaktime, soaktime_BCD)
    lcall update_soak_time
    Set_Cursor(1,13)
    Cursor_On
    Wait_Milli_Seconds(#250)
    ; if UP is held, set a flag so the program knows
    jnb UP, set_soak_seconds_h
    ; if DOWN button is held, set a flag so the program knows 
    jnb DOWN, set_soak_seconds_i
    ljmp set_soak_seconds_a
set_soak_seconds_h:
    setb hold_button
    ljmp set_soak_seconds_a
set_soak_seconds_i:
    setb hold_button
    ljmp set_soak_seconds_b

; set reflow time
; MAX: 1 minute
; MIN: 30 seconds
set_reflow_seconds:
    load_time(reflowtime, reflowtime_BCD)
    Set_Cursor(2,13)
    Cursor_On
    ; if BOOT_BUTTON is being pressed, wait for release
    jnb BOOT_BUTTON, $
set_reflow_seconds_a:
    ; if UP is pressed, increment seconds
    jb UP, set_reflow_seconds_b
    mov a, time+1
    ; if minutes < 1, increment seconds
    cjne a, #0x1, set_reflow_seconds_d
set_reflow_seconds_b:
    ; if DOWN button is pressed, increment seconds  
    jb DOWN, set_reflow_seconds_c
    mov a, time+1
    ; if minutes > 0, decrement seconds
    jnz set_reflow_seconds_e
    mov a, time+0
    ; if seconds > 30, decrement seconds
    cjne a, #0x1E, set_reflow_seconds_e
set_reflow_seconds_c:  
    clr hold_button
    Set_Cursor(2,13)
    Cursor_On
    ; if BOOT_BUTTON is pressed, setup is done
    jb BOOT_BUTTON, set_reflow_seconds_a
    Wait_Milli_Seconds(#50) ; debounce time
    jb BOOT_BUTTON, set_reflow_seconds_a
    ljmp setup_done
set_reflow_seconds_d:
    ; increment reflow seconds
    Cursor_Off
    lcall increment_seconds
    ; if UP is held, increment seconds rapidly
    jb hold_button, set_reflow_seconds_f
    sjmp set_reflow_seconds_g
set_reflow_seconds_e:
    ; decrement reflow seconds
    Cursor_Off
    lcall decrement_seconds
    ; if DOWN button is held, decrement seconds rapidly
    jb hold_button, set_reflow_seconds_f
    sjmp set_reflow_seconds_g
set_reflow_seconds_f:
    ; update display and wait 25 ms
    unload_time(reflowtime, reflowtime_BCD)
    lcall update_reflow_time
    Wait_Milli_Seconds(#25)
    ; if UP is held, increment seconds
    jnb UP, set_reflow_seconds_a
    ; if DOWN button is held, decrement seconds
    jnb DOWN, set_reflow_seconds_b
    clr hold_button
set_reflow_seconds_g:
    ; update display and wait 250 ms
    unload_time(reflowtime, reflowtime_BCD)
    lcall update_reflow_time
    Set_Cursor(2,13)
    Cursor_On
    Wait_Milli_Seconds(#250)
    ; if UP is held, set a flag so the program knows
    jnb UP, set_reflow_seconds_h
    ; if DOWN button is held, set a flag so the program knows 
    jnb DOWN, set_reflow_seconds_i
    ljmp set_reflow_seconds_a
set_reflow_seconds_h:
    setb hold_button
    ljmp set_reflow_seconds_a
set_reflow_seconds_i:
    setb hold_button
    ljmp set_reflow_seconds_b

setup_done:
    Cursor_Off
    ljmp State_0
