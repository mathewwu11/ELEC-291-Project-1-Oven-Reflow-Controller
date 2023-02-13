update_uptime:
	Set_Cursor(1,4)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(uptime_BCD+0) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 1)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(uptime_BCD+1) ; This macro is also in 'LCD_4bit.inc'
    ret

update_soak_time:
	Set_Cursor(1, 12)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(soaktime_BCD+0) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(soaktime_BCD+1) ; This macro is also in 'LCD_4bit.inc'
    ret

update_reflow_time:
	Set_Cursor(2, 12)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(reflowtime_BCD+0) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(reflowtime_BCD+1) ; This macro is also in 'LCD_4bit.inc'
    ret

update_calc_time:
	Set_Cursor(1, 15)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(calc_time_BCD+0) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 14)
	Display_char(#':')
	Set_Cursor(1, 12)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(calc_time_BCD+1) ; This macro is also in 'LCD_4bit.inc'
    ret

load_time mac
	mov time+0, %0+0
	mov time+1, %0+1
	mov time_BCD+0, %1+0
	mov time_BCD+1, %1+1
endmac

unload_time mac
	mov %0+0, time+0
	mov %0+1, time+1
	mov %1+0, time_BCD+0
	mov %1+1, time_BCD+1
endmac

increment_seconds:
    ; check seconds	
	mov a, time+0
    ; if seconds < 59, increment seconds 
	cjne a, #0x3B, increment_seconds_a
    ; if seconds = 59, set seconds = 0 & increment minutes
	mov time+0, #0
	mov time_BCD+0, #0
	lcall increment_minutes
	ret
increment_seconds_a:
	inc time+0
	mov a, time_BCD+0
	add a, #0x1
	da a
	mov time_BCD+0, a	
	ret

increment_minutes:
    ; check minutes
	mov a, time+1
    ; if minutes < 59, increment minutes
	cjne a, #0x3B, increment_minutes_a
    ; if minutes = 59, set minutes = 0
	mov time+1, #0
	mov time_BCD+1, #0
	ret
increment_minutes_a:
	inc time+1
	mov a, time_BCD+1
	add a, #0x1
	da a
	mov time_BCD+1, a	
	ret

decrement_seconds:
    ; check seconds	
	mov a, time+0
    ; if seconds > 0, decrement seconds 
	cjne a, #0x0, decrement_seconds_a
    ; if seconds = 0, set seconds = 59 & decrement minutes
	mov time+0, #0x3B
	mov time_BCD+0, #0x59
	lcall decrement_minutes
	ret
decrement_seconds_a:
	dec time+0
	mov a, time_BCD+0
	add a, #0x99
	da a
	mov time_BCD+0, a	
	ret

decrement_minutes:
    ; check minutes
	mov a, time+1
    ; if minutes > 0, decrement minutes
	cjne a, #0x0, decrement_minutes_a
    ; if minutes = 0, set minutes = 59
	mov time+1, #0x3B
	mov time_BCD+1, #0x59
	ret
decrement_minutes_a:
	dec time+1
	mov a, time_BCD+1
	add a, #0x99
	da a
	mov time_BCD+1, a	
	ret

count_uptime:
	jb seconds_flag, count_uptime_a
	ret
count_uptime_a:
	clr seconds_flag
	load_time(uptime, uptime_BCD)
	lcall increment_seconds
    unload_time(uptime, uptime_BCD)
    lcall update_uptime
	ret

add_time mac
	push acc
	push AR2
	mov R2, %0+0
	inc R2
	lcall calculate_seconds
	mov R2, %0+1
	inc R2
	lcall calculate_minutes
	pop AR2
	pop acc
endmac

calculate_seconds:
	djnz R2, calculate_seconds_a
	ret
calculate_seconds_a:
	lcall increment_seconds
	sjmp calculate_seconds

calculate_minutes:
	djnz R2, calculate_minutes_a
	ret
calculate_minutes_a:
	lcall increment_minutes
	sjmp calculate_minutes

; Moves current state into state pins to communicate with other MCU. Also clears the STATE_STABLE flag to signify that the state is set and stable
output_state:
	clr STATE_STABLE
	mov a, fsm_state
	rrc a
	mov STATE_bit0, c
	rrc a
	mov STATE_bit1, c	
	rrc a
	mov STATE_bit2, c
	setb STATE_STABLE
	ret 
