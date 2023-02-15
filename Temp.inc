Left_blank mac
	mov a, %0
	anl a, #0xf0
	swap a
	jz Left_blank_%M_a
	ljmp %1
Left_blank_%M_a:
	Display_char(#' ')
	mov a, %0
	anl a, #0x0f
	jz Left_blank_%M_b
	ljmp %1
Left_blank_%M_b:
	Display_char(#' ')
endmac

; Displays 4-digit BCD
Display_temp_BCD mac
	Set_Cursor(%0, %1)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	; Replace all the zeros to the left with blanks
	Set_Cursor(%0, %1)
	lcall replace_zeros
endmac

replace_zeros:
	Left_blank(bcd+1, skip_blank)
	mov a, bcd+0
	anl a, #0f0h
	swap a
	jnz skip_blank
	Display_char(#' ')
skip_blank:
    ret

Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret

; Displays 4-digit BCD number & 
; sends 10-digit BCD number in bcd to PuTTY in ASCII
Send_temp_BCD:
	Display_temp_BCD(1,8)
    Send_BCD(bcd+4)
    Send_BCD(bcd+3)
    Send_BCD(bcd+2)
    Send_BCD(bcd+1)
    Send_BCD(bcd+0)
    mov a, #'\r'
    lcall putchar
    mov a, #'\n'
    lcall putchar
    ret
	
; Reads the output voltage from the ADC and stores it in volt_reading
Read_ADC:
    clr CE_ADC
    mov R0, #00000001B ; Start bit:1
    lcall DO_SPI_G

    mov R0, #10000000B ; Single ended, read channel 0
    lcall DO_SPI_G
    mov a, R1 ; R1 contains bits 8 and 9
    anl a, #00000011B ; We need only the two least significant bits
    mov volt_reading+1, a ; Save result high.

    mov R0, #55H ; It doesn't matter what we transmit...
    lcall DO_SPI_G
    mov volt_reading+0, R1 ; R1 contains bits 0 to 7. Save result low.
    setb CE_ADC
	ret

DO_SPI_G:
	push acc
	mov R1, #0 ; Received byte stored in R1
	mov R2, #8 ; Loop counter (8-bits)
DO_SPI_G_LOOP:
	mov a, R0 ; Byte to write is in R0
	rlc a ; Carry flag has bit to write
	mov R0, a
	mov MY_MOSI, c
	setb MY_SCLK ; Transmit
	mov c, MY_MISO ; Read received bit
	mov a, R1 ; Save received bit in R1
	rlc a
	mov R1, a
	clr MY_SCLK
	djnz R2, DO_SPI_G_LOOP
	pop acc
	ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret

; Moves current state into the state pins in order to communicate with the other MCU
read_state:
	mov a, #0
	mov c, STATE_bit2
	rlc a
	mov c, STATE_bit1
	rlc a
	mov c, STATE_bit0
	rlc a
	ret


; Converts the voltage reading in the from the ADC to its corresponding temperature
Volt_To_Temp:
	mov x+0, volt_reading+0
	mov x+1, volt_reading+1
	mov x+2, #0
	mov x+3, #0
	
    Load_Y(40039)
	lcall mul32

    Load_Y(41500)
    lcall div32

    Load_y(22)
    lcall add32

	mov temp_reading, x+0
	lcall hex2bcd

	ret

; Checks if temperature is >= 50
; if temperature >= 50, TEMP_50 = 1
; else 0
Check_50:
	Load_X(0)
	mov x+0, temp_reading
	Load_Y(50)
	lcall x_gteq_y
	jb mf, Check_50_gteq
	clr TEMP_50
	ret
Check_50_gteq:
	setb TEMP_50
	ret
