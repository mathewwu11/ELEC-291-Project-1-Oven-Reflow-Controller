;-------------------------------------------------- SOUND ----------------------------------------------------
; NEED TO FIGURE OUT INDEX AND BYTES OF SOUNDS

Play_Sound_Using_Index:
    clr TR1 ; Stop Timer 1 ISR from playing previous request
	setb FLASH_CE
	clr SPEAKER ; Turn off speaker.
	
	clr FLASH_CE ; Enable SPI Flash
    mov a, #READ_BYTES
	lcall Send_SPI

    mov dptr, #sound_index ; The beginning of the index (3 bytes per entry)

    ; multiply R0 by 3 and add it to the dptr
    mov a, R0
    mov b, #3
    mul ab
    add a, dpl
    mov dpl, a
    mov a, b
    addc a, dph
    mov dph, a

    ; dptr is pointing to the MSB of the 24-bit flash memory address
    clr a
    movc a, @a+dptr
    lcall Send_SPI

    inc dptr
    clr a
    movc a, @a+dptr
    lcall Send_SPI

    inc dptr
    clr a
    movc a, @a+dptr
    lcall Send_SPI

    ; Now set how many bytes to play
    mov dptr, #Size_sound
    ; multiply R0 by 3 and add it to the dptr
    mov a, R0
    mov b, #3
    mul ab
    add a, dpl
    mov dpl, a
    mov a, b
    addc a, dph
    mov dph, a

    clr a
    movc a, @a+dptr
    mov w+2,a

    inc dptr
    clr a
    movc a, @a+dptr
    mov w+1,a

    inc dptr
    clr a
    movc a, @a+dptr
    mov w+0,a

    setb SPEAKER ; Turn on speaker.
    setb TR1 ; Start playback by enabling Timer 1
    ret

Play_Temp_Sound:
    Load_X(0)   ; clears all the bits of x
    mov x+0, temp_reading   ; load temp_reading into x
    
; check if x >= 200
    Load_Y(200)
    lcall x_gteq_y
    jnb mf, Checking_Sound_100
    mov r0, #28; load sound index for 200 in r0
    lcall Play_Sound_Using_Index; play the sound for 200
    lcall sub32; subtract 200 from x
    ljmp Checking_Sound_90; jump to Checking_Sound_90

; check if x >= 100
Checking_Sound_100:
    Load_Y(100)
    lcall x_gteq_y
    jnb mf, Checking_Sound_90
    mov r0, #27; load sound index for 100 in r0
    lcall Play_Sound_Using_Index; play the sound for 100
    lcall sub32; subtract 100 from x
    ljmp Checking_Sound_90; jump to Checking_Sound_90

; check if x >= 90
Checking_Sound_90:
    Load_Y(90)
    lcall x_gteq_y
    jnb mf, Checking_Sound_80
    mov r0, #26; load sound index for 90 in r0
    lcall Play_Sound_Using_Index; play the sound for 90
    lcall sub32; subtract 90 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 80
Checking_Sound_80:
    Load_Y(80)
    lcall x_gteq_y
    jnb mf, Checking_Sound_70
    mov r0, #25; load sound index for 80 in r0
    lcall Play_Sound_Using_Index; play the sound for 80
    lcall sub32; subtract 80 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 70
Checking_Sound_70:
    Load_Y(70)
    lcall x_gteq_y
    jnb mf, Checking_Sound_60
    mov r0, #24; load sound index for 70 in r0
    lcall Play_Sound_Using_Index; play the sound for 70
    lcall sub32; subtract 70 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 60
Checking_Sound_60:
    Load_Y(60)
    lcall x_gteq_y
    jnb mf, Checking_Sound_50
    mov r0, #23; load sound index for 60 in r0
    lcall Play_Sound_Using_Index; play the sound for 70
    lcall sub32; subtract 70 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 50
Checking_Sound_50:
    Load_Y(50)
    lcall x_gteq_y
    jnb mf, Checking_Sound_40
    mov r0, #22; load sound index for 50 in r0
    lcall Play_Sound_Using_Index; play the sound for 50
    lcall sub32; subtract 50 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 40
Checking_Sound_40:
    Load_Y(40)
    lcall x_gteq_y
    jnb mf, Checking_Sound_30
    mov r0, #21; load sound index for 40 in r0
    lcall Play_Sound_Using_Index; play the sound for 40
    lcall sub32; subtract 40 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 30
Checking_Sound_30:
    Load_Y(30)
    lcall x_gteq_y
    jnb mf, Checking_Sound_20
    mov r0, #20; load sound index for 30 in r0
    lcall Play_Sound_Using_Index; play the sound for 20
    lcall sub32; subtract 30 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 20
Checking_Sound_20:
    Load_Y(20)
    lcall x_gteq_y
    jnb mf, Checking_Sound_19
    mov r0, #19; load sound index for 20 in r0
    lcall Play_Sound_Using_Index; play the sound for 20
    lcall sub32; subtract 20 from x
    ljmp Checking_Sound_9; jump to Checking_Sound_9

; check if x >= 19
Checking_Sound_19:
    Load_Y(19)
    lcall x_gteq_y
    jnb mf, Checking_Sound_18
    mov r0, #18; load sound index for 19 in r0
    lcall Play_Sound_Using_Index; play the sound for 19
    lcall sub32; subtract 19 from x
    ljmp Checking_Sound_Return; return

; check if x >= 18
Checking_Sound_18:
    Load_Y(18)
    lcall x_gteq_y
    jnb mf, Checking_Sound_17
    mov r0, #17; load sound index for 18 in r0
    lcall Play_Sound_Using_Index; play the sound for 18
    lcall sub32; subtract 18 from x
    ljmp Checking_Sound_Return; return

; check if x >= 17
Checking_Sound_17:
    Load_Y(17)
    lcall x_gteq_y
    jnb mf, Checking_Sound_16
    mov r0, #16; load sound index for 17 in r0
    lcall Play_Sound_Using_Index; play the sound for 17
    lcall sub32; subtract 17 from x
    ljmp Checking_Sound_Return; return

; check if x >= 16
Checking_Sound_16:
    Load_Y(16)
    lcall x_gteq_y
    jnb mf, Checking_Sound_15
    mov r0, #15; load sound index for 16 in r0
    lcall Play_Sound_Using_Index; play the sound for 16
    lcall sub32; subtract 16 from x
    ljmp Checking_Sound_Return; return

; check if x >= 15
Checking_Sound_15:
    Load_Y(15)
    lcall x_gteq_y
    jnb mf, Checking_Sound_14
    mov r0, #14; load sound index for 15 in r0
    lcall Play_Sound_Using_Index; play the sound for 15
    lcall sub32; subtract 15 from x
    ljmp Checking_Sound_Return; jump to Checkincg_90

; check if x >= 14
Checking_Sound_14:
    Load_Y(14)
    lcall x_gteq_y
    jnb mf, Checking_Sound_13
    mov r0, #13; load sound index for 14 in r0
    lcall Play_Sound_Using_Index; play the sound for 14
    lcall sub32; subtract 14 from x
    ljmp Checking_Sound_Return; return

; check if x >= 13
Checking_Sound_13:
    Load_Y(13)
    lcall x_gteq_y
    jnb mf, Checking_Sound_12
    mov r0, #12; load sound index for 13 in r0
    lcall Play_Sound_Using_Index; play the sound for 13
    lcall sub32; subtract 13 from x
    ljmp Checking_Sound_Return; return

; check if x >= 12
Checking_Sound_12:
    Load_Y(12)
    lcall x_gteq_y
    jnb mf, Checking_Sound_11
    mov r0, #11; load sound index for 12 in r0
    lcall Play_Sound_Using_Index; play the sound for 12
    lcall sub32; subtract 12 from x
    ljmp Checking_Sound_Return; return

; check if x >= 11
Checking_Sound_11:
    Load_Y(11)
    lcall x_gteq_y
    jnb mf, Checking_Sound_10
    mov r0, #10; load sound index for 11 in r0
    lcall Play_Sound_Using_Index; play the sound for 11
    lcall sub32; subtract 11 from x
    ljmp Checking_Sound_Return; return

; check if x >= 10
Checking_Sound_10:
    Load_Y(10)
    lcall x_gteq_y
    jnb mf, Checking_Sound_9
    mov r0, #9; load sound index for 10 in r0
    lcall Play_Sound_Using_Index; play the sound for 10
    lcall sub32; subtract 10 from x
    ljmp Checking_Sound_Return; return

; check if x >= 9
Checking_Sound_9:
    Load_Y(9)
    lcall x_gteq_y
    jnb mf, Checking_Sound_8
    mov r0, #8; load sound index for 9 in r0
    lcall Play_Sound_Using_Index; play the sound for 9
    lcall sub32; subtract 9 from x
    ljmp Checking_Sound_Return; return

; check if x >= 8
Checking_Sound_8:
    Load_Y(8)
    lcall x_gteq_y
    jnb mf, Checking_Sound_7
    mov r0, #7; load sound index for 8 in r0
    lcall Play_Sound_Using_Index; play the sound for 8
    lcall sub32; subtract 8 from x
    ljmp Checking_Sound_Return; return

; check if x >= 7
Checking_Sound_7:
    Load_Y(7)
    lcall x_gteq_y
    jnb mf, Checking_Sound_6
    mov r0, #6; load sound index for 7 in r0
    lcall Play_Sound_Using_Index; play the sound for 7
    lcall sub32; subtract 7 from x
    ljmp Checking_Sound_Return; return

; check if x >= 6
Checking_Sound_6:
    Load_Y(6)
    lcall x_gteq_y
    jnb mf, Checking_Sound_5
    mov r0, #5; load sound index for 6 in r0
    lcall Play_Sound_Using_Index; play the sound for 6
    lcall sub32; subtract 6 from x
    ljmp Checking_Sound_Return; return

; check if x >= 5
Checking_Sound_5:
    Load_Y(5)
    lcall x_gteq_y
    jnb mf, Checking_Sound_4
    mov r0, #4; load sound index for 5 in r0
    lcall Play_Sound_Using_Index; play the sound for 5
    lcall sub32; subtract 5 from x
    ljmp Checking_Sound_Return; return

; check if x >= 4
Checking_Sound_4:
    Load_Y(4)
    lcall x_gteq_y
    jnb mf, Checking_Sound_3
    mov r0, #3; load sound index for 4 in r0
    lcall Play_Sound_Using_Index; play the sound for 4
    lcall sub32; subtract 4 from x
    ljmp Checking_Sound_Return; return

; check if x >= 3
Checking_Sound_3:
    Load_Y(3)
    lcall x_gteq_y
    jnb mf, Checking_Sound_2
    mov r0, #2; load sound index for 3 in r0
    lcall Play_Sound_Using_Index; play the sound for 3
    lcall sub32; subtract 3 from x
    ljmp Checking_Sound_Return; return

; check if x >= 2
Checking_Sound_2:
    Load_Y(2)
    lcall x_gteq_y
    jnb mf, Checking_Sound_1
    mov r0, #1; load sound index for 2 in r0
    lcall Play_Sound_Using_Index; play the sound for 2
    lcall sub32; subtract 2 from x
    ljmp Checking_Sound_Return; return

; check if x >= 1
Checking_Sound_1:
    Load_Y(1)
    lcall x_gteq_y
    jnb mf, Checking_Sound_Return
    mov r0, #0; load sound index for 1 in r0
    lcall Play_Sound_Using_Index; play the sound for 1
    lcall sub32; subtract 1 from x
    ljmp Checking_Sound_Return; return

Checking_Sound_Return:
    ret