# ELEC-291-Project-1-Oven-Reflow-Controller

To send the WAV file to the microcontroller, a program that runs on Microsoft Windows named
‘Computer_Sender.c’ (together with the executable ‘Computer_Sender.exe’) should be also available in the
course web page. 

***If you want to recompile this program you can use Visual C from a command prompt:***
C:\Source>cl Computer_Sender.c

***or similarly, if GCC is installed in your computer:***
C:\Source>gcc Computer_Sender.c –o Computer_Sender.exe

C:\Source>Computer_Sender -?

SPI flash memory programmer using serial protocol. (C) Jesus Calvino-Fraga (2012-2019)
***Usage examples:***
Computer_Sender -DCOM3 -w somefile.wav (write 'somefile.wav' to flash via COM3)

Computer_Sender -DCOM3 -w -I somefile.wav (write 'somefile.wav' to flash via COM3, do not
check for valid WAV)

Computer_Sender -DCOM3 -v somefile.wav (compare 'somefile.wav' and flash)

Computer_Sender -DCOM3 -Rotherfile.wav (save content of SPI flash to 'otherfile.wav')

Computer_Sender -DCOM3 -P (play the content of the flash memory)

Computer_Sender -DCOM3 -P0x20000,12540 (play the content of the flash memory starting at
address 0x20000 for 12540 bytes)

Computer_Sender -Amyindex.asm somefile.wav (generate asm index file 'myindex.asm' for
'somefile.wav')

Computer_Sender -Cmyindex.c somefile.wav (generate C index file 'myindex.c' for
'somefile.wav'.)

Computer_Sender -Cmyindex.c -S2000 somefile.wav (same as above but check for 2000 silence
bytes. Default is 512.)
