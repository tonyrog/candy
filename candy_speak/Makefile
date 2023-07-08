
OBJ = candy_speak.o
CFLAGS = -g -O0 -Wall

candy_speak:	candy_speak.o
	$(CC) -o$@ $(CFLAGS) candy_speak.o

candy_speak.o:	candy_speak.c  CandySpeak/CandySpeak.h CandySpeak/CandyDebug.h
	$(CC) -c $(CFLAGS) $<

setup:
	(cd CandySpeak; git clone https://github.com/tonyrog/bitpack)
