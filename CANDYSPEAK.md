# Language to control OUTPUT from CAN frames

This document describe a language to control
relays/digital/analog output given CAN frames.

The idea is based on the STERNOL languae to control
rail yards. And gives a one or more definitions
for an output to be set to a given value.

Candy output only gives a mask expression

    0x218 2 0x40 0x40 0x00
	
This should read

    Output is on if bit 0x40 is set in the byte 2 (counting from 0)
	in frame 0x128
	Output is off if bit 0x40 is cleared in the byte 2
	
It is also possible to specify multiple bits in the same byte
to enable but also disable. More precisly

	ON = data[2] & 0x40 == 0x40
	OFF = data[2] & 0x40 == 0x00
	
This may lead to situation where output is  neither set to ON
nor set to OFF.

There is now a need to express more complex situations
where output is turn off given various bit in various
positions in the same frame and maybe in others. 

# A Language to capture it all

The Candyspeak language handle more comples setups
and include the possiblity to handle expression of
not, and, or of bits in various positions

To control output we write (assuming OUT is not inverted)

	OUT = 1

to turn output of

	OUT = 0
	
to turn output on IF first bit in frame 0x218 byte 1 is ON

	OUT < 0x218[1,0]
	
this is the same as bit number 8

	OUT < 0x218[8]

In candyspeak the bits are numbered from left to right, 0..63
and 0..7 in each byte. Bytes are numbered 0..7

    byte            0                    1                     7 
	FRAME = 0 1 2 3 4 5 6 7 | 8 9 10 11 12 13 14 15 | ... | ... 63

If OUTPUT is controlled ON and OFF by the same expression then
we can write

    OUT = 0x218[8]
	
Then OUT is turned on if bit 8 is on in FRAME 0x218 if not then
OUT is truned off

So to conditional turn ON an output 

    OUT < Expr

To conditionally turn the output off

	!OUT < Expr

To turn on and off with expression

    OUT = Expr
	
# Expression 

	Statement := 
		  Name '=' Cond '\n'
		| Name '<' Cond '\n'
		| '!' Name '<' Cond '\n'
		| Define
		
	Define := 
	       '#' Name '=' Hex 0..7 Hex Hex Hex
		 | '#' Name '=' Hex '[' Pos ']'
		 | '#' Name '=' Hex '[' BytePos ',' BitPos ']'

	Cond := 
	      Bit
		| '(' Expr ')'
		| Cond ',' Cond
		| Cond ';' Cond
		! '!' Cond
		
	Bit := 
	     | '0'
		 | '1'
         | Hex 0..7 Hex Hex Hex
		 | Hex '[' Pos ']'
		 | Hex '[' BytePos ',' BitPos ']'
         | Name
		 
	BytePos := 0..7
	BitPos := 0..7
	Pos := 0..63	
	Hex := '0x' [0..9a..bA..F]+
	Int := [0..9]+
	ByteIndex = Pos
	BitIndex = Pos
	Name = Char+
	Constant = Int | Hex
	
# Normalisation

To get a reasonable output from the above expression and
statements only on "definition" ('=') per output should be
given or one for ON (OUT '<') and one for OFF (!OUT '<') should
be given

For example two definitions of A

    A = Y ; (U, V)
	A = X

is not reasonable but

    A < Y ; (U, V)
	!A < X
	
may be, in this case there are situations where A
is neither turn on of turned off.

    A < X
	A < Y

Can be rewritten to 

	A < X; Y

And

	A < X
	!A < !X

Can be rewritten to

    A = X

# Extension to Analog

    AStatement := 
		Statement |
		  OutName'=' Constant '=' ACond '\n'
		| OutName'=' Constant '<' ACond '\n'

	ADefine := 
		  Define 
		| '#' Name '=' Hex '[' BytePos ':' BitPos ']'

	ACond := 
	    Cond
	  | AExpr '=' AExpr
	  | AExpr '!=' AExpr
      | AExpr '<' AExpr
      | AExpr '<=' AExpr
      | AExpr '>' AExpr
      | AExpr '>=' AExpr

    AExpr := 
	     AValue 
	  | '(' AExpr ')'
	  | '-' AExpr
	  | AExpr '+' AExpr
      | AExpr '-' AExpr
      | AExpr '*' AExpr
      | AExpr '/' AExpr
      | AExpr '%' AExpr
	
	AValue := 
	  | Hex '[' Pos ':' Pos ']'
	  | Name
	  | Constant

# Usage of Analog expressions

Now we can set relay output or analog output
depending on input expressions

    #SPEED 0x100[0:8]
	#RPM   0x200[6:17]
	
	HEADLIGHT < (SPEED > 100), (RPM > 1000)
	!HEADLIGH < (SPEED < 20)
	
Analog output can be set to specific value only right now

    ANALOG=0 = Cond0
    ANALOG=1 = Cond1
    ANALOG=2 = Cond2
	...
	
