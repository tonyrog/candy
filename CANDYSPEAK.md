# Language to control OUTPUT from CAN frames

This document describe a language to control
relays/digital/analog output given CAN frames.

The idea is based on the STERNOL language to control
rail yards and gives one or more definitions
for an output to be set to a given value.

Currently Candy saves an expression on the following form

    0x218 2 0x40 0x40 0x00
	
This should read

    Output is on if bit 0x40 is set in the byte 2 (counting from 0)
	in frame 0x218
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

The CandySpeak language handles more complex setups
and include the possibility to handle expression containing
negation, AND, OR of bits in various positions

To control an output named OUT we write (assuming OUT is not inverted)

	OUT = 1

to turn __off__ output OUT

	OUT = 0
	
to turn output __on__ if first bit in frame 0x218 byte 1 is 1

	OUT = 1 ? 0x218[1,0]
	
this is the same as bit number 8

	OUT = 1 ? 0x218[8]

In CandySpeak the bits are numbered from left to right, 0..63
and 0..7 in each byte. Bytes are numbered 0..7

    byte            0                    1            ...    7 
	FRAME = 0 1 2 3 4 5 6 7 | 8 9 10 11 12 13 14 15 | ... | ... 63

If OUT is controlled __on__ and __off__ by the same expression then
we can write

    OUT = 0x218[8]
	
Then OUT is then turned __on__ if bit 8 is __on__ in FRAME 0x218 otherwise OUT if turned __off__

So to conditionally turn output __on__

    OUT = 1 ? Expr

To conditionally turn the output __off__ we prefix with an exlamation mark '!' 

	OUT = 0 ? Expr

To turn __on__ and __off__ using a value of expression

    OUT = Expr
	
# Expression 

Here is a BNF (like) grammar for the Boolean part of CandySpeak that
preserve backward compatabilty with the old simple expression, except
the case for multiple bit selection.

	<file> :=
		(<statement> '\n')*
		
	<statement> :=
		  <frame-id> 0..7 <hex2> <hex2> <hex2>
		| <declaration>
		| <name> '=' <bit>
		| <name> '=' <bit> '?' <cond>

	<declaration> :=
		  '#' 'digital' <name> [<port>':']<pin>
          '#' 'variable' <name>[:1] ['=' '0'|'1']
		| '#' 'can' <name> <can-bit>
		 
	<cond> :=
		  <bit>
		| '(' <cond> ')'
		| <cond> ',' <cond>
		| <cond> ';' <cond>
		! '!' <cond>
		
	<bit> :=
		  '0' | '1'
	    | <can-bit>
		| <name>
		
	<can-bit> :=
		  <frame-id> 0..7 <hex2> <hex2> <hex2>
		| <frame-id> '[' <bit-pos> ']'
		| <frame-id> '[' <byte-pos> ',' <bit-pos> ']'
	
    <frame-id> := <hex>
	<port> := <int>
	<pin> := <int> | <name>
	<bits> := <int>
	<byte-pos> := 0..7 | 0..63
	<bit-pos>  := 0..7 | 0..512
	<hex-digit> [0..9a..fA..F]
	<hex> := '0x' <hex-digit>+
	<hex2> := '0x' (<hex-digit> | <hex-digit><hex-digit>)
	<int> := [0..9]+
	<name> = <char>+
	<constant> = <int> | <hex>

# Normalisation

To get a reasonable output from the above expression and
statements only one "definition" ('=') per output should be
given or one for ON (OUT '<') and one for OFF (!OUT '<') should
be given

For example two definitions of output A

    A = Y ; (U, V)
	A = X

is not reasonable but

    A < Y ; (U, V)
	!A < X
	
may be, in this case there are situations where A
is neither turn on of turned off.

    A < X
	A < Y

Can be rewritten as

	A < X; Y

and

	A < X
	!A < !X

can be rewritten to

    A = X
	
# Interpretation of old single line or multiple lines

The current output is just one single line, for example

    0x218 2 0x40 0x40 0x00

The interpretation of this line is, where DEFOUT is the,
currently, only output.

	DEFOUT = 0x218[2,1]

But for multiple bits the story is a bit different

    0x218 2 0x30 0x30 0x00

This should be interpreted as 

	DEFOUT = 0x218[2,2], 0x218[2,3]

But in reality it is interpreted as

	DEFOUT = 0x218[2,2]; 0x218[2,3]
	
The suggestion is to interpret multiple bits selected bits as an OR
expression (;) so two lines like

    0x218 2 0x40 0x40 0x00
    0x219 3 0xC0 0xC0 0x00
	
Is coded as

	DEFOUT = 0x218[2,1]; 0x219[3,0]; 0x219[3,1]
	
# Extension to Analog

FIXME: add signed/unsigned/little/big

    <a-statement> := 
		| <statement>
		| <a-declaration>
		| <name> '=' <a-expr> '?' <a-cond>

	<a-declaraion> := 
		| <declaration>
		| '#' 'analog' <name> [':'<size>] [<iodir>] [<port>':'] <pin>
		| '#' 'can' <name> <can-range>

	<a-cond> := 
	  | '(' <a-cond> ')'
	  | <a-cond> ',' <a-cond>
	  | <a-cond> ';' <a-cond>
	  ! '!' <a-cond>
	  | <a-expr> '==' <a-expr>
	  | <a-expr> '!=' <a-expr>
      | <a-expr> '<' <a-expr>
      | <a-expr> '<=' <a-expr>
      | <a-expr> '>' <a-expr>
      | <a-expr> '>=' <a-expr>

    <a-expr> :=
	     <a-value>
	  | '(' <a-expr> ')'
	  | '-' <a-expr>
	  | <a-expr> '+' <a-expr>
      | <a-expr> '-' <a-expr>
      | <a-expr> '*' <a-expr>
      | <a-expr> '/' <a-expr>
      | <a-expr> '%' <a-expr>
	  
	<a-value> :=
	  | <bit>
	  | <can-range>
	  | <name>
	  | <constant>

	<can-range> := <frame-id> '[' <bit-pos> '..' <bit-pos> ']'
    <iodir> := 'in' | 'out' | 'inout'	

# Usage of Analog expressions

Now we can set relay output or analog output
depending on input expressions

    #can SPEED 0x100[0..7]   // 8 bit from can frame 0x100
	#can RPM   0x200[6..15]  // 10 bit from can bit 6
	#digital HEADLIGHT 0:8   // relay output at port 0 pin 8
	
	HEADLIGHT=1 ? (SPEED > 100), (RPM > 1000)
	HEADLIGHT=0 ? (SPEED < 20)
	
Analog output can, currently, be set to specific value

	#analog A 3:10
	
    A=0 ? Cond0
    A=1 ? Cond1
    A=2 ? Cond2
	...
	
# How to execute CandySpeak

The execution is simply top down run every rule for every input as often 
as needed. However, an output should only be set once per execution.
In the above example with analog assignment, if forexample Cond0 is false
and Cond1 and Cond2 are true the output A=1 will be sent and Cond2 will
not be checked.

# Extension to Timer

    <t-statement> := 
		| <statement>
		| <t-declaration>
		|  <name> '=' 1 '?' <t-cond>     // start timer if
		| !<name> '=' 0 '?' <t-cond>     // stop timer if

	<t-declaraion> := 
		| <declaration>
		| '#' 'timer' <name> <milliseconds>
		
     <t-cond> := 
	     | <name>             // timer name (running)

Timer <name> is true when timer (has been) running and is has a 
timeout condition.
	
    #timer    debounce_timer 200
	#digital  button in  13
	#digital  led    out 2
	#variable led_state = 0
	
	// start timer when button is pressed	
	debounce-timer = 1 ? button
	// led=on when timeout and button is still pressed
	led-state = (led_state+1) % 2 ? debounce_timer, button
	led = led-state

# Tick program

	#variable A:10 = 1   // declare a 10 bit variable A
	#timer T1 1000       // declare a 1000ms (1s) timer 
	A = A + 1 ? T1       // set A = A + 1 when timer T1 times out
	T1 ? T1              // restart T1 when T1 times out
	>T1 = 1              // start the timer T1

# loop from 1 to 10

	#variable A:10 = 0
	A = A + 1 ? (A < 10)
