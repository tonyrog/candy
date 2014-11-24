CANDY
=====

Candy is a can frame viewer that can show live can frames,
at a reasonable speed.

epx is used to diplay the can frames in a format that look something like

    +------+ +-+ +-+ +-+ +-+ +--+ +--+ +--+ +--+ +--+ +--+ +--+ +--+
    | ID   | |X| |R| |E| |L| |01| |02| |03| |04| |05| |06| |07| |08|
    +------+ +-+ +-+ +-+ +-+ +--+ +--+ +--+ +--+ +--+ +--+ +--+ +--+

The ID field is in hex and is 3 hex digits wide (11 bits) if the 
basic frame format is used. If the frame is in extended frame format (X flag) 
then 8 hex digits are used (29 bits).
The X flag signals extended frame format and R is the request for transmission
flag. E is the error frame marker, error frames are always produced by 
the local can stack it self. 
L is a length indication in range 0-8.

The rest of the fields are grouped into 8 groups of 8 bits each.
Clicking on the group will toggle the base from 16->10->2->8->16.
This make it simple to change format when looking for patterns among
the CAN frames.

Internally you can change the order of bits and combine them into bigger
groups etc. Editing the groups graphically on in the window is
on the TODO list.

The easiest way to start candy is:

    erl -s candy

This will create a window and show blank window in cyan background. 
candy initialize the can_udp backend by default. In the shell you
may add other can backends at will.

example:

    > can_usb:start(1, [{device, "/dev/ttyUSB0"},{bitrate, 125000}]).
