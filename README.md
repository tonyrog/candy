CANDY
=====

Candy is a can frame viwer that can show live can frames,
at a reasonable speed. (No overload protection yet)

epx is used to diplay the can frames in a format that look something like

    +------+ +-+ +-+ +-+ +-+ +--+ +--+ +--+ +--+ +--+ +--+ +--+ +--+
    | ID   | |X| |R| |E| |L| |01| |02| |03| |04| |05| |06| |07| |08|
    +------+ +-+ +-+ +-+ +-+ +--+ +--+ +--+ +--+ +--+ +--+ +--+ +--+

The ID field is in hex and is 3 hex digits wide (11 bits) if frame the 
basic frame format is used. if frame is in extended frame format (X flag) 
then 8 hex digits is used (29 bits).
The X, R and E filed are flag fields, infact the top part of the ID field.
The X flag signals extended frame format and R is the request for transmission
flag. E is the error frame marker, error frames are always produced by 
the local can stack it self. 
L is a length indication in range 0-8.

The rest of the fields are grouped into 8 groups of 8 bits each,
clicking on the group will toggle the base from 16->10->2->8->16.
This make it simple to change format when looking for bit like patterns.

Internally you can change the order of bits in comobin into bigger
groups etc. In the TODO list is to be able to edit that on screen
by dragging around and regrouping the bits in some nice way.

The easiest way to start candy is:

    erl -s candy

This will open a window and show blank window in cyan. candy
initialize the can_udp backend by default. in the shell you
may add other can backends at will.

example:

    > can_usb:start(1, [{device, "/dev/ttyUSB0"},{bitrate, 125000}]).
