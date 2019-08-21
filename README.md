CANDY
=====

Candy is a can frame viewer that can show live can frames,
at a reasonable speed.

epx is used to diplay the can frames in a format that look something like

    +--+----+ +-+-+-+-+ +--+--+--+--+--+--+--+--+
    |ID|Freq| |X|R|E|L| |01|02|03|04|05|06|07|08|
    +--+----+ +-+-+-+-+ +--+--+--+--+--+--+--+--+

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

This will show a blank window with a cyan background.
candy initialize the can_udp backend by default. In the shell you
may add other can backends at will. Or backends may be added by
configuration file (README can)

# Commands

Key commands used on selected elements:

    X              Hexa decimal format
    D              Decimal format
    B              Binary format
    O              Octal format
    ---
    G              Group selected bits
    Shift+G        Ungroup selected bits
    1-8            Split in groups of 1 to 8 bits
    Ctrl+S         Save information to $HOME/candy.txt

Other key commands

    Q              Quit application
    Up             Arrow up, scroll up
    Down           Arrow down, scroll down
    PageUp         Page up, scroll one page up
    PageDown       Page down, scroll one page down
	Alt-Up         circulate bitrate up  (125K,250K,500K)
	Alt-Down       circulate bitrate down (500K,250K,125K)

# candy.txt format

The candy.txt file is generate with hitting Ctrl-S, if
a bit group is selected then that bit will be saved
in the following format.

## single selection

    <frame-id> <byte-index> <byte-mask> <match-on> <match-off>

Example

    0x123 4 0x20 0x20 0x00

Lines not starting with a digit are assumed to be comments
and are ignores. A decent choice of leading characters for
comments could be a space or a ';'.

## multiple selection

    <frame-id1> <byte-index> <byte-mask> <match-on> <match-off> ...
    <frame-id2> <byte-index> <byte-mask> <match-on> <match-off> ...
    ...
