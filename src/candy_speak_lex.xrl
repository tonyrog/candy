%% -*- erlang -*-
%% CandySpeak scanner
%%

Definitions.

B	= [0-1]
D	= [0-9]
L       = [a-zA-Z_\$]
A       = ({L}|{U})
H	= [a-fA-F0-9]
E	= [Ee][+-]?{D}+
FS	= (f|F|l|L)
IS	= (u|U|l|L)*
WS      = [\t|\s|\r]

Rules.

%% keywords
can		   : {token,{can,TokenLine}}.
digital		   : {token,{digital,TokenLine}}.
analog		   : {token,{analog,TokenLine}}.
variable	   : {token,{variable,TokenLine}}.
constant	   : {token,{constant,TokenLine}}.
in		   : {token,{in,TokenLine}}.
out		   : {token,{out,TokenLine}}.
inout      	   : {token,{inout,TokenLine}}.
timeout      	   : {token,{timeout,TokenLine}}.
reset      	   : {token,{reset,TokenLine}}.
push      	   : {token,{push,TokenLine}}.
pop      	   : {token,{pop,TokenLine}}.
save      	   : {token,{save,TokenLine}}.
list      	   : {token,{list,TokenLine}}.
clear      	   : {token,{clear,TokenLine}}.

{L}({L}|{D})*	   : {token,{name,TokenLine,TokenChars}}.
0[xX]{H}+{IS}?     : {token,{hexnum,TokenLine,TokenChars}}.
0[b]{B}+{IS}?      : {token,{binnum,TokenLine,TokenChars}}.
0{D}+{IS}?         : {token,{octnum,TokenLine,TokenChars}}.
{D}+{IS}?          : {token,{decnum,TokenLine,TokenChars}}.

==                 : {token,{'==',TokenLine}}.
!=                 : {token,{'==',TokenLine}}.
<=                 : {token,{'<=',TokenLine}}.
>=                 : {token,{'>=',TokenLine}}.

<<		    : {token,{'<<',TokenLine}}.
>>		    : {token,{'>>',TokenLine}}.

=		    : {token,{'=',TokenLine}}.
<		    : {token,{'<',TokenLine}}.
!		    : {token,{'!',TokenLine}}.
#		    : {token,{'#',TokenLine}}.
-		    : {token,{'-',TokenLine}}.
\+		    : {token,{'+',TokenLine}}.
/		    : {token,{'/',TokenLine}}.
\%		    : {token,{'%',TokenLine}}.
\*		    : {token,{'*',TokenLine}}.
\?		    : {token,{'?',TokenLine}}.

&&	            : {token,{'&&',TokenLine}}.
&		    : {token,{'&',TokenLine}}.
||		    : {token,{'||',TokenLine}}.
|		    : {token,{'|',TokenLine}}.
\^		    : {token,{'^',TokenLine}}.
\~		    : {token,{'~',TokenLine}}.

\(		    : {token,{'(',TokenLine}}.
\)		    : {token,{')',TokenLine}}.
\[		    : {token,{'[',TokenLine}}.
\]		    : {token,{']',TokenLine}}.
,		    : {token,{',',TokenLine}}.
;		    : {token,{';',TokenLine}}.
:		    : {token,{':',TokenLine}}.
\n                  : {token,{newline,TokenLine}}.
{WS}+		    : skip_token .

Erlang code.
