%% -*- erlang -*-

Terminals
        newline decnum octnum binnum hexnum name digital can analog
        '=' '<' '!' '#' '(' ')' '[' ']' ',' ';' ':'
        .

Nonterminals
        file statement declaration cond bit canbit hex int
        .

Rootsymbol file.
Endsymbol '$end'.

Left 100 ';'.
Left 200 ','.
Unary 300 '!'. 


file -> statement newline : ['$1'].
file -> statement newline file : ['$1'|'$3'].

statement -> hex int int int int : {can,'$1','$2','$3','$4'}.
statement -> declaration : '$1'.
statement -> name '=' cond  : {'=','$1','$3'}.
statement -> name '<' cond  : {'<','$1','$3'}.
statement -> '!' name '<' cond : {'!<','$1','$3'}.

declaration -> '#' 'digital' name int : {'#',digital,'$3',0,'$4'}.
declaration -> '#' 'digital' name int ':' int : {'#',digital,'$3','$4','$6'}.
declaration -> '#' 'can' name canbit : {'#',can,'$3','$4'}.

cond -> bit : '$1'.
cond -> '!' cond : {'!','$2'}.
cond -> '(' cond ')' : '$2'.
cond -> cond ',' cond : {'and','$1','$3'}.
cond -> cond ';' cond : {'or','$1','$3'}.

bit -> int : '$1'.
bit -> canbit : '$1'.
bit -> name : '$1'.
		
canbit -> hex int int int int : {can,'$1','$2','$3','$4'}.
canbit -> hex '[' int ']' : {can,'$1','$3'}.
canbit -> hex '[' int ',' int ']' : {can,'$1','$3','$4'}.

hex -> hexnum : '$1'.
    
int -> decnum : '$1'.
int -> binnum : '$1'.
int -> octnum : '$1'.
int -> hexnum : '$1'.
