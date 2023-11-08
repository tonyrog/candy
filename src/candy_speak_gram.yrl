%% -*- erlang -*-

Terminals
        newline decnum octnum binnum hexnum name 
        digital can analog timer variable constant timeout in out inout
        reset push pop save list clear
        '=' '!' '#' '(' ')' '[' ']' '&&' '||' ',' ';' ':' '.' '?'
        '==' '!=' '<' '<=' '>' '>='
        '-' '+' '*' '/' '%'
        '&' '|' '^' '<<' '>>' '~'
        .

Nonterminals
        file statement declaration rule condition immediate
        expr var_expr candy_bit candy_range 
        bit_size port_pin iodir frame_id hex int neg
        .

Rootsymbol file.
Endsymbol '$end'.

Unary 1100 neg. 
Unary 1050 '!' '~'.
Left 1000 '*' '/' '%'.
Left 900  '+' '-'.
Left 800  '<<' '>>'.
Left 700  '<' '<=' '>' '>='.
Left 600  '==' '!='.
Left 500  '&'.
Left 400  '^'.
Left 300  '|'.

Left 10   '||'.
Left 20   '&&'.

neg -> '-' : '$1'.
     

file -> statement newline : ['$1'].
file -> statement ';' : ['$1'].
file -> statement newline file : ['$1'|'$3'].
file -> statement ';' file : ['$1'|'$3'].
file -> newline file : '$2'.

statement -> hex int int int int : {can,'$1','$2','$3','$4'}.
statement -> declaration : '$1'.
statement -> rule : '$1'.
statement -> immediate : '$1'.
    
declaration ->
    '#' 'digital' name bit_size iodir port_pin : 
	{'#',digital,'$3','$4','$5','$6'}.
declaration ->
    '#' 'analog' name bit_size iodir port_pin :
	{'#',analog,'$3','$4','$5','$6'}.
declaration ->
    '#' 'variable' name bit_size var_expr :
	{'#',variable,'$3','$3','$3'}.
declaration ->
    '#' 'constant' name bit_size '=' expr :
	{'#',constant,'$3','$4','$6'}.
declaration ->
    '#' 'timer' name expr :
	{'#',timer,'$3','$4'}.
declaration ->
    '#' 'can' name candy_bit : 
	{'#',can,'$3','$4'}.
declaration ->
    '#' 'can' name candy_range : 
	{'#',can,'$3','$4'}.

bit_size -> ':' int : '$2'.
bit_size -> '$empty' : default.

var_expr -> '=' expr : '$2'.
var_expr -> '$empty' : undefined.

port_pin -> int ':' int : {'$1','$3'}.
port_pin -> int : {default,'$1'}.

iodir -> in    : '$1'.
iodir -> out   : '$1'.
iodir -> inout : '$1'.

immediate -> '>' name '=' expr  : {set,'$2','$4'}.
immediate -> '>' name       : {get,'$2'}.
immediate -> '>' reset      : {reset,'$2'}.
immediate -> '>' push       : {push,'$2'}.
immediate -> '>' pop        : {pop,'$2'}.
immediate -> '>' save       : {save,'$2'}.
immediate -> '>' list       : {list,'$2'}.
immediate -> '>' clear      : {clear,'$2'}.
    
rule -> name '=' expr '?' condition  : {rule,'$1','$3','$5'}.

condition -> expr : '$1'.
condition -> '!' condition :
		 case '$2' of
		     {'!', Cond} -> Cond;
		     Cond -> {'!',Cond}
		 end.
condition -> '(' condition ')' : '$2'.
condition -> expr '==' expr : {'==','$1','$3'}.
condition -> expr '!=' expr : {'!=','$1','$3'}.
condition -> expr '<=' expr : {'<=','$1','$3'}.
condition -> expr '<' expr : {'<','$1','$3'}.
condition -> expr '>=' expr : {'>=','$1','$3'}.
condition -> expr '>' expr : {'>','$1','$3'}.
condition -> condition '&&' condition : {'and','$1','$3'}.
condition -> condition '||' condition : {'or','$1','$3'}.

expr -> int         : '$1'.
expr -> candy_bit   : '$1'.
expr -> name        : '$1'.
expr -> candy_range : '$1'.
expr -> neg expr    : {'-', '$2'}.
expr -> '~' expr    : {'~', '$2'}.
expr -> expr '+' expr : {'+','$1','$3'}.
expr -> expr '-' expr : {'-','$1','$3'}.
expr -> expr '*' expr : {'*','$1','$3'}.
expr -> expr '/' expr : {'/','$1','$3'}.
expr -> expr '%' expr : {'%','$1','$3'}.

expr -> expr '&' expr : {'&','$1','$3'}.
expr -> expr '|' expr : {'|','$1','$3'}.
expr -> expr '^' expr : {'^','$1','$3'}.
expr -> expr '<<' expr : {'<<','$1','$3'}.
expr -> expr '>>' expr : {'>>','$1','$3'}.
expr -> 'timeout' '(' name ')' : {timeout, '$3'}.
    
candy_bit -> frame_id int int int int     : {candy_bit,'$1','$2','$3','$4'}.
candy_bit -> frame_id '[' int ']'         : {candy_bit1,'$1','$3'}.
candy_bit -> frame_id '[' int ',' int ']' : {candy_bit2,'$1','$3','$4'}.

candy_range -> frame_id '[' int ':' int ']' :
		   {candy_range, '$1', '$3', '$5'}.
candy_range -> frame_id '[' int '.' '.' int ']' :
		   {candy_range, '$1', ('$3'-'$1')+1, '$6'}.

frame_id -> hex : '$1'.
    
hex -> hexnum : '$1'.

int -> decnum : '$1'.
int -> binnum : '$1'.
int -> octnum : '$1'.
int -> hexnum : '$1'.
