// Scanner and Parser for Candy
#ifndef __CANDY_PARSE_H__
#define __CANDY_PARSE_H__

#include <stdint.h>
#include <ctype.h>

// Use unicode numbers?
enum {
    T_END                = 0,
    T_STAR               = '*',
    T_PLUS               = '+',
    T_MINUS              = '-',
    T_SLASH              = '/',
    T_PERCENT            = '%',
    T_AMPERSAND          = '&',
    T_CIRKUMFLEX         = '^',
    T_VERTICAL_BAR       = '|',
    T_LESS_THAN_SIGN     = '<',
    T_GREATER_THAN_SIGN  = '>',
    T_EQUALS_SIGN        = '=',
    T_EXCLAMATION_MARK   = '!',
    T_TILDE              = '~',
    T_LEFT_PARANTHESES   = '(',
    T_RIGHT_PARANTHESES  = ')',
    T_LEFT_BRACKET       = '[',
    T_RIGHT_BRACKET      = ']',
    T_QUESTION_MARK      = '?',
    T_COLON              = ':',
    T_COMMA              = ',',
    T_FULL_STOP          = '.',
    T_DOUBLE_AMPERSAND           = 256,   // &&
    T_DOUBLE_VERTICAL_BAR        = 257,   // ||
    T_DOUBLE_LESS_THAN_SIGN      = 258,   // <<
    T_LESS_THAN_OR_EQUAL_SIGN    = 259,   // <=
    T_DOUBLE_GREATER_THAN_SIGN   = 260,   // >>
    T_GREATER_THAN_OR_EQUAL_SIGN = 261,   // >=
    T_DOUBLE_EQUALS_SIGN         = 262,   // ==
    T_NOT_EQUALS_SIGN            = 263,   // !=    
    T_DEC      = 264,   // [-](0-9)+
    T_HEX      = 265,   // [-]0x(0-9A-Fa-f)+
    T_NEGATE   = 266,   // - (unary)    
    // words/keywords
    T_WORD     = 267,   // [A-Za-z_@][0-9A-Za-z_@]
    T_DIGITAL  = 268,   // #digital
    T_ANALOG   = 269,   // #analog
    T_TIMER    = 270,   // #timer
    T_CAN      = 271,   // #can
    T_VARIABLE = 272,   // #variable
    T_CONSTANT = 273,   // #constant
    T_IN       = 274,   // in
    T_OUT      = 275,   // out
    T_INOUT    = 276,   // inout
    T_TIMEOUT  = 277,   // timeout ( t )
    T_RESET    = 278,   // reset
    T_PUSH     = 279,   // push
    T_POP      = 280,   // pop
    T_SAVE     = 281,   // save
    T_LIST     = 282,   // list
    T_CLEAR    = 283,   // clear
};

typedef struct {
    const char* ptr;  // pointer into input buffer
    uint16_t len;     // string length
    uint16_t tval;    // token value (char | 256..263..)
} token_t;

static int candy_precedence(uint16_t tval)
{
    switch(tval) {
    case T_EXCLAMATION_MARK: return 105;
    case T_TILDE: return 105;
    case T_NEGATE:  return 105; // special unary minus
    case T_STAR: return 100;
    case T_SLASH: return 100;
    case T_PERCENT:  return 100;	
    case T_PLUS: return 90;
    case T_MINUS: return 90;
    case T_DOUBLE_LESS_THAN_SIGN: return 80;
    case T_DOUBLE_GREATER_THAN_SIGN: return 80;

    case T_LESS_THAN_SIGN: return 70;
    case T_LESS_THAN_OR_EQUAL_SIGN: return 70;	
    case T_GREATER_THAN_SIGN: return 70;
    case T_GREATER_THAN_OR_EQUAL_SIGN: return 70;
    case T_DOUBLE_EQUALS_SIGN: return 60;
    case T_NOT_EQUALS_SIGN: return 60;
	
    case T_AMPERSAND: return 50;
    case T_CIRKUMFLEX: return 40;	
    case T_VERTICAL_BAR: return 30;
    case T_DOUBLE_AMPERSAND: return 20;
    case T_DOUBLE_VERTICAL_BAR: return 10;
    case T_COMMA: return 1;
    default: return -1;
    }
}

// return -1 for left assoc, 0 for no assoc and 1 for right assoc
static int candy_assoc(uint16_t tval)
{
    switch(tval) {
    case T_EXCLAMATION_MARK: return 1;
    case T_TILDE: return 1;
    case T_NEGATE: return 1;
    case T_STAR: return -1;
    case T_PLUS: return -1;
    case T_MINUS: return -1;
    case T_SLASH: return -1;
    case T_PERCENT: return -1;
    case T_DOUBLE_LESS_THAN_SIGN: return -1;	
    case T_DOUBLE_GREATER_THAN_SIGN: return -1;	
	
    case T_LESS_THAN_SIGN: return -1;
    case T_LESS_THAN_OR_EQUAL_SIGN: return -1;
	
    case T_GREATER_THAN_SIGN: return -1;
    case T_GREATER_THAN_OR_EQUAL_SIGN: return -1;
    case T_DOUBLE_EQUALS_SIGN: return -1;
    case T_NOT_EQUALS_SIGN: return -1;

    case T_AMPERSAND: return -1;
    case T_CIRKUMFLEX: return -1;
    case T_VERTICAL_BAR: return -1;
	
    case T_DOUBLE_AMPERSAND: return -1;
    case T_DOUBLE_VERTICAL_BAR: return -1;

    default: return 0;
    }
}

// return instruction from token
static int candy_instr(uint16_t tval)
{
    switch(tval) {
    case T_EXCLAMATION_MARK: return EXPR_NOT;
    case T_TILDE: return EXPR_BNOT;
    case T_STAR: return EXPR_TIMES;
    case T_PLUS: return EXPR_PLUS;
    case T_MINUS: return EXPR_MINUS;
    case T_NEGATE: return EXPR_NEG;
    case T_SLASH: return EXPR_DIVIDE;
    case T_PERCENT: return EXPR_REMAINDER;
    case T_DOUBLE_LESS_THAN_SIGN: return EXPR_BSL;	
    case T_DOUBLE_GREATER_THAN_SIGN: return EXPR_BSR;	
	
    case T_LESS_THAN_SIGN: return EXPR_LT;
    case T_LESS_THAN_OR_EQUAL_SIGN: return EXPR_LTE;
	
    case T_GREATER_THAN_SIGN: return EXPR_GT;
    case T_GREATER_THAN_OR_EQUAL_SIGN: return EXPR_GTE;
    case T_DOUBLE_EQUALS_SIGN: return EXPR_EQ;
    case T_NOT_EQUALS_SIGN: return EXPR_NEQ;

    case T_AMPERSAND: return EXPR_BAND;
    case T_CIRKUMFLEX: return EXPR_BXOR;
    case T_VERTICAL_BAR: return EXPR_BOR;
	
    case T_DOUBLE_AMPERSAND: return EXPR_AND;
    case T_DOUBLE_VERTICAL_BAR: return EXPR_OR;

    default: return 0;
    }
}

int is_const(int tok)
{
    return ((tok == T_DEC)||(tok == T_HEX));
}

static int dec_to_int(const char* ptr, int len)
{
    int x = 0, sign = 0;
    if (ptr[0] == '-') {
	sign = 1;
	ptr++;
	len--;
    }
    while(len) {
	x = 10*x + (*ptr - '0');
	ptr++;
	len--;
    }
    return sign ? -x : x;
}

static int hex_to_int(const char* ptr, int len)
{
    int x = 0, sign = 0;
    if (ptr[0] == '-') {
	sign = 1;
	ptr++;
	len--;
    }
    if ((ptr[0] == '0') && (ptr[1] == 'x')) {
	ptr += 2;
	len -= 2;
    }
    while(len) {
	if (isdigit(*ptr))
	    x = 16*x + (*ptr - '0');
	else if (isupper(*ptr))
	    x = 16*x + ((*ptr - 'A')+10);
	else if (islower(*ptr))
	    x = 16*x + ((*ptr - 'a')+10);
	ptr++;
	len--;
    }
    return sign ? -x : x;
}


static int tok_to_int(token_t* tp)
{
    if (tp->tval == T_HEX)
	return hex_to_int(tp->ptr, tp->len);
    else if (tp->tval == T_DEC)
	return dec_to_int(tp->ptr, tp->len);
    return -1;
}

// scan next token return pointer after the token found
static char* candy_tok0(char* p, token_t* tp)
{
    int i = 1;
    uint16_t tv;
    
    switch(p[0]) {
    case '-':
	if ((p[1]=='0') && (p[2]== 'x')) { // hex
	    i = 3;
	    goto hex;
	}
	if (isdigit(p[1])) {
	    i = 2;
	    goto dec;
	}
	tv = T_MINUS;
	break;
    case '*': tv = T_STAR; break;
    case '+': tv = T_PLUS; break;
    case '/': tv = T_SLASH; break;
    case '%': tv = T_PERCENT; break;
    case '&':
	if (p[1] == '&') { tv = T_DOUBLE_AMPERSAND; goto two_char; }
	tv = T_AMPERSAND;
	break;
    case '|':
	if (p[1] == '|') { tv = T_DOUBLE_VERTICAL_BAR; goto two_char; }
	tv = T_VERTICAL_BAR;
	break;
    case '^': tv = T_CIRKUMFLEX; break;
    case '<':
	switch(p[1]) {
	case '<': tv = T_DOUBLE_LESS_THAN_SIGN; goto two_char;
	case '=': tv = T_LESS_THAN_OR_EQUAL_SIGN; goto two_char;
	default: tv = T_LESS_THAN_SIGN; break;
	}
	break;
    case '>':
	switch(p[1]) {
	case '>': tv = T_DOUBLE_GREATER_THAN_SIGN; goto two_char;
	case '=': tv = T_GREATER_THAN_OR_EQUAL_SIGN; goto two_char;
	default: tv = T_GREATER_THAN_SIGN; break;
	}
	break;
    case '=':
	switch(p[1]) {
	case '=': tv = T_DOUBLE_EQUALS_SIGN; goto two_char;
	default: tv = T_EQUALS_SIGN; break;
	}
	break;
    case '!':
	switch(p[1]) {
	case '=': tv = T_NOT_EQUALS_SIGN; goto two_char;
	default: tv = T_EXCLAMATION_MARK; break;
	}
	break;
    case '~': tv = T_TILDE; break;
    case '(': tv = T_LEFT_PARANTHESES; break;
    case ')': tv = T_RIGHT_PARANTHESES; break;
    case '[': tv = T_LEFT_BRACKET; break;
    case ']': tv = T_RIGHT_BRACKET; break;
    case '0':
	if (p[1]== 'x') { // hex
	    i = 2;
	    goto hex;
	}
	i = 1;
	goto dec;
    default:
	if (isdigit(p[0])) {
	    i = 1;
	    goto dec;
	}
	else if (isalpha(p[0]) || (p[0]=='_') || (p[0]=='@')) {
	    i = 1;
	    while(isalnum(p[i]) || (p[i]=='_') || (p[i]=='@'))
		i++;
	    tp->tval = T_WORD;
	    return p+i;
	}
	tv = p[0];
	break;
    }
//one_char:
    tp->tval = tv;
    return p+1;
two_char:
    tp->tval = tv;
    return p+2;    
dec:	    
    while (isdigit(p[i]))
	i++;
    tp->tval = T_DEC;
    return p+i;
hex:
    while (isxdigit(p[i]))
	i++;
    tp->tval = T_HEX;
    return p+i;
}

static int tokeq(const char* name, token_t* tp)
{
    const char* tptr = tp->ptr;
    int len = tp->len;
    while(len) {
	if (*name != *tptr)
	    return 0;
	name++;
	tptr++;
	len--;
    }
    if ((len == 0) && (*name == '\0'))
	return 1;
    return 0;
}

static char* candy_tok(char* p, token_t* tp)
{
    tp->ptr = p;
    p = candy_tok0(p, tp);
    tp->len = p - tp->ptr;
    if (tp->tval==T_WORD) {
	if (tokeq("digital", tp))
	    tp->tval = T_DIGITAL;
	else if (tokeq("can", tp))
	    tp->tval = T_CAN;
	else if (tokeq("analog", tp))
	    tp->tval = T_ANALOG;
	else if (tokeq("timer", tp))
	    tp->tval = T_TIMER;
	else if (tokeq("variable", tp))
	    tp->tval = T_VARIABLE;
	else if (tokeq("constant", tp))
	    tp->tval = T_CONSTANT;
	else if (tokeq("in", tp))
	    tp->tval = T_IN;
	else if (tokeq("out", tp))
	    tp->tval = T_OUT;
	else if (tokeq("inout", tp))
	    tp->tval = T_INOUT;
	else if (tokeq("timeout", tp))
	    tp->tval = T_TIMEOUT;	
	else if (tokeq("reset", tp))
	    tp->tval = T_RESET;
	else if (tokeq("push", tp))
	    tp->tval = T_PUSH;
	else if (tokeq("pop", tp))
	    tp->tval = T_POP;
	else if (tokeq("save", tp))
	    tp->tval = T_SAVE;
	else if (tokeq("list", tp))
	    tp->tval = T_LIST;
	else if (tokeq("clear", tp))
	    tp->tval = T_CLEAR;
    }    
    return p;
}


int candy_scan_line(token_t* ts, char* ptr)
{
    int i = 0;
next:
    while(isblank(*ptr))
	ptr++;
    if ((ptr[0]=='/') && (ptr[1]=='/')) {
	ts[i].tval = T_END;
	ts[i].len = 0;
	ts[i].ptr = "end";
	return i;
    }
    else if ((*ptr == '\n') || (*ptr == '\0')) {
	ts[i].tval = T_END;
	ts[i].len = 0;
	ts[i].ptr = "end";
	return i;
    }
    ptr = candy_tok(ptr, &ts[i]);
    i++;
    goto next;
}

#ifdef DEBUG

void print_tokens(token_t* ts, int n)
{
    int i;
    for (i = 0; i < n; i++) {
	switch(ts[i].tval) {
	case T_STAR: candy_print_char('*'); break;
	case T_PLUS: candy_print_char('+'); break;
	case T_MINUS: candy_print_char('-'); break;
	case T_SLASH: candy_print_char('/'); break;
	case T_PERCENT: candy_print_char('%'); break;
	case T_AMPERSAND: candy_print_char('&'); break;
	case T_CIRKUMFLEX: candy_print_char('^'); break;
	case T_VERTICAL_BAR: candy_print_char('|'); break;
	case T_LESS_THAN_SIGN: candy_print_char('<'); break;
	case T_GREATER_THAN_SIGN: candy_print_char('>'); break;
	case T_EQUALS_SIGN: candy_print_char('='); break;
	case T_EXCLAMATION_MARK: candy_print_char('!'); break;
	case T_TILDE: candy_print_char('~');  break;
	case T_LEFT_PARANTHESES: candy_print_char('('); break;
	case T_RIGHT_PARANTHESES: candy_print_char(')'); break;
	case T_LEFT_BRACKET: candy_print_char('['); break;
	case T_RIGHT_BRACKET: candy_print_char(']'); break;
	case T_QUESTION_MARK: candy_print_char('?'); break;
	case T_COLON: candy_print_char(':'); break;
	case T_COMMA: candy_print_char(','); break;
	case T_FULL_STOP: candy_print_char('.'); break;
	case T_DOUBLE_AMPERSAND: candy_print_str("&&"); break;
	case T_DOUBLE_VERTICAL_BAR: candy_print_str("||"); break;
	case T_DOUBLE_LESS_THAN_SIGN: candy_print_str("<<"); break;
 	case T_LESS_THAN_OR_EQUAL_SIGN: candy_print_str("<="); break;
	case T_DOUBLE_GREATER_THAN_SIGN: candy_print_str(">>"); break;
	case T_GREATER_THAN_OR_EQUAL_SIGN: candy_print_str(">="); break;
	case T_DOUBLE_EQUALS_SIGN: candy_print_str("=="); break;
	case T_NOT_EQUALS_SIGN: candy_print_str("!="); break;
	case T_DEC:
	    candy_print_str(";DEC:");
	    candy_print_int(dec_to_int(ts[i].ptr,ts[i].len));
	    break;	    
	case T_HEX:
	    candy_print_str(";HEX:");
	    candy_print_hex(hex_to_int(ts[i].ptr,ts[i].len));
	    break;
	case T_WORD:
	    candy_print_str(";WORD:'");
	    candy_print_str_len(ts[i].ptr, ts[i].len);
	    candy_print_str("'");
	    break;
	case T_DIGITAL: candy_print_str(";#DIGITAL:"); break;
	case T_ANALOG: candy_print_str(";#ANALOG:");break;
	case T_TIMER: candy_print_str(";#TIMER:");break;
	case T_CAN: candy_print_str(";#CAN:"); break;
	case T_VARIABLE: candy_print_str(";#VARIABLE:"); break;
	case T_CONSTANT: candy_print_str(";#CONSTANT:"); break;
	case T_IN: candy_print_str(" IN "); break;
	case T_OUT:candy_print_str(" OUT "); break;
	case T_INOUT: candy_print_str(" INOUT "); break;
	case T_TIMEOUT: candy_print_str(" TIMEOUT "); break;
	case T_RESET: candy_print_str(" RESET "); break;
	case T_PUSH: candy_print_str(" PUSH "); break;
	case T_POP: candy_print_str(" POP "); break;
	case T_SAVE: candy_print_str(" SAVE "); break;
	case T_LIST: candy_print_str(" LIST "); break;
	case T_CLEAR: candy_print_str(" CLEAR "); break;
	default:
	    candy_print_str(";CHAR:'");
	    candy_print_char(*ts[i].ptr);
	    candy_print_str("'");
	    break;
	}
    }
    if (ts[n].tval == T_END)
	printf(";END");
    printf("\n");
}
#endif

#if 0

// RPN execution style
// word: two bit low bit tag
//    00: element index! variable/constant/can...
//    01: integer / number
//    10: operator (EXPR_...)
//    11: word/function
//
#define TAG_ELEM  0
#define TAG_INT30 1
#define TAG_INSTR 2
#define MAX_PARSE_STACK_DEPTH 16

static int candy_parse_expr(char* p)
{
    token_t tok;
    int ip = ncodes;
    int pstack[MAX_PARSE_STACK_DEPTH];
    int pp = 0;
    int ptval = 0;  // previous token value
    int lbl = 0;
    tok.tval = T_END;
next:
    ptval = tok.tval;  // keep previous token value
    p = candy_tok(p, &tok);
again:
    switch(tok.tval) {
    case T_HEX: {
	int value = hex_to_int(tok.ptr, tok.len);
	code[ip++] = (value<<2)|TAG_INT30;
	break;
    }
    case T_DEC: {
	int value = dec_to_int(tok.ptr, tok.len);
	code[ip++] = (value<<2)|TAG_INT30;
	break;
    }
    case T_WORD: {
	eindex_t ei;
	if ((ei = lookup_element(&tok)) == INVALID_INDEX)
	    return -1;
	code[ip++] = (ei<<2)|TAG_ELEM;
	break;
    }
    case T_LEFT_PARANTHESES:
	pstack[pp++] = T_LEFT_PARANTHESES;
	break;
    case T_RIGHT_PARANTHESES:
	if (pp == 0) return -1;
	while(pp && (pstack[pp-1] != T_LEFT_PARANTHESES)) {
	    code[ip++] = (pstack[pp-1]<<2)|TAG_INSTR;
	    pp--;
	}
	break;
    case T_COMMA:
	if (pp == 0) return -1;
	while(pp && (pstack[pp-1] != T_LEFT_PARANTHESES)) {
	    code[ip++] = (pstack[pp-1]<<2)|TAG_INSTR;
	    pp--;
	}
	pstack[pp++] = T_LEFT_PARANTHESES;
	break;
    case T_MINUS:
	if (((ptval != T_RIGHT_PARANTHESES) &&
	     (candy_precedence(ptval) != -1)) ||
	    ((ptval != T_DEC) && (ptval != T_HEX) && (ptval != T_WORD)) ||
	    (ip == ncodes)) {
	    tok.tval = T_NEGATE;
	    goto again;
	}
	// fall through
    default:
	if (pp > 0) {
	    int p1 = candy_precedence(tok.tval);
	    int p2 = candy_precedence(pstack[pp-1]);
	    if ( ((p2 > p1) && (pstack[pp-1] != T_LEFT_PARANTHESES)) ||
		 ((p2 == p1) && (candy_assoc(pstack[pp-1]) < 0))) {
		code[ip++] = (pstack[--pp]<<2)|TAG_INSTR;
	    }
	    else {
		if (tok.val == T_DOUBLE_AMPERSAND) {
		    int l = label++;
		    code[ip++] = EXPR_JZ;
		    code[ip++] = l;
		    
		    lstack[l] = pp;
		    pstack[pp++] = tok.val;

		}
		else if (tok.val == T_DOUBLE_VERTICAL_BAR) {
		}
		else {
		    pstack[pp++] = tok.val;
		}
	    }
	}
    }
}

#endif



#endif
