//
//  Process CANDY SPEAK rules and actions
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>
#include <ctype.h>

typedef uint64_t tick_t;

#define MAX_LINE_LENGTH  256
#define MAX_STRING_TAB   512
#define MAX_NUM_TOKENS   64
#define MAX_NUM_ELEMENTS 32
#define MAX_NUM_EXPRS    32
#define MAX_NUM_RULES    32
#define MAX_XNODES       5

typedef struct {
    uint32_t id;
    int8_t   byte_pos;   // 0..7  | 0..63 | -1
    uint16_t bit_pos;    // 0..7  | 0..512 |
} can_bit_t;

// frame(id)[byte_pos] & bit_mask == 
typedef struct {
    uint32_t id;
    int8_t   byte_pos;  // 0..7  | 0..63 | -1
    uint8_t  mask;
    uint8_t  match1;
    uint8_t  match0;
} can_match_t;

// FrameID '[' pos0 .. pos1 ']  - extract pos1-pos0+1 bits (pos1 >= pos0)
typedef struct {
    uint32_t id;         // Frame ID
    uint16_t bit_pos0;   // 0..7  | 0..512 |
    uint16_t bit_pos1;   // 0..7  | 0..512 |
} can_range_t;

#define IODIR_NONE  0x0
#define IODIR_IN    0x1
#define IODIR_OUT   0x2
#define IODIR_INOUT 0x3

// Digital / Analog port definition
typedef struct {
    uint8_t dir;  // IODIR_xxx
    int8_t port;  // -1 if port is not used
    int8_t pin;   // pin number
} portbit_t;

// Timer
#define C_TIMER_TIMEOUT  0x01
#define C_TIMER_RUNNING  0x02
#define C_TIMER_STOPPED  0x04

typedef struct {
    uint32_t timeout;    
    uint32_t flags;        
    tick_t start_tick;
} ctimer_t;
    
enum {
    T_END = 0,
    T_DEC = 256,       // [-](0-9)+
    T_HEX = 257,       // [-]0x(0-9A-Fa-f)+
    T_WORD = 258,      // [A-Za-z_@][0-9A-Za-z_@]
    T_DIGITAL = 259,   // #digital
    T_ANALOG = 260,   // #analog
    T_TIMER = 261,    // #timer
    T_CAN = 262,       // #can
    T_VARIABLE = 263, // #variable
    T_CONSTANT = 264,    // #constant
    T_IN = 265,         // in
    T_OUT = 266,       // out
    T_INOUT = 267,     // inout
};

typedef struct {
    char* ptr;      // pointer into input buffer
    uint16_t len;   // string length    
    uint16_t tval;  // token value (char | 256..263..)
} token_t;

// Def :=
// '#' 'constant' <name> [:<size>] '=' Value
// '#' 'variable' <name> [:<size>] ['=' Value]
// '#' 'digital'  <name> [<iodir>] [<port> ':'] <pin>
// '#' 'analog'   <name> [<iodir>] [<port> ':'] <pin>
// '#' 'analog'   <name>[:<size>] [IODir] [<port> ':'] <pin>
// '#' 'timer'    <name> <milli-seconds>
// '#' 'can' <name> <frame-id>[7]
// '#' 'can' <name> <frame-id>[0,3]
// '#' 'can' <name> <frame-id>[8..15]
// '#' 'can' <name> <frame-id> <byte-pos> <hex8> <hex8> <hex8>
//
// <hex>   := '0x' [0..9a..bA..F]+
// <dec>   := ['0'..'9']+
// <const> := <dec> | <hex>
// <pin>   := <const>|<name>
// <port>  := <dec>
// <byte-pos> := 0..63
// <bit-pos>  := 0..7 | 0..512
// <name> := Char+
// <iodir> := 'in' | 'out' | 'inout'
// <milli-seconds> := <dec>
//
#define CANDY_ERR  -1   // syntax error | overflow ...
#define CANDY_EMPTY 0   // no tokens
#define CANDY_DEF   1   // #digital|#analog|#timer|#can|#variable
#define CANDY_RULE  2   // <rule>
#define CANDY_EVENT 3   // <rule>

typedef enum {
    C_CONSTANT  = 1,
    C_VARIABLE  = 2,    
    C_DIGITAL   = 3,
    C_ANALOG    = 4,
    C_CAN_BIT1  = 5,   // bitpos
    C_CAN_BIT2  = 6,   // byte & bit
    C_CAN_MATCH = 7,
    C_CAN_RANGE = 8,
    C_TIMER     = 9,
} candy_element_type_t;

typedef struct _candy_element_t {
    char* name;      // str8 name! preceeded by 8-bit length terminated in 0
    uint8_t bn;      // number of bits (1 for digital default=8? for analog)
    uint8_t c_type;  // candy_element_type_t...    
    int32_t value;   // 0..(2^bn)-1, constant/variable/io-buffer value
    union {
	portbit_t   io;
	can_bit_t   can;
	can_match_t canm;
	can_range_t canr;
	ctimer_t    timer;
    };
} candy_element_t;

typedef enum {
    EXPR_NAME,  // analog/digital/variable/timer...
    EXPR_CONST, // constant value
    EXPR_CAN_RANGE,
    EXPR_CAN_BIT, 
    // rel-op
    EXPR_LT,        // A < B
    EXPR_LTE,       // A <= B
    EXPR_GT,        // A > B
    EXPR_GTE,       // A >= B
    EXPR_EQ,        // A == B
    EXPR_NEQ,       // A != B
    // logic-op
    EXPR_NOT,       // !A    
    EXPR_OR,        // A ; B
    EXPR_AND,       // A , B
    // arith-op
    EXPR_NEG,       // -A
    EXPR_PLUS,      // A + B
    EXPR_MINUS,     // A - B
    EXPR_TIMES,     // A * B
    EXPR_DIVIDE,    // A / B
    EXPR_REMAINDER, // A % B
} candy_op_t;

#define INVALID_INDEX 0xffff   // may not be used as index
typedef uint16_t  xindex_t;    // expr index
typedef uint16_t  eindex_t;    // element index

typedef struct _candy_expr_t {
    candy_op_t op;
    union {
	int value;                           // CONST
	eindex_t     ni;                     // NAME element[ni]
	can_range_t  crange;                 // CAN_RANGE
	can_bit_t    cbit;                   // CAN_BIT
	xindex_t     mi;                     // NOT, NEG
	struct {  // bin-op: LT,LTE,GT,GTE,EQ,NEQ,AND,OR...
	    xindex_t li;
	    xindex_t ri;
	} bin;
    };
} candy_expr_t;

typedef enum {
    RULE_SET    = 1,  // set 1 on condition true, set 0 on condition false
    RULE_COND   = 2,  // set 1 on condition true only
    RULE_TOGGLE = 3,  // toggle value on condition true    
    RULE_ASSIGN = 4,  // set Value on condition true
    RULE_EVENT  = 5,   // immediate event
} candy_rule_op_t;

typedef struct _candy_rule_t {
    eindex_t ni;                  // (element-index) NAME
    uint8_t neg;                  // negated NAME 0|1
    uint8_t  op;                 // candy_rule_op_t  '=' '<' '^' ':='
    xindex_t ci;                  // (cond-index)  NAME < Cond
    xindex_t vi;                  // (value-index) NAME := Value < Cond
} candy_rule_t;

token_t ts[MAX_NUM_TOKENS+1];

int nelements = 0;
candy_element_t element[MAX_NUM_ELEMENTS];

int nexprs = 0;
candy_expr_t expr[MAX_NUM_EXPRS];

int nrules = 0;
candy_rule_t rule[MAX_NUM_RULES];

int nstr = 0;
char str[MAX_STRING_TAB];

// current parsed event (must execute immediatly)
int nevents = 0;  // 0 | 1
candy_rule_t event;

int verbose = 0;

#define VERBOSE(...) if (verbose) { printf(__VA_ARGS__); }

#ifdef ARDUINO
#if defined(ARDUINO_ZERO) || defined(ARDUINO_DUE) || defined(ARDUINO_MKR)
#define MAX_ADC_RESOLUTION 12
#define MAX_DAC_RESOLUTION 12
#elif defined(ARDUINO_PORENTA_H7)
#define MAX_ADC_RESOLUTION 16
#define MAX_DAC_RESOLUTION 16
#else
#define MAX_ADC_RESOLUTION 10
#define MAX_DAC_RESOLUTION 10
#endif
#else
#define MAX_ADC_RESOLUTION 10
#define MAX_DAC_RESOLUTION 10
#endif

#ifdef ARDUINO
typedef unsigned long tick_t;

void time_init()
{
}

tick_t time_tick(void)
{
    return millis();
}

uint32_t time_elapsed_ms(tick_t since, tick_t* nowp)
{
    tick_t now = time_tick();
    uint32_t td  = now - since;
    if (nowp) *nowp = now;
    return td;
}
#else

#include <sys/time.h>

struct timeval boot_time;

void time_init()
{
    gettimeofday(&boot_time, 0);
}

tick_t time_tick(void)
{
    struct timeval now;
    struct timeval t;
    gettimeofday(&now, 0);
    timersub(&now, &boot_time, &t);
    return t.tv_sec*1000000 + t.tv_usec;
}

// Must be called with in about half an hour since
uint32_t time_elapsed_ms(tick_t since, tick_t* nowp)
{
    tick_t now = time_tick();
    uint32_t td  = now - since;
    if (nowp) *nowp = now;
    return td / 1000;
}
#endif


char* make_str8(char** pptr, token_t* tp)
{
    char* dst = &str[nstr];
    
    if (nstr+tp->len+2 > MAX_STRING_TAB) {
	printf("stding table full\n");
	return NULL;
    }
    *dst++ = tp->len;
    memcpy(dst, tp->ptr, tp->len);
    *pptr = dst;
    dst[tp->len] = '\0';
    nstr += (tp->len+2);
    return dst;
}

int dec_to_int(char* ptr, int len)
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

int hex_to_int(char* ptr, int len)
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

int tok_to_int(token_t* tp)
{
    if (tp->tval == T_HEX)
	return hex_to_int(tp->ptr, tp->len);
    else if (tp->tval == T_DEC)
	return dec_to_int(tp->ptr, tp->len);
    return -1;
}

int const_to_int(char* ptr, int len)
{
    if ((ptr[0] == '0') && (ptr[1] == 'x'))
	return hex_to_int(ptr, len);
    else if ((ptr[0] == '-') && (ptr[1] == '0') && (ptr[2] == 'x'))
	return hex_to_int(ptr, len);
    else
	return dec_to_int(ptr, len);
}
	
int tokeq(char* name, token_t* tp)
{
    char* tptr = tp->ptr;
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

#define T(tv,i,v) ((tv)[(i)].tval == (v))

eindex_t lookup_name(token_t* tp)
{
    int i = 0;

    while(i < nelements) {
	uint8_t* ptr = element[i].name;
	uint8_t len = *(ptr-1);
	if (tp->len == len) {
	    if (memcmp(tp->ptr, ptr,  len) == 0)
		return i;
	}
	i++;
    }
    return INVALID_INDEX;
}

char* tok0(char* p, token_t* tp)
{
    int i;
    if (p[0]=='-') {
	if ((p[1]=='0') && (p[2]== 'x')) { // hex
	    i = 3;
	    goto hex;
	}
	if (isdigit(p[1])) {
	    i = 2;
	    goto dec;
	}
    }
    else if ((p[0]=='0') && (p[1]== 'x')) { // hex
	i = 2;
	goto hex;
    }
    else if (isdigit(p[0])) {
	i = 1;
	goto dec;
    }
    else if (isalpha(p[0]) || (p[0]=='_') || (p[0]=='@')) {
	i = 1;
	goto word;
    }
    tp->tval = p[0];
    return p+1; 
    
word:
    while(isalnum(p[i]) || (p[i]=='_') || (p[i]=='@'))
	i++;
    tp->tval = T_WORD;
    return p+i;
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

void print_tokens(token_t* ts, int n)
{
    int i;
    for (i = 0; i < n; i++) {
	switch(ts[i].tval) {
	case T_DEC: printf(";DEC:%d", dec_to_int(ts[i].ptr,ts[i].len)); break;
	case T_HEX: printf(";HEX:%x", hex_to_int(ts[i].ptr,ts[i].len)); break;
	case T_DIGITAL: printf(";#DIGITAL:"); break;
	case T_ANALOG: printf(";#ANALOG:");break;
	case T_TIMER: printf(";#TIMER:");break;
	case T_CAN: printf(";#CAN:"); break;
	case T_VARIABLE: printf(";#VARIABLE:"); break;
	case T_CONSTANT: printf(";#CONSTANT:"); break;
	case T_WORD:printf(";WORD:'%.*s'", ts[i].len, ts[i].ptr); break;
	default: printf(";CHAR:'%c'", *ts[i].ptr); break;
	}
    }
    if (ts[n].tval == T_END)
	printf(";END");
    printf("\n");
}

const char* format_iodir(int dir)
{
    switch(dir) {
    case IODIR_IN: return "in";
    case IODIR_OUT: return "out";
    case IODIR_INOUT: return "inout";
    case IODIR_NONE: return "";
    default: return "--";
    }
}

void print_element(candy_element_t* elem)
{
    switch(elem->c_type) {
    case C_DIGITAL:
	printf("#digital %s %s ",
	       elem->name, format_iodir(elem->io.dir));	
	if (elem->io.port == -1)
	    printf("%d", elem->io.pin);
	else 
	    printf("%d:%d", 
		   elem->io.port, elem->io.pin);
	break;
    case C_ANALOG:
	printf("#analog %s:%d %s ",
	       elem->name,
	       elem->bn,
	       format_iodir(elem->io.dir));
	if (elem->io.port == -1)
	    printf("%d", elem->io.pin);
	else 
	    printf("%d:%d", 
		   elem->io.port, elem->io.pin);
	break;	
    case C_CAN_BIT1:
	printf("#can %s %x[%d]", elem->name,
	       elem->can.id, elem->can.bit_pos);
    case C_CAN_BIT2:
	printf("#can %s %x[%d,%d]", elem->name,
	       elem->can.id, elem->can.byte_pos, elem->can.bit_pos);
    case C_CAN_RANGE:
	printf("#can %s %x[%d..%d]", elem->name,
	       elem->canr.id, elem->canr.bit_pos0, elem->canr.bit_pos1);	
    case C_TIMER:
	printf("#timer %s %d",  elem->name, elem->timer.timeout);
	break;
    case C_VARIABLE:
	printf("#variable %s:%d %d", elem->name, elem->bn, elem->value);
	break;
    case C_CONSTANT:
	printf("#constant %s:%d %d", elem->name, elem->bn, elem->value);
	break;		
    }
}
static const char* rule_op(candy_rule_op_t op)
{
    switch(op) {
    case RULE_SET:    return "=";
    case RULE_COND:   return "<";
    case RULE_TOGGLE: return "^";
    case RULE_ASSIGN: return ":=";
    case RULE_EVENT:  return ">";
    default: return "??";
    }
}

static const char* format_op(candy_op_t op)
{
    switch(op) {
    case EXPR_LT: return "<";
    case EXPR_LTE: return "<=";
    case EXPR_GT:  return ">";
    case EXPR_GTE: return ">=";
    case EXPR_EQ:  return "==";
    case EXPR_NEQ: return "!=";
    case EXPR_NOT: return "!";
    case EXPR_OR:  return ";";
    case EXPR_AND: return ",";
    case EXPR_NEG: return "-";
    case EXPR_PLUS: return "+";
    case EXPR_MINUS: return "-";
    case EXPR_TIMES: return "*";
    case EXPR_DIVIDE: return "/";
    case EXPR_REMAINDER: return "%";
    default: return "??";
    }
}

void print_expr(xindex_t xi)
{
    candy_expr_t* xp = &expr[xi];
    switch(xp->op) {
    case EXPR_NAME:
	printf("%s", element[xp->ni].name);
	break;
    case EXPR_CONST:
	printf("%d", xp->value);
	break;
    case EXPR_CAN_RANGE:
	printf("0x%x[%d..%d]", xp->crange.id,
	       xp->crange.bit_pos0,
	       xp->crange.bit_pos0);
	break;	
    case EXPR_CAN_BIT:
	if (xp->cbit.byte_pos == -1)
	    printf("0x%x[%d]", xp->cbit.id, xp->cbit.bit_pos);
	else
	    printf("0x%x[%d,%d]",
		   xp->cbit.id,
		   xp->cbit.byte_pos,
		   xp->cbit.bit_pos);
	break;	
    case EXPR_LT:
    case EXPR_LTE:
    case EXPR_GT:
    case EXPR_GTE:
    case EXPR_EQ:
    case EXPR_NEQ:
    case EXPR_OR:
    case EXPR_AND:
    case EXPR_PLUS:
    case EXPR_MINUS:
    case EXPR_TIMES:
    case EXPR_DIVIDE:
    case EXPR_REMAINDER:		
	print_expr(xp->bin.li);
	printf(" %s ", format_op(xp->op));
	print_expr(xp->bin.ri);
	break;
    case EXPR_NOT:
    case EXPR_NEG:	
	printf("%s", format_op(xp->op));
	print_expr(xp->mi);
	break;
    default:
	break;
    }
}

void print_rule(candy_rule_t* rp)
{
    if (rp->op == RULE_EVENT) {
	printf(">%s=", element[rp->ni].name);
	print_expr(rp->vi);
    }
    else {
	printf("%s%s %s ", (rp->neg ? "-" : ""),
	       element[rp->ni].name, rule_op(rp->op));
	switch(rp->op) {
	case RULE_SET: print_expr(rp->ci); break;
	case RULE_COND: print_expr(rp->ci); break;
	case RULE_TOGGLE: print_expr(rp->ci); break;
	case RULE_ASSIGN:
	    print_expr(rp->vi);
	    printf(" < ");
	    print_expr(rp->ci);
	    break;
	default: break;
	}
    }
}

int is_const(int tok)
{
    return ((tok == T_DEC)||(tok == T_HEX));
}

// '#' 'digital' <name> [IODir] [Port ':'] Pin

int tok_to_digital_pin(token_t* tp, int dir)
{
    if ((tp->tval == T_HEX)||((tp->tval == T_DEC))) {
	return tok_to_int(tp);
    }
#ifdef ARDUINO
    if ((dir & IODIR_OUT) && tokeq("LED_BUILTIN", tp)) return LED_BUILTIN;
    if (tokeq("D0", tp)) return D0;
    if (tokeq("D1", tp)) return D1;
    if (tokeq("D2", tp)) return D2;
    if (tokeq("D3", tp)) return D3;
    if (tokeq("D4", tp)) return D4;
    if (tokeq("D5", tp)) return D5;
    if (tokeq("D6", tp)) return D6;
    if (tokeq("D7", tp)) return D7;
    if (tokeq("D8", tp)) return D8;
    if (tokeq("D9", tp)) return D9;
    if (tokeq("D10", tp)) return D10;
    if (tokeq("D11", tp)) return D11;
    if (tokeq("D12", tp)) return D12;
    if (tokeq("D13", tp)) return D13;
    if (tokeq("D14", tp)) return D14;
    if (tokeq("D15", tp)) return D15;
#endif
    return -1;    
}

int is_digital_pin(int tok)
{
    return ((tok == T_DEC)||(tok == T_WORD));
}

// '#' 'digital' <name> [<iodir>] [<port> ':'] <pin>
int parse_digital(int j)
{
    int j0 = j;
    int k = nelements;
    int dir = IODIR_NONE; // use default
    if (!T(ts,j,T_WORD)) return CANDY_ERR;
    j++;
    if (T(ts,j,T_IN)||T(ts,j,T_OUT)||T(ts,j,T_INOUT)) {
	switch(ts[j].tval) {
	case T_IN: dir = IODIR_IN; break;
	case T_OUT: dir = IODIR_OUT; break;
	case T_INOUT: dir = IODIR_INOUT; break;
	default: break;
	}
	j++;
    }
    element[k].c_type = C_DIGITAL;
    element[k].bn = 1;
    element[k].io.dir = dir;

    if (T(ts,j,T_DEC) && T(ts,j+1,':') &&
	is_digital_pin(ts[j+2].tval) && T(ts,j+3,T_END)) {
	element[k].io.port = tok_to_int(&ts[j]);
	if ((element[k].io.pin = tok_to_digital_pin(&ts[j+2],dir)) < 0)
	    return CANDY_ERR;
    }
    else if (is_digital_pin(ts[j].tval) && T(ts,j+1,T_END)) {
	element[k].io.port = -1;
	if ((element[k].io.pin =  tok_to_digital_pin(&ts[j],dir)) < 0)
	    return CANDY_ERR;	    
    }
    else
	return CANDY_ERR;
    make_str8(&element[k].name, &ts[j0]);
    nelements++;
    return CANDY_DEF;
}

int tok_to_analog_pin(token_t* tp, int dir)
{
    if ((tp->tval == T_HEX)||((tp->tval == T_DEC))) {
	// Arudino PWM Pins:
	//   3,5,6,9,10,11          Uno,Nano,Mini
	//   2-13,44-66             Mega
	//   3,5,6,9,10,11,13       Leonardo, Micro, Yun
	//   3,5,6,9,10             Uno Wifi rev2, Nano Every
	//   0-9,10,A3,A4           MKR boards
	//   0-9,10,11,A3,A4        MKR 1000 Wifi
	//   3-13,A0,A1             Zero
	//   2,3,5,6,9-12,A2,A3,A5  Nano 33 IoT
	//   1-13,A0-A7             Nano BLE/BLE Sense
	//   2-13                   Due         
	//   3,5,6,9                101       
	return tok_to_int(tp);
    }
#ifdef ARDUINO
    if (tokeq("DAC0", tp)) return DAC0;
    if (tokeq("DAC1", tp)) return DAC1;
    if (tokeq("A0", tp)) return A0;
    if (tokeq("A1", tp)) return A1;
    if (tokeq("A2", tp)) return A2;
    if (tokeq("A3", tp)) return A3;
    if (tokeq("A4", tp)) return A4;
    if (tokeq("A5", tp)) return A5; // A0..A5 most boards
    if (tokeq("A6", tp)) return A6; // A0..A6 MKR boards
    if (tokeq("A7", tp)) return A7; // A0..A7 Mini/Nano
    if (tokeq("A8", tp)) return A8;
    if (tokeq("A9", tp)) return A9;
    if (tokeq("A10", tp)) return A10;
    if (tokeq("A11", tp)) return A11;
    if (tokeq("A12", tp)) return A12;
    if (tokeq("A13", tp)) return A13;
    if (tokeq("A14", tp)) return A14;
    if (tokeq("A15", tp)) return A15; // A0..A15 Mega
#endif
    return -1;
}

int is_analog_pin(int tok)
{
    return ((tok == T_DEC)||(tok == T_WORD));
}

// '#' 'analog' <name> [<iodir>] [<port> ':'] <pin> 
// '#' 'analog' <name> [<iodir>] [':' <size>] [<port> ':'] <pin>

int parse_analog(int j)
{
    int j0 = j;
    int k = nelements;
    int dir = IODIR_NONE;
    
    if (!T(ts,j,T_WORD)) return CANDY_ERR;
    j++;
    element[k].c_type = C_ANALOG;
    if (T(ts,j,':') && T(ts,j+1,T_DEC)) {
	int n = tok_to_int(&ts[j+1]);
	element[k].bn = (n > MAX_ADC_RESOLUTION) ? MAX_ADC_RESOLUTION : n;
	j += 2;
    }
    else {
	element[k].bn = MAX_ADC_RESOLUTION;
    }
    if (T(ts,j,T_IN)||T(ts,j,T_OUT)||T(ts,j,T_INOUT)) {
	switch(ts[j].tval) {
	case T_IN: dir = IODIR_IN; break;
	case T_OUT: dir = IODIR_OUT; break;
	case T_INOUT: dir = IODIR_INOUT; break;
	default: break;
	}
	j++;
    }
    element[k].io.dir = dir;
    if (T(ts,j,T_DEC) && T(ts,j+1,':') && is_analog_pin(ts[j+2].tval) &&
	T(ts,j+3,T_END)) {
	element[k].io.port = tok_to_int(&ts[j]);
	if ((element[k].io.pin =  tok_to_analog_pin(&ts[j+2],dir)) == -1)
	    return CANDY_ERR;
    }
    else if (is_analog_pin(ts[j].tval) && T(ts,j+1,T_END)) {
	element[k].io.port = -1;
	if ((element[k].io.pin = tok_to_analog_pin(&ts[j],dir)) == -1)
	    return CANDY_ERR;
    }
    else {
	return CANDY_ERR;
    }
    make_str8(&element[k].name, &ts[j0]);
    nelements++;
    return CANDY_DEF;    
}

// '#' 'timer' <name> <milli-seconds>
int parse_timer(int j)
{
    int j0 = j;
    int k = nelements;
    if (!T(ts,j,T_WORD)) return CANDY_ERR;
    j++;
    if (!T(ts,j,T_DEC)) return CANDY_ERR;
    element[k].timer.timeout = tok_to_int(&ts[j]);
    j++;
    if (!T(ts,j,T_END)) return CANDY_ERR;
    element[k].c_type = C_TIMER;
    element[k].bn = sizeof(int)*8;
    element[k].timer.flags = 0;
    nelements++;
    make_str8(&element[k].name, &ts[j0]);
    return CANDY_DEF;
}
//
// '#' 'can' <name> <frame-id>[7]
// '#' 'can' <name> <frame-id>[0,3]
// '#' 'can' <name> <frame-id>[8..15]
// '#' 'can' <name> <frame-id> <byte-pos> <hex8> <hex8> <hex8>
//
int parse_can(int j)
{
    int j0 = j;
    int k = nelements;
    if (!T(ts,j,T_WORD)) return CANDY_ERR;
    j++;
    if (!T(ts,j,T_HEX)) return CANDY_ERR;
    element[k].can.id = tok_to_int(&ts[j]);    
    j++;
    if (T(ts,j,T_DEC)&&T(ts,j+1,T_HEX)&&T(ts,j+2,T_HEX)&&
	T(ts,j+3,T_HEX)&&T(ts,j+4,T_END)) {
	element[k].c_type = C_CAN_MATCH;
	element[k].canm.byte_pos = tok_to_int(&ts[j]);
	element[k].canm.mask = tok_to_int(&ts[j+1]);
	element[k].canm.match1 = tok_to_int(&ts[j+2]);
	element[k].canm.match0 = tok_to_int(&ts[j+3]);
    }
    else if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,']')&&
	     T(ts,j+3,T_END)) {
	element[k].c_type = C_CAN_BIT1;
	element[k].can.byte_pos = 0;  // not used
	element[k].can.bit_pos = tok_to_int(&ts[4]);
    }    
    else if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,',') &&
	     T(ts,j+3,T_DEC)&&T(ts,j+4,']')&&T(ts,j+5,T_END)) {
	element[k].c_type = C_CAN_BIT2;
	element[k].can.byte_pos = tok_to_int(&ts[j+1]);
	element[k].can.bit_pos = tok_to_int(&ts[j+3]);
    }
    else if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,'.') && T(ts,j+3,'.') &&
	     T(ts,j+4,T_DEC)&&T(ts,j+5,']')&&T(ts,j+6,T_END)) {
	int p0, p1;
	element[k].c_type = C_CAN_RANGE;
	p0 = tok_to_int(&ts[j+1]);
	p1 = tok_to_int(&ts[j+4]);
	if ((p0 < 0) || (p0 > p1))
	    return CANDY_ERR;	    
	element[k].canr.bit_pos0 = p0;
	element[k].canr.bit_pos1 = p1;
    }
    else
	return CANDY_ERR;	
    make_str8(&element[k].name, &ts[j0]);
    nelements++;
    return CANDY_DEF;    
}
//
// '#' 'constant' <name> [:Size] '=' Value
//
int parse_constant(int j)
{
    int j0 = j;
    int k = nelements;

    if (!T(ts,j,T_WORD)) return CANDY_ERR;
    j++;
    element[k].c_type = C_CONSTANT;
    element[k].bn = 1; // fixme: default size?
    if (T(ts,j,':') && T(ts,j+1,T_DEC)) {
	element[k].bn = tok_to_int(&ts[j+1]);
	j += 2;
    }
    if (T(ts,j,'=') && is_const(ts[j+1].tval) && T(ts,j+2,T_END)) {
	element[k].value = tok_to_int(&ts[j+1]);
    }
    else
	return CANDY_ERR;
    make_str8(&element[k].name, &ts[j0]);    
    nelements++;
    return CANDY_DEF;
}
//
// '#' 'variable' <name> [:Size] ['=' Value]
//
int parse_variable(int j)
{
    int j0 = j;
    int k = nelements;

    if (!T(ts,j,T_WORD)) return CANDY_ERR;
    j++;
    element[k].c_type = C_VARIABLE;
    element[k].bn = 1;  // fixme: default size? 
    if (T(ts,j,':') && T(ts,j+1,T_DEC)) {
	element[k].bn = tok_to_int(&ts[j+1]);
	j += 2;
    }
    if (T(ts,j,'=') && is_const(ts[j+1].tval) && T(ts,j+2,T_END)) {
	element[k].value = tok_to_int(&ts[j+1]);
    }
    else if (T(ts,j,T_END)) {
	element[k].value = 0;	
    }
    else
	return CANDY_ERR;	
    make_str8(&element[k].name, &ts[j0]);    
    nelements++;
    return CANDY_DEF;
}

//
// Cond :=  AExpr rel-op AExpr |
//          Bit | '!' Cond | '(' Cond ')' | Cond ',' Cond | Cond ';' Cond | 
//        
// AExpr := AValue | '(' AExpr ')' | AExpr arith-op AExpr
// AValue := Bit | CanRange | <name> | Constant
//
// Event :=
//   ><name>=AValue
//
//
xindex_t parse_aexpr(int* ppos);
xindex_t parse_cond(int* ppos);
int is_cond_expr = 0;

xindex_t parse_avalue(int* ppos)
{
    int j = *ppos;
    
    if (T(ts,j,'(')) {
	xindex_t xi;
	j++;
	if (is_cond_expr) {
	    if ((xi = parse_cond(&j)) == INVALID_INDEX)
		return INVALID_INDEX;
	}
	else {
	    if ((xi = parse_aexpr(&j)) == INVALID_INDEX)
		return INVALID_INDEX;
	}
	if (T(ts,j,')')) {
	    *ppos = j+1;
	    return xi;
	}
	return INVALID_INDEX; 	// missing ')'
    }
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,'[') && T(ts,j+2,T_DEC) &&	
	     T(ts,j+3,'.') && T(ts,j+4,'.') &&
	     T(ts,j+5,T_DEC) && T(ts,j+6,']')) {
	candy_expr_t* xp = &expr[nexprs];
	xp->op = EXPR_CAN_RANGE;
	xp->crange.id = tok_to_int(&ts[j]);
	xp->crange.bit_pos0 = tok_to_int(&ts[j+2]);
	xp->crange.bit_pos1 = tok_to_int(&ts[j+5]);
	*ppos = j+6;
	return nexprs++;
    }
    else if (T(ts,j,T_DEC) || T(ts,j,T_HEX)) {
	candy_expr_t* xp = &expr[nexprs];
	xp->op = EXPR_CONST;
	xp->value = tok_to_int(&ts[j]);
	*ppos = j+1;
	return nexprs++;
    }
    else if (T(ts,j,T_WORD)) {
	eindex_t ni;
	candy_expr_t* xp;
	if ((ni = lookup_name(&ts[j])) == INVALID_INDEX) {
	    fprintf(stderr, "word %.*s not found\n",
		    ts[j].len, ts[j].ptr);
	    return INVALID_INDEX;	    
	}
	xp = &expr[nexprs];
	xp->op = EXPR_NAME;
	xp->ni = ni;
	*ppos = j+1;
	return nexprs++;	
    }
    return INVALID_INDEX;
}

xindex_t parse_mul(int* ppos)
{
    xindex_t li;
    int j;
    
    if ((li = parse_avalue(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,'*') || T(ts,j,'/') || T(ts,j,'%')) {
	xindex_t ri;
	candy_expr_t* xp;
	candy_op_t op;
	*ppos = j+1;
	if ((ri = parse_avalue(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	switch(ts[j].tval) {
	case '*': xp->op = EXPR_TIMES; break;
	case '/': xp->op = EXPR_DIVIDE; break;
	case '%': xp->op = EXPR_REMAINDER; break;
	default: return INVALID_INDEX;
	}
	xp->bin.li = li;
	xp->bin.ri = ri;
	j = *ppos;
	li = nexprs++;
    }
    return li;
}

xindex_t parse_add(int* ppos)
{
    xindex_t li;    
    int j;
    
    if ((li = parse_mul(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,'+') || T(ts,j,'-')) {
	xindex_t ri;	
	candy_expr_t* xp;
	candy_op_t op;
	*ppos = j+1;	
	if ((ri = parse_mul(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	switch(ts[j].tval) {
	case '+': xp->op = EXPR_PLUS; break;
	case '-': xp->op = EXPR_MINUS; break;
	default: return INVALID_INDEX;
	}
	xp->bin.li = li;
	xp->bin.ri = ri;
	j = *ppos;	
	li = nexprs++;
    }
    return li;
}

xindex_t parse_aexpr(int* ppos)
{
    return parse_add(ppos);
}

xindex_t parse_relation(int* ppos)
{
    xindex_t li;
    xindex_t ri;    
    candy_expr_t* xp;    
    candy_op_t op;
    int j;
    
    if ((li = parse_aexpr(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;	
    j = *ppos;
    if (T(ts,j,'<') && T(ts,j+1,'=')) {
	op = EXPR_LTE;
	j += 2;
    }
    else if (T(ts,j,'<')) {
	op = EXPR_LT;
	j += 1;
    }
    else if (T(ts,j,'>') && T(ts,j+1,'=')) {
	op = EXPR_GTE;
	j += 2;
    }
    else if (T(ts,j,'>')) {
	op = EXPR_GT;
	j += 1;
    }
    else if (T(ts,j,'=') && T(ts,j+1,'=')) {
	op = EXPR_EQ;
	j += 2;
    }
    else if (T(ts,j,'!') && T(ts,j+1,'=')) {
	op = EXPR_NEQ;
	j += 2;
    }
    else {
	// fixme: check that li is boolean?
	return li;
    }
    *ppos = j;    
    if ((ri = parse_aexpr(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;	
    xp = &expr[nexprs];
    xp->op = op;
    xp->bin.li = li;
    xp->bin.ri = ri;
    return nexprs++;
}

// A,B,C,D
xindex_t parse_conjunction(int* ppos)
{
    xindex_t li;
    int j;
    
    if ((li = parse_relation(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;	
    j = *ppos;
    while(T(ts,j,',')) {
	xindex_t ri;        
	candy_expr_t* xp;
	*ppos = j+1;	
	if ((ri = parse_relation(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;	    
	xp = &expr[nexprs];
	xp->op = EXPR_AND;
	xp->bin.li = li;
	xp->bin.ri = ri;
	j = *ppos;
	li = nexprs++;
    }
    return li;
}

// A;B;C;D
xindex_t parse_disjunction(int* ppos)
{
    xindex_t li;    
    int j;
    
    if ((li = parse_conjunction(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,';')) {
	xindex_t ri;
	candy_expr_t* xp;
	*ppos = j+1;
	if ((ri = parse_conjunction(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	xp->op = EXPR_OR;
	xp->bin.li = li;
	xp->bin.ri = ri;
	j = *ppos;
	li = nexprs++;
    }
    return li;
}

xindex_t parse_cond(int* ppos)
{
    xindex_t xi;
    is_cond_expr = 1;
    xi = parse_disjunction(ppos);
    is_cond_expr = 1;
    return xi;
}

// Rule :=
//  <name> = Cond           // Set <name> to 1 if Cond is True 0 otherwise
// !<name> = Cond           // Set <name> to 1 if Cond is True 0 otherwise
//  <name> < Cond           // Set <name> to 1 if Cond is True
// !<name> < Cond           // Set <name> to 0 if Cond is True
//  <name> ^ Cond           // Toggle <name> if Cond is True
// !<name> ^ Cond           // Toggle <name> if Cond is True
//  <name> := AExpr < Cond  // Set analog output / variable to Const if Cond
//
int parse_rule()
{
    candy_rule_t* rp;
    int j = 0;

    rp = &rule[nrules];
    rp->neg = 0;
    if (ts[j].tval=='!') {
	rp->neg = 1;
	j++;
    }
    if (!T(ts,j,T_WORD)) return 0;
    if ((rp->ni = lookup_name(&ts[j])) == INVALID_INDEX) {
	printf("name %.*s not found\n", ts[j].len, ts[j].ptr);
	return 0;
    }
    j++;
    switch(ts[j].tval) {
    case '=': rp->op = RULE_SET; j++; break;
    case '<': rp->op = RULE_COND; j++;  break;
    case '^': rp->op = RULE_TOGGLE; j++; break;
    case ':':
	j++;
	if (!T(ts,j,'='))
	    return CANDY_ERR;
	j++;	
	rp->op = RULE_ASSIGN;
	rp->vi = parse_aexpr(&j);
	if (!T(ts,j,'<'))
	    return CANDY_ERR;
	j++;
	break;
    default:
	return CANDY_ERR;	
    }
    rp->ci  = parse_cond(&j);
    nrules++;
    return CANDY_RULE;
}

int parse_event()
{
    int k = nrules;

    nevents = 0;
    if (T(ts,0,'>') && T(ts,1,T_WORD) && T(ts,2,'=')) {
	eindex_t ni;
	int j;
	if ((ni = lookup_name(&ts[1])) == INVALID_INDEX) {
	    printf("name %.*s not found\n", ts[1].len, ts[1].ptr);
	    return 0;
	}
	event.op = RULE_EVENT;
	event.ni = ni;
	event.neg = 0;
	event.ci  = INVALID_INDEX;
	j = 3;
	if ((event.vi = parse_aexpr(&j)) == INVALID_INDEX)
	    return 0;
	nevents = 1;
	return CANDY_EVENT;
    }
    return 0;
}

int parse()
{
    // parse tokens
    switch(ts[0].tval) {
    case T_END:
	return CANDY_EMPTY;
    case '#':
	switch(ts[1].tval) {
	case T_DIGITAL:
	    return parse_digital(2);
	case T_ANALOG:
	    return parse_analog(2); 
	case T_TIMER:
	    return parse_timer(2); 		
	case T_CAN:
	    return parse_can(2); 			
	case T_VARIABLE:
	    return parse_variable(2);
	case T_CONSTANT:
	    return parse_constant(2);
	default:
	    return CANDY_ERR;
	}
	break;
    case '!':
    case T_WORD:
	return parse_rule();
    case '>':
	return parse_event();
    default:
	// parse rules
    }
error:    
    return CANDY_ERR;    
}

int scan_line(token_t* ts, char* ptr)
{
    int i = 0;
next:
    while(isblank(*ptr))
	ptr++;
    if ((*ptr == '\n') || (*ptr == '\0')) {
	ts[i].tval = T_END;
	ts[i].len = 0;
	ts[i].ptr = "end";
	return i;
    }
    ts[i].ptr = ptr;
    ptr = tok0(ptr, &ts[i]);
    ts[i].len = ptr - ts[i].ptr;
    // check for keywords
    if (ts[i].tval==T_WORD) { 
	if (tokeq("digital", &ts[i]))
	    ts[i].tval = T_DIGITAL;
	else if (tokeq("can", &ts[i]))
	    ts[i].tval = T_CAN;
	else if (tokeq("analog", &ts[i]))
	    ts[i].tval = T_ANALOG;
	else if (tokeq("timer", &ts[i]))
	    ts[i].tval = T_TIMER;
	else if (tokeq("variable", &ts[i]))
	    ts[i].tval = T_VARIABLE;
	else if (tokeq("constant", &ts[i]))
	    ts[i].tval = T_CONSTANT;
	else if (tokeq("in", &ts[i]))
	    ts[i].tval = T_IN;
	else if (tokeq("out", &ts[i]))
	    ts[i].tval = T_OUT;
	else if (tokeq("inout", &ts[i]))
	    ts[i].tval = T_INOUT;
    }
    i++;
    goto next;
}

int gpio_get(int port, int pin)
{
#ifdef ARDUINO
    return  (digitalRead(pin) == HIGH) ? 1 : 0;
#endif
    return 0;
}

void gpio_set(int port, int pin, int value)
{
#ifdef ARDUINO    
    digitalWrite(pin, value ? HIGH : LOW);
#endif
}

int adc_get(int port, int chan)
{
#ifdef ARDUINO
    return  analogRead(pin);
#endif
    return 0;
}

void dac_set(int port, int chan, int value)
{
#ifdef ARDUINO
    return analogWrite(pin, value);
#endif    
}

const char* can_frame_get(uint32_t id, int* lenp)
{
    if (id == 0x123) {
	*lenp = 8;
	return "\0\1\2\3\4\5\6\7";
    }
    return NULL;
}

void init()
{
    printf("sizeof(candy_element_t) = %ld\n", sizeof(candy_element_t));
    printf("sizeof(candy_expr_t) = %ld\n",  sizeof(candy_expr_t));
    printf("sizeof(candy_rule_t) = %ld\n",  sizeof(candy_rule_t));
    printf("sizeof(token_t) = %ld\n",  sizeof(token_t));
    time_init();
#ifdef ARDUINO
    analogReadResolution(MAX_ADC_RESOLUTION);
    analogWriteResolution(MAX_DAC_RESOLUTION);    
#endif
}

// load all gpio/analog/can-data into the buffer value
void update_elements()
{
    int i;
    
    for (i = 0; i < nelements; i++) {
	candy_element_t* elem = &element[i];
	switch(elem->c_type) {
	case C_CONSTANT: break;
	case C_VARIABLE: break;
	case C_DIGITAL:
	    if (elem->io.dir & IODIR_IN)
		elem->value = gpio_get(elem->io.port, elem->io.pin);
	    break;
	case C_ANALOG:
	    if (elem->io.dir & IODIR_IN)
		elem->value = adc_get(elem->io.port, elem->io.pin);
	    break;
	case C_CAN_BIT1: {  // bitpos
	    int len;	    
	    const uint8_t* ptr = can_frame_get(elem->can.id, &len);	    
	    if (ptr != NULL) {
		int bitpos = elem->can.bit_pos;
		int pos = bitpos >> 3;  // byte pos
		if (pos < len)
		    elem->value = (ptr[pos] >> (7-(bitpos & 7))) & 1;
	    }
	    break;
	}
	case C_CAN_BIT2: {  // bytepos & bitpos
	    int len;
	    const uint8_t* ptr = can_frame_get(elem->can.id, &len);
	    if (ptr != NULL) {
		int pos    = elem->can.byte_pos;
		int bitpos = elem->can.bit_pos;
		if (pos < len)
		    elem->value = (ptr[pos] >> (7-(bitpos & 7))) & 1;
	    }
	    break;
	}
	case C_CAN_MATCH: {
	    int len;
	    const uint8_t* ptr = can_frame_get(elem->canm.id, &len);	    
	    if (ptr != NULL) {
		int pos = elem->canm.byte_pos;
		if (pos < len) {
		    uint8_t val = ptr[pos];
		    if ((val & elem->canm.mask) == elem->canm.match1)
			elem->value = 1;
		    else if ((val & elem->canm.mask) == elem->canm.match0)
			elem->value = 0;
		}
	    }
	    break;
	}
	case C_CAN_RANGE: {
	    int len;
	    const uint8_t* ptr = can_frame_get(elem->canr.id, &len);
	    int bit_pos0 = elem->canr.bit_pos0;
	    int bit_pos1 = elem->canr.bit_pos1;
	    int n = (bit_pos1-bit_pos0)+1;
	    
	    
	}
	    
	case C_TIMER: {
	    // true if timeout
	    if (elem->timer.flags & C_TIMER_TIMEOUT)
		elem->value = 1;
	    else if (elem->timer.flags & C_TIMER_RUNNING) {
		uint32_t elapsed = time_elapsed_ms(elem->timer.start_tick,NULL);
		if (elem->value = (elapsed >= elem->timer.timeout)) {
		    elem->timer.flags &= ~C_TIMER_RUNNING;
		    elem->timer.flags |= C_TIMER_TIMEOUT;
		}
	    }
	    break;
	}
	default:
	    break;
	}	    
    }
}

int eval_expr(xindex_t xi)
{
    candy_expr_t* xp = &expr[xi];
    switch(xp->op) {
    case EXPR_NAME:  return element[xp->ni].value;
    case EXPR_CONST: return xp->value;
    case EXPR_CAN_RANGE: return xp->value;
    case EXPR_CAN_BIT: return xp->value;
    // rel-op
    case EXPR_LT: return eval_expr(xp->bin.li) < eval_expr(xp->bin.ri);
    case EXPR_LTE:return eval_expr(xp->bin.li) <= eval_expr(xp->bin.ri);
    case EXPR_GT: return eval_expr(xp->bin.li) > eval_expr(xp->bin.ri);
    case EXPR_GTE: return eval_expr(xp->bin.li) >= eval_expr(xp->bin.ri);
    case EXPR_EQ: return eval_expr(xp->bin.li) == eval_expr(xp->bin.ri);
    case EXPR_NEQ: return eval_expr(xp->bin.li) != eval_expr(xp->bin.ri);
    // logic-op
    case EXPR_NOT: return !eval_expr(xp->mi);
    case EXPR_OR: return eval_expr(xp->bin.li) || eval_expr(xp->bin.ri);
    case EXPR_AND: return eval_expr(xp->bin.li) && eval_expr(xp->bin.ri);
    // arith-op
    case EXPR_NEG: return -eval_expr(xp->mi);
    case EXPR_PLUS: return eval_expr(xp->bin.li) + eval_expr(xp->bin.ri);
    case EXPR_MINUS: return eval_expr(xp->bin.li) - eval_expr(xp->bin.ri);
    case EXPR_TIMES: return eval_expr(xp->bin.li) * eval_expr(xp->bin.ri);
    case EXPR_DIVIDE: {
	int den =  eval_expr(xp->bin.ri);
	if (den == 0) return 0;
	return eval_expr(xp->bin.li) / den;
    }
    case EXPR_REMAINDER: {
	int den =  eval_expr(xp->bin.ri);
	if (den == 0) return 0;
	return eval_expr(xp->bin.li) % den;
    }
    default:
	return 0;
    }
}

void rule_set(eindex_t ni, int toggle, int value)
{
    candy_element_t* elem = &element[ni];
    switch(elem->c_type) {
    case C_CONSTANT:
	break;
    case C_VARIABLE:
	if (toggle)
	    elem->value ^= value;
	else
	    elem->value = value;
	VERBOSE("SET: %s = %d\n", elem->name, elem->value);
	break;
    case C_DIGITAL:
	if (elem->io.dir & IODIR_OUT) {
	    gpio_set(elem->io.port, elem->io.pin, value);
	    VERBOSE("GPIO-SET: %s %d:%d = %d\n",
		    elem->name, elem->io.port, elem->io.pin, 
		    elem->value);
	}
	break;
    case C_ANALOG:
	if (elem->io.dir & IODIR_OUT) {
	    dac_set(elem->io.port, elem->io.pin, value);
	    VERBOSE("DAC-SET: %s %d:%d = %d\n",
		    elem->name, elem->io.port, elem->io.pin, 
		    elem->value);
	}
	break;
    case C_CAN_BIT1:
    case C_CAN_BIT2:	
    case C_CAN_MATCH:
    case C_CAN_RANGE:
	break;
    case C_TIMER:
	if (value) { // start timer
	    if (!(elem->timer.flags & C_TIMER_RUNNING)) {
		elem->timer.start_tick = time_tick();
		elem->timer.flags |= C_TIMER_RUNNING;
		elem->timer.flags &= ~C_TIMER_TIMEOUT;
		VERBOSE("TIMER-START: %s %d\n",
			elem->name, elem->timer.timeout);
	    }
	}
	else { // stop timer
	    elem->timer.flags &= ~C_TIMER_RUNNING;
	    elem->timer.flags |= C_TIMER_STOPPED;
	    VERBOSE("TIMER-STOPPED: %s\n", elem->name);
	}
    }
}

void run_rule(candy_rule_t* rp)
{
    int value;
    switch(rp->op) {
    case RULE_SET:
	value = eval_expr(rp->ci);
	if (rp->neg) value = !value;
	rule_set(rp->ni, 0, value);
	break;
    case RULE_COND:
	value = eval_expr(rp->ci);
	if (rp->neg) value = !value;
	if (value)
	    rule_set(rp->ni, 0, value);
	break;
    case RULE_TOGGLE:
	value = eval_expr(rp->ci);
	if (rp->neg) value = !value;
	rule_set(rp->ni, 1, value);
	break;
    case RULE_ASSIGN:	    
	if (eval_expr(rp->ci)) {
	    value = eval_expr(rp->vi);
	    rule_set(rp->ni, 0, value);
	}
	break;
    case RULE_EVENT:
	value = eval_expr(rp->vi);	
	rule_set(rp->ni, 0, value);
	break;
    }
}

void run_rules()
{
    int i;
    for (i = 0; i < nrules; i++) {
	run_rule(&rule[i]);
    }
}

void parse_line(char* buf)
{
    int n;

    if ((n = scan_line(ts, buf)) <= 0)
	return;
    // print_tokens(ts, n);
    switch (parse()) {
    case CANDY_EMPTY:
	break;
    case CANDY_ERR:
	printf("parse error\n");
	print_tokens(ts, n);
	break;
    case CANDY_DEF:
	printf("DEF[%d]: ", nelements-1);	
	print_element(&element[nelements-1]);
	printf("\n");
	break;
    case CANDY_RULE:
	printf("RULE[%d]: ", nrules-1);
	print_rule(&rule[nrules-1]);
	printf("\n");
	break;
    case CANDY_EVENT:
	printf("EVENT: ");
	print_rule(&event);
	printf("\n");
	run_rule(&event);
	nevents = 0;
    }
}    

int main(int argc, char** argv)
{
    char line[128];
    FILE* fin = stdin;
    int i = 1;

    init();
    
    if (strcmp(argv[i], "-v") == 0) {
	verbose = 1;
	i++;
    }
    if (strcmp(argv[i], "-f") == 0) {
	if ((fin = fopen(argv[i+1], "r")) == NULL) {
	    fprintf(stderr, "unable to open file %s\n", argv[i+1]);
	    exit(1);
	}
	i += 2;
    }

    while(i < argc) {
	parse_line(argv[i]);
	i++;
    }
    while(fgets(line, sizeof(line), fin))
	parse_line(line);

    while(1) {
	update_elements();  // read io/can/analog etc
	run_rules();        // generate output new variable value etc
    }
    exit(0);
}
