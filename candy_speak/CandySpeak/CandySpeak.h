#ifndef __CANDY_SPEAK_H__
#define __CANDY_SPEAK_H__

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>
#include <ctype.h>

#include "bitpack/bitpack.h"

#define MIN_LINE_LENGTH  128
#define STACK_SIZE       8
#define MAX_STRING_TAB   512
#define MAX_NUM_TOKENS   64
#define MAX_NUM_ELEMENTS 32
#define MAX_NUM_RULES    32
#define MAX_NUM_EXPRS    64
#define MAX_XNODES       5

typedef struct {
    uint32_t id;
    int8_t   byte_pos;   // 0..7  | 0..63 | -1
    uint16_t bit_pos;    // 0..7  | 0..512 |
} can_bit_t;

// <frame-id> '[' pos0 .. pos1 ']  - extract pos1-pos0+1 bits (pos1 >= pos0)
// <frame-id> '[' pos ':' len ']  - extract pos-(pos+len-1) len bits
typedef struct {
    uint32_t id;         // Frame ID
    uint16_t pos;   // 0..63  | 0..512 - start bit
    uint16_t len;   // 1..32           - number of bits
} can_range_t;

#define DIR_NONE  0x0
#define DIR_IN    0x1
#define DIR_OUT   0x2
#define DIR_INOUT 0x3

// Digital / Analog port definition
typedef struct {
    int8_t port;  // -1 if port is not used
    int8_t pin;   // pin number
} portbit_t;

// Timer
#define C_TIMER_TIMEOUT  0x01
#define C_TIMER_RUNNING  0x02
#define C_TIMER_STOPPED  0x04

typedef uint32_t tick_t;

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
    const char* ptr;  // pointer into input buffer
    uint16_t len;     // string length    
    uint16_t tval;    // token value (char | 256..263..)
} token_t;

// Def :=
// '#' 'constant' <name> [:<size>] '=' Value
// '#' 'variable' <name> [:<size>] ['=' Value]
// '#' 'digital'  <name> [<iodir>] [<port> ':'] <pin>
// '#' 'analog'   <name> [<iodir>] [<port> ':'] <pin>
// '#' 'analog'   <name>[:<size>] [<iodir>] [<port> ':'] <pin>
// '#' 'timer'    <name> <milli-seconds>
// '#' 'can' <name> <frame-id>[7]
// '#' 'can' <name> <frame-id>[0,3]
// '#' 'can' <name> <frame-id>[8..15]
// '#' 'can' <name> <frame-id> <byte-pos> <hex8> <hex8> <hex8>
//
// <hex>   := '0x' [0..9a..fA..F]+
// <dec>   := ['0'..'9']+
// <const> := <dec> | <hex>
// <pin>   := <const>|<name>
// <port>  := <dec>
// <byte-pos> := 0..7 | 0..63
// <bit-pos>  := 0..7 | 0..512
// <name> := <char>+
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
    C_CAN_RANGE = 7,
    C_TIMER     = 8,
} candy_element_type_t;

typedef enum {
    BOOL    = 0,
    INT8    = 1,    
    UINT8   = 2,
    INT16   = 3,
    UINT16  = 4,        
    INT32   = 5,
    UINT32  = 6,
} candy_value_type_t;

typedef union {
    uint8_t u8;
    int8_t  i8;
    uint16_t u16;
    int16_t  i16;
    uint32_t u32;
    int32_t  i32;
} candy_value_t;
    
typedef struct _candy_element_t {
    const char* name;   // str8 name! preceeded by 8-bit length terminated in 0
    uint8_t size;       // number of bits (1 for digital default=8? for analog)
    uint8_t type;       // candy_element_type_t...
    uint8_t dir;        // DIR_xxx variables | const=in | io | analog
    uint8_t vtype;      // value type
    uint32_t clk;       // assign clock cycle
    candy_value_t cur;  // current value
    candy_value_t nxt;  // new output value
    union {
	portbit_t   io;
	can_bit_t   can;
	can_range_t canr;
	ctimer_t    timer;
    };
} candy_element_t;

typedef enum {
    EXPR_NAME,  // analog/digital/variable/timer...
    EXPR_CONST, // constant value
    EXPR_CAN_RANGE,
    EXPR_CAN_BIT1,   // bitpos FrameID[bit-pos]
    EXPR_CAN_BIT2,   // byte&bit-pos  FrameID[byte-pos,bit-pos]
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
	candy_value_t v;                     // CONST (depend on type)
	eindex_t     ei;                     // NAME element[ei]
	can_range_t  crange;                 // CAN_RANGE
	can_bit_t    cbit;                   // CAN_BIT
	xindex_t     mi;                     // NOT, NEG
	struct {  // bin-op: LT,LTE,GT,GTE,EQ,NEQ,AND,OR...
	    xindex_t li;
	    xindex_t ri;
	} bin;
    };
} candy_expr_t;

typedef struct _candy_rule_t {
    eindex_t ei;                  // target element index
    xindex_t vi;                  // value expression
    xindex_t ci;                  // conditional expression
} candy_rule_t;

typedef struct _candy_state_t {
    int nelements;
    int nexprs;
    int nrules;
    int nstr;
} candy_state_t;

typedef enum {
    E_OK = 0,             // no error
    E_SYNTAX,             // syntax error
    E_RANGE,              // value range error
    E_ELEMENT_NOT_FOUND,  // no such element  
    E_EXPR_OVERFLOW,      // too many exprssions
    E_RULE_OVERFLOW,      // too many rules
    E_ELEM_OVERFLOW,      // too many element definitions
} candy_error_t;


token_t ts[MAX_NUM_TOKENS+1];

int nelements;
candy_element_t element[MAX_NUM_ELEMENTS];

int nexprs;
candy_expr_t expr[MAX_NUM_EXPRS];

int nrules;
candy_rule_t rule[MAX_NUM_RULES];

int nstr;
char str[MAX_STRING_TAB];

// current parsed event (must execute immediatly)
int nevents;  // 0 | 1
candy_rule_t event;

int nupdates; // number of elements updated during rules eval

int verbose = 0;

int sp = 0;
candy_state_t stack[STACK_SIZE];

xindex_t expr_vi_0;   // constant 0
xindex_t expr_vi_1;   // constant 1

candy_element_t* tick;   // uint32 tick
candy_element_t* cycle;  // uint32 cycle
candy_element_t* latch;  // bool   latch

candy_error_t candy_errno = E_OK;

#define CANDY_ERROR(err) (candy_errno=(err), CANDY_ERR)
#define CANDY_INVALID(err) (candy_errno=(err), INVALID_INDEX)
    
// name is str8 first character is length

#define VARIABLE(ep, name8, dr, typ, sz, val)				\
    do {								\
	(ep)->name    = (name8)+1;					\
	(ep)->size    = (sz);						\
	(ep)->type    = C_VARIABLE;					\
	(ep)->dir     = (dr);						\
	(ep)->vtype   = (typ);						\
	(ep)->cur.u32 = (val);						\
	(ep)->nxt.u32 = 0;						\
    } while(0)

#define RO_VARIABLE_U32(elem, name, value) \
    VARIABLE((elem), (name), DIR_IN, UINT32, 32, (value))

#define RW_VARIABLE_BOOL(elem, name, value) \
    VARIABLE((elem), (name), DIR_INOUT, BOOL, 1, (value))

// boards:
// ARUDINO_AVR_UNO
// ARUDINO_AVR_DUEMILANOVE
// ARDUINO_ITSYBITSY_M0
// ARDUINO_SAM_DUE
// ARDUINO_SAM3X8E
// ARDUINO_SAM_ZERO
// SAMD_SERIES
// .. boards.txt howto find families and properties?
// platform.txt ?


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
static void time_init()
{
}

static tick_t time_tick(void)
{
    return millis();
}

static size_t candy_print_str(const char* str)
{
    return Serial.print(str);
}

static size_t candy_print_int(int val)
{
    return Serial.print(val);
}

static size_t candy_print_ln()
{
    return Serial.println();
}
    
#else

#include <sys/time.h>

struct timeval boot_time;

static void time_init()
{
    gettimeofday(&boot_time, 0);
}

static tick_t time_tick(void)
{
    struct timeval now;
    struct timeval t;
    gettimeofday(&now, 0);
    timersub(&now, &boot_time, &t);
    return t.tv_sec*1000 + t.tv_usec/1000;
}

static size_t candy_print_str(const char* str)
{
    return printf("%s", str);
}

static size_t candy_print_int(int val)
{
    return printf("%d", val);    
}

static size_t candy_print_ln()
{
    return printf("\n");
}

#endif


static size_t candy_print_error(candy_error_t err)
{
    switch(err) {
    case E_OK: return candy_print_str("no error");
    case E_SYNTAX: return candy_print_str("syntax error");
    case E_RANGE: return candy_print_str("value out of range");
    case E_ELEMENT_NOT_FOUND: return candy_print_str("no such element");
    case E_EXPR_OVERFLOW: return candy_print_str("too many exprssions");
    case E_RULE_OVERFLOW: return candy_print_str("too many rules");
    case E_ELEM_OVERFLOW: return candy_print_str("too many elements");
    default: return candy_print_str("internal");
    }
}

// Must be called with in about half an hour since
static uint32_t time_elapsed_ms(tick_t since, tick_t* nowp)
{
    tick_t now = time_tick();
    uint32_t td  = now - since;
    if (nowp) *nowp = now;
    return td;
}

static void candy_push()
{
    if (sp >= STACK_SIZE) return;
    stack[sp].nelements = nelements;
    stack[sp].nexprs = nexprs;
    stack[sp].nrules = nrules;
    stack[sp].nstr = nstr;
    sp++;
}

static void candy_pop()
{
    if (sp <= 0) return;
    sp--;
    nelements = stack[sp].nelements;
    nexprs = stack[sp].nexprs;
    nrules = stack[sp].nrules;
    nstr = stack[sp].nstr;
}

static void candy_commit()
{
    if (sp <= 0) return;
    sp--;
}

// initalize builtin variables/constants reset all rules/defintions etc
static void candy_reset()
{
    candy_expr_t* xp;
    xindex_t i;
    
    nelements = 0;
    nexprs    = 0;
    nrules    = 0;
    nstr      = 0;
    nevents   = 0;
    sp        = 0;
    nupdates  = 0;    

    xp = &expr[i=nexprs++];
    xp->op = EXPR_CONST;
    xp->v.i32 = 0;
    expr_vi_0 = i;

    xp = &expr[i=nexprs++];    
    xp->op = EXPR_CONST;
    xp->v.i32 = 1;
    expr_vi_1 = i;
    
    // builtin uint32 "tick" (milliseconds since start)
    tick = &element[nelements++];
    RO_VARIABLE_U32(tick, "\4tick", time_tick());

    // builtin uint32 "cycle"    (cylce counter)
    cycle = &element[nelements++];
    RO_VARIABLE_U32(cycle, "\5cycle", 1);

    latch = &element[nelements++];
    RW_VARIABLE_BOOL(latch, "\5latch", 0);
}

#ifdef DEBUG
#include "CandyDebug.h"
#endif

static char* make_str8(const char** pptr, token_t* tp)
{
    char* dst = &str[nstr];
    
    if (nstr+tp->len+2 > MAX_STRING_TAB) {
	return NULL;
    }
    *dst++ = tp->len;
    memcpy(dst, tp->ptr, tp->len);
    *pptr = dst;
    dst[tp->len] = '\0';
    nstr += (tp->len+2);
    return dst;
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

#define T(tv,i,v) ((tv)[(i)].tval == (v))

static eindex_t lookup_element_by_str(const char* str, uint16_t len)
{
    int i = 0;

    while(i < nelements) {
	const char* ptr = element[i].name;
	uint8_t elen = (uint8_t) *(ptr-1);
	if (len == elen) {
	    if (memcmp(str, ptr, len) == 0)
		return i;
	}
	i++;
    }
    return INVALID_INDEX;
}

static eindex_t lookup_element(token_t* tp)
{
    return lookup_element_by_str(tp->ptr, tp->len);
}

static char* candy_tok0(char* p, token_t* tp)
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

int is_const(int tok)
{
    return ((tok == T_DEC)||(tok == T_HEX));
}

// '#' 'digital' <name> [IODir] [Port ':'] Pin

// linux? rasberryip - wiring pi ?
#ifndef ARDUINO
#define LED_BUILTIN 13
#define D0  0
#define D1  1
#define D2  2
#define D3  3
#define D4  4
#define D5  5
#define D6  6
#define D7  7
#define D8  8
#define D9  9
#define D10 10
#define D11 11
#define D12 12
#define D13 13
#define D14 14
#define D15 15
#endif

#ifndef CANDY_OUT_PIN
#define CANDY_OUT_PIN 2
#endif


int tok_to_digital_pin(token_t* tp, int dir)
{
    if ((tp->tval == T_HEX)||((tp->tval == T_DEC))) {
	return tok_to_int(tp);
    }
    if ((dir & DIR_OUT) && tokeq("LED_BUILTIN", tp)) return LED_BUILTIN;
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
    return -1;    
}

int is_digital_pin(int tok)
{
    return ((tok == T_DEC)||(tok == T_WORD));
}

// '#' 'digital' <name> [<iodir>] [<port> ':'] <pin>
static int candy_parse_digital(int j)
{
    int j0 = j;
    int k;
    int dir = DIR_NONE; // use default

    if ((k=nelements) >= MAX_NUM_ELEMENTS)
	return CANDY_ERROR(E_ELEM_OVERFLOW);
    if (!T(ts,j,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    j++;
    if (T(ts,j,T_IN)||T(ts,j,T_OUT)||T(ts,j,T_INOUT)) {
	switch(ts[j].tval) {
	case T_IN: dir = DIR_IN; break;
	case T_OUT: dir = DIR_OUT; break;
	case T_INOUT: dir = DIR_INOUT; break;
	default: break;
	}
	j++;
    }
    element[k].type = C_DIGITAL;
    element[k].size = 1;
    element[k].dir = dir;

    if (T(ts,j,T_DEC) && T(ts,j+1,':') &&
	is_digital_pin(ts[j+2].tval) && T(ts,j+3,T_END)) {
	element[k].io.port = tok_to_int(&ts[j]);
	if ((element[k].io.pin = tok_to_digital_pin(&ts[j+2],dir)) < 0)
	    return CANDY_ERROR(E_SYNTAX);
    }
    else if (is_digital_pin(ts[j].tval) && T(ts,j+1,T_END)) {
	element[k].io.port = -1;
	if ((element[k].io.pin =  tok_to_digital_pin(&ts[j],dir)) < 0)
	    return CANDY_ERROR(E_SYNTAX);
    }
    else
	return CANDY_ERROR(E_SYNTAX);
    make_str8(&element[k].name, &ts[j0]);
    nelements++;
    return CANDY_DEF;
}

// linux/raspberry wiringpi?
#ifndef ARDUINO
#define DAC 0
#define A0  0
#define A1  1
#define A2  2
#define A3  3
#define A4  4
#define A5  5
#define A6  6
#define A7  7
#define A8  8
#define A9  9
#define A10 10
#define A11 11
#define A12 12
#define A13 13
#define A14 14
#define A15 15
#endif

static int tok_to_analog_pin(token_t* tp, int dir)
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
    if (tokeq("DAC", tp)) return DAC;
//    if (tokeq("DAC1", tp)) return DAC1;
    if (tokeq("A0", tp)) return A0;
    if (tokeq("A1", tp)) return A1;
    if (tokeq("A2", tp)) return A2;
    if (tokeq("A3", tp)) return A3;
    if (tokeq("A4", tp)) return A4;
    if (tokeq("A5", tp)) return A5; // A0..A5 most boards
#if !defined(ARDUINO) || defined(ARDUINO_MKR) // what names to use?
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

static int is_analog_pin(int tok)
{
    return ((tok == T_DEC)||(tok == T_WORD));
}

// '#' 'analog' <name> [<iodir>] [<port> ':'] <pin> 
// '#' 'analog' <name> [<iodir>] [':' <size>] [<port> ':'] <pin>

static int candy_parse_analog(int j)
{
    int j0 = j;
    int k;
    int dir = DIR_NONE;

    if ((k=nelements) >= MAX_NUM_ELEMENTS)
	return CANDY_ERROR(E_ELEM_OVERFLOW);	
    if (!T(ts,j,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    j++;
    element[k].type = C_ANALOG;
    if (T(ts,j,':') && T(ts,j+1,T_DEC)) {
	int n = tok_to_int(&ts[j+1]);
	element[k].size = (n > MAX_ADC_RESOLUTION) ? MAX_ADC_RESOLUTION : n;
	j += 2;
    }
    else {
	element[k].size = MAX_ADC_RESOLUTION;
    }
    if (T(ts,j,T_IN)||T(ts,j,T_OUT)||T(ts,j,T_INOUT)) {
	switch(ts[j].tval) {
	case T_IN: dir = DIR_IN; break;
	case T_OUT: dir = DIR_OUT; break;
	case T_INOUT: dir = DIR_INOUT; break;
	default: break;
	}
	j++;
    }
    element[k].dir = dir;
    if (T(ts,j,T_DEC) && T(ts,j+1,':') && is_analog_pin(ts[j+2].tval) &&
	T(ts,j+3,T_END)) {
	element[k].io.port = tok_to_int(&ts[j]);
	if ((element[k].io.pin =  tok_to_analog_pin(&ts[j+2],dir)) == -1)
	    return CANDY_ERROR(E_SYNTAX);
    }
    else if (is_analog_pin(ts[j].tval) && T(ts,j+1,T_END)) {
	element[k].io.port = -1;
	if ((element[k].io.pin = tok_to_analog_pin(&ts[j],dir)) == -1)
	    return CANDY_ERROR(E_SYNTAX);
    }
    else {
	return CANDY_ERROR(E_SYNTAX);
    }
    element[k].vtype = INT32;
    element[k].cur.i32 = element[k].nxt.i32 = 0;
    element[k].clk = 0;
    make_str8(&element[k].name, &ts[j0]);
    nelements++;
    return CANDY_DEF;    
}

// '#' 'timer' <name> <milli-seconds>
static int candy_parse_timer(int j)
{
    int j0 = j;
    int k;
    if ((k=nelements) >= MAX_NUM_ELEMENTS)
	return CANDY_ERROR(E_ELEM_OVERFLOW);
    if (!T(ts,j,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    j++;
    if (!T(ts,j,T_DEC))
	return CANDY_ERROR(E_SYNTAX);
    element[k].timer.timeout = tok_to_int(&ts[j]);
    j++;
    if (!T(ts,j,T_END))
	return CANDY_ERROR(E_SYNTAX);
    element[k].type = C_TIMER;
    element[k].size = sizeof(int)*8;
    element[k].timer.flags = 0;
    element[k].vtype = INT32;
    element[k].dir   = DIR_INOUT;    
    element[k].cur.i32 = element[k].nxt.i32 = 0;
    element[k].clk = 0;
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
static int candy_parse_can(int j)
{
    int j0 = j;
    int k;
    if ((k=nelements) >= MAX_NUM_ELEMENTS)
	return CANDY_ERROR(E_ELEM_OVERFLOW);
    if (!T(ts,j,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    j++;
    if (!T(ts,j,T_HEX))
	return CANDY_ERROR(E_SYNTAX);
    element[k].can.id = tok_to_int(&ts[j]);    
    j++;
    if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,']') && T(ts,j+3,T_END)) {
	element[k].type = C_CAN_BIT1;
	element[k].can.byte_pos = 0;  // not used
	element[k].can.bit_pos = tok_to_int(&ts[4]);
    }    
    else if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,',') &&
	     T(ts,j+3,T_DEC)&&T(ts,j+4,']')&&T(ts,j+5,T_END)) {
	element[k].type = C_CAN_BIT2;
	element[k].can.byte_pos = tok_to_int(&ts[j+1]);
	element[k].can.bit_pos = tok_to_int(&ts[j+3]);
    }
    else if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,'.') && T(ts,j+3,'.') &&
	     T(ts,j+4,T_DEC)&&T(ts,j+5,']')&&T(ts,j+6,T_END)) {
	int len;
	int pos = tok_to_int(&ts[j+1]);
	int pos1 = tok_to_int(&ts[j+4]);
	if ((pos < 0) || (pos > pos1) || (pos1 >= 512))
	    return CANDY_ERROR(E_RANGE);
	if ((len = (pos1-pos)+1) > 32)
	    return CANDY_ERROR(E_RANGE);
	element[k].type = C_CAN_RANGE;	
	element[k].canr.pos = pos;
	element[k].canr.len = len;
    }
    else if (T(ts,j,'[')&&T(ts,j+1,T_DEC)&&T(ts,j+2,':') &&
	     T(ts,j+3,T_DEC)&&T(ts,j+4,']')&&T(ts,j+5,T_END)) {
	int pos = tok_to_int(&ts[j+1]);
	int len = tok_to_int(&ts[j+3]);
	if ((pos < 0) || (pos >= 512) || (len <= 0) || (len > 32))
	    return CANDY_ERROR(E_RANGE);	    
	element[k].type = C_CAN_RANGE;	
	element[k].canr.pos = pos;
	element[k].canr.len = len;
    } 
    else
	return CANDY_ERROR(E_SYNTAX);
    element[k].vtype = INT32;
    element[k].dir   = DIR_IN;
    element[k].cur.i32 = element[k].nxt.i32 = 0;
    element[k].clk = 0;    
    make_str8(&element[k].name, &ts[j0]);
    nelements++;
    return CANDY_DEF;    
}
//
// '#' 'constant' <name> [:Size] '=' Value
//
static int candy_parse_constant(int j)
{
    int j0 = j;
    int k;
    if ((k=nelements) >= MAX_NUM_ELEMENTS)
	return CANDY_ERROR(E_ELEM_OVERFLOW);
    if (!T(ts,j,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    j++;
    element[k].type = C_CONSTANT;
    element[k].size = 1; // fixme: default size?
    if (T(ts,j,':') && T(ts,j+1,T_DEC)) {
	element[k].size = tok_to_int(&ts[j+1]);
	j += 2;
    }
    element[k].vtype = INT32;
    element[k].dir   = DIR_IN;
    element[k].clk   = 0;    
    if (T(ts,j,'=') && is_const(ts[j+1].tval) && T(ts,j+2,T_END)) {
	element[k].cur.i32 = tok_to_int(&ts[j+1]);
	element[k].nxt.i32 = element[k].cur.i32;
    }
    else
	return CANDY_ERROR(E_SYNTAX);
    make_str8(&element[k].name, &ts[j0]);    
    nelements++;
    return CANDY_DEF;
}
//
// '#' 'variable' <name> [:Size] ['=' Value]
//
static int candy_parse_variable(int j)
{
    int j0 = j;
    int k;

    if ((k=nelements) >= MAX_NUM_ELEMENTS)
	return CANDY_ERROR(E_ELEM_OVERFLOW);
    if (!T(ts,j,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    j++;
    element[k].type = C_VARIABLE;
    element[k].size = 1;  // fixme: default size? 
    if (T(ts,j,':') && T(ts,j+1,T_DEC)) {
	element[k].size = tok_to_int(&ts[j+1]);
	j += 2;
    }
    element[k].vtype = INT32;
    element[k].dir   = DIR_INOUT;
    element[k].clk = 0;    
    if (T(ts,j,'=') && is_const(ts[j+1].tval) && T(ts,j+2,T_END)) {
	element[k].cur.i32 = tok_to_int(&ts[j+1]);
    }
    else if (T(ts,j,T_END)) {
	element[k].cur.i32 = 0;
    }
    else
	return CANDY_ERROR(E_SYNTAX);
    element[k].nxt.i32 = element[k].cur.i32;
    make_str8(&element[k].name, &ts[j0]);    
    nelements++;
    return CANDY_DEF;
}

//
// Cond :=  <a-expr> <rel-op> <a-expr> |
//          <bit> | '!' <cond> | '(' <cond> ')' |
//          <cond> ',' <cond> | <cond> ';' <cond>
//        
// <a-expr> := <a-value> | '(' <a-expr> ')' | <a-expr> <arith-op> <aexpr>
// <a-value> := <bit> | <can-range> | <name> | Constant
//
// Event :=
//   ><name>=AValue
//
//
static xindex_t candy_parse_expr(int* ppos);
static xindex_t candy_parse_cond(int* ppos);
int is_cond_expr = 0;

static xindex_t candy_parse_avalue(int* ppos)
{
    int j = *ppos;
    
    if (T(ts,j,'(')) {
	xindex_t xi;
	j++;
	if (is_cond_expr) {
	    if ((xi = candy_parse_cond(&j)) == INVALID_INDEX)
		return INVALID_INDEX;
	}
	else {
	    if ((xi = candy_parse_expr(&j)) == INVALID_INDEX)
		return INVALID_INDEX;
	}
	if (T(ts,j,')')) {
	    *ppos = j+1;
	    return xi;
	}
	return CANDY_INVALID(E_SYNTAX);  // missing ')'
    }
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,'[') && is_const(ts[j+2].tval) &&	
	     T(ts,j+3,'.') && T(ts,j+4,'.') &&
	     is_const(ts[j+5].tval) && T(ts,j+6,']')) {
	candy_expr_t* xp = &expr[nexprs];
	int len;
	int pos = tok_to_int(&ts[j+2]);
	int pos1 = tok_to_int(&ts[j+5]);
	if ((pos < 0) || (pos > pos1))
	    return CANDY_INVALID(E_SYNTAX);
	if ((len = (pos1-pos)+1) > 32)
	    return CANDY_INVALID(E_RANGE);
	xp->op = EXPR_CAN_RANGE;
	xp->crange.id = tok_to_int(&ts[j]);
	xp->crange.pos = pos;
	xp->crange.len = len;
	*ppos = j+7;
	return nexprs++;
    }
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,'[') && is_const(ts[j+2].tval) &&	
	     T(ts,j+3,':') && is_const(ts[j+4].tval) && T(ts,j+5,']')) {
	candy_expr_t* xp = &expr[nexprs];
	int pos = tok_to_int(&ts[j+2]);
	int len = tok_to_int(&ts[j+4]);
	if ((pos < 0) || (pos >= 512))
	    return CANDY_INVALID(E_RANGE);
	if (len > 32)
	    return CANDY_INVALID(E_RANGE);
	xp->op = EXPR_CAN_RANGE;
	xp->crange.id = tok_to_int(&ts[j]);
	xp->crange.pos = pos;
	xp->crange.len = len;
	*ppos = j+6;
	return nexprs++;
    }    
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,'[') &&
	     is_const(ts[j+2].tval) &&	
	     T(ts,j+3,',') &&
	     is_const(ts[j+4].tval) &&
	     T(ts,j+5,']')) {
	candy_expr_t* xp = &expr[nexprs];
	int byte_pos, bit_pos;
	xp->op = EXPR_CAN_BIT2;
	xp->cbit.id = tok_to_int(&ts[j]);
	byte_pos = tok_to_int(&ts[j+2]);
	bit_pos = tok_to_int(&ts[j+4]);
	if ((byte_pos < 0) || (byte_pos >= 64))
	    return CANDY_INVALID(E_RANGE);
	if ((bit_pos < 0) || (bit_pos >= 8))
	    return CANDY_INVALID(E_RANGE);	    
	xp->cbit.byte_pos = byte_pos;
	xp->cbit.bit_pos = bit_pos;
	*ppos = j+6;
	return nexprs++;
    }
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,'[') &&
	     is_const(ts[j+2].tval) &&	
	     T(ts,j+3,']')) {
	candy_expr_t* xp = &expr[nexprs];
	xp->op = EXPR_CAN_BIT1;
	xp->cbit.id = tok_to_int(&ts[j]);
	xp->cbit.byte_pos = 0;  // bit used
	xp->cbit.bit_pos = tok_to_int(&ts[j+2]);
	*ppos = j+4;
	return nexprs++;
    }    
    else if (T(ts,j,T_DEC) || T(ts,j,T_HEX)) {
	candy_expr_t* xp = &expr[nexprs];
	xp->op = EXPR_CONST;
	xp->v.i32 = tok_to_int(&ts[j]);
	*ppos = j+1;
	return nexprs++;
    }
    else if (T(ts,j,T_WORD)) {
	eindex_t ei;
	candy_expr_t* xp;
	if ((ei = lookup_element(&ts[j])) == INVALID_INDEX) {
#ifdef DEBUG
	    fprintf(stderr, "word %.*s not found\n",
		    ts[j].len, ts[j].ptr);
#endif
	    return CANDY_INVALID(E_ELEMENT_NOT_FOUND);
	}
	xp = &expr[nexprs];
	xp->op = EXPR_NAME;
	xp->ei = ei;
	*ppos = j+1;
	return nexprs++;	
    }
    return CANDY_INVALID(E_SYNTAX);
}

static xindex_t candy_parse_mul(int* ppos)
{
    xindex_t li;
    int j;
    
    if ((li = candy_parse_avalue(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,'*') || T(ts,j,'/') || T(ts,j,'%')) {
	xindex_t ri;
	candy_expr_t* xp;
	*ppos = j+1;
	if ((ri = candy_parse_avalue(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	switch(ts[j].tval) {
	case '*': xp->op = EXPR_TIMES; break;
	case '/': xp->op = EXPR_DIVIDE; break;
	case '%': xp->op = EXPR_REMAINDER; break;
	default: return CANDY_INVALID(E_SYNTAX);
	}
	xp->bin.li = li;
	xp->bin.ri = ri;
	j = *ppos;
	li = nexprs++;
    }
    return li;
}

static xindex_t candy_parse_add(int* ppos)
{
    xindex_t li;    
    int j;
    
    if ((li = candy_parse_mul(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,'+') || T(ts,j,'-')) {
	xindex_t ri;	
	candy_expr_t* xp;
	*ppos = j+1;	
	if ((ri = candy_parse_mul(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	switch(ts[j].tval) {
	case '+': xp->op = EXPR_PLUS; break;
	case '-': xp->op = EXPR_MINUS; break;
	default: return CANDY_INVALID(E_SYNTAX);
	}
	xp->bin.li = li;
	xp->bin.ri = ri;
	j = *ppos;	
	li = nexprs++;
    }
    return li;
}

static xindex_t candy_parse_expr(int* ppos)
{
    return candy_parse_add(ppos);
}

static xindex_t candy_parse_relation(int* ppos)
{
    xindex_t li;
    xindex_t ri;
    candy_expr_t* xp;
    candy_op_t op;
    int j;
    
    if ((li = candy_parse_expr(ppos)) == INVALID_INDEX)
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
    if ((ri = candy_parse_expr(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;	
    xp = &expr[nexprs];
    xp->op = op;
    xp->bin.li = li;
    xp->bin.ri = ri;
    return nexprs++;
}

// A,B,C,D
static xindex_t candy_parse_conjunction(int* ppos)
{
    xindex_t li;
    int j;
    
    if ((li = candy_parse_relation(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,',')) {
	xindex_t ri;        
	candy_expr_t* xp;
	*ppos = j+1;	
	if ((ri = candy_parse_relation(ppos)) == INVALID_INDEX)
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
static xindex_t candy_parse_disjunction(int* ppos)
{
    xindex_t li;    
    int j;
    
    if ((li = candy_parse_conjunction(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,';')) {
	xindex_t ri;
	candy_expr_t* xp;
	*ppos = j+1;
	if ((ri = candy_parse_conjunction(ppos)) == INVALID_INDEX)
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

static xindex_t candy_parse_cond(int* ppos)
{
    xindex_t xi;
    is_cond_expr = 1;
    xi = candy_parse_disjunction(ppos);
    is_cond_expr = 0;
    return xi;
}

// <rule> :=
//  <name> = <expr>            // Set <name> to value of <expr>
//  <name> = <expr>  ? <cond>  // Conditionally set <name> to value
//
static int candy_parse_rule()
{
    candy_rule_t* rp;
    int j = 0;

    if (nrules >= MAX_NUM_RULES)
	return CANDY_ERROR(E_RULE_OVERFLOW);
    rp = &rule[nrules];
    if (!T(ts,0,T_WORD))
	return CANDY_ERROR(E_SYNTAX);
    if ((rp->ei = lookup_element(&ts[0])) == INVALID_INDEX) {
#ifdef DEBUG	
	fprintf(stder, "element %.*s not found\n", ts[0].len, ts[0].ptr);
#endif
	return CANDY_ERROR(E_ELEMENT_NOT_FOUND);
    }
    if (!T(ts,1,'='))
	return CANDY_ERROR(E_SYNTAX);
    j = 2;
    if ((rp->vi = candy_parse_expr(&j)) == INVALID_INDEX)
	return CANDY_ERR;
    rp->ci = INVALID_INDEX;
    if (T(ts,j,'?')) {
	j++;
	rp->ci = candy_parse_cond(&j);
    }
    if (!T(ts,j,T_END))
	return CANDY_ERROR(E_SYNTAX);
    nrules++;
    return CANDY_RULE;
}

// backward compatabilty
// create once:
//  #digital DEF_OUT out CANDY_OUT_PIN
//
// FrameID    BytePos    BitMask MaskOn MaskOff
// <frame-id> <byte-pos> <hex8>  <hex8>  <hex8>
//
// DEF_OUT = 1 ? FrameID[BytePos,i]  (when bit i in MaskOn is set)
// DEF_OUT = 0 ? FrameID[BytePos,i]  (when bit i in MaskOff is set)
//
static int candy_parse_old_rule()
{
    int n = 0;
    
    if (T(ts,0,T_HEX) && is_const(ts[1].tval) &&
	is_const(ts[2].tval) && is_const(ts[3].tval) &&
	is_const(ts[4].tval) && T(ts,5,T_END)) {
	uint8_t  mask      = tok_to_int(&ts[2]);
	uint8_t  match_1   = tok_to_int(&ts[3]);
	uint8_t  match_0   = tok_to_int(&ts[4]);
	uint16_t ei;	
	uint8_t  bit = 0x80;
	can_bit_t cbit;
	int i, k;

	cbit.id = tok_to_int(&ts[0]);
	cbit.byte_pos = tok_to_int(&ts[1]);

	ei = lookup_element_by_str("DEF_OUT", 9);
	if (ei == INVALID_INDEX) {
	    if (nelements >= MAX_NUM_ELEMENTS)
		return CANDY_ERROR(E_ELEM_OVERFLOW);	    
	    k = nelements++;
	    element[k].type = C_DIGITAL;
	    element[k].size = 1;
	    element[k].vtype = BOOL;
	    element[k].dir   = DIR_OUT;
	    element[k].io.port = 0;
	    element[k].io.pin = CANDY_OUT_PIN;
	    element[k].cur.i32 = element[k].nxt.i32 = 0;
	    element[k].clk = 0;	
	    element[k].name = "\7DEF_OUT"+1;
	    ei = k;
	    nelements++;
	}
	// Generate the ON rule(s)
	bit = 0x80;
	for (i = 0; i < 8; i++) {
	    if ((mask & bit) && (match_1 & bit)) {
		int j;
		if ((j=nrules++) >= MAX_NUM_RULES)
		    return CANDY_ERROR(E_RULE_OVERFLOW);
		if ((k=nexprs++) >= MAX_NUM_EXPRS)
		    return CANDY_ERROR(E_EXPR_OVERFLOW);
		cbit.bit_pos = i;
		expr[k].op   = EXPR_CAN_BIT2;
		expr[k].cbit = cbit;
		rule[j].ei = ei;
		rule[j].vi = expr_vi_1;
		rule[j].ci = k;
		n++;
	    }
	    bit >>= 1;
	}
	// Generate the OFF rule(s)
	bit = 0x80;
	for (i = 0; i < 8; i++) {
	    if ((mask & bit) && !(match_0 & bit)) {
		int j;
		if ((j=nrules++) >= MAX_NUM_RULES)
		    return CANDY_ERROR(E_RULE_OVERFLOW);
		if ((k=nexprs++) >= MAX_NUM_EXPRS)
		    return CANDY_ERROR(E_EXPR_OVERFLOW);
		cbit.bit_pos = i;
		expr[k].op   = EXPR_CAN_BIT2;
		expr[k].cbit = cbit;
		rule[j].ei = ei;
		rule[j].vi = expr_vi_0;
		rule[j].ci = k;
		n++;
	    }
	    bit >>= 1;
	}
    }
    else
	return CANDY_ERROR(E_SYNTAX);
    if (n) return CANDY_RULE;
    return CANDY_DEF;
}

static int candy_parse_event()
{
    eindex_t ei;
    
    nevents = 0;
    if (T(ts,0,'>') && T(ts,1,T_WORD) && T(ts,2,'=')) {
	int j;
	if ((ei = lookup_element(&ts[1])) == INVALID_INDEX) {
#ifdef DEBUG
	    fprintf(stderr, "element %.*s not found\n", ts[1].len, ts[1].ptr);
#endif
	    return CANDY_ERROR(E_ELEMENT_NOT_FOUND);
	}
	event.ei = ei;
	event.ci = INVALID_INDEX;
	j = 3;
	if ((event.vi = candy_parse_expr(&j)) == INVALID_INDEX)	    
	    return CANDY_ERR;
	// FIXME: look for T_END?
	nevents = 1;
	return CANDY_EVENT;
    }
    else if (T(ts,0,'>') && T(ts,1,T_WORD) && T(ts,2,T_END)) {
	if ((ei = lookup_element(&ts[1])) == INVALID_INDEX)
	    return CANDY_ERROR(E_ELEMENT_NOT_FOUND);
	candy_print_int(element[ei].cur.i32);
	candy_print_ln();
	return CANDY_EVENT;	
    }
    return CANDY_ERROR(E_SYNTAX);    
}

static int candy_parse()
{
    // parse tokens
    switch(ts[0].tval) {
    case T_END:
	return CANDY_EMPTY;
    case '#':
	switch(ts[1].tval) {
	case T_DIGITAL:
	    return candy_parse_digital(2);
	case T_ANALOG:
	    return candy_parse_analog(2); 
	case T_TIMER:
	    return candy_parse_timer(2); 		
	case T_CAN:
	    return candy_parse_can(2); 			
	case T_VARIABLE:
	    return candy_parse_variable(2);
	case T_CONSTANT:
	    return candy_parse_constant(2);
	default:
	    return CANDY_ERROR(E_SYNTAX);
	}
	break;
    case T_WORD:
	return candy_parse_rule();
    case T_HEX:
	return candy_parse_old_rule();
    case '>':
	return candy_parse_event();
    default:
	return CANDY_ERROR(E_SYNTAX);
    }
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
    ts[i].ptr = ptr;
    ptr = candy_tok0(ptr, &ts[i]);
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

int adc_get(int port, int pin)
{
#ifdef ARDUINO
    return  analogRead(pin);
#endif
    return 0;
}

void dac_set(int port, int pin, int value)
{
#ifdef ARDUINO
    return analogWrite(pin, value);
#endif    
}

const uint8_t* can_frame_get(uint32_t id, int* lenp)
{
    if (id == 0x123) {
	*lenp = 8;
	return (uint8_t*) "\0\1\2\3\4\5\6\7";
    }
    return NULL;
}

void candy_init()
{
#ifdef DEBUG
    printf("sizeof(candy_element_t) = %ld\n", sizeof(candy_element_t));
    printf("sizeof(candy_expr_t) = %ld\n",  sizeof(candy_expr_t));
    printf("sizeof(candy_rule_t) = %ld\n",  sizeof(candy_rule_t));
    printf("sizeof(token_t) = %ld\n",  sizeof(token_t));
#endif
    time_init();
#ifdef ARDUINO
    analogReadResolution(MAX_ADC_RESOLUTION);
    analogWriteResolution(MAX_DAC_RESOLUTION);    
#endif
    candy_reset();
}

// <can-frame>'[' <bit-pos> ']'
static int32_t candy_read_can_bit1(can_bit_t* cp, int32_t* valuep)
{
    int len;
    const uint8_t* ptr = can_frame_get(cp->id, &len);	    
    if (ptr != NULL) {
	int bitpos = cp->bit_pos;
	int pos = bitpos >> 3;  // byte pos
	if (pos < len) {
	    *valuep = get_bit_le(ptr, bitpos);
	    return 1;
	}
    }
    return 0;
}

// <can-frame>'['<byte-pos>',' <bit-pos>']'
static int candy_read_can_bit2(can_bit_t* cp, int32_t* valuep)
{
    int len;
    const uint8_t* ptr = can_frame_get(cp->id, &len);
    if (ptr != NULL) {
	if (cp->byte_pos < len) {
	    *valuep = get_bit_le2(ptr, cp->byte_pos, cp->bit_pos);
	    return 1;
	}
    }
    return 0;
}

// <frame-id>'[' <bit-pos1> '.''.' <bit-pos2>']'
// <frame-id>'[' <bit-pos> ':' <bit-len> ']'
static int candy_read_can_range(can_range_t* crp, int32_t* valuep)
{
    int len;
    const uint8_t* ptr = can_frame_get(crp->id, &len);

    if (ptr != NULL) {
	int bpos = (crp->pos + crp->len - 1) >> 3;

	if (bpos < len) {
	    get_bits_be(ptr, (uint32_t*)valuep, crp->pos, crp->len);
	    return 1;
	}
    }
    return 0;
}

int32_t eval_expr(xindex_t xi)
{
    candy_expr_t* xp = &expr[xi];
    switch(xp->op) {
    case EXPR_NAME:      return element[xp->ei].cur.i32;
    case EXPR_CONST:     return xp->v.i32;
    case EXPR_CAN_RANGE: {
	int32_t val;
	if (candy_read_can_range(&xp->crange, &val))
	    return val;
	return 0;
    }
    case EXPR_CAN_BIT1: {
	int32_t val;
	if (candy_read_can_bit1(&xp->cbit, &val))
	    return val;
	return 0;
    }
    case EXPR_CAN_BIT2: {
	int32_t val;
	if (candy_read_can_bit2(&xp->cbit, &val))
	    return val;
	return 0;
    }	
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


// load all gpio/analog/can-data into the buffer value
static void candy_read_input()
{
    int i;

    cycle->cur.u32++;
    tick->cur.u32 = time_tick();
    
    for (i = 0; i < nelements; i++) {
	candy_element_t* elem = &element[i];
	switch(elem->type) {
	case C_CONSTANT: break;
	case C_VARIABLE: break;
	case C_DIGITAL:
	    if (elem->dir & DIR_IN)
		elem->cur.i32 = gpio_get(elem->io.port, elem->io.pin);
	    break;
	case C_ANALOG:
	    if (elem->dir & DIR_IN)
		elem->cur.i32 = adc_get(elem->io.port, elem->io.pin);
	    break;
	case C_CAN_BIT1:
	    candy_read_can_bit1(&elem->can, &elem->cur.i32);
	    break;
	case C_CAN_BIT2:
	    candy_read_can_bit2(&elem->can, &elem->cur.i32);
	    break;
	case C_CAN_RANGE:
	    candy_read_can_range(&elem->canr, &elem->cur.i32);
	    break;
	case C_TIMER: {
	    // true if timeout
	    elem->cur.i32 = 0;
	    if (elem->timer.flags & C_TIMER_TIMEOUT)
		elem->cur.i32 = 1;
	    else if (elem->timer.flags & C_TIMER_RUNNING) {
		uint32_t elapsed = time_elapsed_ms(elem->timer.start_tick,NULL);
		if (elapsed >= elem->timer.timeout) {
		    elem->cur.i32 = 1;
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

static void candy_write_output()
{
    int i;
    for (i = 0; i < nelements; i++) {
	candy_element_t* elem = &element[i];
	switch(elem->type) {
	case C_CONSTANT: break;
	case C_VARIABLE:
	    if ((elem->dir & DIR_OUT) && (elem->cur.i32 != elem->nxt.i32)) {
		elem->cur.i32 = elem->nxt.i32;
		if (verbose) {
		    candy_print_str("SET: ");
		    candy_print_str(elem->name);
		    candy_print_str(" = ");
		    candy_print_int(elem->cur.i32);
		    candy_print_ln();
		}
	    }
	    break;
	case C_DIGITAL:
	    if (latch->cur.u32) break;
	    if (elem->dir & DIR_OUT) {
		if (elem->cur.i32 != elem->nxt.i32) {
		    elem->cur.i32 = elem->nxt.i32;
		    gpio_set(elem->io.port, elem->io.pin, elem->cur.i32);
		    if (verbose) {
			candy_print_str("GPIO-SET: ");
			candy_print_str(elem->name);
			candy_print_str(" ");			
			if (elem->io.port >= 0) {
			    candy_print_int(elem->io.port);
			    candy_print_str(":");
			}
			candy_print_int(elem->io.pin);
			candy_print_str(" = ");
			candy_print_int(elem->cur.i32);
			candy_print_ln();
		    }
		}
	    }
	    break;
	case C_ANALOG:
	    if (latch->cur.u32) break;
	    if (elem->dir & DIR_OUT) {
		if (elem->cur.i32 != elem->nxt.i32) {
		    elem->cur.i32 = elem->nxt.i32;
		    dac_set(elem->io.port, elem->io.pin, elem->cur.i32);
		    if (verbose) {
			candy_print_str("DAC-SET: ");
			candy_print_str(elem->name);
			candy_print_str(" ");
			if (elem->io.port >= 0) {
			    candy_print_int(elem->io.port);
			    candy_print_str(":");
			}
			candy_print_int(elem->io.pin);
			candy_print_str(" = ");
			candy_print_int(elem->cur.i32);
			candy_print_ln();
		    }		    
		}
	    }
	    break;
	case C_CAN_BIT1:
	case C_CAN_BIT2:
	case C_CAN_RANGE:
	    if (latch->cur.u32) break;
	    // fixme: write bits into can frame
	    break;
	case C_TIMER:
	    if (elem->nxt.i32 && !(elem->timer.flags & C_TIMER_RUNNING)) {
		elem->cur.i32 = 0; // not timed out, just started
		elem->timer.start_tick = time_tick();
		elem->timer.flags |= C_TIMER_RUNNING;
		elem->timer.flags &= ~C_TIMER_TIMEOUT;
		if (verbose) {
		    candy_print_str("TIMER-START: ");
		    candy_print_str(elem->name);
		    candy_print_str(" ");
		    candy_print_int(elem->timer.timeout);
		    candy_print_ln();
		}
	    }
	    else if ((elem->nxt.i32==0) &&
		     elem->timer.flags & C_TIMER_RUNNING) {
		// stop timer
		elem->cur.i32 = 0; // stopped not timed out!
		elem->timer.flags &= ~C_TIMER_RUNNING;
		elem->timer.flags |= C_TIMER_STOPPED;
		if (verbose) {
		    candy_print_str("TIMER-STOPPED: ");
		    candy_print_str(elem->name);
		    candy_print_ln();
		}	    
	    }
	}
    }
}

// emit all updated CAN frames 
static void candy_emit_frames()
{

}

static void candy_run_rule_(candy_rule_t* rp, uint32_t clk)
{
    candy_element_t* elem = &element[rp->ei];
    // element may be set AND element not set in this cycle
    if ((elem->dir & DIR_OUT) && (elem->clk != clk)) { 
	int32_t value = (rp->ci == INVALID_INDEX) ? 1 : eval_expr(rp->ci);
	if (value) {
	    elem->nxt.i32 = eval_expr(rp->vi);
	    elem->clk = clk;
	    nupdates++;
	}
    }
}

static void candy_run_event(candy_rule_t* rp)
{
    candy_run_rule_(&event, cycle->cur.u32);
    candy_pop();
}

static void candy_run_rules()
{
    int i;
    uint32_t clk = cycle->cur.u32;
    nupdates = 0;
    for (i = 0; i < nrules; i++) {
	candy_run_rule_(&rule[i], clk);
    }
}

static void candy_parse_line(char* buf)
{
    int n;

    if ((n = candy_scan_line(ts, buf)) <= 0)
	return;
#ifdef DEBUG
    print_tokens(ts, n);
#endif
    candy_push();
    switch (candy_parse()) {
    case CANDY_EMPTY:
	candy_pop();
	break;
    case CANDY_ERR:
	candy_pop();
	candy_print_str("ERR (");
	candy_print_error(candy_errno);
	candy_print_str(")\n");
#ifdef DEBUG	
	printf("parse error\n");
	print_tokens(ts, n);
#endif
	break;
    case CANDY_DEF:
	candy_commit();
	candy_print_str("DEF ");
	candy_print_str(element[nelements-1].name);
	candy_print_str(" OK\n");
#ifdef DEBUG
	printf("DEF[%d]: ", nelements-1);	
	print_element(&element[nelements-1]);
	printf("\n");
#endif
	break;
    case CANDY_RULE:
	candy_commit();
	candy_print_str("RULE ");
	candy_print_str(element[rule[nrules-1].ei].name);
	candy_print_str(" OK\n");	
#ifdef DEBUG	
	printf("RULE[%d]: ", nrules-1);
	print_rule(&rule[nrules-1]);
	printf("\n");
#endif	
	break;
    case CANDY_EVENT:
	candy_print_str("EVENT ");
	candy_print_str(element[event.ei].name);
	candy_print_str(" OK\n");
#ifdef DEBUG
	printf("EVENT: ");
	print_rule(&event);
	printf("\n");
#endif
    }
}

#endif
