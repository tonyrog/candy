#ifndef __CANDY_SPEAK_H__
#define __CANDY_SPEAK_H__

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>

#include "bitpack/bitpack.h"

#define MIN_LINE_LENGTH  128
#define STACK_SIZE       8
#define MAX_STRING_TAB   512
#define MAX_NUM_TOKENS   64
#define MAX_NUM_ELEMENTS 32
#define MAX_NUM_RULES    32
#define MAX_NUM_EXPRS    64
#define MAX_CODE_WORDS   64
#define MAX_XNODES       5
#define MAX_CAN_FRAMES   8
#define DEFAULT_BIT_SIZE (sizeof(int)*8)

typedef struct {
    uint32_t id;
    int8_t   byte_pos;   // 0..7  | 0..63 | -1
    uint16_t bit_pos;    // 0..7  | 0..512 |
} candy_bit_t;

// <frame-id> '[' pos0 .. pos1 ']  - extract pos1-pos0+1 bits (pos1 >= pos0)
// <frame-id> '[' pos ':' len ']  - extract pos-(pos+len-1) len bits
typedef struct {
    uint32_t id;         // Frame ID
    uint16_t pos;   // 0..63  | 0..512 - start bit
    uint16_t len;   // 1..32           - number of bits
} candy_range_t;

typedef struct {
    uint32_t inhibit;   // ms before next frame
    uint32_t last_tick; // tick for last send
    uint32_t id;
    uint8_t  mark;      // mark for transmission
    uint8_t  len;
    uint8_t data[8];
} candy_frame_t;

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
    C_CANDY_BIT1  = 5,   // bitpos
    C_CANDY_BIT2  = 6,   // byte & bit
    C_CANDY_RANGE = 7,
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
	portbit_t     io;
	candy_bit_t   can;
	candy_range_t canr;
	ctimer_t      timer;
    };
} candy_element_t;

typedef enum {
    EXPR_NAME,  // analog/digital/variable/timer...
    EXPR_CONST, // constant value
    EXPR_CANDY_RANGE,
    EXPR_CANDY_BIT1,   // bitpos FrameID[bit-pos]
    EXPR_CANDY_BIT2,   // byte&bit-pos  FrameID[byte-pos,bit-pos]
    // rel-op
    EXPR_LT,        // A < B
    EXPR_LTE,       // A <= B
    EXPR_GT,        // A > B
    EXPR_GTE,       // A >= B
    EXPR_EQ,        // A == B
    EXPR_NEQ,       // A != B
    // logic-op
    EXPR_NOT,       // !A    
    EXPR_OR,        // A || B
    EXPR_AND,       // A && B
    // arith-op
    EXPR_NEG,       // -A
    EXPR_PLUS,      // A + B
    EXPR_MINUS,     // A - B
    EXPR_TIMES,     // A * B
    EXPR_DIVIDE,    // A / B
    EXPR_REMAINDER, // A % B
    EXPR_BAND,      // A & B
    EXPR_BOR,       // A | B
    EXPR_BXOR,      // A ^ B
    EXPR_BNOT,      // ~A
    EXPR_BSL,       // A << B
    EXPR_BSR,       // A >> B

    EXPR_JZ,        // followed by label
    EXPR_JNZ,       // followed by label
} candy_op_t;


#define INVALID_INDEX 0xffff   // may not be used as index
typedef uint16_t  xindex_t;    // expr index
typedef uint16_t  eindex_t;    // element index

typedef struct _candy_expr_t {
    candy_op_t op;
    union {
	candy_value_t v;         // CONST (depend on type)
	eindex_t     ei;         // NAME element[ei]
	candy_range_t  crange;   // CANDY_RANGE
	candy_bit_t    cbit;     // CANDY_BIT
	xindex_t       una;      // EXPR_NOT, EXPR_NEG, EXPR_BNOT...
	struct {  // EXPR_xxx
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

int nelements;
candy_element_t element[MAX_NUM_ELEMENTS];

int nexprs;
candy_expr_t expr[MAX_NUM_EXPRS];

int ncodes;
static int32_t code[MAX_CODE_WORDS];  // 2-bit tagged 30-bit signed integers!

int nrules;
candy_rule_t rule[MAX_NUM_RULES];

int nframes;
candy_frame_t send[MAX_CAN_FRAMES];
candy_frame_t recv[MAX_CAN_FRAMES];
		   
int nstr;
char str[MAX_STRING_TAB];

// current parsed event (must execute immediatly)
int nevents;  // 0 | 1
candy_rule_t event;

int nupdates; // number of elements updated during rules eval

int verbose = 1;

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


static int can_recv_find(uint32_t id)
{
    int i;
    for (i = 0; i < nframes; i++) {
	if (recv[i].id == id)
	    return i;
    }
    return -1;
}

static int can_send_find(uint32_t id)
{
    int i;
    for (i = 0; i < nframes; i++) {
	if (send[i].id == id)
	    return i;
    }
    return -1;
}

// boards:
// ARDUINO_AVR_UNO
// ARDUINO_AVR_DUEMILANOVE
// ARUDINO_ITSYBITSY_M0
// ARUDINO_SAM_DUE
// ARDUINO_SAM3X8E
// ARDUINO_SAM_ZERO
// SAMD_SERIES
// .. boards.txt howto find families and properties?
// platform.txt ?

#ifdef ARDUINO
#include <Arduino_CAN.h>
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

static size_t candy_print_hex(int val)
{
    return Serial.print(val, HEX);
}

static size_t candy_print_ln()
{
    return Serial.println();
}

static void candy_can_init()
{
    CAN.begin(CanBitRate::BR_250k);
}

static void candy_send_can_frame(candy_frame_t* cframe)
{
    CanMsg msg(cframe->id, cframe->len, cframe->data);
    CAN.write(msg);
}

static int candy_recv_can_frame(tick_t t)
{
    if (CAN.available()) {
	int i;
	CanMsg const msg = CAN.read();
	if ((i = can_recv_find(msg.id)) >= 0) {
	    memcpy(recv[i].data, msg.data, msg.data_length);
	    recv[i].len = msg.data_length;
	    recv[i].mark = 1;
	    recv[i].last_tick = t;
	}
	return 1;
    }
    return 0;
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

static size_t candy_print_char(char c)
{
    return printf("%c", c);
}

static size_t candy_print_str_len(const char* str, int len)
{
    return printf("%.*s", len, str);
}

static size_t candy_print_str(const char* str)
{
    return printf("%s", str);
}

static size_t candy_print_int(int val)
{
    return printf("%d", val);    
}

static size_t candy_print_hex(int val)
{
    return printf("%x", val);    
}

static size_t candy_print_ln()
{
    return printf("\n");
}

static void candy_can_init()
{
}

static void candy_send_can_frame(candy_frame_t* cframe)
{
}

static int candy_recv_can_frame(tick_t t)
{
    return 0;
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
static uint32_t time_elapsed_ms(tick_t since, tick_t now)
{
    return now - since;
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
    nframes   = 0;

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

#include "CandyParse.h"

token_t ts[MAX_NUM_TOKENS+1];



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

#define T(tv,i,v) ((tv)[(i)].tval == (v))

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

    if (T(ts,j,T_DEC) && T(ts,j+1,T_COLON) &&
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
    if (T(ts,j,T_COLON) && T(ts,j+1,T_DEC)) {
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
    if (T(ts,j,T_DEC) && T(ts,j+1,T_COLON) && is_analog_pin(ts[j+2].tval) &&
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
    element[k].size = DEFAULT_BIT_SIZE;
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
    if (T(ts,j,T_LEFT_BRACKET)&&T(ts,j+1,T_DEC)&&
	T(ts,j+2,T_RIGHT_BRACKET) && T(ts,j+3,T_END)) {
	element[k].type = C_CANDY_BIT1;
	element[k].can.byte_pos = 0;  // not used
	element[k].can.bit_pos = tok_to_int(&ts[4]);
    }    
    else if (T(ts,j,T_LEFT_BRACKET)&&T(ts,j+1,T_DEC)&&T(ts,j+2,T_COMMA) &&
	     T(ts,j+3,T_DEC)&&T(ts,j+4,T_RIGHT_BRACKET)&&T(ts,j+5,T_END)) {
	element[k].type = C_CANDY_BIT2;
	element[k].can.byte_pos = tok_to_int(&ts[j+1]);
	element[k].can.bit_pos = tok_to_int(&ts[j+3]);
    }
    else if (T(ts,j,T_LEFT_BRACKET)&&T(ts,j+1,T_DEC)&&T(ts,j+2,T_FULL_STOP) &&
	     T(ts,j+3,T_FULL_STOP) &&
	     T(ts,j+4,T_DEC)&&T(ts,j+5,T_RIGHT_BRACKET)&&T(ts,j+6,T_END)) {
	int len;
	int pos = tok_to_int(&ts[j+1]);
	int pos1 = tok_to_int(&ts[j+4]);
	if ((pos < 0) || (pos > pos1) || (pos1 >= 512))
	    return CANDY_ERROR(E_RANGE);
	if ((len = (pos1-pos)+1) > 32)
	    return CANDY_ERROR(E_RANGE);
	element[k].type = C_CANDY_RANGE;	
	element[k].canr.pos = pos;
	element[k].canr.len = len;
    }
    else if (T(ts,j,T_LEFT_BRACKET)&&T(ts,j+1,T_DEC)&&T(ts,j+2,T_COLON) &&
	     T(ts,j+3,T_DEC)&&T(ts,j+4,T_RIGHT_BRACKET)&&T(ts,j+5,T_END)) {
	int pos = tok_to_int(&ts[j+1]);
	int len = tok_to_int(&ts[j+3]);
	if ((pos < 0) || (pos >= 512) || (len <= 0) || (len > 32))
	    return CANDY_ERROR(E_RANGE);	    
	element[k].type = C_CANDY_RANGE;	
	element[k].canr.pos = pos;
	element[k].canr.len = len;
    } 
    else
	return CANDY_ERROR(E_SYNTAX);
    // create or remove a frame
    if (can_recv_find(element[k].can.id) < 0) {
	int j;
	if ((j=nframes) >= MAX_CAN_FRAMES)
	    return CANDY_ERROR(E_RANGE);
	send[j].inhibit = 50;  // 20Hz!
	send[j].last_tick = 0;	
	send[j].id = element[k].can.id;
	send[j].mark = 0;
	send[j].len = 8;
	memset(send[j].data, 0, sizeof(send[j].data));
	recv[j].id = element[k].can.id;
	send[j].inhibit = 50;  // 20Hz!
	send[j].last_tick = 0;
	send[j].mark = 0;	
	recv[j].len = 8;
	memset(recv[j].data, 0, sizeof(recv[j].data));
	nframes++;
    }
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
    element[k].size = DEFAULT_BIT_SIZE;
    if (T(ts,j,T_COLON) && T(ts,j+1,T_DEC)) {
	element[k].size = tok_to_int(&ts[j+1]);
	j += 2;
    }
    element[k].vtype = INT32;
    element[k].dir   = DIR_IN;
    element[k].clk   = 0;    
    if (T(ts,j,T_EQUALS_SIGN) && is_const(ts[j+1].tval) && T(ts,j+2,T_END)) {
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
    element[k].size = DEFAULT_BIT_SIZE;
    if (T(ts,j,T_COLON) && T(ts,j+1,T_DEC)) {
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
    
    if (T(ts,j,T_LEFT_PARANTHESES)) {
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
	if (T(ts,j,T_RIGHT_PARANTHESES)) {
	    *ppos = j+1;
	    return xi;
	}
	return CANDY_INVALID(E_SYNTAX);  // missing ')'
    }
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,T_LEFT_BRACKET) && is_const(ts[j+2].tval) &&	
	     T(ts,j+3,T_FULL_STOP) && T(ts,j+4,T_FULL_STOP) &&
	     is_const(ts[j+5].tval) && T(ts,j+6,T_RIGHT_BRACKET)) {
	candy_expr_t* xp = &expr[nexprs];
	int len;
	int pos = tok_to_int(&ts[j+2]);
	int pos1 = tok_to_int(&ts[j+5]);
	if ((pos < 0) || (pos > pos1))
	    return CANDY_INVALID(E_SYNTAX);
	if ((len = (pos1-pos)+1) > 32)
	    return CANDY_INVALID(E_RANGE);
	xp->op = EXPR_CANDY_RANGE;
	xp->crange.id = tok_to_int(&ts[j]);
	xp->crange.pos = pos;
	xp->crange.len = len;
	*ppos = j+7;
	return nexprs++;
    }
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,T_LEFT_BRACKET) && is_const(ts[j+2].tval) &&	
	     T(ts,j+3,T_COLON) && is_const(ts[j+4].tval) &&
	     T(ts,j+5,T_RIGHT_BRACKET)) {
	candy_expr_t* xp = &expr[nexprs];
	int pos = tok_to_int(&ts[j+2]);
	int len = tok_to_int(&ts[j+4]);
	if ((pos < 0) || (pos >= 512))
	    return CANDY_INVALID(E_RANGE);
	if (len > 32)
	    return CANDY_INVALID(E_RANGE);
	xp->op = EXPR_CANDY_RANGE;
	xp->crange.id = tok_to_int(&ts[j]);
	xp->crange.pos = pos;
	xp->crange.len = len;
	*ppos = j+6;
	return nexprs++;
    }    
    else if (T(ts,j,T_HEX) &&
	     T(ts,j+1,T_LEFT_BRACKET) &&
	     is_const(ts[j+2].tval) &&	
	     T(ts,j+3,T_COMMA) &&
	     is_const(ts[j+4].tval) &&
	     T(ts,j+5,T_RIGHT_BRACKET)) {
	candy_expr_t* xp = &expr[nexprs];
	int byte_pos, bit_pos;
	xp->op = EXPR_CANDY_BIT2;
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
	     T(ts,j+1,T_LEFT_BRACKET) &&
	     is_const(ts[j+2].tval) &&	
	     T(ts,j+3,T_RIGHT_BRACKET)) {
	candy_expr_t* xp = &expr[nexprs];
	xp->op = EXPR_CANDY_BIT1;
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
	    candy_print_str("word ");
	    candy_print_str_len(ts[j].ptr,ts[j].len);
	    candy_print_str("not found");
	    candy_print_ln();
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
    while(T(ts,j,T_STAR) || T(ts,j,T_SLASH) || T(ts,j,T_PERCENT)) {
	xindex_t ri;
	candy_expr_t* xp;
	*ppos = j+1;
	if ((ri = candy_parse_avalue(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	switch(ts[j].tval) {
	case T_STAR: xp->op = EXPR_TIMES; break;
	case T_SLASH: xp->op = EXPR_DIVIDE; break;
	case T_PERCENT: xp->op = EXPR_REMAINDER; break;
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
    while(T(ts,j,T_PLUS) || T(ts,j,T_MINUS)) {
	xindex_t ri;	
	candy_expr_t* xp;
	*ppos = j+1;	
	if ((ri = candy_parse_mul(ppos)) == INVALID_INDEX)
	    return INVALID_INDEX;
	xp = &expr[nexprs];
	switch(ts[j].tval) {
	case T_PLUS: xp->op = EXPR_PLUS; break;
	case T_MINUS: xp->op = EXPR_MINUS; break;
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
    switch(ts[j].tval) {
    case T_LESS_THAN_OR_EQUAL_SIGN: op = EXPR_LTE; break;
    case T_LESS_THAN_SIGN: op = EXPR_LT; break;
    case T_GREATER_THAN_OR_EQUAL_SIGN: op = EXPR_GTE; break;
    case T_GREATER_THAN_SIGN: op = EXPR_GT; break;
    case T_DOUBLE_EQUALS_SIGN: op = EXPR_EQ; break;
    case T_NOT_EQUALS_SIGN: op = EXPR_NEQ; break;
    default:
	return li;
    }
    *ppos = j+1;
    if ((ri = candy_parse_expr(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;	
    xp = &expr[nexprs];
    xp->op = op;
    xp->bin.li = li;
    xp->bin.ri = ri;
    return nexprs++;
}

// A && B && C && D
static xindex_t candy_parse_conjunction(int* ppos)
{
    xindex_t li;
    int j;
    
    if ((li = candy_parse_relation(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,T_DOUBLE_AMPERSAND)) {
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

// A || B || C || D
static xindex_t candy_parse_disjunction(int* ppos)
{
    xindex_t li;    
    int j;
    
    if ((li = candy_parse_conjunction(ppos)) == INVALID_INDEX)
	return INVALID_INDEX;
    j = *ppos;
    while(T(ts,j,T_DOUBLE_VERTICAL_BAR)) {
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
	candy_print_str("element '");
	candy_print_str_len(ts[0].ptr, ts[0].len);
	candy_print_str("' not found)");
	candy_print_ln();
#endif
	return CANDY_ERROR(E_ELEMENT_NOT_FOUND);
    }
    if (!T(ts,1,T_EQUALS_SIGN))
	return CANDY_ERROR(E_SYNTAX);
    j = 2;
    if ((rp->vi = candy_parse_expr(&j)) == INVALID_INDEX)
	return CANDY_ERR;
    rp->ci = INVALID_INDEX;
    if (T(ts,j,T_QUESTION_MARK)) {
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
	candy_bit_t cbit;
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
		expr[k].op   = EXPR_CANDY_BIT2;
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
		expr[k].op   = EXPR_CANDY_BIT2;
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
    if (T(ts,0,T_GREATER_THAN_SIGN) &&
	T(ts,1,T_WORD) && T(ts,2,T_EQUALS_SIGN)) {
	int j;
	if ((ei = lookup_element(&ts[1])) == INVALID_INDEX) {
#ifdef DEBUG
	    candy_print_str("word ");
	    candy_print_str_len(ts[1].ptr,ts[1].len);
	    candy_print_str("not found");
	    candy_print_ln();
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
    else if (T(ts,0,T_GREATER_THAN_SIGN) && T(ts,1,T_WORD) && T(ts,2,T_END)) {
	if ((ei = lookup_element(&ts[1])) == INVALID_INDEX)
	    return CANDY_ERROR(E_ELEMENT_NOT_FOUND);
	candy_print_int(element[ei].cur.i32);
	candy_print_ln();
	return CANDY_EVENT;	
    }
    else if (T(ts,0,T_GREATER_THAN_SIGN) && T(ts,1,T_RESET) && T(ts,2,T_END)) {
	candy_reset();
	return CANDY_EVENT;		
    }
    else if (T(ts,0,T_GREATER_THAN_SIGN) && T(ts,1,T_PUSH) && T(ts,2,T_END)) {
	candy_push();  // FIXME: handle overflow
	return CANDY_EVENT;
    }
    else if (T(ts,0,T_GREATER_THAN_SIGN) && T(ts,1,T_POP) && T(ts,2,T_END)) {
	candy_pop();   // FIXME: handle underflow
	return CANDY_EVENT;
    }
    else if (T(ts,0,T_GREATER_THAN_SIGN) && T(ts,1,T_LIST) && T(ts,2,T_END)) {
	// list
	return CANDY_EVENT;	
    }
    else if (T(ts,0,T_GREATER_THAN_SIGN) && T(ts,1,T_CLEAR) && T(ts,2,T_END)) {
	// clear
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
    case T_GREATER_THAN_SIGN:
	return candy_parse_event();
    default:
	return CANDY_ERROR(E_SYNTAX);
    }
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
    candy_can_init();
    candy_reset();
}

// <can-frame>'[' <bit-pos> ']'
static int32_t candy_read_candy_bit1(candy_bit_t* cp, int32_t* valuep)
{
    int i;
    if ((i = can_recv_find(cp->id)) >= 0) {
	int bitpos = cp->bit_pos;
	int pos = bitpos >> 3;  // byte pos
	if (pos < recv[i].len) {
	    *valuep = get_bit_be(recv[i].data, bitpos);
	    return 1;
	}
    }
    return 0;
}

// <can-frame>'['<byte-pos>',' <bit-pos>']'
static int candy_read_candy_bit2(candy_bit_t* cp, int32_t* valuep)
{
    int i;
    if ((i = can_recv_find(cp->id)) >= 0) {
	if (cp->byte_pos < recv[i].len) {
	    *valuep = get_bit_be2(recv[i].data, cp->byte_pos, cp->bit_pos);
	    return 1;
	}
    }
    return 0;
}

// <frame-id>'[' <bit-pos1> '.''.' <bit-pos2>']'
// <frame-id>'[' <bit-pos> ':' <bit-len> ']'
static int candy_read_candy_range(candy_range_t* crp, int32_t* valuep)
{
    int i;
    if ((i = can_recv_find(crp->id)) >= 0) {    
	int bpos = (crp->pos + crp->len - 1) >> 3;
	if (bpos < recv[i].len) {
	    get_bits_be(recv[i].data, (uint32_t*)valuep, crp->pos, crp->len);
	    return 1;
	}
    }
    return 0;
}


// <can-frame>'[' <bit-pos> ']'
static int32_t candy_write_candy_bit1(candy_bit_t* cp, int32_t value)
{
    int i;
    if ((i = can_send_find(cp->id)) >= 0) {        
	int bit_pos = cp->bit_pos;
	int byte_pos = bit_pos >> 3;  // byte pos
	if (byte_pos < send[i].len) {
	    set_bit_be(send[i].data, bit_pos, value);
	    return 1;
	}
    }
    return 0;
}

// <can-frame>'['<byte-pos>',' <bit-pos>']'
static int candy_write_candy_bit2(candy_bit_t* cp, int32_t value)
{
    int i;
    if ((i = can_send_find(cp->id)) >= 0) {    
	if (cp->byte_pos < send[i].len) {
	    set_bit_be2(send[i].data, cp->byte_pos, cp->bit_pos, value);
	    return 1;
	}
    }
    return 0;
}

// <frame-id>'[' <bit-pos1> '.''.' <bit-pos2>']'
// <frame-id>'[' <bit-pos> ':' <bit-len> ']'
static int candy_write_candy_range(candy_range_t* crp, int32_t value)
{
    int i;
    if ((i = can_send_find(crp->id)) >= 0) {        
	int byte_pos = (crp->pos + crp->len - 1) >> 3;
	if (byte_pos < send[i].len) {
	    set_bits_be(send[i].data, value, crp->pos, crp->len);
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
    case EXPR_CANDY_RANGE: {
	int32_t val;
	if (candy_read_candy_range(&xp->crange, &val))
	    return val;
	return 0;
    }
    case EXPR_CANDY_BIT1: {
	int32_t val;
	if (candy_read_candy_bit1(&xp->cbit, &val))
	    return val;
	return 0;
    }
    case EXPR_CANDY_BIT2: {
	int32_t val;
	if (candy_read_candy_bit2(&xp->cbit, &val))
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
    case EXPR_NOT: return !eval_expr(xp->una);
    case EXPR_OR: return eval_expr(xp->bin.li) || eval_expr(xp->bin.ri);
    case EXPR_AND: return eval_expr(xp->bin.li) && eval_expr(xp->bin.ri);
    // arith-op
    case EXPR_NEG: return -eval_expr(xp->una);
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

static void candy_read_can_frames(tick_t t)
{
    while(candy_recv_can_frame(t))
	;
}

// load all gpio/analog/can-data into the buffer value
static void candy_read_input(tick_t t)
{
    int i;

    cycle->cur.u32++;
    tick->cur.u32 = t;

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
	case C_CANDY_BIT1:
	    candy_read_candy_bit1(&elem->can, &elem->cur.i32);
	    break;
	case C_CANDY_BIT2:
	    candy_read_candy_bit2(&elem->can, &elem->cur.i32);
	    break;
	case C_CANDY_RANGE:
	    candy_read_candy_range(&elem->canr, &elem->cur.i32);
	    break;
	case C_TIMER: {
	    // true if timeout
	    elem->cur.i32 = 0;
	    if (elem->timer.flags & C_TIMER_TIMEOUT)
		elem->cur.i32 = 1;
	    else if (elem->timer.flags & C_TIMER_RUNNING) {
		uint32_t elapsed = time_elapsed_ms(elem->timer.start_tick,t);
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

static void candy_write_output(tick_t t)
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
	case C_CANDY_BIT1: {
	    if (latch->cur.u32) break;	    
	    candy_write_candy_bit1(&elem->can, elem->nxt.i32);
	    elem->cur.i32 = elem->nxt.i32;
	    break;
	}
	    
	case C_CANDY_BIT2: {
	    if (latch->cur.u32) break;	    
	    candy_write_candy_bit2(&elem->can, elem->nxt.i32);
	    elem->cur.i32 = elem->nxt.i32;
	    break;
	}
	case C_CANDY_RANGE: {
	    if (latch->cur.u32) break;	    
	    candy_write_candy_range(&elem->canr, elem->nxt.i32);
	    elem->cur.i32 = elem->nxt.i32;
	    break;
	}
	case C_TIMER:
	    if (elem->nxt.i32 && !(elem->timer.flags & C_TIMER_RUNNING)) {
		elem->cur.i32 = 0; // not timed out, just started
		elem->timer.start_tick = t;
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
static void candy_write_can_frames(tick_t t)
{
    int i;
    for (i = 0; i < nframes; i++) {
	if (t > (send[i].last_tick + send[i].inhibit)) { // >= ?
	    candy_send_can_frame(&send[i]);
	    send[i].last_tick = t;
	}
    }
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
