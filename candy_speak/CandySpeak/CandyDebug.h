// debug utils and output
#ifndef __CANDY_DEBUG_H__
#define __CANDY_DEBUG_H__

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

static const char* format_dir(int dir)
{
    switch(dir) {
    case DIR_IN: return "in";
    case DIR_OUT: return "out";
    case DIR_INOUT: return "inout";
    case DIR_NONE: return "";
    default: return "--";
    }
}

static void print_element(candy_element_t* elem)
{
    switch(elem->type) {
    case C_DIGITAL:
	printf("#digital %s %s ",
	       elem->name, format_dir(elem->dir));	
	if (elem->io.port == -1)
	    printf("%d", elem->io.pin);
	else 
	    printf("%d:%d", 
		   elem->io.port, elem->io.pin);
	break;
    case C_ANALOG:
	printf("#analog %s:%d %s ",
	       elem->name,
	       elem->size,
	       format_dir(elem->dir));
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
	printf("#can %s %x[%d:%d]", elem->name,
	       elem->canr.id, elem->canr.pos, elem->canr.len);	
    case C_TIMER:
	printf("#timer %s %d",  elem->name, elem->timer.timeout);
	break;
    case C_VARIABLE:
	printf("#variable %s:%d %d", elem->name, elem->size, elem->cur.i32);
	break;
    case C_CONSTANT:
	printf("#constant %s:%d %d", elem->name, elem->size, elem->cur.i32);
	break;		
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

static void print_expr(xindex_t xi)
{
    candy_expr_t* xp = &expr[xi];
    switch(xp->op) {
    case EXPR_NAME:
	printf("%s", element[xp->ni].name);
	break;
    case EXPR_CONST:
	printf("%d", xp->v.i32);
	break;
    case EXPR_CAN_RANGE:
	printf("0x%x[%d:%d]", xp->crange.id,
	       xp->crange.pos, xp->crange.len);
	break;
    case EXPR_CAN_BIT1:
	printf("0x%x[%d]", xp->cbit.id, xp->cbit.bit_pos);
	break;
    case EXPR_CAN_BIT2:
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

static void print_rule(candy_rule_t* rp)
{
    printf("%s = ", element[rp->ni].name);
    print_expr(rp->vi);
    if (rp->ci == INVALID_INDEX) {
	printf(" ? ");
	print_expr(rp->ci);
    }
}

#endif
