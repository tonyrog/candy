// debug utils and output
#ifndef __CANDY_DEBUG_H__
#define __CANDY_DEBUG_H__

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
	candy_print_str("#digital ");
	candy_print_str(elem->name);
	candy_print_str(" ");
	candy_print_str(format_dir(elem->dir));
	candy_print_str(" ");
	if (elem->io.port == -1)
	    candy_print_int(elem->io.pin);
	else {
	    candy_print_int(elem->io.port);
	    candy_print_str(":");	
	    candy_print_int(elem->io.pin);
	}
	break;
    case C_ANALOG:
	candy_print_str("#analog ");
	candy_print_str(elem->name);
	candy_print_str(":");
	candy_print_int(elem->size);
	candy_print_str(" ");
	candy_print_str(format_dir(elem->dir));
	candy_print_str(" ");	
	if (elem->io.port == -1)
	    candy_print_int(elem->io.pin);
	else {
	    candy_print_int(elem->io.port);
	    candy_print_str(":");	
	    candy_print_int(elem->io.pin);
	}
	break;
    case C_CANDY_BIT1:
	candy_print_str("#can ");
	candy_print_str(elem->name);
	candy_print_str(" 0x");
	candy_print_int(elem->can.id);
	candy_print_str("[");
	candy_print_int(elem->can.bit_pos);
	candy_print_str("]");
	break;
    case C_CANDY_BIT2:
	candy_print_str("#can ");
	candy_print_str(elem->name);
	candy_print_str(" 0x");
	candy_print_int(elem->can.id);
	candy_print_str("[");
	candy_print_int(elem->can.byte_pos);
	candy_print_str(",");
	candy_print_int(elem->can.bit_pos);
	candy_print_str("]");
	break;
    case C_CANDY_RANGE:
	candy_print_str("#can ");
	candy_print_str(elem->name);
	candy_print_str(" 0x");
	candy_print_int(elem->canr.id);
	candy_print_str("[");
	candy_print_int(elem->canr.pos);
	candy_print_str(":");
	candy_print_int(elem->canr.len);
	candy_print_str("]");	
	break;
    case C_TIMER:
	candy_print_str("#timer ");
	candy_print_str(elem->name);
	candy_print_str(" ");
	candy_print_int(elem->timer.timeout);
	break;
    case C_VARIABLE:
	candy_print_str("#variable ");
	candy_print_str(elem->name);
	candy_print_str(":");
	candy_print_int(elem->size);
	candy_print_str(" = ");
	candy_print_int(elem->cur.i32);
	break;
    case C_CONSTANT:
	candy_print_str("#constant ");
	candy_print_str(elem->name);
	candy_print_str(":");
	candy_print_int(elem->size);
	candy_print_str(" = ");
	candy_print_int(elem->cur.i32);	
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

    case EXPR_BAND: return "&";
    case EXPR_BOR: return "|";
    case EXPR_BXOR: return "^";
    case EXPR_BNOT: return "~";
    case EXPR_BSL: return "<<";
    case EXPR_BSR: return "<<";
	
    default: return "??";
    }
}

static void print_expr(xindex_t xi)
{
    candy_expr_t* xp = &expr[xi];
    switch(xp->op) {
    case EXPR_NAME:
	candy_print_str(element[xp->ei].name);
	break;
    case EXPR_CONST:
	candy_print_int(xp->v.i32);
	break;
    case EXPR_CANDY_RANGE:
	candy_print_str("0x");
	candy_print_hex(xp->crange.id);
	candy_print_str("[");
	candy_print_int(xp->crange.pos);
	candy_print_str(":");
	candy_print_int(xp->crange.len);
	candy_print_str("]");
	break;
    case EXPR_CANDY_BIT1:
	candy_print_str("0x");
	candy_print_hex(xp->cbit.id);
	candy_print_str("[");
	candy_print_int(xp->cbit.bit_pos);
	candy_print_str("]");
	break;
    case EXPR_CANDY_BIT2:
	candy_print_str("0x");
	candy_print_hex(xp->cbit.id);
	candy_print_str("[");
	candy_print_int(xp->cbit.byte_pos);
	candy_print_str(",");
	candy_print_int(xp->cbit.bit_pos);
	candy_print_str("]");
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
    case EXPR_BAND:
    case EXPR_BOR:
    case EXPR_BXOR:
    case EXPR_BSL:
    case EXPR_BSR:	
	print_expr(xp->bin.li);
	candy_print_str(" ");
	candy_print_str(format_op(xp->op));
	candy_print_str(" ");
	print_expr(xp->bin.ri);
	break;
    case EXPR_NOT:
    case EXPR_NEG:
    case EXPR_BNOT:
	candy_print_str(format_op(xp->op));
	print_expr(xp->una);
	break;
    default:
	break;
    }
}

static void print_rule(candy_rule_t* rp)
{
    candy_print_str(element[rp->ei].name);
    candy_print_str(" = ");
    print_expr(rp->vi);
    if (rp->ci == INVALID_INDEX) {
	candy_print_str(" ? ");
	print_expr(rp->ci);
    }
}

#endif
