#include <vector>
#include "quad.hpp"

enum vmopcode {
  assign_v,
  add_v,
  sub_v,
  mul_v,
  div_v,
  mod_v,
  uminus_v,
  and_v,
  or_v,
  not_v,
  jeq_v,
  jne_v,
  jle_v,
  jge_v,
  jlt_v,
  jgt_v,
  call_v,
  pusharg_v,
  funcenter_v,
  funcexit_v,
  newtable_v,
  tablegetelem_v,
  tablesetelem_v,
  jump_v,
  nop_v
};

enum vmarg_t {
  label_a,
  global_a,
  formal_a,
  local_a,
  number_a,
  string_a,
  bool_a,
  nil_a,
  userfunc_a,
  libfunc_a,
  retval_a
};

typedef struct vmarg {
  vmarg_t type;
  unsigned val;
} vmarg;

typedef struct instruction {
  vmopcode opcode;
  vmarg result;
  vmarg arg1;
  vmarg arg2;
  unsigned srcLine;
} instruction;

typedef struct userfunc {
  unsigned address;
  unsigned localSize;
  char* id;
} userfunc;

double* numConsts;
unsigned totalNumConsts;
char** stringConsts;
unsigned totalStringConsts;
char** namedLibFuncs;
unsigned totalNamedLibFuncs;
vector<userfunc*> userFuncz;
unsigned totalUserFuncz;

unsigned currentProcessedQuad();
vector<instruction*> instructionz;

unsigned consts_newstring(char* s);
unsigned consts_newnumber(double n);
unsigned libfuncs_newused(char* s);
unsigned userfuncs_newfunc(SymbolTableEntry* s);

void emit_instr(instruction t) {
  instruction* new_t = new instruction();
  new_t->opcode = t.opcode;
  new_t->result = t.result;
  new_t->arg1 = t.arg1;
  new_t->arg2 = t.arg2;
  new_t->srcLine = t.srcLine;

  instructionz.push_back(new_t);
}

void make_operand(expr* e, vmarg* arg) {
  switch (e->type) {
    case var_e:
    case tableitem_e:
    case arithexpr_e:
    case boolexpr_e:
    case newtable_e: {
      assert(e->sym);
      arg->val = e->sym->offset;
      switch (e->sym->space) {
        case program_var:
          arg->type = global_a;
          break;
        case function_local:
          arg->type = local_a;
          break;
        case formal_arg:
          arg->type = formal_a;
          break;
        default:
          assert(false);
      }
      break;
    }
    /*cosntants*/
    case constbool_e: {
      arg->val = e->boolConst;
      arg->type = bool_a;
      break;
    }
    case conststring_e: {
      arg->val = consts_newstring(e->strConst);
      arg->type = string_a;
      break;
    }
    case constnum_e: {
      arg->val = consts_newnumber(e->numConst);
      arg->type = number_a;
      break;
    }
    case nil_e:
      arg->type = nil_a;
      break;

    /*Functions*/
    case programfunc_e: {
      arg->type = userfunc_a;
      // arg->val = e->sym->taddress;
      /* or alternatively*/
      arg->val = userfuncs_newfunc(e->sym);
      break;
    }
    case libraryfunc_e: {
      arg->type = libfunc_a;
      arg->val = libfuncs_newused((char *)e->sym->value.funcVal->name);
      break;
    }
    case assignexpr_e: {
      assert(0);
    }
    default:
      assert(0);
  }
}

void make_numberoperand(vmarg* arg, double val) {
  arg->val = consts_newnumber(val);
  arg->type = number_a;
}

void make_booloperand(vmarg* arg, unsigned val) {
  arg->val = val;
  arg->type = bool_a;
}

void make_retvaloperand(vmarg* arg) { arg->type = retval_a; }

typedef struct incomplete_jump {
  unsigned instrNo;
  unsigned iaddress;
}incomplete_jump;

unsigned ij_total = 0;
unsigned totalInstructions = 0;

unsigned nextInstructionLabel() { return instructionz.size(); }

vector<incomplete_jump> incomplete_jumps;

void add_incomplete_jump(unsigned int instrNo, unsigned iaddress){
  incomplete_jump tmp;
  tmp.instrNo = instrNo;
  tmp.iaddress = iaddress;
  incomplete_jumps.pushback(tmp);
}

void patch_incomplete_jumps() {
  for (jump : incomplete_jumps) {
    if (x.iaddress = intermediate_code_size)
      instructions[x.instrNo].result = target_code_size;
    else
      instructions[x.instrNo].result = quads[x.iaddress].taddress;
  }
}

void generate(vmopcode op, quad* quad) {
  instruction t;
  t.opcode = op;
  make_operand(quad->arg1, &t.arg1);
  make_operand(quad->arg2, &t.arg2);
  make_operand(quad->result, &t.result);
  quad->taddress = nextInstructionLabel();
  emit_instr(t);
}

void generate_relational(vmopcode op, quad* quad) {
  instruction t;
  t.opcode = op;
  make_operand(quad->arg1, &t.arg1);
  make_operand(quad->arg2, &t.arg2);

  t.result.type = label_a;
  if (quad->label < currentProcessedQuad()) {
    t.result.val = quads[quad->label]->taddress;
  } else {
    add_incomplete_jump(nextInstructionLabel(), quad->label);
  }
  quad->taddress = nextInstructionLabel();
  emit_instr(t);
}

void generate_ADD(quad* quad) { generate(add_v, quad); }
void generate_SUB(quad* quad) { generate(sub_v, quad); }
void generate_MUL(quad* quad) { generate(mul_v, quad); }
void generate_DIV(quad* quad) { generate(div_v, quad); }
void generate_MOD(quad* quad) { generate(mod_v, quad); }
void generate_UMINUS(quad* quad) { }
void generate_NEWTABLE(quad* quad) { generate(newtable_v, quad); }
void generate_TABLEGETELEM(quad* quad) { generate(tablegetelem_v, quad); }
void generate_TABLESETELEM(quad* quad) { generate(tablesetelem_v, quad); }
void generate_ASSIGN(quad* quad) { generate(assign_v, quad); }
void generate_NOP(quad* quad = NULL) {
  instruction t;
  t.opcode = nop_v;
  emit_instr(t);
}
void generate_JUMP(quad* quad) { generate_relational(jump_v, quad); }
void generate_IF_EQ(quad* quad) { generate_relational(jeq_v, quad); }
void generate_IF_NOTEQ(quad* quad) { generate_relational(jne_v, quad); }
void generate_IF_GREATER(quad* quad) { generate_relational(jgt_v, quad); }
void generate_IF_GREATEREQ(quad* quad) { generate_relational(jge_v, quad); }
void generate_IF_LESS(quad* quad) { generate_relational(jlt_v, quad); }
void generate_IF_LESSEQ(quad* quad) { generate_relational(jle_v, quad); }
void generate_NOT(quad* quad){};
void generate_OR(quad* quad){};
void generate_AND(quad* quad){};
void generate_PARAM(quad* quad) {
  quad->taddress = nextInstructionLabel();
  instruction t;
  t.opcode = pusharg_v;
  make_operand(quad->arg1, &t.arg1);
  emit_instr(t);
};
void generate_CALL(quad* quad) {
  quad->taddress = nextInstructionLabel();
  instruction t;
  t.opcode = call_v;
  make_operand(quad->arg1, &t.arg1);
  emit_instr(t);
};
void generate_GETRETVAL(quad* quad) {
  quad->taddress = nextInstructionLabel();
  instruction t;
  t.opcode = assign_v;
  make_operand(quad->result, &t.result);
  make_retvaloperand(&t.arg1);
  emit_instr(t);
};
void generate_FUNCSTART(quad* quad){

};
void generate_RETURN(quad* quad){};
void generate_FUNCEND(quad* quad){};

typedef void (*generator_func_t) (quad*);

generator_func_t generators[] = {
  generate_ASSIGN,        generate_ADD,           generate_SUB,         
  generate_MUL,           generate_DIV,           generate_MOD,          
  generate_UMINUS,        generate_AND,           generate_OR,        
  generate_NOT,           generate_IF_EQ,         generate_IF_NOTEQ,    
  generate_IF_LESSEQ,     generate_IF_GREATEREQ,  generate_IF_LESS,
  generate_IF_GREATER,    generate_CALL,          generate_PARAM,
  generate_RETURN,        generate_GETRETVAL,     generate_FUNCSTART,
  generate_FUNCEND,       generate_NEWTABLE,      generate_TABLEGETELEM,
  generate_TABLESETELEM,  generate_JUMP,          generate_NOP
};

void generateAll(void) {
  for (unsigned int i = 0; i < quads.size(); ++i) {
    (*generators[quads[i]->iop])(quads[i]);
  }
}
