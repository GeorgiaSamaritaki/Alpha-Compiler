#include "avm_utility.hpp"


void avm_dec_top(void) {
  if(!top)
    avm_error("stack overflow");
  else
    --top;
}
void avm_tableIncrRC(avm_table* t) { ++(t->rc); }

void avm_tableDecRC(avm_table* t) {
  assert(t->rc > 0);
  if (--(t->rc) == 0) avm_tableDestroy(t);
}

void execute_arithmetic(instruction* instr) {
  avm_memcell* lv = avm_translate_operand(instr->result, (avm_memcell*)0);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, bx);

  assert(lv && (&stack_m[AVM_STACKSIZE - 1] >= lv && lv > &stack_m[top] ||
                lv == retval));
  assert(rv1 && rv2);
  if (rv1->type != number_m || rv2->type != number_m) {
    avm_error("not a number in arithmetic!");

  } else {
    arithmetic_func_t op = arithmeticFuncs[instr->opcode - add_v];
    avm_memcellClear(lv);
    lv->type = number_m;
    lv->data.numVal = (*op)(rv1->data.numVal, rv2->data.numVal);
  }
}

void execute_assign(instruction* instr) {
  avm_memcell* lv = avm_translate_operand(instr->result, NULL);
  avm_memcell* rv = avm_translate_operand(instr->arg1, ax);

  assert(lv && (&stack_m[AVM_STACKSIZE - 1] >= lv && lv >= &stack_m[top] ||
                lv == retval));
  assert(rv);

  avm_assign(lv, rv);
}

void execute_call(instruction* instr) {
  avm_memcell* func = avm_translate_operand(instr->arg1, ax);
  assert(func);
  avm_callsaveenvironment();
  switch (func->type) {
    case userfunc_m: {
      pc = userFunczRead[func->data.funcVal]->address;//auto den einai opvw tiw dialekseis
      assert(pc < AVM_ENDING_PC);
      // cout<< " here pc : "<<pc<<endl;
      assert(instructionzRead[pc]->opcode == funcenter_v);
      break;
    }
    case string_m:
      avm_callLibFunc(func->data.strVal);
      break;
    case libfunc_m:
      avm_callLibFunc(func->data.libfuncVal);
      break;
    default: {
      char* s = (char*)avm_toString(func).c_str();
      avm_error("call: cannot bind '%s' to function!", s);
    }
  }
}

void execute_funcenter(instruction* instr) {
  avm_memcell* func = avm_translate_operand(instr->result, ax);
  assert(func);
  assert(pc == userFunczRead[func->data.funcVal]->address); //auto den einai opvw tiw dialekseis

  totalActuals = 0;
  userfunc* funcInfo = avm_getFuncInfo(pc);
  assert(funcInfo);
  topsp = top;
  top = top - funcInfo->localSize;
}


void execute_funcexit(instruction* unused) {
  unsigned oldTop = top;
  top = avm_get_envvalue(topsp + AVM_SAVEDTOP_OFFSET);
  pc = avm_get_envvalue(topsp + AVM_SAVEDPC_OFFSET);
  topsp = avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);
  while (++oldTop <= top) avm_memcellClear(&stack_m[oldTop]);
}

void execute_pusharg(instruction* instr) {
  avm_memcell* arg = avm_translate_operand(instr->arg1, ax);
  // cout << avm_toString(arg) << endl;
  // printStack();
  assert(arg);
  avm_assign(&stack_m[top], arg);
  ++totalActuals;
  avm_dec_top();
}


void execute_rel(instruction* instr) {
  assert(instr->result->type == label_a);

  avm_memcell* rv1 = avm_translate_operand(instr->arg1, ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, bx);

  bool result = 0;

  if (rv1->type != number_m || rv2->type != number_m) {
    avm_error("Comparing args are not numbers %s %s", typeStrings[rv1->type],
              typeStrings[rv2->type]);

  } else {
    result = numberCmpFuncs[instr->opcode - jle_v](rv1->data.numVal,
                                                   rv2->data.numVal);
    if (!executionFinished && result) pc = instr->result->val;  
  }
}

void execute_newtable(instruction* instr) {
  avm_memcell* lv = avm_translate_operand(instr->result, (avm_memcell*)0);
  assert(lv && (&stack_m[AVM_STACKSIZE - 1] >= lv && lv > &stack_m[top] ||
                lv == retval));

  avm_memcellClear(lv);

  lv->type = table_m;
  lv->data.tableVal = avm_tablenew();
  avm_tableIncrRC(lv->data.tableVal);
}

void execute_tablegetelem(instruction* instr) {
  avm_memcell* lv = avm_translate_operand(instr->result, (avm_memcell*)0);
  avm_memcell* t = avm_translate_operand(instr->arg1, (avm_memcell*)0);
  avm_memcell* i = avm_translate_operand(instr->arg2, ax);

  assert(lv && (&stack_m[AVM_STACKSIZE - 1] >= lv && lv > &stack_m[top] ||
                lv == retval));
  assert(t && (&stack_m[AVM_STACKSIZE - 1] >= t && t > &stack_m[top]));
  assert(i);
  avm_memcellClear(lv);
  lv->type = nil_m;

  if (t->type != table_m) avm_error("illegal use of type %s as table!", typeStrings[t->type]);

  else {
    avm_memcell* content = avm_tablegetelem(t->data.tableVal, i);
    if (content)
      avm_assign(lv, content);
    else {
      char* ts = (char*)avm_toString(t).c_str();
      char* is = (char*)avm_toString(i).c_str();
      avm_warning("%s[%s] not found!", ts, is);
    }
  }
}

void execute_tablesetelem(instruction* instr) {
  cout << instr->opcode << endl;
  avm_memcell* t = avm_translate_operand(instr->result, (avm_memcell*)0);
  avm_memcell* i = avm_translate_operand(instr->arg1, ax);
  avm_memcell* c = avm_translate_operand(instr->arg2, bx);
  
  assert(t && &stack_m[AVM_STACKSIZE - 1] >= t && t > &stack_m[top]);
  assert(i && c);
  if (t->type != table_m) {
    avm_error("illegal use of type %s as table!", typeStrings[t->type]);

  } else
    avm_tablesetelem(t->data.tableVal, i, c);
}

void execute_jump(instruction* instr) { 
  assert(instr->result);
  assert(instr->result->type == label_a);
  if (!executionFinished && instr->result->val>0) pc = instr->result->val;
}

void execute_uminus(instruction* instr) {assert(0);}
void execute_and(instruction* instr) {assert(0);}
void execute_or(instruction* instr) {assert(0);}
void execute_not(instruction* instr) {assert(0);}
void execute_nop(instruction* instr) {assert(0);}
void execute_getretval(instruction* instr) {assert(0);}
void execute_ret(instruction* instr) {assert(0);}


void execute_jeq(instruction* instr) {
  assert(instr->result->type == label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, bx);

  bool result = 0;

  if (rv1->type == undef_m || rv2->type == undef_m)
    avm_error("'undef' involved in equality!");
  else if (rv1->type == nil_m || rv2->type == nil_m)
    result = rv1->type == nil_m && rv2->type == nil_m;
  else if (rv1->type == bool_m || rv2->type == bool_m)
    result = (avm_tobool(rv1) == avm_tobool(rv2));
  else if (rv1->type != rv2->type) {
    avm_error("%s == %s is illegal", typeStrings[rv1->type],
              typeStrings[rv2->type]);

  } else {
    /* Equality check with dipatching */
    result = checks[rv1->type](rv1, rv2);
  }

  if (!executionFinished && result) pc = instr->result->val;
}

void execute_jne(instruction* instr) {
  assert(instr->result->type == label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, bx);
  bool result = 0;

  if (rv1->type == undef_m || rv2->type == undef_m) {
    avm_error("'undef' involved in equality!");

  } else if (rv1->type == nil_m || rv2->type == nil_m)
    result = rv1->type != nil_m && rv2->type != nil_m;
  else if (rv1->type == bool_m || rv2->type == bool_m)
    result = (avm_tobool(rv1) != avm_tobool(rv2));
  else if (rv1->type != rv2->type) {
    avm_error("%s != %s is illegal", typeStrings[rv1->type],
              typeStrings[rv2->type]);

  } else {
    /* Equality check with dipatching */
    result = !(checks[rv1->type](rv1, rv2));
  }

  if (!executionFinished && result) pc = instr->result->val;
}