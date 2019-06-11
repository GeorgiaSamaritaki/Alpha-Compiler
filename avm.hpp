
// #include "codegen.hpp"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <string>
#include <vector>
#include "avm_library_funcs.hpp"
// #include "avm_utility.hpp"
#include "avm_execute.hpp"

#ifndef avm
#define avm

static void avm_initstack(void) {
  for (unsigned i = 0; i < AVM_STACKSIZE; ++i) {
    AVM_WIPEOUT(stack_m[i]);
    stack_m[i].type = undef_m;
  }
}

unsigned hash_number(avm_memcell* me) {
  // Fatoyritsa-style hash
  assert(me);
  assert(me->type == number_m);
  return (int)me->data.numVal % AVM_TABLE_HASHSIZE;
}

unsigned hash_string(avm_memcell* me) {
  // Mpilas-style hash
  assert(me);
  assert(me->type == string_m);
  size_t ui;
  unsigned int uiHash = 0U;
  for (ui = 0U; me->data.strVal[ui] != '\0'; ui++)
    uiHash = uiHash * HASH_MUL + me->data.strVal[ui];
  return uiHash % AVM_TABLE_HASHSIZE;
}

hash_func_t hashMe[] = {hash_number, hash_string};


void avm_tableBucketsInit(avm_table_bucket** p) {
  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) p[i] = NULL;
}

avm_table* avm_tablenew(void) {
  avm_table* t = new avm_table;
  AVM_WIPEOUT(*t);
  t->rc = 0;
  t->total = 0;
  avm_tableBucketsInit(t->numIndexed);
  avm_tableBucketsInit(t->strIndexed);

  return t;
}

void avm_tableBucketsDestroy(avm_table_bucket** p) {
  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) {
    for (avm_table_bucket* b = p[i]; b;) {
      avm_table_bucket* toDel = b;
      b = b->next;
      avm_memcellClear(&toDel->key);
      avm_memcellClear(&toDel->value);
      free(toDel);
    }
    p[i] = NULL;
  }
}

void avm_tableDestroy(avm_table* t) {
  avm_tableBucketsDestroy(t->strIndexed);
  avm_tableBucketsDestroy(t->numIndexed);
  delete (t);
}

double consts_getNumber(unsigned index) {
  assert(!numConstsRead.empty());
  return numConstsRead[index];
}
char* consts_getString(unsigned index) {
  assert(!stringConstsRead.empty());
  return stringConstsRead[index];
}
char* libfuncs_getUsed(unsigned index) {
  assert(!namedLibFuncsRead.empty());
  return namedLibFuncsRead[index];
}

avm_memcell* avm_translate_operand(vmarg* arg, avm_memcell* reg) {
  // AVM_WIPEOUT(reg);
  switch (arg->type) {
    case global_a:
      return &stack_m[AVM_STACKSIZE - 1 - arg->val];
    case local_a:
      return &stack_m[topsp - arg->val];
    case formal_a:
      return &stack_m[topsp + AVM_STACKENV_SIZE + 1 + arg->val];
    case retval_a:
      return retval;
    case number_a: {
      reg->type = number_m;
      reg->data.numVal = consts_getNumber(arg->val);
      return reg;
    }
    case string_a: {
      reg->type = string_m;
      reg->data.strVal = strdup(consts_getString(arg->val));
      return reg;
    }
    case bool_a: {
      reg->type = bool_m;
      reg->data.boolVal = arg->val;
      return reg;
    }
    case nil_a: {
      reg->type = nil_m;
      return reg;
    }
    case userfunc_a: {
      reg->type = userfunc_m;
      reg->data.funcVal = arg->val;
      return reg;
    }
    case libfunc_a: {
      reg->type = libfunc_m;
      reg->data.libfuncVal = libfuncs_getUsed(arg->val);
      return reg;
    }
    default:
      assert(0);
  }
}

void memclear_string(avm_memcell* m) {
  assert(m->data.strVal);
  free(m->data.strVal);
}

void memclear_table(avm_memcell* m) {
  assert(m->data.tableVal);
  avm_tableDecRC(m->data.tableVal);
}

memclear_func_t memclearFuncs[] = {
    0,  // number
    memclear_string,
    0,  // bool
    memclear_table,
    0,  // userfunc
    0,  // libfunc
    0,  // nil
    0   // undef
};

void avm_memcellClear(avm_memcell* m) {
  if (m->type != undef_m) {
    memclear_func_t f = memclearFuncs[m->type];
    if (f) (*f)(m);
    m->type = undef_m;
  }
}

void avm_warning(char* format, ...) {
  printf("Runtime Warning: [isntr:%d,line:%d]: ",pc,instructionzRead[pc]->srcLine);
  va_list args;
  va_start(args, format);
  printf(format, args);
  va_end(args);
  cout << endl;
}

void avm_error(char* format, ...) {
  executionFinished = true;
  printf("\nRuntime Error[isntr:%d,line:%d]: ",pc,instructionzRead[pc]->srcLine);
  va_list args;
  va_start(args, format);
  printf(format, args);
  va_end(args);
  printStack();
  cout<<endl;
}

void avm_assign(avm_memcell* lv, avm_memcell* rv) {
  if (lv == rv) return;

  if (lv->type == table_m && rv->type == table_m &&
      lv->data.tableVal == rv->data.tableVal)
    return;

  if (rv->type == undef_m) avm_warning("assigning from 'undef' content!");

  avm_memcellClear(lv);
  memcpy(lv, rv, sizeof(avm_memcell));
  

  if (lv->type == libfunc_m){
    lv->data.libfuncVal = strdup(rv->data.libfuncVal);
  }else if (lv->type == string_m){
    lv->data.strVal = strdup(rv->data.strVal);
  }else if (lv->type == table_m)
    avm_tableIncrRC(lv->data.tableVal);
}


void avm_push_envvalue(unsigned val) {
  stack_m[top] = *new avm_memcell();
  stack_m[top].type = number_m;
  stack_m[top].data.numVal = val;
  avm_dec_top();
}
void avm_callsaveenvironment() {
  avm_push_envvalue(totalActuals);
  avm_push_envvalue(pc + 1);
  avm_push_envvalue(top + totalActuals + 1); //FIXME:
  lasttop.push(top + totalActuals + 1);
  avm_push_envvalue(topsp);
  // cout<<"~~~~~~~~~~pushed "<<totalActuals<<" "<<pc + 1<<" "<<top + totalActuals + 1<<" "<<topsp<<endl;
  // cout<<"sp saved "<<topsp<<endl;
  // cout<<"top saved "<<top<<endl;
  // cout<<"totalActuals saved "<<totalActuals<<endl;
  
}

userfunc* avm_getFuncInfo(unsigned address) {
  for (int i = 0; i < userFunczRead.size(); i++)
    if (userFunczRead[i]->address == address) return userFunczRead[i];
  return NULL;
}

unsigned avm_get_envvalue(unsigned i) {
  if(stack_m[i].type != number_m) {//cout<<"--------found nil "<<i<<endl;
  return -1;}
  assert(stack_m[i].type == number_m);
  unsigned val = (unsigned)stack_m[i].data.numVal;
  // cout<< "Val "<<val <<" numval "<< stack_m[i].data.numVal<<endl;
  assert(stack_m[i].data.numVal == ((double)val));
  return val;
}


library_func_t avm_getLibraryFunc(char* id) {
  int lib_index =
      libfuncs_newUsed(id);  // retrieve index from the lib funcs array
  return libraryFuncz[lib_index];
}

void avm_callLibFunc(char* id) {
  library_func_t f = avm_getLibraryFunc(id);
  if (!f) {
    avm_error("unsupported lib func '%s' called!", id);
  } else {
    topsp = top;
    totalActuals = 0;
    (*f)();
    if (!executionFinished) execute_funcexit((instruction*)0);
  }
}

unsigned avm_totalActuals() {
  return avm_get_envvalue(topsp + AVM_NUMACTUALS_OFFSET);
}

avm_memcell* avm_getActual(unsigned i) {
  assert(i < avm_totalActuals());
  return &stack_m[topsp + AVM_STACKENV_SIZE + 1 + i];
}


avm_memcell* avm_tablegetelem(avm_table* table, avm_memcell* index) {
  assert(table && index);
  unsigned array_index = hashMe[index->type](index);

  switch (index->type) {
    case number_m: {
      avm_table_bucket* tmp = table->numIndexed[array_index];
      while (tmp) {
        if (tmp->key.data.numVal == index->data.numVal) {
          avm_memcell* ret = &tmp->value;
          return ret;
        }
        tmp = tmp->next;
      }
      if( tmp == NULL) {
        avm_memcell* ret = new avm_memcell();
        ret->type = nil_m;
        return ret;
      }
      break;
    }
    case string_m: {
      avm_table_bucket* tmp = table->strIndexed[array_index];
      while (tmp) {
        if (strcmp(tmp->key.data.strVal, index->data.strVal) == 0) {
          avm_memcell* ret = &tmp->value;
          return ret;
        }
        tmp = tmp->next;
      }
      if( tmp == NULL) {
        avm_memcell* ret = new avm_memcell();
        ret->type = nil_m;
        return ret;
      }
      break;
    }
    default:
      assert(0);
  }
  return NULL;
}

void avm_tablesetelem(avm_table* table, avm_memcell* index,
                      avm_memcell* content) {
  assert(table && index && content);
  if (index->type != number_m && index->type != string_m) {
    avm_error("Table key can only be string or integer\n");
    return;
  }

  unsigned array_index = hashMe[index->type](index);
  avm_table_bucket* tmp = table->numIndexed[array_index];
  if (index->type == string_m) 
    tmp = table->strIndexed[array_index];
  avm_table_bucket* prev = NULL;
  while (tmp != NULL) {
    if( tmp->key.data.strVal  ){
      if (strcmp(tmp->key.data.strVal, index->data.strVal) == 0) break;
    }
    else  
      if( tmp->key.data.numVal == index->data.numVal) {
        break;
      }
    prev = tmp;
    tmp = tmp->next;
  }

  if (tmp == NULL) {
    if(content->type == nil_m ) return;
    avm_table_bucket* new_bucket = new avm_table_bucket();
    avm_assign(&new_bucket->key, index);
    avm_assign(&new_bucket->value, content);
    new_bucket->next = NULL;
    if (prev != NULL){
      prev->next = new_bucket;
    }
    else {
      if (index->type == string_m)
        table->strIndexed[array_index] = new_bucket;
      else {
        table->numIndexed[array_index] = new_bucket;
      }
    }
    table->total++;
  } else {
    if( content->type == nil_m ){
      if( prev != NULL) {
        prev->next = tmp->next;
      } else {
        if (index->type == string_m)
          table->strIndexed[array_index] = tmp->next;
        else {
          table->numIndexed[array_index] = tmp->next;
        }
      }
      table->total--;
    } else{
        avm_assign(&tmp->value, content);
    }
  }
}


execute_func_t executeFuncs[] = {
    execute_assign,       execute_add,       execute_sub,
    execute_mul,          execute_div,       execute_mod,
    execute_uminus,       execute_and,       execute_or,
    execute_not,          execute_jeq,       execute_jne,
    execute_jle,          execute_jge,       execute_jlt,
    execute_jgt,          execute_call,      execute_pusharg,
    execute_ret,          execute_getretval, execute_funcenter,
    execute_funcexit,     execute_newtable,  execute_tablegetelem,
    execute_tablesetelem, execute_jump,      execute_nop};

void execute_cycle(void) {
  if (executionFinished) return;
  if (pc == AVM_ENDING_PC) {
    executionFinished = true;
    return;
  } else {
    assert(pc <= AVM_ENDING_PC);
    instruction* instr = instructionzRead[pc];
    assert(instr->opcode >= 0 && instr->opcode <= AVM_MAX_INSTRUCTIONS);
    if (instr->srcLine) currLine = instr->srcLine;
    unsigned oldPc = pc;

    // cout << "~~~~pc is " <<pc <<" "<<toString(instr->opcode)<<" "<<instr->opcode << endl;
    (*executeFuncs[instr->opcode])(instr);
    if (pc == oldPc) ++pc;
  }
}

void printStack() {
  //   cout "______________ "<<endl;
  // for (int i = 0; i < 4041; i++) {
  //   if (stack_m[i].type == undef_m) continue;
  //   cout << i << ": ";
  //   cout << avm_toString(&stack_m[i]) << endl;
  // }
  //   cout << "--------------- "<<endl;
}

void avm_initialize() {
  ax = new avm_memcell();
  cx = new avm_memcell();
  bx = new avm_memcell();
  retval = new avm_memcell();
  avm_initstack();
  init_libfuncs();
  lasttop.push(-1);
}

void avm_close() {
  delete(ax);
  delete(bx);
  delete(cx);
  delete(retval);
}

#endif