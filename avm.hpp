#include "codegen.hpp"
#include "math.h"

#define AVM_STACKSIZE 4096
#define AVM_WIPEOUT(m) memset(&(m), 0, sizeof(m))
#define AVM_TABLE_HASHSIZE 211
#define AVM_STACKENV_SIZE 4
#define AVM_MAX_INSTRUCTIONS (unsigned)nop_v
#define AVM_ENDING_PC codeSize
#define AVM_NUMACTUALS_OFFSET 4  
#define AVM_SAVEDPC_OFFSET    3  
#define AVM_SAVEDTOP_OFFSET   2     
#define AVM_SAVEDTOPSP_OFFSET 1  


// Arithmetic functions
#define execute_add execute_arithmetic
#define execute_sub execute_arithmetic
#define execute_mul execute_arithmetic
#define execute_div execute_arithmetic
#define execute_mod execute_arithmetic
// Relational functions
#define execute_jle execute_rel
#define execute_jge execute_rel
#define execute_jlt execute_rel
#define execute_jgt execute_rel

/*Structs*/
struct avm_table;

enum avm_memcell_t {
  number_m = 0,
  string_m = 1,
  bool_m = 2,
  table_m = 3,
  userfunc_m = 4,
  libfunc_m = 5,
  nil_m = 6,
  undef_m = 7
};

typedef struct avm_memcell {
  avm_memcell_t type;
  union {
    double numVal;
    char* strVal;
    bool boolVal;
    avm_table* tableVal;
    unsigned funcVal;
    char* libfuncVal;
  } data;
} avm_memcell;

typedef struct avm_table_bucket {
  avm_memcell key;
  avm_memcell value;
  avm_table_bucket* next;
} avm_table_bucket;

typedef struct avm_table {
  unsigned rc;
  avm_table_bucket* strIndexed[AVM_TABLE_HASHSIZE];
  avm_table_bucket* numIndexed[AVM_TABLE_HASHSIZE];
  unsigned total;
} avm_table;

/*Var definitions*/
avm_memcell stack_m[AVM_STACKSIZE];
avm_memcell ax, bx, cx;
avm_memcell retval;
unsigned top, topsp;
bool executionFinished = false;
unsigned pc = 0;
unsigned currLine = 0;
unsigned codeSize = 0;

char* typeStrings[] = {
  "number",
  "string",
  "bool",
  "table",
  "userfunc",
  "libfunc",
  "nill",
  "undef"
};

typedef void (*execute_func_t)(instruction*);
typedef void (*memclear_func_t)(avm_memcell*);
typedef char* (*tostring_func_t)(avm_memcell*);
typedef void(*library_func_t)();
typedef double (*arithmetic_func_t)(double, double );
typedef unsigned char(*tobool_func_t)(avm_memcell*);
typedef bool (*eq_check_t)(avm_memcell*, avm_memcell* );
typedef bool (*cmp_func_t)(double, double);
typedef unsigned (*hash_func_t)(avm_memcell*);

/*Funcs*/
void avm_tableDestroy(avm_table* t);
void avm_memcellClear(avm_memcell* m);
void avm_callsaveenvironment();
void avm_callLibFunc(char* id);
char* avm_toString(avm_memcell* m);

static void avm_initstack(void) {
  for (unsigned i = 0; i < AVM_STACKSIZE; ++i) {
    AVM_WIPEOUT(stack_m[i]);
    stack_m[i].type = undef_m;
  }
}

unsigned hash_number(avm_memcell* me){
  //Fatoyritsa-style hash
  assert(me);
  assert(me->type == number_m);
  return (int)me->data.numVal % AVM_TABLE_HASHSIZE;
}

unsigned hash_string(avm_memcell* me){
  //Mpilas-style hash
  assert(me);
  assert(me->type == string_m);
  size_t ui;
  unsigned int uiHash = 0U;
  for (ui = 0U; me->data.strVal[ui] != '\0'; ui++) uiHash = uiHash * HASH_MUL + me->data.strVal[ui];
  return uiHash % AVM_TABLE_HASHSIZE;
}

hash_func_t hashMe[] = {
  hash_number,
  hash_string
};

void avm_tableIncrRC(avm_table* t) { ++(t->rc); }

void avm_tableDecRC(avm_table* t) {
  assert(t->rc > 0);
  if (--(t->rc) == 0) {
    avm_tableDestroy(t);
  }
}

void avm_tableBucketsInit(avm_table_bucket** p) {
  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) {
    p[i] = NULL;
  }
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
    for (avm_table_bucket* b = *p; b;) {
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
  assert(!numConsts.empty());
  return numConsts[index];
}
char* consts_getString(unsigned index) {
  assert(!stringConsts.empty());
  return stringConsts[index];
}
char* libfuncs_getUsed(unsigned index) {
  assert(!namedLibFuncs.empty());
  return namedLibFuncs[index];
}

avm_memcell* avm_translate_operand(vmarg* arg, avm_memcell* reg) {
  switch (arg->type) {
    case global_a:
      return &stack_m[AVM_STACKSIZE - 1 - arg->val];
    case local_a:
      return &stack_m[topsp - arg->val];
    case formal_a:
      return &stack_m[topsp + AVM_STACKENV_SIZE + 1 + arg->val];
    case retval_a:
      return &retval;
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
  printf("Runtime Warning: ");
  va_list args;
  va_start(args, format);
  printf(format, args);
  va_end(args);
}
void avm_error(char* format, ...) {
  printf("Runtime Error: ");
  va_list args;
  va_start(args, format);
  printf(format, args);
  va_end(args);
}

void avm_assign(avm_memcell* lv, avm_memcell* rv) {
  if (lv == rv) return;

  if (lv->type == table_m && rv->type == table_m &&
      lv->data.tableVal == rv->data.tableVal)
    return;

  if (rv->type == undef_m) avm_warning("assigning from 'undef' contnet!");

  avm_memcellClear(lv);

  memcpy(lv, rv, sizeof(avm_memcell));

  if (lv->type == string_m)
    lv->data.strVal = strdup(rv->data.strVal);
  else if (lv->type == table_m)
    avm_tableIncrRC(lv->data.tableVal);
}

void execute_assign(instruction* instr) {
  avm_memcell* lv = avm_translate_operand(instr->result, NULL);
  avm_memcell* rv = avm_translate_operand(instr->arg1, &ax);

  assert(lv && (&stack_m[AVM_STACKSIZE - 1] >= lv && lv >= &stack_m[top] || lv == &retval)); //FIXME: selida17
  assert(rv);

  avm_assign(lv, rv);
}

void execute_call(instruction* instr) {
  avm_memcell* func = avm_translate_operand(instr->result, &ax); 
  assert(func);
  avm_callsaveenvironment();
  switch (func->type) {
    case userfunc_m: {
      pc = func->data.funcVal;
      assert(pc < AVM_ENDING_PC);
      assert(instructionz[pc]->opcode == funcenter_v);
      break;
    }
    case string_m:
      avm_callLibFunc(func->data.strVal);
      break;
    case libfunc_m:
      avm_callLibFunc(func->data.libfuncVal);
      break;
    default: {
      char* s = avm_toString(func);
      avm_error("call: cannot bind '%s' to function!", s);
      free(s);
      executionFinished = true;
    }
  }
}

unsigned totalActuals = 0;

void avm_dec_top(void){
    if(!top){
        avm_error("stack overflow");
        executionFinished=1;
    }else --top;
}
void avm_push_envvalue(unsigned val){
    stack_m[top].type = number_m;
    stack_m[top].data.numVal = val;
    avm_dec_top();
}
void avm_callsaveenvironment(){
    avm_push_envvalue(totalActuals);
    avm_push_envvalue(pc+1);
    avm_push_envvalue(top+totalActuals+2);
    avm_push_envvalue(topsp);
}

userfunc* avm_getFuncInfo(unsigned address){
  for(int i=0; i<userFuncz.size(); i++)
    if( userFuncz[i]->address == address ) return userFuncz[i];
  return NULL;
}

void execute_funcenter(instruction* instr){
  avm_memcell* func = avm_translate_operand(instr->result,&ax);
  assert(func);
  assert(pc == func->data.funcVal);

  totalActuals = 0;
  userfunc* funcInfo = avm_getFuncInfo(pc);
  assert(funcInfo);
  topsp = top;
  top = top - funcInfo->localSize;
}
unsigned avm_get_envvalue(unsigned i){
  assert(stack_m[i].type = number_m);
  unsigned val = (unsigned) stack_m[i].data.numVal;
  assert(stack_m[i].data.numVal == ((double)val));
  return val;
}

void execute_funcexit(instruction* unused){
  unsigned oldTop = top;
  top = avm_get_envvalue(topsp + AVM_SAVEDTOP_OFFSET);
  pc = avm_get_envvalue(topsp + AVM_SAVEDPC_OFFSET);
  topsp = avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);
  while(++oldTop <= top)
    avm_memcellClear(&stack_m[oldTop]);
}

library_func_t libraryFuncz[12];

library_func_t avm_getLibraryFunc(char* id){
  int lib_index = libfuncs_newUsed(id); // retrieve index from the lib funcs array 
  return libraryFuncz[lib_index];
}

void avm_registerLibFunc(char* id, library_func_t addr){
  int lib_index = libfuncs_newUsed(id); // retrieve index from the lib funcs array 
  libraryFuncz[lib_index] = addr;
}

void avm_callLibFunc(char* id){
    library_func_t f = avm_getLibraryFunc(id);
    if(!f){
        avm_error("unsupported lib func '%s' called!",id);
        executionFinished = true;
    }else{
        topsp = top;
        totalActuals = 0;
        (*f)();
        if(!executionFinished)
            execute_funcexit((instruction*) 0);
    }
}

unsigned avm_totalActuals(){
  return avm_get_envvalue(topsp + AVM_NUMACTUALS_OFFSET);
}

avm_memcell* avm_getActual (unsigned i){
  assert( i < avm_totalActuals() );
  return &stack_m[topsp + AVM_STACKENV_SIZE+1+i];
}

/*Library Functions - Start*/
void libfunc_print(){
  unsigned n = avm_totalActuals();
  for(unsigned i =0; i < n; ++i ){
      char* s = avm_toString(avm_getActual(i));
      puts(s);
      free(s);
  }
}

void libfunc_typeof(void){
  unsigned n = avm_totalActuals();

  if(n!=1){
    executionFinished = true;
    avm_error("one argument expected in 'typeof' (not %d) !",n);
  }    
  else{
    avm_memcellClear(&retval);
    retval.type = string_m;
    retval.data.strVal = strdup(typeStrings[avm_getActual(0)->type]);
  }
}
void libfunc_sin(void){
  unsigned n = avm_totalActuals();

  if(n!=1){
    executionFinished = true;
    avm_error("one argument expected in 'sin' (not %d) !",n);
  }    
  else{
    if(avm_getActual(0)->type != number_m){
      executionFinished = true;
      avm_error("Sin expects number!",n);
    }else{
      avm_memcellClear(&retval);
      retval.type = number_m;
      retval.data.numVal = sin(avm_getActual(0)->data);
    }
  }
}

void libfunc_cos(void){
  unsigned n = avm_totalActuals();

  if(n!=1){
    executionFinished = true;
    avm_error("one argument expected in 'cos' (not %d) !",n);
  }    
  else{
    if(avm_getActual(0)->type != number_m){
      executionFinished = true;
      avm_error("Cos expects number!",n);
    }else{
      avm_memcellClear(&retval);
      retval.type = number_m;
      retval.data.numVal = cos(avm_getActual(0)->data);
    }
  }
}

void libfunc_sqrt(void){
  unsigned n = avm_totalActuals();

  if(n!=1){
    executionFinished = true;
    avm_error("one argument expected in 'sqrt' (not %d) !",n);
  }    
  else{
    if(avm_getActual(0)->type != number_m){
      executionFinished = true;
      avm_error("Sqrt expects number!",n);
    }else{
      avm_memcellClear(&retval);
      retval.type = number_m;
      double tmp = sqrt(avm_getActual(0)->data);
      if(tmp == null ) retval.type = nil_m;
      else retval.data.numVal = tmp;
    }
  }
}
bool is_number(const string& s){
    std::string::const_iterator it = s.begin();
    while (it != s.end() && std::isdigit(*it)) ++it;
    return !s.empty() && it == s.end();
}

void libfunc_input(void){
  string s;
  cin >> s;

  if(is_number(s)){
    avm_memcellClear(&retval);
    retval.type = number_m;
    retval.data.numVal = atof(s.c_str())
  }else if(s.compare("true") || s.compare("TRUE")) {
    
  }

}

void libfunc_argument(void){
}

void libfunc_strtonum(void){
  
}

void libfunc_objectcopy(void){
}
void libfunc_totalarguments(void){
}
void libfunc_objectmemberkeys(void){
}
void libfunc_objecttotalmembers(void){
}

//FIXME: The other functions
/*Library Functions - End*/

void execute_pusharg(instruction* instr){
  avm_memcell* arg = avm_translate_operand(instr->arg1, &ax);
  assert(arg);
  avm_assign(&stack_m[top], arg);
  ++totalActuals;
  avm_dec_top();
}

char* number_toString(avm_memcell* m){
  assert(m);
  assert(m->type == number_m);
  stringstream ss; 
  ss << (m->data.numVal);
  return (char*)ss.str().c_str();
}
char* string_toString(avm_memcell* m){
  assert(m);
  assert(m->type == string_m);
  return strdup(m->data.strVal);
}
char* bool_toString(avm_memcell* m){
  assert(m);
  assert(m->type == bool_m);
  stringstream ss; 
  ss << (m->data.boolVal);
  return (char*)ss.str().c_str();
}
char* table_toString(avm_memcell* m){
  assert(m);
  assert(m->type == table_m);
  return "FIXME";
}
char* userfunc_toString(avm_memcell* m){
  assert(m);
  assert(m->type == userfunc_m);
  stringstream ss; 
  ss << (m->data.funcVal);
  return (char*)ss.str().c_str();
}
char* libfunc_toString(avm_memcell* m){
  assert(m);
  assert(m->type == libfunc_m);
  return strdup(m->data.libfuncVal);
}
char* nil_toString(avm_memcell* m){
  assert(m);
  assert(m->type == nil_m);
  return "nil";
}
char* undef_toString(avm_memcell* m){
  assert(m);
  assert(m->type == undef_m);
  return "undefined";
}

tostring_func_t tostringFuncs[] = {
  number_toString,
  string_toString,
  bool_toString,
  table_toString,
  userfunc_toString,
  libfunc_toString,
  nil_toString,
  undef_toString
};

char* avm_toString(avm_memcell* m){
  assert(m->type >= 0 && m->type == undef_m);
  return (*tostringFuncs[m->type])(m);
}

double add_impl (double x, double y){ return x+y; }
double sub_impl (double x, double y){ return x-y; }
double mul_impl (double x, double y){ return x*y; }
double div_impl (double x, double y){ return x/y; } //FIXME: error check?
double mod_impl (double x, double y){ return x+y; }

arithmetic_func_t arithmeticFuncs[] = {
  add_impl,
  sub_impl,
  mul_impl,
  div_impl,
  mod_impl
};

void execute_arithmetic(instruction* instr){
  avm_memcell* lv  = avm_translate_operand(instr->result, (avm_memcell *) 0);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, &ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, &bx);

  assert(lv && (&stack_m[AVM_STACKSIZE - 1] >= lv && lv > &stack_m[top] || lv==&retval));
  assert(rv1 && rv2);
  if(rv1->type != number_m ||rv2->type!=number_m){
      avm_error("not a number in arithmetic!");
      executionFinished = 1;
  }else{
      arithmetic_func_t op = arithmeticFuncs[instr->opcode - add_v];
      avm_memcellClear(lv);
      lv->type        = number_m;
      lv->data.numVal = (*op)(rv1->data.numVal, rv2->data.numVal);
  }
}
/*
Με τρόπο παρόμοιο των αριθμητικών εκφράσεων
υλοποιούνται και οι συσχετιστικοί τελεστές διάταξης < <=
> >=, δηλ. οι εντολές JGE, JGT, JLE, JLT, καθώς
αφορούν μόνο αριθμούς.
 Προσοχή θέλει το γεγονός ότι δεν χρειάζεται να
μετατρέψουμε το operand στο οποίο είναι αποθηκευμένη
η διεύθυνση (label) της εντολής προορισμού
 Οι βοηθητικές συναρτήσεις comparisonFuncs θα είναι
έχουν αντίστοιχο signature, δηλ.
 bool (*cmp_func) (double, double) 

*/

unsigned char number_tobool  (avm_memcell* m){ return m->data.numVal != 0;}
unsigned char string_tobool  (avm_memcell* m){ return m->data.strVal[0] != 0;}
unsigned char bool_tobool    (avm_memcell* m){ return m->data.boolVal;}
unsigned char table_tobool   (avm_memcell* m){ return 1; }
unsigned char userfunc_tobool(avm_memcell* m){ return 1; }
unsigned char libfunc_tobool (avm_memcell* m){ return 1; }
unsigned char nill_tobool    (avm_memcell* m){ return 0; }
unsigned char undef_tobool   (avm_memcell* m){ assert(0); return 0;}

tobool_func_t toboolFuncs[] = {
  number_tobool,
  string_tobool,
  bool_tobool,
  table_tobool,
  userfunc_tobool,
  libfunc_tobool,
  nill_tobool,
  undef_tobool
};

bool avm_tobool(avm_memcell* m){
  assert(m->type >= 0 && m->type < undef_m);
  return (*toboolFuncs[m->type])(m);
}

bool number_check(avm_memcell* a, avm_memcell* b){
  return numConsts[a->data.numVal] == numConsts[b->data.numVal];
}

bool string_check(avm_memcell* a, avm_memcell* b){
  if( strcmp(a->data.strVal,b->data.strVal) == 0 )
    return true;
  else 
    return false; 
}

bool table_check(avm_memcell* a, avm_memcell* b){
  //TODO: Maybe check every element not the address
  return a->data.tableVal == b->data.tableVal;
}

bool userfunc_check(avm_memcell* a, avm_memcell* b){
  return !strcmp(a->data.funcVal,b->data.funcVal);
}

bool libfunc_check(avm_memcell* a, avm_memcell* b){
  if( strcmp(a->data.libfuncVal,b->data.libfuncVal) == 0 )
    return true;
  else 
    return false;
}

eq_check_t checks[] = {
  number_check,
  string_check,
  0, //bool
  table_check,
  userfunc_check,
  libfunc_check, 
  0, //nil
  0 //undef
};

void execute_jeq(instruction* instr){
  assert(instr->result->type == label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, &ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, &bx);
  
  bool result = 0;

  if(rv1->type == undef_m || rv2->type == undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type == nil_m || rv2->type == nil_m)
    result = rv1->type == nil_m && rv2->type == nil_m;
  else if(rv1->type == bool_m || rv2->type == bool_m)
    result = (avm_tobool(rv1) == avm_tobool(rv2));
  else if(rv1->type != rv2->type)
    avm_error("%s == %s is illegal", typeStrings[rv1->type], typeStrings[rv2->type]);
  else {
    /* Equality check with dipatching */
    result =  checks[rv1->type](rv1, rv2);
  }    

  if(!executionFinished && result)
    pc = instr->result->val;

}

void execute_jne(instruction* instr){
  assert(instr->result->type == label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, &ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, &bx);
  
  bool result = 0;

  if(rv1->type == undef_m || rv2->type == undef_m){
    executionFinished = true;
    avm_error("'undef' involved in equality!");
  }
  else if(rv1->type == nil_m || rv2->type == nil_m)
    result = rv1->type == nil_m && rv2->type == nil_m;
  else if(rv1->type == bool_m || rv2->type == bool_m)
    result = (avm_tobool(rv1) != avm_tobool(rv2));
  else if(rv1->type != rv2->type){
    executionFinished = true;
    avm_error("%s != %s is illegal", typeStrings[rv1->type], typeStrings[rv2->type]);
  }
  else {
    /* Equality check with dipatching */
    result =  !(checks[rv1->type](rv1, rv2));
  }    

  if(!executionFinished && result)
    pc = instr->result->val;
}

bool le_impl(double a, double b){ return a < b;  };
bool ge_impl(double a, double b){ return a > b;  };
bool lt_impl(double a, double b){ return a <= b; };
bool gt_impl(double a, double b){ return a >= b; };

cmp_func_t numberCmpFuncs[] = {
  le_impl,
  ge_impl,
  lt_impl,
  gt_impl
};

void execute_rel(instruction* instr){
  assert(instr->result->type == label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1, &ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2, &bx);
  
  bool result = 0;

  if( rv1->type != number_m || rv2->type != number_m ) {
    executionFinished = true;
    avm_error("Comparing args are not numbers %s %s",  typeStrings[rv1->type], typeStrings[rv2->type]);
  } else {
    result = numberCmpFuncs[instr->opcode - jle_v](rv1->data.numVal, rv2->data.numVal);
  }
}

void execute_newtable(instruction* instr){
  avm_memcell* lv = avm_translate_operand(instr->result,(avm_memcell*)0);
  assert(lv && (&stack_m[AVM_STACKSIZE-1] >= lv && lv > &stack_m[top] || lv == &retval));

  avm_memcellClear(lv);

  lv->type = table_m;
  lv->data.tableVal = avm_tablenew();
  avm_tableIncrRC(lv->data.tableVal);
}

avm_memcell* avm_tablegetelem( avm_table* table, avm_memcell* index){
  assert(table && index);
  unsigned array_index = hashMe[index->type](index);

  switch(index->type){
    case number_m: {
      avm_table_bucket* tmp = table->numIndexed[array_index];
      while( tmp ) {
        if( tmp->key.data.numVal == index->data.numVal){
          avm_memcell* ret = &tmp->value;
          return ret;
        }
        tmp = tmp->next;
      }
      break;
    }
    case string_m: {
      avm_table_bucket* tmp = table->strIndexed[array_index];
      while( tmp ) {
        if( strcmp(tmp->key.data.strVal, index->data.strVal) == 0){
          avm_memcell* ret = &tmp->value;
          return ret;
        }
        tmp = tmp->next;
      }
      break;
    }
    default: 
      assert(0);
  }
  return NULL;
}

void insert_numIndexed(avm_table* table, avm_memcell* index, avm_memcell* content){
  unsigned array_index = hashMe[index->type](index);
  avm_table_bucket* new_bucket = new avm_table_bucket();
  avm_assign(&new_bucket->key, index);
  avm_assign(&new_bucket->value, content);

  if(table->numIndexed[array_index] == NULL){
    new_bucket->next = NULL;
    table->numIndexed[array_index] = new_bucket;
  } else{
    new_bucket->next = table->numIndexed[array_index];
    table->numIndexed[array_index] = new_bucket;
  }
}

void insert_strIndexed(avm_table* table, avm_memcell* index, avm_memcell* content){
  unsigned array_index = hashMe[index->type](index);
  avm_table_bucket* new_bucket = new avm_table_bucket();
  avm_assign(&new_bucket->key, index);
  avm_assign(&new_bucket->value, content);

  if(table->strIndexed[array_index] == NULL){
    new_bucket->next = NULL;
    table->strIndexed[array_index] = new_bucket;
  } else{
    new_bucket->next = table->strIndexed[array_index];
    table->strIndexed[array_index] = new_bucket;
  }
}

void avm_tablesetelem( avm_table* table, avm_memcell* index, avm_memcell* content){
  assert(table && index && content);

  switch(index->type){
    case number_m: {
      insert_numIndexed(table, index, content);
    }
    case string_m: {
      insert_strIndexed(table, index, content);
    }
    default: 
      assert(0);
  }
}
void execute_tablegetelem(instruction* instr) {
    avm_memcell* lv = avm_translate_operand(instr-> result,(avm_memcell*) 0);  
    avm_memcell* t  = avm_translate_operand(instr-> arg1  ,(avm_memcell*) 0);
    avm_memcell* i  = avm_translate_operand(instr-> arg2  ,&ax);

    assert(lv && (&stack_m[AVM_STACKSIZE-1] >= lv && lv > &stack_m[top] || lv==&retval) ); 
    assert(t && (&stack_m[AVM_STACKSIZE-1] >= t && t > &stack_m[top] ) );
    assert(i);
    avm_memcellClear(lv);
    lv->type = nil_m;

    if(t->type != table_m){
        avm_error("illegal use of type %s as table!", typeStrings[t->type]);
    }else{
        avm_memcell* content = avm_tablegetelem(t->data.tableVal,i);
        if(content)
            avm_assign(lv,content);
        else{
            char* ts = avm_toString(t);
            char* is = avm_toString(i);
            avm_warning("%s[%s] not found!",ts, is);
            free(ts);
            free(is);
        }
    } 
}

void execute_tablesetelem(instruction* instr){
    avm_memcell* t = avm_translate_operand(instr-> result, (avm_memcell*) 0);
    avm_memcell* i = avm_translate_operand(instr-> arg1, &ax);
    avm_memcell* c = avm_translate_operand(instr-> arg2, &bx);

    assert(t && &stack_m[AVM_STACKSIZE-1] >= t && t > &stack_m[top] );
    assert(i && c);
    if(t->type != table_m)
        avm_error("illegal use of type %s as table!", typeStrings[t->type]);
    else    
        avm_tablesetelem(t->data.tableVal,i,c);
}

void avm_initialize(){
    avm_initstack();
    avm_registerLibFunc("print", libfunc_print);
    avm_registerLibFunc("print", libfunc_typeof);
}

void libfunc_totalarguments(){
    unsigned p_topsp = avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);
    avm_memcellClear(&retval);

    if(!p_topsp){
        avm_error("'totalargument' called outside a function!");
        retval.type = nil_m;
    }else{
        retval.type = number_m;
        retval.data.numVal = avm_get_envvalue(p_topsp + AVM_NUMACTUALS_OFFSET);
    }

}

void execute_jump(instruction* instr){}
void execute_uminus(instruction* instr){}
void execute_and(instruction* instr){}
void execute_or(instruction* instr){}
void execute_not(instruction* instr){}
void execute_nop(instruction* instr){}

execute_func_t executeFuncs[] = {
    execute_assign,       execute_add,          execute_sub,
    execute_mul,          execute_div,          execute_mod,
    execute_uminus,       execute_and,          execute_or,
    execute_not,          execute_jeq,          execute_jne,
    execute_jle,          execute_jge,          execute_jlt,
    execute_jgt,          execute_call,         execute_pusharg,
    execute_funcenter,    execute_funcexit,     execute_newtable,     
    execute_tablegetelem, execute_tablesetelem, execute_jump,
    execute_nop
  };

void execute_cycle(void) {
  if (executionFinished) return;
  if (pc == AVM_ENDING_PC) {
    executionFinished = true;
    return;
  } else {
    assert(pc < AVM_ENDING_PC);
    instruction* instr = instructionz[pc];
    assert(instr->opcode >= 0 && instr->opcode <= AVM_MAX_INSTRUCTIONS);
    if (instr->srcLine) currLine = instr->srcLine;
    unsigned oldPc = pc;
    (*executeFuncs[instr->opcode])(instr);
    if (pc == oldPc) ++pc;
  }
}