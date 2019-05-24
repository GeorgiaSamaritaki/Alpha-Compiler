#include <stdarg.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#ifndef avm_utility
#define avm_utility

using namespace std;

#define AVM_STACKSIZE 4096
#define AVM_WIPEOUT(m) memset(&(m), 0, sizeof(m))
#define AVM_TABLE_HASHSIZE 211
#define AVM_STACKENV_SIZE 4
#define AVM_MAX_INSTRUCTIONS (unsigned)nop_v
#define AVM_ENDING_PC codeSize
#define AVM_NUMACTUALS_OFFSET 4
#define AVM_SAVEDPC_OFFSET 3
#define AVM_SAVEDTOP_OFFSET 2
#define AVM_SAVEDTOPSP_OFFSET 1

#define HASH_MUL 65599

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
  ret_v,
  getretval_v,
  funcenter_v,
  funcexit_v,
  newtable_v,
  tablegetelem_v,
  tablesetelem_v,
  jump_v,
  nop_v
};

enum vmarg_t {
  label_a = 0,
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
  vmarg* result;
  vmarg* arg1;
  vmarg* arg2;
  unsigned srcLine;
} instruction;

typedef struct userfunc {
  unsigned address;
  unsigned localSize;
  char* id;
  string toString(){
    stringstream ss; 
    string s = id;
    ss << s << " " << address << " " << localSize <<" ";
    return ss.str();
  }

} userfunc;

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
avm_memcell ax;
avm_memcell bx;
avm_memcell cx;
avm_memcell retval;
unsigned top, topsp;
bool executionFinished = false;
unsigned pc = 0;
unsigned currLine = 0;
unsigned codeSize = 0;

unsigned totalActuals = 0;

//For reading 
vector<double> numConstsRead;
vector<char*> stringConstsRead;
vector<char*> namedLibFuncsRead;
vector<userfunc*> userFunczRead;
vector<instruction*> instructionzRead;

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


typedef void (*library_func_t)();
typedef void (*execute_func_t)(instruction*);
typedef void (*memclear_func_t)(avm_memcell*);
typedef char* (*tostring_func_t)(avm_memcell*);
typedef double (*arithmetic_func_t)(double, double);
typedef unsigned char (*tobool_func_t)(avm_memcell*);
typedef bool (*eq_check_t)(avm_memcell*, avm_memcell*);
typedef bool (*cmp_func_t)(double, double);
typedef unsigned (*hash_func_t)(avm_memcell*);
void avm_tablesetelem(avm_table* table, avm_memcell* index,
                      avm_memcell* content);

/*Funcs*/
void avm_tableDestroy(avm_table* t);
void avm_memcellClear(avm_memcell* m);
void avm_callsaveenvironment();
void avm_callLibFunc(char* id);
char* avm_toString(avm_memcell* m);
void avm_assign(avm_memcell* lv, avm_memcell* rv);
userfunc* avm_getFuncInfo(unsigned address);

void avm_error(char* format, ...);
void avm_warning(char* format, ...);

void avm_push_envvalue(unsigned val);
avm_table* avm_tablenew(void);
unsigned avm_totalActuals();
avm_memcell* avm_getActual(unsigned i);
unsigned avm_get_envvalue(unsigned i);
avm_memcell* avm_translate_operand(vmarg* arg, avm_memcell* reg);
string table_tostring(avm_table* t);
avm_memcell* avm_tablegetelem(avm_table* table, avm_memcell* index);

string print_cell(avm_memcell*);
void printStack();

unsigned get_magic(string s) { return 69420666; }



char* number_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == number_m);
  stringstream ss;
  ss << (m->data.numVal);
  return (char*)ss.str().c_str();
}
char* string_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == string_m);
  return strdup(m->data.strVal);
}
char* bool_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == bool_m);
  if (m->data.boolVal) {
    return "true";
  } else {
    return "false";
  }
}
char* table_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == table_m);
  return (char*)table_tostring(m->data.tableVal).c_str();
}
char* userfunc_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == userfunc_m);
  stringstream ss;
  ss << (m->data.funcVal);
  return (char*)ss.str().c_str();
}
char* libfunc_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == libfunc_m);
  return strdup(m->data.libfuncVal);
}
char* nil_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == nil_m);
  return "nil";
}
char* undef_toString(avm_memcell* m) {
  assert(m);
  assert(m->type == undef_m);
  return "undefined";
}

tostring_func_t tostringFuncs[] = {
    number_toString,   string_toString,  bool_toString, table_toString,
    userfunc_toString, libfunc_toString, nil_toString,  undef_toString};

char* avm_toString(avm_memcell* m) {
  assert(m->type >= 0);
  assert(m->type != undef_m);
  return (*tostringFuncs[m->type])(m);
}

string table_tostring(avm_table* t) {
  string s = "[";
  for (int i = 0; i < AVM_TABLE_HASHSIZE; i++) {
    avm_table_bucket* tmp = t->numIndexed[i];
    while (tmp != NULL) {
      s+="{"; 
      s+=avm_toString(&tmp->key);
      s+= ":";
      s+=avm_toString(&tmp->value);
      s+="}";
      tmp = tmp->next;
    }
  }
  for (int i = 0; i < AVM_TABLE_HASHSIZE; i++) {
    avm_table_bucket* tmp = t->strIndexed[i];
    while (tmp != NULL) {
      s+="{";
      s+=avm_toString(&tmp->key);
      s+= ":";
      s+=avm_toString(&tmp->value);
      s+="}";
      tmp = tmp->next;
    }
  }
  s += "]";
  return s;
}

double add_impl(double x, double y) { return x + y; }
double sub_impl(double x, double y) { return x - y; }
double mul_impl(double x, double y) { return x * y; }
double div_impl(double x, double y) { return y==0? -1 : x / y; }  // FIXME: error check?
double mod_impl(double x, double y) { return x + y; }

arithmetic_func_t arithmeticFuncs[] = {add_impl, sub_impl, mul_impl, div_impl,
                                       mod_impl};

unsigned char number_tobool(avm_memcell* m) { return m->data.numVal != 0; }
unsigned char string_tobool(avm_memcell* m) { return m->data.strVal[0] != 0; }
unsigned char bool_tobool(avm_memcell* m) { return m->data.boolVal; }
unsigned char table_tobool(avm_memcell* m) { return 1; }
unsigned char userfunc_tobool(avm_memcell* m) { return 1; }
unsigned char libfunc_tobool(avm_memcell* m) { return 1; }
unsigned char nill_tobool(avm_memcell* m) { return 0; }
unsigned char undef_tobool(avm_memcell* m) {
  assert(0);
  return 0;
}

tobool_func_t toboolFuncs[] = {number_tobool, string_tobool,   bool_tobool,
                               table_tobool,  userfunc_tobool, libfunc_tobool,
                               nill_tobool,   undef_tobool};

bool avm_tobool(avm_memcell* m) {
  assert(m->type >= 0 && m->type < undef_m);
  return (*toboolFuncs[m->type])(m);
}

bool number_check(avm_memcell* a, avm_memcell* b) {
  return numConstsRead[a->data.numVal] == numConstsRead[b->data.numVal];
}

bool string_check(avm_memcell* a, avm_memcell* b) {
  if (strcmp(a->data.strVal, b->data.strVal) == 0)
    return true;
  else
    return false;
}

bool table_check(avm_memcell* a, avm_memcell* b) {
  // TODO: Maybe check every element not the address
  return a->data.tableVal == b->data.tableVal;
}

bool userfunc_check(avm_memcell* a, avm_memcell* b) {
  return a->data.funcVal == b->data.funcVal;
}

bool libfunc_check(avm_memcell* a, avm_memcell* b) {
  if (strcmp(a->data.libfuncVal, b->data.libfuncVal) == 0)
    return true;
  else
    return false;
}

eq_check_t checks[] = {
    number_check,
    string_check,
    0,  // bool
    table_check,
    userfunc_check,
    libfunc_check,
    0,  // nil
    0   // undef
};


bool le_impl(double a, double b) { return a < b; };
bool ge_impl(double a, double b) { return a > b; };
bool lt_impl(double a, double b) { return a <= b; };
bool gt_impl(double a, double b) { return a >= b; };

cmp_func_t numberCmpFuncs[] = {le_impl, ge_impl, lt_impl, gt_impl};



#endif