#ifndef avm_utility
#define avm_utility

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
library_func_t libraryFuncz[12];

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

void avm_error(char* format, ...);
void avm_warning(char* format, ...);

void avm_push_envvalue(unsigned val);
avm_table* avm_tablenew(void);
unsigned avm_totalActuals();
avm_memcell* avm_getActual(unsigned i);
unsigned avm_get_envvalue(unsigned i);

#endif