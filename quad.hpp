#include <vector>
#include <string>
#include "symtable.hpp"

#define EXPAND_SIZE 1024
#define CURR_SIZE (total * sizeof(quad))
#define NEW_SIZE (EXPAND_SIZE * sizeof(quad) + CURR_SIZE)

enum iopcode {
  assign_op,
  add,
  sub,
  mul_op,
  div_op,
  mod_op,
  uminus_op,
  and_op,
  or_op,
  not_op,
  if_eq,
  if_noteq,
  if_lesseq,
  if_greater_eq,
  if_less,
  if_greater,
  call,
  param,
  ret,
  getretval,
  funcstart,
  funcend,
  tablecreate,
  tablegetelem,
  tablesetelem
};

struct expr;

struct quad {
  iopcode iop;
  expr* result;
  expr* arg1;
  expr* arg2;
  unsigned label;
  unsigned line;
};

vector<quad*> quads;
unsigned total = 0;
unsigned int currQuad = 0;
int tmpcounter = 0;

SymTable symbol_table = *new SymTable();


typedef enum expr_t {
  var_e,
  tableitem_e,

  programfunc_e,
  libraryfunc_e,

  arithexpr_e,
  boolexpr_e,
  assignexpr_e,
  newtable_e,

  constnum_e,
  constbool_e,
  conststring_e,

  nil_e,
} expr_t;

typedef struct expr {
  expr_t type;
  SymbolTableEntry* sym;
  struct expr* index;
  double numConst;
  char* strConst;
  unsigned char boolConst;
  struct expr* next;
} expr;

// Functions

quad* expand() {
  assert(currQuad == total);
  quad* p = new quad();
  quads.push_back(p);
  total += EXPAND_SIZE;
  // currQuad++;
  return p;
}

void emit(iopcode iop, expr* arg1, expr* arg2, expr* result, unsigned label,
          unsigned line) {
  quad* p;
  if (currQuad == total)
    p = expand();
  else
    p = quads[currQuad++];
  p->arg1 = arg1;
  p->arg2 = arg2;
  p->result = result;
  p->label = label;
  p->line = line;
}

char* new_tmpname() { 
  char name[1000];
  sprintf(name, "$t%d", tmpcounter);
  return strdup(name); 
  }

void reset_tmp() { tmpcounter = 0; }

SymbolTableEntry* new_tmp(unsigned int lineno) {
  char* name = new_tmpname();
  SymbolTableEntry* sym ;//= symbol_table.find_node(name, LOCAL);  // TODO: FIX ME
  if (NULL == sym){

    return symbol_table.insert(name, lineno, LOCAL);
  }else
    return sym;
}