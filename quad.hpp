#include <string>
#include <vector>
#include <stdarg.h>
#include "symtable.hpp"

#include <sys/stat.h>
#include <errno.h>

#define EXPAND_SIZE 1024
#define CURR_SIZE (total * sizeof(quad))
#define NEW_SIZE (EXPAND_SIZE * sizeof(quad) + CURR_SIZE)

typedef enum iopcode {
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
  tablesetelem,
  jump
}iopcode;

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

struct call_l{
  expr* elist;
  bool method;
  char* name;
};


// Functions
quad* expand() {
  assert(currQuad == total);
  quad* p = new quad();
  quads.push_back(p);
  total += EXPAND_SIZE;
  // currQuad++;
  return p;
}
void emit(iopcode iop, expr* arg1, expr* arg2, expr* result, unsigned int label = 0) {
  quad* p;
  if (currQuad == total)
    p = expand();
  else{
    p = new quad();
    quads[currQuad++] = p;
  }
  p->arg1 = arg1;
  p->arg2 = arg2;
  p->result = result;
  p->label = label;
}
void emit_function(iopcode iop, expr* result){
  emit(iop,NULL,NULL,result, result->sym->value.funcVal->iaddress);
}

void patchLabel( unsigned int quadNo, unsigned int label){
  assert(quadNo < currQuad);
  quads[quadNo]->label = label;
}

char* new_tmpname() {
  char name[1000];
  sprintf(name, "$t%d", tmpcounter);
  return strdup(name);
}

void reset_tmp() { tmpcounter = 0; }

SymbolTableEntry* new_tmp(unsigned int lineno) {
  char* name = new_tmpname();
  SymbolTableEntry* sym = symbol_table.lookUp_curscope(name);
  if (NULL == sym) {
    return symbol_table.insert(name, lineno, LOCAL);
  } else
    return sym;
}

unsigned int nextQuadLabel(){return currQuad;}

expr* lvalue_expr(SymbolTableEntry* entry){
  assert(entry);
  expr* new_expr =  new expr();

  new_expr->sym = entry;
  new_expr->next = (expr*) 0;
  
  switch(entry->type){
    case FORMAL:{}
    case LOCAL:{}
    case GLOBAL:{
      new_expr->type = var_e;
      break;
    } 
    case LIBFUNC:{
      new_expr->type = libraryfunc_e;
      break;
    }
    case USERFUNC:{
      new_expr->type = programfunc_e;
      break;
    }
    default: assert(0);                     
  }

  return new_expr;
}

expr* newExpr(expr_t t){
  expr* new_expr = new expr();
  new_expr->type = t;
  return new_expr;
}

expr* newExpr_constString(char* s){
  expr* e = newExpr(conststring_e);
  e->strConst = strdup(s);
  return e;
}
expr* newExpr_constNum(double i){
  expr* e = newExpr(constnum_e);
  e->numConst = i;
  return e;
}
expr* newExpr_constBool(bool b){
  expr* e = newExpr(constbool_e);
  e->boolConst = b;
  return e;
}

expr* emit_ifTableItem(expr* e){ //FIXME:
  if(e->type != tableitem_e) return e;
  else{
    expr* result = newExpr(var_e);
    result->sym = new_tmp(symbol_table.get_lineno(e->sym));
    emit(tablegetelem, e, e->index, result, 0);
    return result;
  }
}
// function to create new expression/quad ??

expr* member_item(expr* lvalue, char* name){
  lvalue = emit_ifTableItem(lvalue);
  expr* item = newExpr(tableitem_e);
  item->sym = lvalue->sym;
  item->index = newExpr_constString(name);
  return item;
}

expr* make_call(expr* lvalue, expr* elist){
  expr* func = emit_ifTableItem(lvalue);
  assert(!symbol_table.is_var(func->sym->type));
  expr* curr = elist;
  while(curr!=NULL){
    emit(param, curr, NULL, NULL, 0);
    curr = curr->next;
  }
    printf("here!!!!!\n");
  emit(call, func, NULL, NULL,0);
  expr* result = newExpr(var_e);
  
  result->sym = new_tmp(lvalue->sym->value.funcVal->line);
  emit(getretval,NULL,NULL,result);
  return result;
}

void comperror(char* format, ...){
  va_list args;
  va_start(args, format);
  printf(format, args);
  va_end(args);
}

void checkUminus(expr * e){
  if(e->type == constbool_e   ||
    e->type == conststring_e  ||
    e->type == nil_e          ||
    e->type == newtable_e     ||
    e->type == programfunc_e  ||
    e->type == libraryfunc_e  ||
    e->type == boolexpr_e)
    comperror("Illegal expr to unary -");
}


double compute(iopcode op, double a, double b){
  switch (op){
    case add:
        return a+b;
    case sub :
      return a-b;
    case mul_op :
      return a*b;
    case div_op:
      return b ? a/b : 0;
    case mod_op:
      return ((int) a) % ((int) b);
    default:
      assert(false);
  }
}

bool compute_rel(iopcode op, double a, double b){
  switch (op){
    case if_greater:
      return a>b;
    case if_less :
      return a<b;
    case if_greater_eq :
      return a>=b;
    case if_lesseq :
      return a<=b;
    case if_eq :
      return a==b;
    case if_noteq :
      return a!=b;
    default :
      assert(false);    
  }
}

bool compute(iopcode op, bool a, bool b){
  switch (op){
    case and_op :
      return a&&b;
    case or_op : 
      return a||b;
    case if_eq: 
      return a==b;
    case if_noteq:
      return a!=b;
    default :
      assert(false);
  }    
}


void printQuads(){
  cout << "Yeeeeeeeeee";
}

bool is_same(expr_t a, expr_t b){
  if(a == var_e || b == var_e) 
    return false;
  if(a == b) 
    return true;
  if( (a == arithexpr_e || a == constnum_e) && 
    (b == arithexpr_e || b == constnum_e))
    return true;
  if( (a == constbool_e || a == boolexpr_e) && 
    (b == constbool_e || b == boolexpr_e))
    return true;
  if( (a == newtable_e || b == nil_e) && 
    (b == newtable_e || a == nil_e))
    return true;
  
  return false;
}


bool get_bool(expr* e){
  switch(e->type){
    case newtable_e: 
    case programfunc_e: 
    case libraryfunc_e:
      return true;
    case nil_e:   
      return false;
    case conststring_e: 
      return e->strConst != "";
    case constnum_e: 
    case arithexpr_e:    
      return e->numConst != 0;
    case boolexpr_e: 
    case constbool_e: 
      return e->boolConst; 
    
    case tableitem_e:
      assert(false);
    default:
      assert(false);
  }
}


