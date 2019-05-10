#include <stdarg.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "symtable.hpp"

#include <errno.h>
#include <sys/stat.h>

// #define EXPAND_SIZE 1024
// #define CURR_SIZE (total * sizeof(quad))
// #define NEW_SIZE (EXPAND_SIZE * sizeof(quad) + CURR_SIZE)
typedef struct expr expr;
typedef struct quad quad;

void debug_quad(quad* q, int i = 0);

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
} iopcode;

string iop_tostr(iopcode iop) {
  switch (iop) {
    case assign_op:
      return "assign_op";
    case add:
      return "add";
    case sub:
      return "sub";
    case mul_op:
      return "mul_op";
    case div_op:
      return "div_op";
    case mod_op:
      return "mod_op";
    case uminus_op:
      return "uminus_op";
    case and_op:
      return "and_op";
    case or_op:
      return "or_op";
    case not_op:
      return "not_op";
    case if_eq:
      return "if_eq";
    case if_noteq:
      return "if_noteq";
    case if_lesseq:
      return "if_lesseq";
    case if_greater_eq:
      return "if_greater_eq";
    case if_less:
      return "if_less";
    case if_greater:
      return "if_greater";
    case call:
      return "call";
    case param:
      return "param";
    case ret:
      return "ret";
    case getretval:
      return "getretval";
    case funcstart:
      return "funcstart";
    case funcend:
      return "funcend";
    case tablecreate:
      return "tablecreate";
    case tablegetelem:
      return "tablegetelem";
    case tablesetelem:
      return "tablesetelem";
    case jump:
      return "jump";
    default:
      assert(0);
  }
}

struct quad {
  iopcode iop;
  expr* result;
  expr* arg1;
  expr* arg2;
  int label;
  unsigned line;
};

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

string expr_t_tostr(expr_t type) {
  switch (type) {
    case var_e:
      return "var_e";
    case tableitem_e:
      return "tableitem_e";
    case programfunc_e:
      return "programfunc_e";
    case libraryfunc_e:
      return "libraryfunc_e";
    case arithexpr_e:
      return "arithexpr_e";
    case boolexpr_e:
      return "boolexpr_e";
    case assignexpr_e:
      return "assignexpr_e";
    case newtable_e:
      return "newtable_e";
    case constnum_e:
      return "constnum_e";
    case constbool_e:
      return "constbool_e";
    case conststring_e:
      return "conststring_e";
    case nil_e:
      return "nil_e,";
    default:
      assert(0);
  }
}

typedef struct expr {
  expr_t type;
  SymbolTableEntry* sym;
  struct expr* index;
  double numConst;
  char* strConst;
  unsigned char boolConst;
  struct expr* next;
  vector<unsigned int> truelist;
  vector<unsigned int> falselist;
} expr;

struct X {
  expr* e;
  int label;
};

struct call_l {
  expr* elist;
  bool method;
  char* name;
};

struct stmt_l {
  vector<unsigned int> breaklist;
  vector<unsigned int> contlist;
};

struct for_prefix {
  int test;
  int enter;
};

expr* newExpr(expr_t t);
vector<quad*> quads;
expr* nil_expr = newExpr(nil_e);
int tmpcounter = 0;
int loopcnt = 0;
stack<int> loopcntStack;

SymTable symbol_table = *new SymTable();

// // Functions
// quad* expand() {
//   assert(currQuad == total);
//   quad* p = new quad();
//   quads.push_back(p);
//   total += EXPAND_SIZE;
//   return p;
// }
void emit(iopcode iop, expr* arg1 = NULL, expr* arg2 = NULL,
          expr* result = NULL, int label = -1) {
  quad* p;
  // if (currQuad == total)
  //   p = expand();
  // else{
  //   p = new quad();
  //   quads.at(currQuad) = p;
  // }
  // currQuad++;
  p = new quad();

  p->iop = iop;
  p->arg1 = arg1 == NULL ? nil_expr : arg1;
  p->arg2 = arg2 == NULL ? nil_expr : arg2;
  p->result = result == NULL ? nil_expr : result;
  p->label = label;
  debug_quad(p);
  quads.push_back(p);
}
void emit_function(iopcode iop, expr* result) { emit(iop, NULL, NULL, result); }

void patchLabel(unsigned int quadNo, int label) {
  assert(quadNo < quads.size());
  assert(label != -1);
  // currQuad);
  quads[quadNo]->label = (unsigned int)label;
}

void patchLabel(vector<unsigned int> quads_c, int label) {
  for (int i = 0; i < quads_c.size(); i++) {
    printf(" %d", quads_c[i]);
    patchLabel(quads_c[i], label);
  }
  printf(" \n");
}

char* new_tmpname() {
  char name[1000];
  sprintf(name, "$t%d", tmpcounter);
  // printf("tmp: $t%d\n", tmpcounter);
  tmpcounter++;
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

unsigned int nextQuadLabel() { return quads.size(); }  // currQuad;}

expr* lvalue_expr(SymbolTableEntry* entry) {
  assert(entry);
  expr* new_expr = new expr();

  new_expr->sym = entry;
  new_expr->next = (expr*)0;

  switch (entry->type) {
    case FORMAL: {
    }
    case LOCAL: {
    }
    case GLOBAL: {
      new_expr->type = var_e;
      break;
    }
    case LIBFUNC: {
      new_expr->type = libraryfunc_e;
      break;
    }
    case USERFUNC: {
      new_expr->type = programfunc_e;
      break;
    }
    default:
      assert(0);
  }

  return new_expr;
}

expr* newExpr(expr_t t) {
  expr* new_expr = new expr();
  new_expr->type = t;
  return new_expr;
}

expr* newExpr_constString(char* s) {
  expr* e = newExpr(conststring_e);
  e->strConst = strdup(s);
  return e;
}
expr* newExpr_constNum(double i) {
  expr* e = newExpr(constnum_e);
  e->numConst = i;
  return e;
}
expr* newExpr_constBool(bool b) {
  expr* e = newExpr(constbool_e);
  e->boolConst = b;
  return e;
}

expr* emit_ifTableItem(expr* e) {  // FIXME:
  if (e->type != tableitem_e)
    return e;
  else {
    expr* result = newExpr(var_e);
    result->sym = new_tmp(symbol_table.get_lineno(e->sym));
    emit(tablegetelem, e, e->index, result);
    return result;
  }
}
// function to create new expression/quad ??

expr* member_item(expr* lvalue, char* name) {
  lvalue = emit_ifTableItem(lvalue);
  expr* item = newExpr(tableitem_e);
  item->sym = lvalue->sym;
  item->index = newExpr_constString(name);
  return item;
}

expr* make_call(expr* lvalue, expr* elist) {
  expr* func = emit_ifTableItem(lvalue);
  assert(func);
  // assert(!symbol_table.is_var(func->sym->type));
  expr* curr = elist;
  while (curr != NULL && curr != nil_expr) {
    emit(param, curr, NULL, NULL);
    curr = curr->next;
  }
  emit(call, func, NULL, NULL);
  expr* result = newExpr(var_e);
  assert(lvalue->sym);  
  result->sym = new_tmp(lvalue->sym->value.funcVal->line);
  emit(getretval, NULL, NULL, result);
  return result;
}

void comperror(char* format, ...) {
  va_list args;
  va_start(args, format);
  printf(format, args);
  va_end(args);
}

void checkUminus(expr* e) {
  if (e->type == constbool_e || e->type == conststring_e || e->type == nil_e ||
      e->type == newtable_e || e->type == programfunc_e ||
      e->type == libraryfunc_e || e->type == boolexpr_e)
    comperror("Illegal expr to unary -");
}

double compute(iopcode op, double a, double b) {
  switch (op) {
    case add:
      return a + b;
    case sub:
      return a - b;
    case mul_op:
      return a * b;
    case div_op:
      return b ? a / b : 0;
    case mod_op:
      return ((int)a) % ((int)b);
    default:
      assert(false);
  }
}

bool compute_rel(iopcode op, double a, double b) {
  switch (op) {
    case if_greater:
      return a > b;
    case if_less:
      return a < b;
    case if_greater_eq:
      return a >= b;
    case if_lesseq:
      return a <= b;
    case if_eq:
      return a == b;
    case if_noteq:
      return a != b;
    default:
      assert(false);
  }
}

bool compute(iopcode op, bool a, bool b) {
  switch (op) {
    case and_op:
      return a && b;
    case or_op:
      return a || b;
    case if_eq:
      return a == b;
    case if_noteq:
      return a != b;
    default:
      assert(false);
  }
}

bool is_same(expr_t a, expr_t b) {
  if (a == var_e || b == var_e) return true;
  if (a == b) return true;
  if ((a == arithexpr_e || a == constnum_e) &&
      (b == arithexpr_e || b == constnum_e))
    return true;
  if ((a == constbool_e || a == boolexpr_e) &&
      (b == constbool_e || b == boolexpr_e))
    return true;
  if ((a == newtable_e || b == nil_e) && (b == newtable_e || a == nil_e))
    return true;
  if (a == nil_e || b == nil_e) return true;

  return false;
}

bool isvalid_arithmeticCheck(expr_t a, expr_t b) {
  printf("isvalid_arithmeticCheck: %s : %s\n", expr_t_tostr(a).c_str(),
         expr_t_tostr(b).c_str());
  if (a == programfunc_e || a == libraryfunc_e || b == programfunc_e ||
      b == libraryfunc_e) {
    return false;
  }
  if (a == nil_e || a == boolexpr_e || a == constbool_e || a == newtable_e ||
      a == conststring_e) {
    return false;
  }
  if (b == nil_e || b == boolexpr_e || b == constbool_e || b == newtable_e ||
      b == conststring_e) {
    return false;
  }

  return true;
}

bool get_bool(expr* e) {
  switch (e->type) {
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

string get_string(expr* e, int index = 0) {
  if (e == NULL) return " ";
  switch (e->type) {
    case constbool_e:
      return e->boolConst ? "true" : "false";
    case constnum_e: {
      std::ostringstream ss;
      ss << e->numConst;
      return ss.str();
    }
    case conststring_e:
      return strdup(e->strConst);
    case nil_e:
      return " ";
    case tableitem_e:
    case newtable_e:
    case var_e:
    case programfunc_e:
    case libraryfunc_e:
    case arithexpr_e:
    case boolexpr_e:
    case assignexpr_e:
      return symbol_table.get_name(e->sym);
  }
}

void change_type(expr* lvalue, expr* expr) {
  // printf("~~~~~~~~Changed %s %s type %s\n", symbol_table.get_name(lvalue->sym),
  //        expr_t_tostr(lvalue->type).c_str(), expr_t_tostr(expr->type).c_str());
  assert(lvalue->type != libraryfunc_e);
  if (expr->type == constnum_e) {
    lvalue->numConst = expr->numConst;
    lvalue->type = arithexpr_e;
    if (lvalue->sym->type == USERFUNC) {
      Variable* v = new Variable();
      Function* f = lvalue->sym->value.funcVal;
      v->scope = f->scope;
      v->line = f->line;
      f->totalLocals--;
      if (f->totalLocals == 0) {
        // delete args and delete function struct
      }
      lvalue->sym->value.varVal = v;
    }
  } else if (expr->type == constbool_e) {
    lvalue->boolConst = expr->boolConst;
    lvalue->type = boolexpr_e;
    if (lvalue->sym->type == USERFUNC) {
      Variable* v = new Variable();
      Function* f = lvalue->sym->value.funcVal;
      v->scope = f->scope;
      v->line = f->line;
      v->name = f->name;
      f->totalLocals--;
      if (f->totalLocals == 0) {
        // delete args and delete function struct
      }
      lvalue->sym->value.varVal = v;
    }
  } else if (expr->type == conststring_e) {
    lvalue->strConst = expr->strConst;
    if (lvalue->sym->type == USERFUNC) {
      Variable* v = new Variable();
      Function* f = lvalue->sym->value.funcVal;
      v->scope = f->scope;
      v->line = f->line;
      f->totalLocals--;
      if (f->totalLocals == 0) {
        // delete args and delete function struct
      }
      lvalue->sym->value.varVal = v;
    }
  } else if (expr->type == nil_e) {
    return;  // assignment of null expression

  } else {
    assert(expr->sym);
    lvalue->sym = expr->sym;
  }
  lvalue->type = expr->type;
  // probably delete expr node
}

void debug_quad(quad* q, int i) {
  printf("Quad %d", i);
  printf(" %s", iop_tostr(q->iop).c_str());
  printf(" %s", get_string(q->result).c_str());
  printf(" %s", get_string(q->arg1).c_str());
  printf(" %s", get_string(q->arg2).c_str());
  printf(" %d\n", (q->label));
}

void printQuads() {
  
  ofstream myfile;
  string s = "quads.txt";
  myfile.open(s.c_str());
  

  int sizes[6] = {7, 8, 8, 6, 6, 7};

  for (int i = 0; i < quads.size(); i++) {
    quad* q = quads.at(i);
    stringstream ss;
    ss << q->label;
    string label = (q->label == -1) ? "" : ss.str();

    if (sizes[5] < label.size() + 2) sizes[5] = label.size() + 2;
    ss << i;
    if (sizes[0] < ss.str().size() + 2) sizes[0] = ss.str().size() + 2;
    if (sizes[1] < iop_tostr(q->iop).size() + 2)
      sizes[1] = iop_tostr(q->iop).size() + 2;
    if (sizes[2] < get_string(q->result).size() + 2)
      sizes[2] = get_string(q->result).size() + 2;
    if (sizes[3] < get_string(q->arg1).size() + 2)
      sizes[3] = get_string(q->arg1).size() + 2;
    if (sizes[4] < get_string(q->arg2).size() + 2)
      sizes[4] = get_string(q->arg2).size() + 2;
  }
  int sum = 0;
  for (int i = 0; i < 6; i++) sum += sizes[i];

  myfile << "\n\n"
         << string(((sum - 7) / 2), '-') << " Quads "
         << string(((sum - 7) / 2)+(sum%2==0), '-') << "\n"
         << setw(sizes[0]) << "quad#" << setw(sizes[1]) << "opcode"
         << setw(sizes[2]) << "result" << setw(sizes[3]) << "arg1"
         << setw(sizes[4]) << "arg2" << setw(sizes[5]) << "label" << endl
         << string(sum, '-') << endl;

  for (int i = 0; i < quads.size(); i++) {
    quad* q = quads.at(i);
    stringstream ss;
    ss << q->label;
    string label = (q->label == -1) ? "" : ss.str();
    myfile << setw(sizes[0]) << i << setw(sizes[1]) << iop_tostr(q->iop)
           << setw(sizes[2]) << get_string(q->result) << setw(sizes[3])
           << get_string(q->arg1) << setw(sizes[4]) << get_string(q->arg2)
           << setw(sizes[5]) << label << endl;
  }

  myfile.close();
}

vector<unsigned int> newList(unsigned int a) {
  vector<unsigned int> b;
  b.push_back(a);
  return b;
}

vector<unsigned int> merge(vector<unsigned int> a, vector<unsigned int> b) {
  vector<unsigned int> c;
  // printf("ton pairno\n");
  // printf("A:");
  // for (int i = 0; i < a.size(); i++) {
  //   printf("%d ", a[i]);
  // }
  // printf("\nB:");
  // for (int i = 0; i < b.size(); i++) {
  //   printf("%d ", b[i]);
  // }
  c.reserve(a.size() + b.size());  // preallocate memory
  c.insert(c.end(), a.begin(), a.end());
  c.insert(c.end(), b.begin(), b.end());
  for (int i = 0; i < c.size(); i++) {
    printf("%d ", c[i]);
  }
  printf("\n");
  return c;
}

stmt_l* merge(stmt_l* a, stmt_l* b) {
  stmt_l* c = new stmt_l();
  c->breaklist = merge((a != NULL ? (a->breaklist) : (c->breaklist)),
                       (b != NULL ? (b->breaklist) : (c->breaklist)));
  c->contlist = merge((a != NULL ? (a->contlist) : (c->contlist)),
                      (b != NULL ? (b->contlist) : (c->contlist)));
  return c;
}

void reverseUtil(expr* curr, expr* prev, expr** head) {
  if (!curr->next) {
    *head = curr;
    curr->next = prev;
    return;
  }
  expr* next = curr->next;
  curr->next = prev;
  reverseUtil(next, curr, head);
}

void reverse_list(expr** head) {
  if (!head) return;
  reverseUtil(*head, NULL, head);
}

// struct quad {)
//   iopcode iop;
//   expr* result;
//   expr* arg1;
//   expr* arg2;
//   unsigned label;
//   unsigned line;
// };

// struct quad {)
//