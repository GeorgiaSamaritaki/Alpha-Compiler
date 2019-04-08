#include <vector>

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
  unsigned line_no;
};

vector<quad*> quads;
unsigned total = 0;
unsigned int currQuad = 0;

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
