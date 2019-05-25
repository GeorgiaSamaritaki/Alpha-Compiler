#include "avm_utility.hpp"
#include <math.h>

#ifndef avm_library_funcs
#define avm_library_funcs

library_func_t libraryFuncz[12];

/*Library Functions - Start*/
void libfunc_print() {
  cout<<endl<<"Output:"<<endl;
  unsigned n = avm_totalActuals();
  for (unsigned i = 0; i < n; ++i) {
    cout << avm_toString(avm_getActual(i));
  }
  cout<<endl<<"output_end"<<endl;
}

void libfunc_typeof(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("one argument expected in 'typeof' (not %d) !", n);
  } else {
    avm_memcellClear(retval);
    retval->type = string_m;
    retval->data.strVal = strdup(typeStrings[avm_getActual(0)->type]);
  }
}

void libfunc_sin(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    executionFinished = true;
    avm_error("one argument expected in 'sin' (not %d) !", n);
  } else {
    if (avm_getActual(0)->type != number_m) {
      avm_error("Sin expects number!", n);
    } else {
      avm_memcellClear(retval);
      retval->type = number_m;
      retval->data.numVal = sin(avm_getActual(0)->data.numVal);
    }
  }
}

void libfunc_cos(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("one argument expected in 'cos' (not %d) !", n);
  } else {
    if (avm_getActual(0)->type != number_m) {
      avm_error("Cos expects number!", n);
    } else {
      avm_memcellClear(retval);
      retval->type = number_m;
      retval->data.numVal = cos(avm_getActual(0)->data.numVal);
    }
  }
}

void libfunc_sqrt(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("one argument expected in 'sqrt' (not %d) !", n);
  } else {
    if (avm_getActual(0)->type != number_m) {
      avm_error("Sqrt expects number!", n);
    } else {
      avm_memcellClear(retval);
      retval->type = number_m;
      double tmp = sqrt(avm_getActual(0)->data.numVal);
      if (tmp < 0)
        retval->type = nil_m;
      else
        retval->data.numVal = tmp;
    }
  }
}

bool is_number(const string& s) {
  std::string::const_iterator it = s.begin();
  while (it != s.end() && std::isdigit(*it)) ++it;
  return !s.empty() && it == s.end();
}

string toLower(string s) {
  const char* ss = strdup(s.c_str());
  string news = "";
  for (int i = 0; i < s.length(); i++) {
    news += tolower(ss[i]);
  }
  return news;
}

void libfunc_input(void) {
  string s;
  cin >> s;
  string sl = toLower(s);
  char autakia = '\"';

  avm_memcellClear(retval);
  if (is_number(s)) {
    retval->type = number_m;
    retval->data.numVal = atof(s.c_str());
  } else if (sl.compare("true")) {
    retval->type = bool_m;
    retval->data.boolVal = true;
  } else if (sl.compare("false")) {
    retval->type = bool_m;
    retval->data.boolVal = false;
  } else if (sl.compare("nil")) {
    retval->type = nil_m;
  } else {
    retval->type = string_m;
    if (sl[0] == autakia && sl[s.length() - 1] == autakia) {
      retval->data.strVal = strdup(s.substr(1, s.length() - 2).c_str());
    } else {
      retval->data.strVal = strdup(s.c_str());
    }
  }
}

void libfunc_argument(void) {
  unsigned p_topsp = avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("error in arguement0");
  } else if (avm_getActual(0)->type != number_m) {
    avm_error("error in arguement1");
  } else if (!p_topsp) {
    avm_memcellClear(retval);
    avm_error("error in arguement2");
    retval->type = nil_m;
  } else if (avm_getActual(0)->data.numVal >
                 avm_get_envvalue(p_topsp + AVM_NUMACTUALS_OFFSET) ||
             avm_getActual(0)->data.numVal < 0) {
    avm_error("error in arguement3");
  } else {
    avm_memcellClear(retval);
    int offset = (int)avm_getActual(0)->data.numVal;
    retval = &stack_m[p_topsp + AVM_NUMACTUALS_OFFSET + offset];
  }
}

void libfunc_strtonum(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("Strtonum expects one parameter");
  } else if (avm_getActual(0)->type != string_m) {
    avm_error("String expected");
  } else {
    avm_memcellClear(retval);
    retval->type = number_m;
    retval->data.numVal = atof(avm_getActual(0)->data.strVal);
  }
}
avm_table* copy_table(avm_table* tocopy) {
  avm_table* new_one = avm_tablenew();
  avm_table_bucket** p = tocopy->strIndexed;
  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) {
    for (avm_table_bucket* b = p[i]; b;) {
      avm_table_bucket* node = b;
      b = b->next;
      avm_tablesetelem(new_one,&node->key,&node->value);
    }
  }
  p = tocopy->numIndexed;
  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) {
    for (avm_table_bucket* b = p[i]; b;) {
      avm_table_bucket* node = b;
      b = b->next;
      avm_tablesetelem(new_one,&node->key,&node->value);
    }
  }
  return new_one;
}

void libfunc_objectcopy(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("ObjectCopy expects one parameter");
  } else if (avm_getActual(0)->type != table_m) {
    avm_error("Table expected");
  } else {
    avm_memcellClear(retval);
    retval->type = table_m;
    retval->data.tableVal = copy_table(avm_getActual(0)->data.tableVal);
  }
}
avm_table* getindexes(avm_table* derived) {
  avm_table* new_one = avm_tablenew();
  avm_table_bucket** p = derived->strIndexed;
  avm_memcell new_index; 
  new_index.type = number_m;
  new_index.data.numVal = 0;

  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) {
    for (avm_table_bucket* b = p[i]; b;) {
      avm_table_bucket* node = b;
      b = b->next;
      avm_tablesetelem(new_one, &new_index, &node->key);
      new_index.data.numVal ++;
    }
  }
  p = derived->numIndexed;
  for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i) {
    for (avm_table_bucket* b = p[i]; b;) {
      avm_table_bucket* node = b;
      b = b->next;
      avm_tablesetelem(new_one, &new_index, &node->key);
      new_index.data.numVal ++;
    }
  }
  return new_one;
}

void libfunc_objectmemberkeys(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("ObjectMemberKeys expects one parameter");
  } else if (avm_getActual(0)->type != table_m) {
    avm_error("Table expected");
  } else {
    avm_memcellClear(retval);
    retval->type = table_m;
    retval->data.tableVal = getindexes(avm_getActual(0)->data.tableVal);
  }
}
void libfunc_objecttotalmembers(void) {
  unsigned n = avm_totalActuals();

  if (n != 1) {
    avm_error("ObjectTotalMembers expects one parameter");
  } else if (avm_getActual(0)->type != table_m) {
    avm_error("Table expected");
  } else {
    avm_memcellClear(retval);
    retval->type = number_m;
    retval->data.numVal = avm_getActual(0)->data.tableVal->total;
  }
}

void libfunc_totalarguments() {
  unsigned p_topsp = avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);
  avm_memcellClear(retval);

  if (!p_topsp) {
    avm_error("'totalargument' called outside a function!");
    retval->type = nil_m;
  } else {
    retval->type = number_m;
    retval->data.numVal = avm_get_envvalue(p_topsp + AVM_NUMACTUALS_OFFSET);
  }
}

unsigned libfuncs_newUsed(char* s) {
  /*Check if we use its already*/
  for (int i = 0; i < namedLibFuncsRead.size(); i++) {
    if (!strcmp(namedLibFuncsRead[i], s)) {
      /*Found it -> return the index*/
      return i;
    }
  }
  /*New lib func is used*/
  char* dup = strdup(s);
  namedLibFuncsRead.push_back(dup);
  return namedLibFuncsRead.size() - 1;
}

void registerLibFunc(char* id, library_func_t addr) {
  int lib_index =
      libfuncs_newUsed(id);  // retrieve index from the lib funcs array
  libraryFuncz[lib_index] = addr;
}

string toString(vmopcode op) {
  switch (op) {
    case assign_v:
      return "assign";
    case add_v:
      return "add";
    case sub_v:
      return "sub";
    case mul_v:
      return "mul";
    case div_v:
      return "div";
    case mod_v:
      return "mod";
    case uminus_v:
      return "uminus";
    case and_v:
      return "and";
    case or_v:
      return "or";
    case not_v:
      return "not";
    case jeq_v:
      return "jeq";
    case jne_v:
      return "jne";
    case jle_v:
      return "jle";
    case jge_v:
      return "jge";
    case jlt_v:
      return "jlt";
    case jgt_v:
      return "jgt";
    case call_v:
      return "call";
    case pusharg_v:
      return "pusharg";
    case funcenter_v:
      return "funcenter";
    case funcexit_v:
      return "funcexit";
    case newtable_v:
      return "newtable";
    case tablegetelem_v:
      return "tablegetelem";
    case tablesetelem_v:
      return "tablesetelem";
    case jump_v:
      return "jump";
    case nop_v:
      return "nop";
    default:
      assert(false);
  }
}


void init_libfuncs(){
  registerLibFunc("objecttotalmembers", libfunc_objecttotalmembers);
  registerLibFunc("objectmemberkeys", libfunc_objectmemberkeys);
  registerLibFunc("objectcopy", libfunc_objectcopy);
  registerLibFunc("strtonum", libfunc_strtonum);
  registerLibFunc("argument", libfunc_argument);
  registerLibFunc("typeof", libfunc_typeof);
  registerLibFunc("input", libfunc_input);
  registerLibFunc("print",  libfunc_print);
  registerLibFunc("sqrt", libfunc_sqrt);
  registerLibFunc("cos", libfunc_cos);
  registerLibFunc("sin", libfunc_sin);
}

#endif