#include <stdlib.h>
#include <string.h>
#include <iomanip>
#include <iostream>
#include <stack>
#include <string>
#include <vector>

#define HASH_MUL 65599

using namespace std;
typedef struct SymbolTableEntry SymbolTableEntry;

unsigned int scope;
bool return_flag;
stack<int> last_func;
unsigned int anonymous_count;

/* συναρτήσεις βιβλιοθήκης LIBFUNC
  συναρτήσεις προγράμματος USERFUNC
  global οι μεταβλητές GLOBAL
  τα τυπικά ορίσματα συναρτήσεων */
enum SymbolType { GLOBAL, LOCAL, FORMAL, USERFUNC, LIBFUNC };

typedef struct Variable {
  const char *name;
  unsigned int scope;
  unsigned int line;
} Variable;

typedef struct Function {
  const char *name;
  vector<SymbolTableEntry *> args;
  unsigned int scope;
  unsigned int line;
} Function;

typedef struct SymbolTableEntry {
  bool isActive;
  union {
    Variable *varVal;
    Function *funcVal;
  } value;
  enum SymbolType type;
  SymbolTableEntry *next;
  SymbolTableEntry *scope_next;
} SymbolTableEntry;

class SymTable {
 private:
  SymbolTableEntry **symbol_table;
  unsigned int size;
  vector<SymbolTableEntry *> scopes;  // scopes.at(i) push_back(entry)

  const char *get_name(SymbolTableEntry *entry) {
    switch (entry->type) {
      case USERFUNC:
      case LIBFUNC:
        return entry->value.funcVal->name;
      case GLOBAL:
      case LOCAL:
      case FORMAL:
        return entry->value.varVal->name;
    }
  }
  unsigned int get_scope(SymbolTableEntry *entry) {
    switch (entry->type) {
      case USERFUNC:
      case LIBFUNC:
        return entry->value.funcVal->scope;
      case GLOBAL:
      case LOCAL:
      case FORMAL:
        return entry->value.varVal->scope;
    }
  }
  unsigned int get_lineno(SymbolTableEntry *entry) {
    switch (entry->type) {
      case USERFUNC:
      case LIBFUNC:
        return entry->value.funcVal->line;
      case GLOBAL:
      case LOCAL:
      case FORMAL:
        return entry->value.varVal->line;
    }
  }
  char *enumtostring(SymbolType s) {
    switch (s) {
      case USERFUNC:
        return "user function";
      case LIBFUNC:
        return "library function";
      case GLOBAL:
        return "global var";
      case LOCAL:
        return "local var";
      case FORMAL:
        return "formal var";
    }
  }
  bool is_var(SymbolType symtyp) {
    switch (symtyp) {
      case USERFUNC:
      case LIBFUNC:
        return false;
      case GLOBAL:
      case LOCAL:
      case FORMAL:
        return true;
    }
  }

  void initialize() {
    // init scopes
    scope = 0;
    anonymous_count = 0;
    last_func.push(-1);
    return_flag = false;
    // insert all libfuncs
    insert("sin", 0, LIBFUNC);
    insert("cos", 0, LIBFUNC);
    insert("sqrt", 0, LIBFUNC);
    insert("print", 0, LIBFUNC);
    insert("input", 0, LIBFUNC);
    insert("typeof", 0, LIBFUNC);
    insert("argument", 0, LIBFUNC);
    insert("strtonum", 0, LIBFUNC);
    insert("objectcopy", 0, LIBFUNC);
    insert("totalarguments", 0, LIBFUNC);
    insert("objectmemberkeys", 0, LIBFUNC);
    insert("objecttotalmembers", 0, LIBFUNC);
    printf("\n\n");
  }

 public:
  SymTable(unsigned int s_size = 100) : size(s_size) {
    symbol_table = new SymbolTableEntry *[size];
    initialize();
  }

  unsigned int SymTable_hash(const char *name) {
    size_t ui;
    unsigned int uiHash = 0U;
    for (ui = 0U; name[ui] != '\0'; ui++) uiHash = uiHash * HASH_MUL + name[ui];
    return uiHash % size;
  }

  void insert_arg(SymbolTableEntry *newnode) {
    SymbolTableEntry *function = scopes.at(last_func.top());

    if (is_var(function->type)) printf("\n__________ERROR________\n");
    function->value.funcVal->args.push_back(newnode);
  }

  // enum SymbolType { GLOBAL, LOCAL, FORMAL, USERFUNC, LIBFUNC };
  void insert(const char *name, unsigned int lineno, SymbolType symtp) {
    int myscope = scope;
    SymbolTableEntry *newnode = new SymbolTableEntry();

    newnode->next = NULL;
    newnode->scope_next = NULL;
    newnode->isActive = true;

    printf("(inserting %s in scope: %d as %s)", name, scope,
           enumtostring(symtp));

    newnode->type = symtp;
    switch (symtp) {
      case LIBFUNC:
        myscope = 0;
      case USERFUNC:
        newnode->value.funcVal = new Function();
        newnode->value.funcVal->line = lineno;
        newnode->value.funcVal->scope = myscope;
        newnode->value.funcVal->name = strdup(name);
        break;
      case FORMAL:
        insert_arg(newnode);
      case GLOBAL:
      case LOCAL:
        newnode->value.varVal = new Variable();
        newnode->value.varVal->line = lineno;
        newnode->value.varVal->scope = myscope;
        newnode->value.varVal->name = strdup(name);
        break;
    }

    newnode->next = symbol_table[SymTable_hash(name)];
    symbol_table[SymTable_hash(name)] = newnode;

    if (myscope > scopes.size() + 1) {
      // error case
    } else if (myscope == scopes.size()) {  // add new level of scopes
      newnode->scope_next = NULL;
      scopes.resize(myscope + 1);
    } else {  // connect to current scope list
      newnode->scope_next = scopes.at(myscope);
    }
    scopes[myscope] = newnode;
  }

  /* local x; function f() calls this
   *  return -1: error libfunc/ redefinition
   *  return  0: need to be defined
   *  return  1: already declared refers to previous declaration
   */

  int lookUp_curscope(const char *name) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];

    for (; curr; curr = curr->next) {
      if (!curr->isActive || strcmp(name, get_name(curr))) continue;
      // print  libfunc error
      if (curr->type == LIBFUNC) return -1;

      if (scope == get_scope(curr)) {
        // name refers to previous declaration / no need to insert
        if (is_var(curr->type))
          return 1;  // var
        else
          return 2;  // func
      }
    }

    return 0;
  }
  SymbolTableEntry *find_node(char *name, SymbolType symtp) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];

    int func_scope = -1;
    if (is_var(symtp)) func_scope = last_func.top();

    for (; curr; curr = curr->next) {
      if (!curr->isActive || strcmp(name, get_name(curr))) continue;
      if (scope >= get_scope(curr) && (int)get_scope(curr) > func_scope){
          if(symtp == curr->type ) return curr;
          if(is_var(symtp) && is_var(curr->type)) return curr;
      }
      
    }
    return NULL;
  }

  /*  x()   x= 5 x=7
   *  return -1: error libfunc/ redefinition
   *  return  0: need to be defined
   *  return  1: already declared refers to previous declaration
   */
  int lookUp_allscope(const char *name, SymbolType symtp) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];
    int declared = 0;
    int func_scope = -1;
    if (is_var(symtp)) func_scope = last_func.top();

    for (; curr; curr = curr->next) {
      if (!curr->isActive || strcmp(name, get_name(curr))) continue;
      // print  libfunc error
      if (curr->type == LIBFUNC) return -1;

      // printf("[allscope:checking curr %s scope %d for %s scope %d funcscopr
      // %d || %d k %d]",
      //     get_name(curr),get_scope(curr),name,scope,func_scope,scope >=
      //     get_scope(curr),(int)get_scope(curr) > func_scope);
      if ((scope >= get_scope(curr) && (int)get_scope(curr) > func_scope) ||
      get_scope(curr) == 0) {
        if (is_var(symtp)) {
          // name refers to previous declaration of var/ no need to insert
          if (is_var(curr->type)) {
            return 1;
          }  // var
          // name refers to previous declaration of func/ no need to insert
          else
            return 2;                            // func
        } else {                                 // if you a func is looked for
          if (is_var(curr->type)) declared = 1;  // var
          // name refers to previous declaration of func/ no need to insert
          else
            return 2;  // func
        }
      }
    }

    return declared;
  }
  int hide(int scope) {
    if (scope >= scopes.size()) return -1;
    SymbolTableEntry *curr = scopes.at(scope);
    const char *curr_name = get_name(curr);
    while (curr) {
      curr->isActive = false;
      curr = curr->scope_next;
    }
    return 0;
  }

  void print() {
    SymbolTableEntry *curr;
    unsigned int max_name = 0;
    unsigned int max_lineno = 0;

    for (int i = 0; i < scopes.size(); i++) {
      curr = scopes.at(i);
      while (curr != NULL) {
        if (strlen(get_name(curr)) > max_name)
          max_name = strlen(get_name(curr));
        curr = curr->scope_next;
      }
    }

    for (int i = 0; i < scopes.size(); i++) {
      curr = scopes.at(i);
      printf("\n%s Scope %d %s\n", string((max_name + 50) / 2, '-').c_str(), i,
             string((max_name + 50) / 2, '-').c_str());

      while (curr != NULL) {
        cout << setw(max_name - strlen(get_name(curr)) + 2) << "\""
             << get_name(curr) << "\""
             << setw(16 - strlen(enumtostring(curr->type)) + 3) << "["
             << enumtostring(curr->type) << "]  ";
        printf("(lineno:%3d) (scope:%2d) (active:%d)", get_lineno(curr),
               get_scope(curr), curr->isActive);
        if (curr->type == USERFUNC) {
          printf("[args:");
          for (unsigned i = 0; i < curr->value.funcVal->args.size(); i++)
            printf("'%s'", get_name(curr->value.funcVal->args.at(i)));

          printf("]");
        }
        printf("\n");

        curr = curr->scope_next;
      }
    }
  }
};
