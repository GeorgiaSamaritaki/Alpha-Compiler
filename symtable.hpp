#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

using namespace std;

unsigned int scope;
unsigned int func_scope;

typedef struct Variable {
  const char *name;
  unsigned int scope;
  unsigned int line;
} Variable;

typedef struct Function {
  const char *name;
  // List of arguments
  unsigned int scope;
  unsigned int line;
} Function;

/* συναρτήσεις βιβλιοθήκης LIBFUNC
  συναρτήσεις προγράμματος USERFUNC
  global οι μεταβλητές GLOBAL
  τα τυπικά ορίσματα συναρτήσεων */
enum SymbolType { GLOBAL, LOCAL, FORMAL, USERFUNC, LIBFUNC };

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
  unsigned int buckets;
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

 public:
  SymTable() { initialize(); }

  void initialize() {
    // init scopes
    // insert all libfuncs
  }

  static unsigned int SymTable_hash(const char *pcKey) {
    // size_t ui;
    // unsigned int uiHash = 0U;
    // for (ui = 0U; pcKey[ui] != '\0'; ui++)
    //   uiHash = uiHash * HASH_MULTIPLIER + pcKey[ui];
    // return uiHash % buckets;
    return 0;
  }

  // enum SymbolType { GLOBAL, LOCAL, FORMAL, USERFUNC, LIBFUNC };
  void insert(const char *name, unsigned int lineno, SymbolType symtp) {
    char *newkey;
    int myscope = scope;
    SymbolTableEntry *newnode = new SymbolTableEntry();
    newnode->next = NULL;
    // assert(oSymTable && pcKey);

    strcpy(newkey, name);
    newnode->type = symtp;
    switch (symtp) {
      case LIBFUNC:
        myscope = 0;
      case USERFUNC:
        newnode->value.funcVal->line = lineno;
        newnode->value.funcVal->scope = myscope;
        newnode->value.funcVal->name = name;
        break;
      case GLOBAL:
        myscope = 0;
      case LOCAL:
      case FORMAL:
        newnode->value.varVal->line = lineno;
        newnode->value.varVal->scope = myscope;
        newnode->value.varVal->name = name;
        break;
    }

    newnode->next = symbol_table[SymTable_hash(name)];
    symbol_table[SymTable_hash(name)] = newnode;

    if (myscope > scopes.size() + 1) {
      // error case
    }else if(myscope == scopes.size()){ //add new level of scopes
      newnode->scope_next = NULL;
    }else{ //connect to current scope list
      newnode->scope_next = scopes.at(myscope);
    }
    scopes.insert( scopes.begin() + myscope ,newnode);

    size++;

  }

  /* local x; function f() calls this
   *  return -1: error libfunc/ redefinition
   *  return  0: need to be defined
   *  return  1: already declared refers to previous declaration
   */

  int lookUp_curscope(const char *name, SymbolType symtp) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];
    int declared = 0;

    for (; curr; curr = curr->next) {
      if (!curr->isActive) continue;

      // print  libfunc error
      if (curr->type = LIBFUNC) return -1;

      if (scope == get_scope(curr)) {
        // name refers to previous declaration / no need to insert
        if (is_var(symtp)) declared = 1;
        // print error redefinition
        else
          return -1;
      }
    }

    return declared;
  }
  //  x 
  int lookUp_allscope(const char *name, SymbolType symtp) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];
    int declared = 0;
    unsigned int min_scope = scope - func_scope + 1;

    for (; curr; curr = curr->next) {
      if (!curr->isActive || name != get_name(curr)) continue;

      // print  libfunc error
      if (curr->type = LIBFUNC) return -1;

      if(scope <= get_scope(curr) && scope >= min_scope ){
        if (is_var(symtp)) 
          declared = 1;
        
        // name refers to previous declaration of func/ no need to insert
        else
          declared = 1;
      }
    }

    return declared;
  }

  int hide(int scope) {
    if (scope > scopes.size()) return -1;
    SymbolTableEntry *curr = scopes.at(scope);
    const char *curr_name = get_name(curr);
    while (curr) {
      curr->isActive = false;
      curr = curr->scope_next;
    }
    return 0;
  }
};

