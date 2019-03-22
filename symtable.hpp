#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

using namespace std;

extern int scope;

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

void initialize();
void insert(string name, unsigned int scope, unsigned int lineno,
            SymbolType symtp);
SymbolTableEntry *lookUp(int key);
/*
Απενεργοποίηση (όχι διαγραφή) όλων
των συμβόλων ενός επιπέδου εμβέλειας
*/
int hide(int scope);

class SymTable {
 private:
  SymbolTableEntry **symbol_table;
  unsigned int buckets;
  unsigned int size;
  vector<SymbolTableEntry *> scopes;  // scopes.at(i) push_back(entry)

  char *get_name(SymbolTableEntry *entry) {
    switch (entry->type) {
      case USERFUNC:
      case LIBFUNC:
        return newnode->value.funcVal->name;
      case GLOBAL:
      case LOCAL:
      case FORMAL:
        return newnode->value.varVal->name;
    }
  }
  char *get_scope(SymbolTableEntry *entry) {
    switch (entry->type) {
      case USERFUNC:
      case LIBFUNC:
        return newnode->value.funcVal->scope;
      case GLOBAL:
      case LOCAL:
      case FORMAL:
        return newnode->value.varVal->scope;
    }
  }

 public:
  SymTable() { initialize(); }

  void initialize() {
    // insert all libfuncs
  }

  static void expand() {
    struct binding *contents, *cur, *next;
    int i;
    if (buckets == MAX_HASH || size < buckets) return;

    contents = NULL; /* transfer all bindings in a list*/
    for (i = 0; i < buckets; i++) {
      cur = table[i];
      while (cur) {
        next = cur->next;
        cur->next = contents;
        contents = cur;
        cur = next;
      }
      table[i] = NULL;
    }

    /*define the next size*/
    buckets = getNextSize(buckets);
    table = NULL;  // FIXME:
                   // realloc(table, buckets *
                   // sizeof(struct binding));
    // assert(table);

    /*add again the bindings*/
    cur = contents;
    while (cur) {
      next = cur->next;
      i = SymTable_hash(cur->key, buckets);
      cur->next = table[i];
      table[i] = cur;
      cur = next;
    }
  }

  static unsigned int SymTable_hash(const char *pcKey) {
    size_t ui;
    unsigned int uiHash = 0U;
    for (ui = 0U; pcKey[ui] != '\0'; ui++)
      uiHash = uiHash * HASH_MULTIPLIER + pcKey[ui];
    return uiHash % buckets;
  }

  // enum SymbolType { GLOBAL, LOCAL, FORMAL, USERFUNC, LIBFUNC };
  void insert(const char *name, unsigned int lineno, SymbolType symtp) {
    char *newkey;
    int myscope = scope;
    SymbolTableEntry *newnode = new SymbolTableEntry();
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

    newnode->next = SymbolTableEntry[SymTable_hash(name)];
    SymbolTableEntry[SymTable_hash(name)] = newnode;

    if (myscope > scopes.size() + 1) {
      // error
    }else if(myscope == scopes.size(){
      newnode->scope_next = NULL;
    }else{
      newnode->scope_next = scopes.at(myscope);
    }
    scopes.insert(myscope ,newnnode);

    size++;

    expand();
    return 1;
  }

  SymbolTableEntry *lookUp(const char *name) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];
    char *curr_name = get_name(curr);
    while (curr) {
      if (strcmp(curr_name, name) == 0) return curr;
      curr = curr->next;
    }
    return NULL;
  }

  int hide(int scope) {
    SymbolTableEntry *curr = symbol_table[SymTable_hash(name)];
    // char* curr_name = get_name(curr);
    // while (curr) {
    //   if (strcmp(curr_name, name) == 0) return 1;
    //   curr = curr->next;
    // }
    // return NULL;
    return 0;
  }
}
