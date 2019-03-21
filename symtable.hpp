int scope = 0;

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

enum SymbolType { GLOBAL, LOCAL, FORMAL, USERFUNC, LIBFUNC };

typedef struct SymbolTableEntry {
  bool isActive;
  union {
    Variable *varVal;
    Function *funcVal;
  } value;
  enum SymbolType type;
} SymbolTableEntry;

// SymbolTableEntry** a;

void initialize();

void insert(int key);

SymbolTableEntry* lookUp(int key);
/*
Απενεργοποίηση (όχι διαγραφή) όλων
των συμβόλων ενός επιπέδου εμβέλειας
*/
int hide(int scope);
