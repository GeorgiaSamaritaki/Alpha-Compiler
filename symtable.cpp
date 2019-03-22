#include "symtable.hpp"

#define HASH_MULTIPLIER 65599
#define MAX_HASH 65521


int scope;


typedef struct SymTable_S *SymTable_T;
SymTable_T SymTable_new(void);

int SymTable_contains(SymTable_T oSymTable, const char *pcKey);



/*
    Hashtable with single linked list
*/
struct SymTable_S {
  struct binding **table;
  unsigned int buckets;
  unsigned int size;
};
struct binding {
  char *key;
  void *value;
  struct binding *next;
};

static int getNextSize(int n) {
  switch (n) {
    case 509:
      return 1021;
    case 1021:
      return 2053;
    case 2053:
      return 4093;
    case 4093:
      return 8191;
    case 8191:
      return 16381;
    case 16381:
      return 32771;
    case 32771:
      return MAX_HASH;
  }
  return 0;
}
/* Return a hash code for pcKey. */




SymTable_T SymTable_new(void) {
  SymTable_T new_node = NULL;  // FIXME:
  // malloc(sizeof(SymTable_T *));
  // assert(new_node);
  new_node->table = NULL;  // FIXME:
  // malloc(509 * sizeof(struct binding *));
  new_node->size = 0;
  new_node->buckets = 509;
  return new_node;
}

unsigned int SymTable_getLength(SymTable_T oSymTable) {
  // assert(oSymTable);
  return oSymTable->size;
}

void SymTable_free(SymTable_T oSymTable) {
  struct binding *tmp, *next;
  int i;
  if (oSymTable) return;
  for (i = 0; i < oSymTable->buckets; i++) {
    tmp = oSymTable->table[i];
    while (tmp) {
      next = tmp->next;
      free(tmp->key);
      free(tmp);
      tmp = next;
    }
  }
  free(oSymTable->table);
}

int SymTable_remove(SymTable_T oSymTable, const char *pcKey) {
  struct binding *prev, *curr;
  // assert(oSymTable && pcKey);

  curr = oSymTable->table[SymTable_hash(pcKey, oSymTable->buckets)];
  prev = NULL;
  while (curr) {
    //   if(strcmp(curr->key,pcKey) == 0) break;
    prev = curr;
    curr = curr->next;
  }
  if (curr == NULL) return 0; /*was not found*/

  if (prev == NULL)
    oSymTable->table[SymTable_hash(pcKey, oSymTable->buckets)] = curr->next;
  else
    prev->next = curr->next;
  free(curr->key);
  free(curr);
  oSymTable->size--;
  return 1;
}

int SymTable_contains(SymTable_T oSymTable, const char *pcKey) {
  struct binding *curr;
  // assert(oSymTable && pcKey);

  curr = oSymTable->table[SymTable_hash(pcKey, oSymTable->buckets)];
  while (curr) {
    //   if (strcmp(curr->key, pcKey) == 0) return 1;
    curr = curr->next;
  }

  return 0;
}

void *SymTable_get(SymTable_T oSymTable, const char *pcKey) {
  struct binding *curr;
  // assert(oSymTable && pcKey);

  curr = oSymTable->table[SymTable_hash(pcKey, oSymTable->buckets)];
  while (curr) {
    //   if (strcmp(curr->key, pcKey) == 0) return curr->value;
    curr = curr->next;
  }

  return NULL;
}
