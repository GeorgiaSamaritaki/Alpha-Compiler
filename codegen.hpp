#include "quad.hpp"

#ifndef codegen
#define codegen
/*Structs*/
enum vmopcode {
  assign_v,
  add_v,
  sub_v,
  mul_v,
  div_v,
  mod_v,
  uminus_v,
  and_v,
  or_v,
  not_v,
  jeq_v,
  jne_v,
  jle_v,
  jge_v,
  jlt_v,
  jgt_v,
  call_v,
  pusharg_v,
  ret_v,
  getretval_v,
  funcenter_v,
  funcexit_v,
  newtable_v,
  tablegetelem_v,
  tablesetelem_v,
  jump_v,
  nop_v
};

enum vmarg_t {
  label_a = 0,
  global_a,
  formal_a,
  local_a,
  number_a,
  string_a,
  bool_a,
  nil_a,
  userfunc_a,
  libfunc_a,
  retval_a
};

typedef struct vmarg {
  vmarg_t type;
  unsigned val;
} vmarg;



typedef struct instruction {
  vmopcode opcode;
  vmarg* result;
  vmarg* arg1;
  vmarg* arg2;
  unsigned srcLine;
} instruction;

typedef struct userfunc {
  unsigned address;
  unsigned localSize;
  char* id;
  string toString(){
    stringstream ss; 
    string s = id;
    ss << s << " " << address << " " << localSize <<" ";
    return ss.str();
  }

} userfunc;

typedef struct stackNode {
  SymbolTableEntry* func;
  vector<unsigned> returnList;
} stackNode;

string toString(vmopcode op);
string toString(vmarg_t t);
void printInstruction(instruction* i);

vector<double> numConsts;
// unsigned totalNumConsts;
vector<char*> stringConsts;
// unsigned totalStringConsts;
vector<char*> namedLibFuncs;
// unsigned totalNamedLibFuncs;
vector<userfunc*> userFuncz;
// unsigned totalUserFuncz;

vmarg** globals;

stack<stackNode*> funcStack;

unsigned currentProcessedQuad() {
  /*FIXME*/
  return 0;
}

vector<instruction*> instructionz;

unsigned consts_newNumber(double n) {
   /*Check if we use its already*/
  for(int i = 0; i < numConsts.size(); i++)
    if( numConsts[i] == n )
      return i;

  numConsts.push_back(n);
  assert(!numConsts.empty());
  return numConsts.size() - 1;
}

unsigned consts_newString(char* s) {
  /*Check if we use its already*/
  for(int i = 0; i < stringConsts.size(); i++)
    if( !strcmp(stringConsts[i], s) )
      return i;

  char* dup = strdup(s);
  stringConsts.push_back(dup);
  assert(!stringConsts.empty());
  return stringConsts.size() - 1;
}

unsigned libfuncs_newUsed(char* s) {
  /*Check if we use its already*/
  for (int i = 0; i < namedLibFuncs.size(); i++) {
    if (!strcmp(namedLibFuncs[i], s)) {
      /*Found it -> return the index*/
      return i;
    }
  }
  /*New lib func is used*/
  char* dup = strdup(s);
  namedLibFuncs.push_back(dup);
  return namedLibFuncs.size() - 1;
}

unsigned userfuncs_newFunc(SymbolTableEntry* s) {
  for (int i = 0; i < userFuncz.size(); i++) {
    /*Check if we already use this function*/
    if (!strcmp(userFuncz[i]->id, (char*)s->value.funcVal->name)){
      cout<<"1 "<<userFuncz[i]->id<<" index: "<< i<<endl;
      return i;
    } 
  
  }
  /*New user func*/
  userfunc* f = new userfunc();
  f->id = strdup((char*)s->value.funcVal->name);
  f->localSize = s->value.funcVal->totalLocals;
  f->address = s->taddress;
  userFuncz.push_back(f); 
  cout<<"2 "<<f->id<<" index: "<< userFuncz.size()-1<<endl;
  return userFuncz.size()-1;
}

void emit_instr(instruction *t) {
  instruction* new_t = new instruction();
  new_t->opcode = t->opcode;
  cout<<"emit instr: "<<toString(t->opcode)<<endl;
  new_t->result = t->result;
  new_t->arg1 = t->arg1;
  new_t->arg2 = t->arg2;
  new_t->srcLine = t->srcLine;
  instructionz.push_back(new_t);
}

void make_operand(expr* e, vmarg* arg) {
  switch (e->type) {
    case var_e:
    case tableitem_e:
    case arithexpr_e:
    case boolexpr_e:
    case assignexpr_e:
    case newtable_e: {
      assert(e->sym);
      arg->val = e->sym->offset;
      switch (e->sym->space) {
        case program_var:
          arg->type = global_a;
          break;
        case function_local:
          arg->type = local_a;
          break;
        case formal_arg:
          arg->type = formal_a;
          break;
        default:
          assert(false);
      }
      break;
    }
    /*cosntants*/
    case constbool_e: {
      arg->val = e->boolConst;
      arg->type = bool_a;
      break;
    }
    case conststring_e: {
      arg->val = consts_newString(e->strConst);
      arg->type = string_a;
      break;
    }
    case constnum_e: {
      arg->val = consts_newNumber(e->numConst);
      arg->type = number_a;
      break;
    }
    case nil_e:{
      arg->type = nil_a;
      break;
    }
    /*Functions*/
    case programfunc_e: {
      arg->type = userfunc_a;
      // arg->val = e->sym->taddress;
      /* or alternatively*/
      arg->val = userfuncs_newFunc(e->sym);
      cout<< " val is " <<arg->val <<endl; 
      break;
    }
    case libraryfunc_e: {
      arg->type = libfunc_a;
      arg->val = libfuncs_newUsed((char*)e->sym->value.funcVal->name);
      break;
    }
    default:
      assert(0);
  }
}

void make_numberoperand(vmarg* arg, double val) {
  arg->val = consts_newNumber(val);
  arg->type = number_a;
}

void make_booloperand(vmarg* arg, unsigned val) {
  arg->val = val;
  arg->type = bool_a;
}

void make_retvaloperand(vmarg* arg) { arg->type = retval_a; }

typedef struct incomplete_jump {
  unsigned instrNo;
  unsigned iaddress;
} incomplete_jump;

unsigned ij_total = 0;
unsigned totalInstructions = 0;

unsigned nextInstructionLabel() { return instructionz.size(); }

vector<incomplete_jump*> incomplete_jumps;

void add_incomplete_jump(unsigned int instrNo, unsigned iaddress) {
  incomplete_jump* tmp =  new incomplete_jump();
  cout<<" need to patch instr "<<instrNo <<" to "<<iaddress<<endl;
  tmp->instrNo = instrNo;
  tmp->iaddress = iaddress;
  incomplete_jumps.push_back(tmp);
}

void patch_incomplete_jumps() {
  for (int i = 0; i < incomplete_jumps.size(); i++) {
    assert(incomplete_jumps[i]);
    if (incomplete_jumps[i]->iaddress == quads.size())
      instructionz[incomplete_jumps[i]->instrNo]->result->val =
          nextInstructionLabel();
    else{
    cout <<"patch instr: "<<incomplete_jumps[i]->instrNo<<" quad: "
        << incomplete_jumps[i]->iaddress<<endl;
    assert(incomplete_jumps[i]->iaddress< quads.size());
    assert(quads[incomplete_jumps[i]->iaddress]);
    assert(quads[incomplete_jumps[i]->iaddress]->taddress >=(unsigned)0);
      instructionz[incomplete_jumps[i]->instrNo]->result->val =
          quads[incomplete_jumps[i]->iaddress]->taddress;
    }
    delete(incomplete_jumps[i]);
  }
}

void clear_instruction(instruction* t,quad* quad) {
  t->arg2 = NULL;
  t->arg1 = NULL;
  t->result = NULL;
  switch (t->opcode) {
    case add_v:
    case sub_v:
    case mul_v:
    case div_v:
    case mod_v:
    case jeq_v:
    case jne_v:
    case jle_v:
    case jge_v:
    case jlt_v:
    case jgt_v:
    case tablesetelem_v:
    case tablegetelem_v:
      t->arg2 = new vmarg();
      make_operand(quad->arg2, t->arg2);
    case assign_v:
      t->result = new vmarg();
      make_operand(quad->result, t->result);
    case call_v:
    case pusharg_v:
      t->arg1 = new vmarg();
      make_operand(quad->arg1, t->arg1);
      break;
    case funcexit_v:
    case funcenter_v:
    case newtable_v:
    case jump_v:
      t->result = new vmarg();
      make_operand(quad->result, t->result);
      
      break;
    case uminus_v:
    case and_v:
    case or_v:
    case not_v:
    case ret_v:
    case nop_v:
    case getretval_v:
      assert(0);
  }
}

void generate(vmopcode op, quad* quad) {
  // cout << "generate op: " << toString(op) << " ";
  debug_quad(quad);
  instruction* t = new instruction();
  t->opcode = op;

  clear_instruction(t, quad);

  quad->taddress = nextInstructionLabel();
  t->srcLine = symbol_table.get_lineno(quad->result->sym);
  emit_instr(t);
}

void generate_relational(vmopcode op, quad* quad) {
  // cout << "generate_relational op: " << toString(op) << " ";
  instruction* t = new instruction();
  t->opcode = op;
  if( op != jump_v ){
    t->arg1 = new vmarg();
    t->arg2 = new vmarg();
    make_operand(quad->arg1, t->arg1);
    make_operand(quad->arg2, t->arg2);
  } else {
    t->arg1 = NULL;
    t->arg2 = NULL;
  }
  
  t->result = new vmarg();

  t->result->type = label_a;
  if (quad->label < currentProcessedQuad()) {
    t->result->val = quads[quad->label]->taddress;
  }else if(quad->label > quads.size()){
    assert(0);
  } else {
    cout<<"~~~~~~~~~~~~~~~~~~here"<<endl;
    add_incomplete_jump(nextInstructionLabel(), quad->label);
  }
  quad->taddress = nextInstructionLabel();
  if (quad->arg1->sym) {
    t->srcLine = symbol_table.get_lineno(quad->arg1->sym);
  } else {
    t->srcLine = 0;
  }
  emit_instr(t);
}

void generate_ASSIGN(quad* quad) { generate(assign_v, quad); }
void generate_ADD(quad* quad) { generate(add_v, quad); }
void generate_SUB(quad* quad) { generate(sub_v, quad); }
void generate_MUL(quad* quad) { generate(mul_v, quad); }
void generate_DIV(quad* quad) { generate(div_v, quad); }
void generate_MOD(quad* quad) { generate(mod_v, quad); }
void generate_UMINUS(quad* quad) {
  quad->iop = mul_op;
  quad->arg2 = newExpr(constnum_e);
  quad->arg2->numConst = -1;
  generate(mul_v, quad);
}
void generate_NEWTABLE(quad* quad) { generate(newtable_v, quad); }
void generate_TABLEGETELEM(quad* quad) { generate(tablegetelem_v, quad); }
void generate_TABLESETELEM(quad* quad) { generate(tablesetelem_v, quad); }
void generate_NOP(quad* quad = NULL) {
  instruction* t = new instruction();
  t->opcode = nop_v;
  emit_instr(t);
}
void generate_JUMP(quad* quad) { generate_relational(jump_v, quad); }
void generate_IF_EQ(quad* quad) { generate_relational(jeq_v, quad); }
void generate_IF_NOTEQ(quad* quad) { generate_relational(jne_v, quad); }
void generate_IF_GREATER(quad* quad) { generate_relational(jgt_v, quad); }
void generate_IF_GREATEREQ(quad* quad) { generate_relational(jge_v, quad); }
void generate_IF_LESS(quad* quad) { generate_relational(jlt_v, quad); }
void generate_IF_LESSEQ(quad* quad) { generate_relational(jle_v, quad); }

void generate_NOT(quad* quad){
  generate(assign_v, quad);
};
void generate_OR(quad* quad){
  assert(0);
};
void generate_AND(quad* quad){
  assert(0);
};

void generate_PARAM(quad* quad) {
  quad->taddress = nextInstructionLabel();
  instruction* t= new instruction();
  t->opcode = pusharg_v;
  t->arg1 = new vmarg();
  make_operand(quad->arg1, t->arg1);
  t->arg2 = NULL;
  t->result = NULL;
  if (quad->arg1->sym) {
    t->srcLine = symbol_table.get_lineno(quad->arg1->sym);
  } else {
    t->srcLine = 0;
  }
  emit_instr(t);
};
void generate_CALL(quad* quad) {
  quad->taddress = nextInstructionLabel();
  instruction* t= new instruction();
  t->opcode = call_v;
  t->arg1 = new vmarg();
  make_operand(quad->arg1, t->arg1);
  t->arg2 = NULL;
  t->result = NULL;
  t->srcLine = symbol_table.get_lineno(quad->arg1->sym);
  emit_instr(t);
};
void generate_GETRETVAL(quad* quad) {
  quad->taddress = nextInstructionLabel();
  instruction* t= new instruction();
  t->opcode = assign_v;
  t->arg1 = new vmarg();
  t->arg2 = NULL;
  t->result = new vmarg();
  make_operand(quad->result, t->result);
  make_retvaloperand(t->arg1);
  t->srcLine = symbol_table.get_lineno(quad->result->sym);

  emit_instr(t);
};

void generate_FUNCSTART(quad* quad) {
  // /*We need 2 emits*/
  // /*First emit jump instruction*/
  // instruction jump;
  // jump.opcode = jump_v;
  // jump.arg1 = NULL;
  // jump.arg2 = NULL;
  // jump.result = new vmarg();
  // jump.result->type = label_a;
  // jump.srcLine = symbol_table.getA_lineno(quad->result->sym);

  // emit_instr(jump);
  assert(quad->result->sym);
  quad->taddress = nextInstructionLabel();
  quad->result->sym->taddress = nextInstructionLabel();
  /*Then emit funcnter*/
  instruction* f_enter= new instruction();
  f_enter->opcode = funcenter_v;
  f_enter->arg1 = NULL;
  f_enter->arg2 = NULL;
  f_enter->result = new vmarg();
  make_operand(quad->result, f_enter->result);
  f_enter->result->val = userfuncs_newFunc(quad->result->sym);
  f_enter->srcLine = symbol_table.get_lineno(quad->result->sym);
  emit_instr(f_enter);

  // STACK -> push
  stackNode* node = new stackNode();
  node->func = quad->result->sym;
  funcStack.push(node);
};

void generate_RETURN(quad* quad) {
  /*Also 2 emits*/
  /*First emit assign*/
  quad->taddress = nextInstructionLabel();

  instruction* ass = new instruction();
  ass->opcode = assign_v;
  ass->arg1 = new vmarg();
  make_operand(quad->result, ass->arg1);
  ass->arg2 = NULL;
  ass->result = new vmarg();
  make_retvaloperand(ass->result);
  if (quad->arg1->sym || quad->arg2->sym || quad->result->sym) {
    ass->srcLine = symbol_table.get_lineno(quad->result->sym);
  } else {
    ass->srcLine = 0;
  }

  emit_instr(ass);

  // STACK -> FIX - top
  stackNode* f = funcStack.top();
  f->returnList.push_back(nextInstructionLabel());

  // /*Then emit jump*/
  // instruction jump;
  // jump.opcode = jump_v;
  // jump.arg1 = NULL;
  // jump.arg2 = NULL;
  // jump.result = new vmarg();
  // jump.result->type = label_a;
  // if (quad->arg1->sym || quad->arg2->sym || quad->result->sym) {
  //   jump.srcLine = symbol_table.get_lineno(quad->result->sym);
  // } else {
  //   jump.srcLine = 0;
  // }

  // emit_instr(jump);
};

void generate_FUNCEND(quad* quad) {
  // STACK -> pop
  stackNode* f = funcStack.top();
  funcStack.pop();
  patchLabel(f->returnList, nextInstructionLabel());

  quad->taddress = nextInstructionLabel();
  instruction* t = new instruction();
  t->opcode = funcexit_v;
  t->result = new vmarg();
  t->arg1 = NULL;
  t->arg2 = NULL;
  make_operand(quad->result, t->result);
  t->srcLine = symbol_table.get_lineno(quad->result->sym);

  emit_instr(t);
};

typedef void (*generator_func_t)(quad*);

generator_func_t generators[] = {
    generate_ASSIGN,       generate_ADD,          generate_SUB,
    generate_MUL,          generate_DIV,          generate_MOD,
    generate_UMINUS,       generate_AND,          generate_OR,
    generate_NOT,          generate_IF_EQ,        generate_IF_NOTEQ,
    generate_IF_LESSEQ,    generate_IF_GREATEREQ, generate_IF_LESS,
    generate_IF_GREATER,   generate_CALL,         generate_PARAM,
    generate_RETURN,       generate_GETRETVAL,    generate_FUNCSTART,
    generate_FUNCEND,      generate_NEWTABLE,     generate_TABLEGETELEM,
    generate_TABLESETELEM, generate_JUMP,         generate_NOP};

void generateAll(void) {
  for (unsigned int i = 0; i < quads.size(); ++i) {
    cout << "generateAll:";
    debug_quad(quads[i], i);
    (*generators[quads[i]->iop])(quads[i]);
  }
  cout<<"1"<<endl;
  patch_incomplete_jumps();
  cout<<"2"<<endl;
}

unsigned get_magic(string s) { return 69420666; }

void create_binary() {
  unsigned magic_number = get_magic("NoobMaster69");
  FILE* outfile;
  outfile = fopen("binary.abc", "wb");
  if( !outfile ){
    cerr << " Error while opening the file" << endl;
  }
  cout << "magic number: " << magic_number << endl;

  //Magic number
  if( fwrite(&magic_number, sizeof(unsigned), 1, outfile) == -1) 
    cerr << " Error writing binary file" << endl;

  //String consts
  size_t size = stringConsts.size(); 
  cout << "string size: " << size << endl;
  if( fwrite(&size, sizeof(size_t), 1, outfile) == -1)
    cerr << " Error writing binary file" << endl;
    
  for(int i=0; i < stringConsts.size();i++) {
    size_t len = strlen(stringConsts[i]);
    cout << "length: " << len << endl;
    if( fwrite(&len, sizeof(size_t), 1, outfile) == -1 )
      cerr << " Error writing binary file len"  << endl; 
    if( fwrite(stringConsts[i], sizeof(char), len, outfile) == -1 )
      cerr << " Error writing binary file str" << endl; 
    cout << stringConsts[i] << endl;
  }

  //Num consts
  size = numConsts.size();
  cout << "number size: " << size << endl;
  if( fwrite(&size, sizeof(size_t), 1, outfile) == -1 )
    cerr << " Error writing binary file" << endl;
  for(int i=0; i < numConsts.size();i++) {
    if ( fwrite(&numConsts[i], sizeof(double), 1, outfile) == -1) 
      cerr << " Error writing binary file" << endl;
    cout << numConsts[i] << endl;
  }

  //user funcs
  size = userFuncz.size();
  cout << "userFuncz size: " << size << endl;  
  if( fwrite(&size, sizeof(size_t), 1, outfile) == -1 )
    cerr << " Error writing binary file" << endl;
  for(int i=0; i < userFuncz.size();i++){ 
    size_t len = strlen(userFuncz[i]->id);
    if( fwrite(&len, sizeof(size_t), 1, outfile) == -1)
      cerr << " Error writing binary file" << endl;
    if( fwrite(userFuncz[i]->id, sizeof(char), len, outfile) == -1)
      cerr << " Error writing binary file" << endl;

    if( fwrite(&userFuncz[i]->address, sizeof(unsigned), 1, outfile) == -1)
      cerr << " Error writing binary file" << endl;
    
    if( fwrite(&userFuncz[i]->localSize, sizeof(unsigned), 1, outfile) == -1)
      cerr << " Error writing binary file" << endl;
      
    cout << userFuncz[i] << endl;
  }

  //lib funcs
  size = namedLibFuncs.size();
  cout << "namedLibFuncs size: " << size << endl;
  if( fwrite(&size, sizeof(size_t), 1, outfile) == -1)
    cerr << " Error writing binary file" << endl;
  for(int i=0; i < namedLibFuncs.size();i++) {
    size_t len = strlen(namedLibFuncs[i]);    
    if( fwrite(&len, sizeof(size_t), 1, outfile) == -1 )
      cerr << " Error writing binary file" << endl;
    if( fwrite(namedLibFuncs[i], sizeof(char), len, outfile) == -1 )
      cerr << " Error writing binary file" << endl;
    cout << namedLibFuncs[i] << endl;    
  }

  //Globals size 
  size = programVarOffset;
  cout << "Globals size: " << size << endl; 
  if( fwrite(&size, sizeof(size_t), 1, outfile) == -1) 
    cerr << " Error writing binary file" << endl;

  //Instructions
  size = instructionz.size();
  cout << "instructionz size: " << size << endl;
  if( fwrite(&size, sizeof(size_t), 1, outfile) == -1) 
    cerr << " Error writing binary file" << endl;
  for(int i=0; i < instructionz.size();i++){
    if( fwrite(&(instructionz[i]->opcode), sizeof(vmopcode), 1, outfile) == -1)
      cerr << " Error writing binary file" << endl;
    cout << instructionz[i]->opcode << " ";
    if( instructionz[i]->result ){
      //result
      if( fwrite(&(instructionz[i]->result->type), sizeof(vmarg_t), 1, outfile) == -1)
        cerr << " Error writing binary file" << endl; 
      if( fwrite(&(instructionz[i]->result->val), sizeof(unsigned), 1, outfile) == -1)
        cerr << " Error writing binary file" << endl;
      cout << instructionz[i]->result->type << "," << instructionz[i]->result->val << " ";
    }
    if( instructionz[i]->arg1 ){
      //arg1
      if( fwrite(&(instructionz[i]->arg1->type), sizeof(vmarg_t), 1, outfile) == -1)
        cerr << " Error writing binary file" << endl;
      if( fwrite(&(instructionz[i]->arg1->val), sizeof(unsigned), 1, outfile) == -1)
        cerr << " Error writing binary file" << endl;
      cout << instructionz[i]->arg1->type << "," << instructionz[i]->arg1->val << " ";
    }
    if(instructionz[i]->arg2){
      //arg2
      if( fwrite(&(instructionz[i]->arg2->type), sizeof(vmarg_t), 1, outfile) == -1)
        cerr << " Error writing binary file" << endl;
      if( fwrite(&(instructionz[i]->arg2->val), sizeof(unsigned), 1, outfile) == -1)
        cerr << " Error writing binary file" << endl;
      cout << instructionz[i]->arg2->type << "," << instructionz[i]->arg2->val << " ";
    }
    cout << endl;
  }
  
  fclose(outfile);
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

string toString(vmarg_t t) {
  switch (t) {
    case label_a:
      return "label";
    case global_a:
      return "global";
    case formal_a:
      return "formal";
    case local_a:
      return "local";
    case number_a:
      return "number";
    case string_a:
      return "string";
    case bool_a:
      return "bool";
    case nil_a:
      return "nil";
    case userfunc_a:
      return "userfunc";
    case libfunc_a:
      return "libfunc";
    case retval_a:
      return "retval";
    default:
      assert(false);
  }
}

void printInstructions() {
  ofstream myfile;
  string s = "tcode.txt";
  myfile.open(s.c_str());

  myfile << setw(5) << "Instructions " << setw(5) << "\n"
         << setw(12) << "vmopcode" << setw(13) << "result_type" << setw(11)
         << "result_val" << setw(10) << "arg1_type" << setw(9) << "arg1_val"
         << setw(10) << "arg2_type" << setw(9) << "arg2_val" << setw(12)
         << "srcLine" << endl
         << string(86, '-') << endl;

  for (int j = 0; j < instructionz.size(); j++) {
    instruction* i = instructionz[j];
    assert(i);
    myfile << setw(12) << toString(i->opcode);
    if (i->result != NULL) {
      myfile << setw(13) << toString(i->result->type) << setw(11)
             << i->result->val;
    } else
      myfile << string(13 + 11, ' ');
    if (i->arg1 != NULL)
      myfile << setw(10) << toString(i->arg1->type) << setw(9) << i->arg1->val;
    else
      myfile << string(9 + 10, ' ');

    if (i->arg2 != NULL)
      myfile << setw(10) << toString(i->arg2->type) << setw(9) << i->arg2->val;
    else
      myfile << string(9 + 10, ' ');

    myfile << setw(12) << i->srcLine << endl;
  }

  myfile.close();
}

void printInstruction(instruction* i) {
  assert(i);
  stringstream ss;
  cout << setw(12) << toString(i->opcode);
  if (i->result != NULL)
    cout << setw(13) << toString(i->result->type) << setw(11) << i->result->val;
  else
    cout << string(13 + 11, ' ');

  if (i->arg1 != NULL)
    cout << setw(10) << toString(i->arg1->type) << setw(9) << i->arg1->val;
  else
    cout << string(9 + 10, ' ');

  if (i->arg2 != NULL)
    cout << setw(10) << toString(i->arg2->type) << setw(9) << i->arg2->val;
  else
    cout << string(9 + 10, ' ');

  cout << setw(12) << i->srcLine << endl;
}
#endif