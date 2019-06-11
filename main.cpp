#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <vector>
#include "avm.hpp"

using namespace std;

unsigned globals;


void read_binary() {
  FILE* infile;
  unsigned magic_number;
  size_t size;
  infile = fopen("binary.abc", "rb");
  if (!infile) cerr << " Error while opening the file" << endl;

  if (fread(&magic_number, sizeof(unsigned), 1, infile) == -1)
    cerr << "Error while reading the file - magic" << endl;
  // Check magic number
  if (magic_number != get_magic("I love you 3000"))
    cerr << " Magic number is invalid" << endl;
  // cout << "magic number: " << magic_number << endl;
  // String consts
  if (fread(&size, sizeof(size_t), 1, infile) == -1)
    cerr << " Error while reading the file - strings size" << endl;
  // cout << "strings: " << size << endl;
  for (int i = 0; i < size; i++) {
    size_t s;
    if (fread(&s, sizeof(size_t), 1, infile) == -1)
      cerr << "Error while reading the file - str len" << endl;
    // cout << "length: " << s << endl;
    char* str = (char*)malloc(s + 1);
    if (fread(str, sizeof(char), s, infile) == -1)
      cerr << "Error while reading the file str" << endl;
    str[s] = '\0';
    // cout << str << endl;
    stringConstsRead.push_back(strdup(str));
  }

  // Num consts
  if (fread(&size, sizeof(size_t), 1, infile) == -1)
    cerr << " Error while reading the file - nums size" << endl;
  // cout << "numbers: " << size << endl;
  for (int i = 0; i < size; i++) {
    double d;
    if (fread(&d, sizeof(double), 1, infile) == -1)
      cerr << "Error while reading the file - num" << endl;
    // cout << d << endl;
    numConstsRead.push_back(d);
  }

  // User funcs
  if (fread(&size, sizeof(size_t), 1, infile) == -1)
    cerr << " Error while reading the file - userfuncs size" << endl;
  // cout << "use funcs: " << size << endl;
  for (int i = 0; i < size; i++) {
    size_t s;
    if (fread(&s, sizeof(size_t), 1, infile) == -1)
      cerr << "Error while reading the file - id size" << endl;
    userfunc* f = new userfunc();
    char* str = (char*)malloc(s + 1);
    if (fread(str, sizeof(char), s, infile) == -1)
      cerr << "Error while reading the file - id" << endl;
    str[s] = '\0';
    // cout << "id: " << str << endl;
    f->id = strdup(str);
    if (fread(&f->address, sizeof(unsigned), 1, infile) == -1)
      cerr << "Error while reading the file - address" << endl;
    // cout << "address: " <<f->address << endl;
    if (fread(&f->localSize, sizeof(unsigned), 1, infile) == -1)
      cerr << "Error while reading the file - localsize" << endl;
    // cout << "locals: " << f->localSize<< endl;
    userFunczRead.push_back(f);
  }

  // Lib funcs
  if (fread(&size, sizeof(size_t), 1, infile) == -1)
    cerr << " Error while reading the file - libs funcs size" << endl;
  // cout << "lib funcs: " << size << endl;
  for (int i = 0; i < size; i++) {
    size_t s;
    if (fread(&s, sizeof(size_t), 1, infile) == -1)
      cerr << "Error while reading the file - lib len" << endl;
    char* str = (char*)malloc(s + 1);
    if (fread(str, sizeof(char), s, infile) == -1)
      cerr << "Error while reading the file - lib name" << endl;
    str[s] = '\0';
    // cout << "lib func: " << str << endl;
    namedLibFuncsRead.push_back(strdup(str));
  }

  // instuctions
  if( fread(&size, sizeof(size_t), 1, infile) == -1) 
    cerr << " Error reading binary file - globals" << endl;
  globals = size;  

  if (fread(&size, sizeof(size_t), 1, infile) == -1)
    cerr << " Error while reading the file - instructions size" << endl;
  // cout << "instructions: " << size << endl;
  for (int i = 0; i < size; i++) {
    instruction* instr = new instruction();
    // opcode
    if (fread(&instr->opcode, sizeof(vmopcode), 1, infile) == -1)
      cerr << "Error while reading the file - op" << endl;
    // cout << instr->opcode << " ";
    switch (instr->opcode) {
      case add_v:
      case sub_v:
      case mul_v:
      case mod_v:
      case div_v:
      case jeq_v:
      case jne_v:
      case jle_v:
      case jge_v:
      case jlt_v:
      case jgt_v:
      case tablegetelem_v:
      case tablesetelem_v: {
        // result arg1 arg2
        instr->result = new vmarg();
        if (fread(&instr->result->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->result->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        instr->arg1 = new vmarg();
        if (fread(&instr->arg1->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->arg1->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        instr->arg2 = new vmarg();
        if (fread(&instr->arg2->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->arg2->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        // cout << instr->result->type << "," << instr->result->val << " ";
        // cout << instr->arg1->type << "," << instr->arg1->val << " ";
        // cout << instr->arg2->type << "," << instr->arg2->val << " ";

        break;
      }
      case not_v: {}
      case assign_v: {
        // result and arg1
        instr->result = new vmarg();
        if (fread(&instr->result->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->result->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        instr->arg1 = new vmarg();
        if (fread(&instr->arg1->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->arg1->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        instr->arg2 = NULL;
        // cout << instr->result->type << "," << instr->result->val << " ";
        // cout << instr->arg1->type << "," << instr->arg1->val << " ";
        break;
      }
      case jump_v:
      case funcenter_v:
      case funcexit_v:
      case newtable_v: {
        // result
        instr->result = new vmarg();
        if (fread(&instr->result->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->result->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        instr->arg1 = NULL;
        instr->arg2 = NULL;
        // cout << instr->result->type << "," << instr->result->val << " ";
        break;
      }
      case pusharg_v:
      case call_v: {
        // arg1
        instr->arg1 = new vmarg();
        if (fread(&instr->arg1->type, sizeof(size_t), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        if (fread(&instr->arg1->val, sizeof(unsigned), 1, infile) == -1)
          cerr << "Error while reading the file" << endl;
        instr->result = NULL;
        instr->arg2 = NULL;
        // cout << instr->arg1->type << "," << instr->arg1->val << " ";
        break;
      }
      default: { 
        cout << instr->opcode << " ";
        assert(0); 
      }
    }
    // cout << endl;
    instructionzRead.push_back(instr);
  }
}

int main(int argc, char* argv[]) {
  read_binary();
  // cout << "Instructions Read"<<endl;
  avm_initialize();
  codeSize = instructionzRead.size();
  // cout << "Avm initialized with codeSize: " << codeSize <<endl;
  top = AVM_STACKSIZE - 1 - globals - 2;
  topsp = AVM_STACKSIZE - 1;
  
  while (executionFinished == false) {
    execute_cycle(); 
  }


  return 0;
}