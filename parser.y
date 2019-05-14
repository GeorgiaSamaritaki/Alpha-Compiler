%{
    #include <stdio.h>
    #include <stdlib.h>
    // #include "quad.hpp"
    #include "codegen.hpp"

    int yyerror(char* yaccProvidedMessage);
    int yylex(void);

    extern int yylineno;
    extern char* yytext;
    extern FILE* yyin; 
    
%}

%defines

%union { 
    char* stringValue; 
    int intValue;   
    double realValue; 
    struct expr* exprNode;
    struct SymbolTableEntry* symbol;
    struct call_l* call_l;
    struct stmt_l* stmt_l;
    struct for_prefix* for_prefix;
    // iopcode op;
}

%type <exprNode> lvalue 
%type <exprNode> tableitem 
%type <exprNode> primary 
%type <exprNode> assignexpr 
%type <exprNode> expr 
%type <exprNode> elist 
%type <exprNode> elist_l 
%type <exprNode> idlist 
%type <exprNode> idlist_l 
%type <exprNode> call 
%type <exprNode> objectdef 
%type <exprNode> indexedelem 
%type <exprNode> indexed 
%type <exprNode> term 
%type <exprNode> arithexpr 
%type <exprNode> relexpr 
%type <exprNode> boolexpr
%type <exprNode> member

%type <exprNode> number
%type <exprNode> const

%type <call_l> methodcall 
%type <call_l> normcall 
%type <call_l> callsuffix 

%type <stringValue> func_name
%type <stringValue> method_id
// %type <intValue> func_body
%type <symbol> func_prefix
%type <symbol> funcdef

%type <intValue> ifprefix
%type <intValue> elseprefix
%type <intValue> whilestart
%type <intValue> whilecond

%type <stmt_l> stmt;
%type <stmt_l> loopstmt;
%type <stmt_l> statements;
%type <stmt_l> BREAK;
%type <stmt_l> CONTINUE;
%type <stmt_l> returnstmt 
%type <stmt_l> block_l 
%type <stmt_l> block 
%type <stmt_l> func_body 

%type <intValue> N
%type <intValue> M

%type <for_prefix> forprefix


%start program
%token new_line    
%token whitespace  
%token EOFile  
    
/*Keywords*/
%token AND         
%token OR          
%token NOT         
%token TRUE        
%token FALSE  
/*C keywords*/
%token NIL         
%token IF          
%token ELSE        
%token WHILE       
%token FOR         
%token BREAK       
%token CONTINUE    
%token function    
%token RETURN   

%token local       

/*Operators*/
%token assign      
%token plus        
%token minus  
%token uminus
%token mul         
%token division    
%token mod         
%token increment  
%token decrement  
%token b_equals    
%token b_not_equal 
%token b_greater   
%token b_greater_eq
%token b_less      
%token b_less_eq   

/*Constants-Identifiers*/
%token digit       
%token letter      
%token underscore  
%token integer     
%token real        
%token id           

/*String have to be implemented in code*/
%token STRING  

/*Punstuation marks*/
%token left_curly          
%token right_curly         
%token left_bracket        
%token right_bracket       
%token left_parenthesis     
%token  right_parenthesis   
%token semicolon           
%token comma               
%token colon               
%token double_colon        
%token dot                 
%token double_dot           

/*Comments*/
%token start_comment           
%token end_comment             
%token line_comment        

%token other

%right      assign
%left	    OR
%left	    AND 
%nonassoc   b_not_equal b_equals
%nonassoc	b_greater   b_greater_eq  b_less  b_less_eq 

%left       plus    minus
%left       mul     division    mod
%right      NOT     increment   decrement uminus

%left	    dot     double_dot
%left       comma

%left       left_bracket        right_bracket
%left       left_parenthesis      right_parenthesis
%left       left_curly          right_curly


%%


program:    statements {//printf("Finished\n");
};

stmt:       expr semicolon {
                $stmt = new stmt_l();
                //printf("stmt->expr';' \n");
                }
            | ifstmt     {
                $stmt = new stmt_l();  
                //printf("stmt->ifstmt \n\n");
                }
            | whilestmt  {
                $stmt = new stmt_l();  
                //printf("stmt->whilestmt\n\n");
                }
            | forstmt    {
                $stmt = new stmt_l();  
                //printf("stmt->forstmt   \n\n");
                }
            | returnstmt {
                $stmt = $returnstmt;  
                //printf("stmt->returnstmt \n\n");
                }
            | { scope++;} block {
                $stmt = $block;  
                //printf("stmt->block2\n");
                } //maybe scope++ after left curly
            | funcdef    {
                $stmt = new stmt_l(); 
                //printf("stmt->funcdef \n");   
                }
            | BREAK semicolon {
                //printf("BREAK  ';'  \n\n");
                $stmt = new stmt_l();
                if(loopcnt==0){
                    yyerror("Use of \"Break\" outside of loop");
                }else{
                    $stmt->breaklist = newList(nextQuadLabel()); 
                    emit(jump);
                }
                }
            | CONTINUE semicolon {
                //printf("CONTINUE ';'  \n\n");
                $stmt = new stmt_l();
                if(loopcnt==0){
                    yyerror("Use of \"Continue\" outside of loop");
                }else{
                    $stmt->contlist = newList(nextQuadLabel()); 
                    emit(jump);
                }
                }
            | semicolon  {
                $stmt = new stmt_l();
                //printf("';' \n\n");       
                };
            

statements: statements   stmt { 
                //printf("statements ->statements stmt\n");
                reset_tmp(); 

                $$ = merge($1,$2);
                //free $2?
                }
            | stmt { $$ =$1; reset_tmp(); /*printf("statements->empty\n");*/};

expr:       assignexpr    { 
                assert($1); 
                $$ = $1; //printf("expr->assignexpr \n");
                }
            | boolexpr    { 
                assert($1); 
                $$ = $1; //printf("expr->boolexpr \n");
                }
            | arithexpr   { 
                assert($1); 
                $$ = $1; //printf("expr->arithexpr \n");
                }
            | relexpr     { 
                assert($1); 
                $$ = $1; //printf("expr->relexpr \n");
                }
            | term        { 
                assert($1); 
                $$ = $1; //printf("expr->term \n");
                };     

arithexpr:     expr plus expr     { 
                //printf("opexr->expr+expr \n");
                if(!isvalid_arithmeticCheck($1->type,$3->type) ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = nil_expr;
                }else {
                    if($1->type == constnum_e && $3->type == constnum_e){
                        $$ = newExpr(constnum_e);
                        $$->sym = new_tmp(yylineno);
                        $$->numConst = compute(add, $1->numConst , $3->numConst);
                    }else{
                        $$ = newExpr(arithexpr_e);
                        $$->sym = new_tmp(yylineno);
                        emit(add, $1 , $3, $$);
                    }
                }    
            }  
            | expr minus expr   { 
                //printf("opexr->expr-expr \n");
                if(!isvalid_arithmeticCheck($1->type,$3->type) ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = nil_expr;
                }else {
                    if($1->type == constnum_e && $3->type == constnum_e){
                        $$ = newExpr(constnum_e);
                        $$->sym = new_tmp(yylineno);
                        $$->numConst = compute(sub, $1->numConst , $3->numConst);
                    }else{
                        $$ = newExpr(arithexpr_e);
                        $$->sym = new_tmp(yylineno);
                        emit(sub, $1 , $3, $$);
                    }
                }    
            }
            | expr mul expr      {
                //printf("opexr->expr*expr \n");
                assert($1 && $3);
                if(!isvalid_arithmeticCheck($1->type,$3->type)){
                    yyerror("Invalid arithmetic expressions");
                    $$ = nil_expr;
                }else {
                    if($1->type == constnum_e && $3->type == constnum_e){
                        $$ = newExpr(constnum_e);
                        $$->sym = new_tmp(yylineno);
                        $$->numConst = compute(mul_op, $1->numConst , $3->numConst);
                    }else{
                        $$ = newExpr(arithexpr_e);
                        $$->sym = new_tmp(yylineno);
                        emit(mul_op, $1 , $3, $$);
                    }
                }
            }
            | expr division expr { 
                //printf("opexr->expr/expr \n");
                if(!isvalid_arithmeticCheck($1->type,$3->type)){
                    yyerror("Invalid arithmetic expressions");
                    $$ = nil_expr;
                }else {
                    if($1->type == constnum_e && $3->type == constnum_e){
                        $$ = newExpr(constnum_e);
                        $$->sym = new_tmp(yylineno);
                        $$->numConst = compute(div_op, $1->numConst , $3->numConst);
                    }else{
                        $$ = newExpr(arithexpr_e);
                        $$->sym = new_tmp(yylineno);
                        emit(div_op, $1 , $3, $$);
                    }
                }
            }
            | expr mod expr      { 
                //printf("opexr->expr\%expr \n");
                if(!isvalid_arithmeticCheck($1->type,$3->type)){
                    yyerror("Invalid arithmetic expressions");
                    $$ = nil_expr;
                }else {
                    if($1->type == constnum_e && $3->type == constnum_e){
                        $$ = newExpr(constnum_e);
                        $$->sym = new_tmp(yylineno);
                        $$->numConst = compute(mod_op, $1->numConst , $3->numConst);
                    }else{
                        $$ = newExpr(arithexpr_e);
                        $$->sym = new_tmp(yylineno);
                        emit(mod_op, $1 , $3, $$);
                    }
                    //printf("expr->expr arithexpr expr \n");
                }
                };

relexpr:      expr b_greater expr   {
                //printf("opexr->expr>expr \n");
                $$ = newExpr(constbool_e);
                $$->truelist = newList(nextQuadLabel());
                $$->falselist = newList(nextQuadLabel()+1);
                //printf("t %d f%d\n", $$->truelist[0], $$->falselist[0]);
                if(!isvalid_arithmeticCheck($1->type,$3->type)){
                    yyerror("Invalid arithmetic expressions");
                    $$ = nil_expr;
                }else {
                    if($1->type == constnum_e && $3->type == constnum_e){
                        $$->sym = new_tmp(yylineno);
                        $$->boolConst = compute_rel(if_greater, $1->numConst , $3->numConst);
                    }
                    emit(if_greater, $1 , $3);
                    emit(jump);
                }
        }
        |  expr b_less expr         {
            //printf("opexr->expr<expr \n");
            $$ = newExpr(constbool_e);
            $$->truelist = newList(nextQuadLabel());
            $$->falselist = newList(nextQuadLabel()+1);
            //printf("t %d f%d\n", $$->truelist[0], $$->falselist[0]);
            if(!isvalid_arithmeticCheck($1->type,$3->type) ){
                yyerror("Invalid arithmetic expressions");
                $$ = nil_expr;
            }else {
                if($1->type == constnum_e && $3->type == constnum_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_less, $1->numConst , $3->numConst);
                }
                emit(if_less, $1 , $3);
                emit(jump);
            }
        }         
        |  expr b_greater_eq expr   {
            //printf("opexr->expr>=expr \n");
            $$ = newExpr(constbool_e);
            $$->truelist = newList(nextQuadLabel());
            $$->falselist = newList(nextQuadLabel()+1);
            //printf("t %d f%d\n", $$->truelist[0], $$->falselist[0]);
            if(!isvalid_arithmeticCheck($1->type,$3->type)){
                yyerror("Invalid arithmetic expressions");
                $$ = nil_expr;
            }else {
                if($1->type == constnum_e && $3->type == constnum_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_greater_eq, $1->numConst , $3->numConst);
                }
                emit(if_greater_eq, $1 , $3);
                emit(jump);
            }
        }
        |  expr b_less_eq expr      {
            //printf("opexr->expr<=expr \n");
            $$ = newExpr(constbool_e);
            $$->truelist = newList(nextQuadLabel());
            $$->falselist = newList(nextQuadLabel()+1);
            //printf("t %d f%d\n", $$->truelist[0], $$->falselist[0]);
            if(!isvalid_arithmeticCheck($1->type,$3->type) ){
                yyerror("Invalid arithmetic expressions");
                $$ = nil_expr;
            }else {
                if($1->type == constnum_e && $3->type == constnum_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_lesseq, $1->numConst , $3->numConst);
                }
                emit(if_lesseq, $1 , $3);
                emit(jump);
            }
        }       
        |  expr b_equals expr       {
            //printf("opexr->expr==expr \n");
            if(!is_same($1->type,$3->type) ){
                yyerror("Invalid operands to boolean expression");
                $$ = nil_expr;
            }else {
                $$ = newExpr(constbool_e);
                $$->truelist = newList(nextQuadLabel());
                $$->falselist = newList(nextQuadLabel()+1);
                //printf("t %d f%d\n", $$->truelist[0], $$->falselist[0]);
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute(if_eq, (bool)$1->boolConst ,(bool) $3->boolConst);
                }else if($1->type == constnum_e && $3->type == constnum_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_eq, $1->numConst , $3->numConst);
                }else if(($1->type == newtable_e && $3->type == nil_e) || 
                                ($3->type == newtable_e && $1->type == nil_e)){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = false;
                }
                emit(if_eq, $1 , $3);
                emit(jump);
            }
        }            
        |  expr b_not_equal expr    {
            if(!is_same($1->type,$3->type) ){
                yyerror("Invalid operands to boolean expression");
                $$ = nil_expr;
            }else {
                //printf("opexr->expr!=expr \n");
                $$ = newExpr(constbool_e);
                $$->truelist = newList(nextQuadLabel());
                $$->falselist = newList(nextQuadLabel()+1);
                //printf(" t %d f%d\n", $$->truelist[0], $$->falselist[0]);
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute(if_noteq, (bool)$1->boolConst ,(bool)$3->boolConst);
                }else if($1->type == constnum_e && $3->type == constnum_e){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_noteq, $1->numConst , $3->numConst);
                }else if(($1->type == newtable_e && $3->type == nil_e) || 
                                ($3->type == newtable_e && $1->type == nil_e)){
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = true;
                } 
                emit(if_noteq, $1 , $3);
                emit(jump);
            }};

boolexpr:  expr OR {
                if($1->truelist.empty() && $1->falselist.empty()){
                    $1->truelist = newList(nextQuadLabel());
                    $1->falselist = newList(nextQuadLabel()+1);
                    emit(
                        if_eq, $1 ,newExpr_constBool(1));
                    emit(jump);
                }

            } M expr {      
                //printf("opexr->expr || expr \n");
                
                patchLabel($1->falselist, $M); // -1 for and quad
                if($5->truelist.empty() && $5->falselist.empty()){
                    $5->truelist = newList(nextQuadLabel());
                    $5->falselist = newList(nextQuadLabel()+1);
                    emit(
                        if_eq, $5 ,newExpr_constBool(1));
                    emit(jump);
                }

                $$ = newExpr(boolexpr_e);
                $$->sym = new_tmp(yylineno);
                // $$->boolConst = true;
                assert($1 && $5);
                emit( assign_op, newExpr_constBool(0), NULL, $$);
                
                emit( jump, NULL, NULL, NULL, nextQuadLabel()+2);
                
                emit(assign_op, newExpr_constBool(1), NULL, $$);
                // //printf("f %d\n", $1->falselist[0]);
                //printf("patching false with %d: ------------------",$M);
                $$->truelist = merge($1->truelist, $5->truelist);
                $$->falselist = $5->falselist;
                patchLabel($$->truelist, nextQuadLabel()-1);
                    
                } 
            | expr AND {
                
                if($1->truelist.empty() && $1->falselist.empty()){
                    $1->truelist = newList(nextQuadLabel());
                    $1->falselist = newList(nextQuadLabel()+1);
                    emit(if_eq, $1 ,newExpr_constBool(1));
                    emit(jump);
                }
                
                }M expr {
                //printf("opexr->expr&&expr \n");

                patchLabel($1->truelist, $M);
                if($5->truelist.empty() && $5->falselist.empty()){
                    $5->truelist = newList(nextQuadLabel());
                    $5->falselist = newList(nextQuadLabel()+1);
                    emit(
                        if_eq, $5 ,newExpr_constBool(1));
                        emit(jump);
                }

                $$ = newExpr(boolexpr_e);
                $$->sym = new_tmp(yylineno);
                // $$->boolConst = false;
                assert($1&&$5);
                emit(
                        assign_op, newExpr_constBool(0), NULL, $$);
                emit(
                        jump, NULL, NULL, NULL,nextQuadLabel()+2);
                emit(
                        assign_op, newExpr_constBool(1), NULL, $$);

                //printf("patching true with %d: ----------------",$M);
                $$->truelist = $5->truelist;
                $$->falselist = merge($1->falselist, $5->falselist);
                patchLabel($$->falselist, nextQuadLabel()-3);

                };


term:       left_parenthesis expr  right_parenthesis {
                //printf("term->(expr) \n");
                $term = $expr;
                } 
            | NOT expr {
                //printf("term->NOTexpr \n");
                assert($expr);
                $term = newExpr(boolexpr_e);
                $term->sym = new_tmp(yylineno);

                $term->truelist = $expr->falselist;
                $term->falselist = $expr->truelist;
                emit(not_op, $expr, NULL, $term);
                }
            | minus expr %prec uminus {
                assert($expr);
                //printf("term->-expr \n");
                checkUminus($expr);
                $term = newExpr(arithexpr_e);
                $term->sym = new_tmp(yylineno);
                emit(uminus_op, $expr, NULL , $term);
                }
            | increment{} lvalue {
                //printf("term->++lvalue \n"); 
                if($3 != NULL) {
                    if( (int)symbol_table.get_scope($3->sym) <= last_func.top() && (int)symbol_table.get_scope($3->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        $$ = nil_expr;
                    }else {
                        if($3->sym->type == USERFUNC){ 
                            $term = nil_expr;
                            yyerror("Using ProgramFunc as an lvalue");
                        }else if ($3->sym->type == LIBFUNC){ 
                            $term = nil_expr;
                            yyerror("Using LibFunc as an lvalue");
                        }else{
                            if ($lvalue->type == tableitem_e) {
                                $term = emit_ifTableItem($lvalue);
                                emit(
                                    add, $term, newExpr_constNum(1), $term);
                                emit( tablesetelem, $lvalue->index, $lvalue, $term);
                            } else {
                                emit(
                                    add, $lvalue, newExpr_constNum(1), $lvalue);
                                $term = newExpr(arithexpr_e);
                                $term->sym = new_tmp(yylineno);
                                emit(assign_op, $lvalue, NULL, $term);
                            }
                        }                
                    }
                }else{ //define as new var
                    $term = nil_expr;
                    yyerror("variable undefined");
                }
            } //lookup maybe
            | lvalue increment {
                //printf("term->lvalue++ \n");
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top() && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        $term = nil_expr;
                    }else{
                        if($1->sym->type == USERFUNC){ 
                            $term = nil_expr;
                            yyerror("Using ProgramFunc as an lvalue");
                        }else if($1->sym->type == LIBFUNC){ 
                            $term = nil_expr;
                            yyerror("Using LibFunc as an lvalue");
                        }else{
                            $term = newExpr(var_e);
                            $term->sym = new_tmp(yylineno);

                            if( $lvalue->type == tableitem_e ) {
                                expr* value = emit_ifTableItem($lvalue);
                                emit(assign_op, value, NULL, $term);
                                emit(
                                    add, value, newExpr_constNum(1), value);
                                emit( tablesetelem, $lvalue->index, value, $lvalue);
                            }else {
                                emit(assign_op, $lvalue, NULL, $term);
                                emit(
                                    add, $lvalue, newExpr_constNum(1), $lvalue);
                            }
                        }
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                    $term = nil_expr;
                } 
                
                } 
            | decrement {} lvalue {
                //printf("term->--lvalue \n");
                if($3 != NULL) {
                    if( (int)symbol_table.get_scope($3->sym) <= last_func.top() && (int)symbol_table.get_scope($3->sym)!=0 ){
                        yyerror("Cant reference variable out of scope");
                        $term = nil_expr;
                    }else {   
                        if($3->sym->type == USERFUNC){ 
                            yyerror("Using ProgramFunc as an lvalue");
                            $term = nil_expr;
                        }else if($3->sym->type == LIBFUNC){ 
                            yyerror("Using LibFunc as an lvalue");
                            $term = nil_expr;
                        }else{
                            if ($lvalue->type == tableitem_e) {
                                $term = emit_ifTableItem($lvalue);   
                                emit(
                                    sub, $term, newExpr_constNum(1), $term);
                                emit( tablesetelem, $lvalue->index, $lvalue, $term);
                            } else {
                                emit(
                                    sub, $lvalue, newExpr_constNum(1), $lvalue);
                                $term = newExpr(arithexpr_e);
                                $term->sym = new_tmp(yylineno);
                                emit(assign_op, $lvalue, NULL, $term);
                            }
                        }
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                    $term = nil_expr;
                }
                } //lookup maybe
            | lvalue decrement {
                //printf("term->lvalue-- \n");
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top() && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        $term = nil_expr;
                    }else {   
                        if($1->sym->type == USERFUNC){
                            yyerror("Using ProgramFunc as an lvalue");
                            $term = nil_expr;
                        }else if($1->sym->type == LIBFUNC){ 
                            yyerror("Using LibFunc as an lvalue");
                            $term = nil_expr;
                        }else{    
                            if ($lvalue->type == tableitem_e) {
                                $term = emit_ifTableItem($lvalue);
                                emit(assign_op, $term, NULL, $term);
                                emit(
                                    sub, $term, newExpr_constNum(1), $term);
                                emit( tablesetelem, $lvalue->index, $lvalue, $term);
                            } else {
                                emit(assign_op, $lvalue, NULL, $term);
                                $term = newExpr(arithexpr_e);
                                $term->sym = new_tmp(yylineno);
                                emit(
                                    sub, $lvalue, newExpr_constNum(1), $lvalue);
                            }
                        }
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                    $term = nil_expr;
                }
            }  //lookup maybe (before --?)
            | primary {
                //printf("term->primary \n");
                assert($1);
                $$ = $1;
                };

assignexpr: lvalue assign expr {
                //printf("assignexpr->lvalue=expr \n");
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top() && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        $assignexpr = nil_expr;
                    }else if($1->sym->type == USERFUNC){ 
                        yyerror("Cannot assign to function");
                        $assignexpr = nil_expr;
                    }else if($1->sym->type == LIBFUNC){
                        yyerror("Cannot assign to libfunc");
                        $assignexpr = nil_expr;
                    }else{ 
                        if($lvalue->type == tableitem_e){
                            emit( //that is lvalue[index] = expr;
                                tablesetelem, $lvalue->index, $expr, $lvalue);
                            //The value f the assignment expression should be gained
                            $assignexpr = emit_ifTableItem($lvalue);
                            $assignexpr->type = assignexpr_e;
                        }else{
                            //change_type($lvalue,$expr);

                            // $lvalue->sym->type = $expr->sym->type;
                            emit(assign_op, $expr, NULL, $lvalue);
                            //printf("lvalue %s\n",symbol_table.get_name($lvalue->sym));
                            // if($lvalue->type == constnum_e)
                            //     $assignexpr = newExpr(constnum_e);
                            // if($lvalue->type == constbool_e)
                            //     $assignexpr = newExpr(constbool_e);
                            // if($lvalue->type == conststring_e)
                            //     $assignexpr = newExpr(conststring_e);
                            // else
                            $assignexpr = newExpr(assignexpr_e);
                            $assignexpr->sym = new_tmp(yylineno);
                            emit(
                                assign_op, $lvalue, NULL, $assignexpr);
                        }
                
                    }
                }else{ //define as new var
                    //printf("member undefined in : assignexpr->lvalue=expr \n");
                    $assignexpr = nil_expr;
                    // assert(false);
                }    
            }; //lookup (before assign?)

primary:    lvalue  {
                //printf("primary->lvalue \n");
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        $lvalue = nil_expr;
                    }
                }else{ //define as new var
                    if(return_flag){
                        yyerror("return values undefined in this scope");
                        $lvalue = nil_expr;
                    }else{
                        $lvalue = newExpr(var_e); 
                        $lvalue->sym = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    }     
                }
                $primary = emit_ifTableItem($lvalue);
                }
            | call  {
                //printf("primary->call \n");
                assert($1);
                $$ = $1;
                }
            | objectdef{ 
                //printf("primary->objectdef \n");
                assert($1);
                $$ = $1;
                }
            | left_parenthesis funcdef right_parenthesis {
                //printf("primary->(funcdef) \n");
                $primary = newExpr(programfunc_e);
                $primary->sym = $funcdef;                
                }
            | const {$$ = $1;//printf("primary->const \n");
            };

lvalue:     id {
                //printf("lvalue->ids '%s'\n",yylval.stringValue);
                SymbolTableEntry *tmp = symbol_table.lookUp_allscope(yylval.stringValue); 
                if(tmp == NULL){
                    //printf("didnt find in sym\n");
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    assert(tmp);
                    tmp->space = currScopeSpace();
                    tmp->offset = currScopeOffset();
                    inCurrScopeOffset();
                }
                $lvalue = lvalue_expr(tmp);

            } //lookup
            | local id {     
                //printf("local id %s \n",yylval.stringValue);
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    assert(tmp);
                    tmp->space = currScopeSpace();
                    tmp->offset = currScopeOffset();
                    inCurrScopeOffset();
                }else{
                    if(tmp->type == LIBFUNC)
                        yyerror("shadowing of library functions not allowed");
                }
                $lvalue = lvalue_expr(tmp);
            }
            | double_colon id {
                unsigned int scope_tmp = scope;
                scope = 0; 
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 

                if(tmp ==  NULL){
                    yyerror("global variable not found");
                    $lvalue = NULL;                    
                }else{
                    $lvalue = lvalue_expr(tmp);                    
                } //undefined
                
                scope = scope_tmp;  
                //printf("lvalue->::id \n");
                }
            | member { $$ = $1; //printf("lvalue->member \n");
            }; 

member:     tableitem {
                $$ = $1;
            }
            |call dot id { 
                //printf("member->call().id \n"); 
               
                assert($$); 
               
                $$ = member_item($$, strdup(yylval.stringValue)); 
            } 
            | call left_bracket expr right_bracket { 
                //printf("member->[expr] \n"); 
 
                char* name = (char*)symbol_table.get_name($expr->sym); 
                $$ = member_item($$, name); 
            }; 
 

tableitem:  lvalue{
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                    }
                    else {
                        if($1->sym->type == USERFUNC) 
                            yyerror("cannot member function");
                        else if($1->sym->type == LIBFUNC) 
                            yyerror("cannot member libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
            } 
            dot id { 
                    //printf("member->lvalue.id \n");
                    $tableitem = member_item($lvalue, strdup(yylval.stringValue));
                }
            | lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  && (int)symbol_table.get_scope($1->sym)!=0){
                            yyerror("Cant reference variable out of scope");
                    }else{
                        if($1->sym->type == USERFUNC){
                            yyerror("cannot use function as array");
                        } else if($1->sym->type == LIBFUNC) {
                            yyerror("cannot use libfunc as array");
                        }
                    }
                }else{ 
                    //define as new var
                    yyerror("array undefined");
                }
            } left_bracket expr right_bracket { 
                $lvalue = emit_ifTableItem($lvalue);
                $tableitem = newExpr(tableitem_e);
                $tableitem->sym = $lvalue->sym;
                $tableitem->index = $expr;
                //printf("member->lvalue[expr] \n");
                }
            ;

call:       call left_parenthesis elist right_parenthesis {
                //printf("call->call(elist) \n");
                $$ = make_call($$, $elist);
                }
            | lvalue callsuffix {
                //printf("call->lvaluecallsuffix \n");
                if($1 != NULL) {
                    if($callsuffix->method){ //method call
                        expr* self = $lvalue;
                        $lvalue = emit_ifTableItem(member_item(self,$callsuffix->name));
                        self->next = $callsuffix->elist;
                        $callsuffix->elist = self;   
                        $call = make_call($lvalue, $callsuffix->elist);
                    }else{ //normcall
                        assert($1->sym);                                        //make sure its defined
                        if($1->type == var_e){
                            if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  &&
                             (int)symbol_table.get_scope($1->sym)!=0){
                                yyerror("Cant reference variable out of scope");
                                $call = NULL;
                            }
                            $call = make_call($lvalue, $callsuffix->elist);
                        }else if($1->sym->type != USERFUNC && $1->sym->type != LIBFUNC  && $1->type !=tableitem_e) {
                            if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  &&
                             (int)symbol_table.get_scope($1->sym)!=0){
                                yyerror("Cant reference variable out of scope");
                                $call = NULL;
                            }else if($1->sym->type == LOCAL||$1->sym->type == GLOBAL||$1->sym->type == FORMAL) {
                                //printf("\n%s",symbol_table.get_name($1->sym));
                                yyerror("cant use variable as function");
                                $call = NULL;
                            }else{
                                assert(false);
                            }
                        }else{
                            $call = make_call($lvalue, $callsuffix->elist);
                        }
                    }

                }else{ //define as new var
                    yyerror("function not found");
                }
            } 
            | left_parenthesis funcdef right_parenthesis left_parenthesis elist right_parenthesis {
                //printf("call->(funcdef)(elist) \n");
                expr* func = newExpr(programfunc_e);
                func->sym = $funcdef;
                $call = make_call(func, $elist);
                };

callsuffix: normcall  {    
                //printf("callsuffix->normcall \n");
                $callsuffix = $normcall; 
            }
            | methodcall { 
                //printf("callsuffix->methodcall \n");
                $callsuffix = $methodcall; 
            };

normcall:   left_parenthesis elist right_parenthesis {
                //printf("normcall->(elist) \n");
                $$ = new call_l();
                $normcall->elist = $elist;
                $normcall->method = false;
                $normcall->name = NULL;

                };
method_id:  id{ 
                $method_id = strdup(yylval.stringValue);
            };

methodcall: double_dot method_id left_parenthesis elist right_parenthesis {
                $$ = new call_l();
                //printf("methodcall->..id(elist) \n");
                $methodcall->elist = $elist;
                $methodcall->name = strdup($method_id);
                $methodcall->method = true;
                }; 

elist_l:    expr {
                //printf("elist_l->expr \n");
                $$ = $expr;
                }
            | elist_l comma expr {
                //printf("elist_l->elist_l,expr \n");
                $expr->next = $1;
                $$ = $expr;
                };

elist:      elist_l {
                //printf("elist->elist_l \n");
                $elist  = $elist_l;
                }
            |/*empty*/  {
                //printf("elist->empty \n");
                $$ = nil_expr;
                };

objectdef:  left_bracket elist right_bracket {
                //printf("objectdef->[elist]\n");
                expr* t = newExpr(newtable_e);
                t->sym = new_tmp(yylineno);
                emit(tablecreate,NULL,NULL, t);
                double i =0;

                expr* x = $elist;
                reverse_list(&x);
                while(x!=NULL && x->type != nil_e){
                    emit(tablesetelem, newExpr_constNum(i++), x, t);
                    x = x->next;
                }
                $objectdef = t;
                }
            |left_bracket indexed right_bracket {
                assert($indexed);
                //printf("objectdef->[indexed] \n");
                expr* t = newExpr(newtable_e);
                t->sym = new_tmp(yylineno);
                emit(tablecreate,NULL,NULL, t);
                double i =0;

                expr* x = $indexed;
                reverse_list(&x);
                while(x!=NULL &&x->type != nil_e){
                    emit(tablesetelem, x->index, x, t);
                    x = x->next;
                }
                $objectdef = t;
                
                
                };

indexedelem: left_curly expr colon expr right_curly {
                //printf("indexedelem->{expr:expr} \n");
                assert($2 && $4);
                $indexedelem = $4;
                $indexedelem->index = $2;
            }; 

indexed:    indexedelem {
                //printf("indexed->indexedelem \n");
                assert($indexedelem);
                $$ = $indexedelem;
            } 
            | indexed comma indexedelem {
                //printf("indexed->indexed,indexedelem \n");
                $3->next = $1;
                $$ = $3;
            };

block_l:    block_l stmt {
                $$ = merge($1, $2);
            }
            |/*empty*/{
                $block_l = new stmt_l();
            };

block:      left_curly { 
                //printf("\n\n-----enter block ------ \n"); 
                } 
            block_l right_curly { 
                    //printf("\n-----exit block ------\n\n"); 
                    $$ = $block_l;
                symbol_table.hide(scope--);};

func_name:  id {
                //printf("func_name->func_id \n");
                $func_name = strdup(yylval.stringValue);
                }  //lookup
            | /*empty*/{
                //printf("func_name->anonymous \n");
                std::ostringstream ss;
                ss << "$anonymous" << anonymous_count;
                anonymous_count++;

                $func_name = strdup(ss.str().c_str());
                }; //probably insert with $_name(anonymous)

funcdef:  N func_prefix func_args func_body {
                //printf("funcdef->prefix args body\n");
                exitScopeSpace();
                $func_prefix->value.funcVal->totalLocals  = functionLocalOffset;
                functionLocalOffset = functionLocalsStack.top();
                functionLocalsStack.pop();
                $funcdef = $func_prefix;
                emit_function(funcend, lvalue_expr($func_prefix));
                patchLabel($N, nextQuadLabel());
                if($func_body != NULL ){
                    patchLabel($func_body->breaklist, nextQuadLabel() - 1);
                }
            };

func_prefix:  function func_name  { 
                //printf("funcdef->( ");
                last_func.push(scope); 
                
                $func_prefix =symbol_table.lookUp_curscope($func_name); 
                if($func_prefix ==  NULL) {//undefined
                    $func_prefix = symbol_table.insert($func_name, yylineno, USERFUNC);
                    $func_prefix->value.funcVal->iaddress = nextQuadLabel();
                    emit_function(funcstart,lvalue_expr($func_prefix));
                }else{
                    switch( $func_prefix->type ){
                        case LIBFUNC:{
                            yyerror("shadowing of library functions not allowed");
                            break;
                        }
                        case USERFUNC:{
                            yyerror("function name already used as func");// error: var redefined as a function
                            break;
                        }
                        case GLOBAL :{}
                        case FORMAL :{}
                        case LOCAL :{
                            yyerror("function name already used as var");// error: var redefined as a function
                            break;
                        } 
                    }
                }
                
                functionLocalsStack.push(functionLocalOffset);
                enterScopeSpace();
                resetFormalArgsOffset();
                scope++; 
                }; 

func_args:  left_parenthesis idlist right_parenthesis {
                    //printf("funcdef->) \n");
                    enterScopeSpace();
                    resetFunctionLocalOffset();
                };

func_body:  func_block_start block func_block_end { 
                    last_func.pop(); 
                    exitScopeSpace();
                    $$ = $block;
                    };

number:     integer     {
                // std::string name = "^" + yylval.intValue;
                // SymbolTableEntry *tmp = symbol_table.lookUp_allscope(name.c_str()); 
                // if(tmp == NULL){
                //     tmp = symbol_table.insert(name.c_str(), 0, LOCAL, 0);
                //     assert(tmp);
                //     tmp->space = currScopeSpace();
                //     tmp->offset = currScopeOffset();
                //     inCurrScopeOffset();
                // }
                // $$ = newExpr(constnum_e);
                // $$->sym = tmp;
                // $$->numConst = yylval.intValue;
                //printf("number->integer \n");
                $$ = newExpr(constnum_e);
                $$->numConst = (double)yylval.intValue;
                }
            | real      {
                //printf("number->real \n");
                $$ = newExpr(constnum_e);
                $$->numConst = yylval.realValue;
                };

const:      number      {$$ = $1; //printf("const->number \n");
                }
            | STRING    {
                //printf("const->string \n");
                $$ = newExpr(conststring_e);
                $$->strConst = strdup(yylval.stringValue);
            }
            | NIL       {
                //printf("const->nil \n");
                $$ = nil_expr;
                $$->boolConst = false;
            }
            | TRUE      {
                //printf("const->true \n");
                $$ = newExpr(constbool_e);
                $$->boolConst = true;
            }
            | FALSE     {
                //printf("const->false \n");
                $$ = newExpr(constbool_e);
                $$->boolConst = false;
            };

//idlist {//printf("'id'");} can be empty
idlist_l:   id {  
                // //printf("idlist_l->id1 \n");
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, FORMAL);
                    assert(tmp);
                    // $idlist_l = newExpr(var_e);
                    // $idlist_l->sym = tmp;
                }else{
                    switch( tmp->type ){
                        case GLOBAL:{} 
                        case FORMAL:{} //first arguement can only be global
                        case LOCAL:{
                            yyerror("variable redefined in same scope");
                            break;
                        }
                        case LIBFUNC:{
                            yyerror("formal arguement trying to shadow library func");
                            break;
                        }
                        default:{
                            yyerror("unknown error occured"); 
                        }
                    }
                }
            
            } //lookup
            |idlist_l comma id {
                // //printf("idlist_l->idlist_l , id \n");
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, FORMAL);
                    assert(tmp);
                    // expr* e = newExpr(var_e);
                    // e->sym = tmp;
                    // e->next = $idlist_l;
                    // $idlist_l = e;
                }else{
                    switch( tmp->type ){
                        case GLOBAL:{}
                        case FORMAL:{}
                        case LOCAL:{
                            yyerror("variable redefined in same scope");
                            break; 
                        }
                        case LIBFUNC:{
                            yyerror("formal arguement trying to shadow library func");
                            break;
                        }
                        default:{
                            yyerror("unknown error occured"); 
                        }
                    }
                }
            };

idlist:     idlist_l { /* printf("idlist->idlist_l \n")*/;} 
            | /*empty*/ {/*printf("idlist->emptyidlist \n");*/};

ifprefix:   IF left_parenthesis expr right_parenthesis {
                if(!$expr->truelist.empty() && !$expr->falselist.empty()){
                    patchLabel($expr->truelist,nextQuadLabel()+2);
                    patchLabel($expr->falselist,nextQuadLabel()+1);
                }
                emit(if_eq, $expr, newExpr_constBool(true), NULL, nextQuadLabel()+2);
                $ifprefix = (int)nextQuadLabel();
                emit(jump);
                //maybe patch expression
            };

elseprefix: ELSE {
                $elseprefix = (int)nextQuadLabel();
                emit(jump);
            };

ifstmt:     ifprefix stmt elseprefix stmt { 
                // //printf("ifstmt->\"if(expr) stmt else stmt\" \n"); 
                patchLabel($ifprefix, $elseprefix + 1);
                patchLabel((unsigned int)$elseprefix, nextQuadLabel());
            } 
            | ifprefix stmt { 
                // //printf("ifstmt->\"if(expr) stmt\" \n");
                patchLabel((unsigned int)$ifprefix, nextQuadLabel());
            };
whilestart: WHILE {
                $whilestart = (int)nextQuadLabel();   
            };

whilecond:  left_parenthesis expr right_parenthesis {
                if(!$expr->truelist.empty() && !$expr->falselist.empty()){
                    patchLabel($expr->truelist,nextQuadLabel()+2);
                    patchLabel($expr->falselist,nextQuadLabel()+1);
                }
                emit(if_eq, $expr, newExpr_constBool(true), NULL, nextQuadLabel()+2);
                $whilecond = (int)nextQuadLabel();
                emit(jump);
            };

whilestmt:  whilestart whilecond loopstmt {
                // //printf("whilestmt->\"while(expr) stmt else stmt\" \n");
                emit(jump,NULL, NULL, NULL, $whilestart);
                patchLabel((unsigned int)$whilecond, nextQuadLabel());
                if($loopstmt!=NULL){
                    patchLabel($loopstmt->breaklist, nextQuadLabel());
                    patchLabel($loopstmt->contlist,  $whilestart);
                }
            };
N:  {
        $N = (int)nextQuadLabel();
        emit(jump);
    };

M:  {
        $M = (int)nextQuadLabel();
    };

forprefix:  FOR left_parenthesis elist semicolon M expr semicolon{
                if(!$expr->truelist.empty() && !$expr->falselist.empty()){
                    patchLabel($expr->truelist,nextQuadLabel()+2);
                    patchLabel($expr->falselist,nextQuadLabel()+1);
                }
                $forprefix = new for_prefix();
                $forprefix->test = $M;
                $forprefix->enter = (int)nextQuadLabel();
                emit(if_eq, $expr, newExpr_constBool(true));
            };

forstmt:    forprefix N elist right_parenthesis N loopstmt N  { 
                // //printf("forstmt->\"for(elist; expr; elist)\" \n");
                assert($forprefix);
                // //printf("\n%d %d\n", $forprefix->enter, $5);
                patchLabel($forprefix->enter, $5 + 1);
                patchLabel($2, nextQuadLabel());
                patchLabel($5, (unsigned int)$forprefix->test);
                patchLabel($7, (unsigned int)($2 + 1));
                if($loopstmt != NULL ){
                    patchLabel($loopstmt->breaklist, nextQuadLabel());
                    patchLabel($loopstmt->contlist, $2 + 1);
                }
            };

loopstart:  { ++loopcnt; };

loopend:    { --loopcnt; };

loopstmt:   loopstart stmt loopend {
                $loopstmt = $stmt;
            };

func_block_start:   {
                        loopcntStack.push(loopcnt);
                        loopcnt = 0;
                    };

func_block_end:   {
                        loopcntStack.pop();
                    };

returnstmt: RETURN {
                return_flag = true;
                if(last_func.top() == -1) yyerror("Use of \"Return\"outside of function");
            } expr semicolon {
                // //printf("returnstmt=>\"return expr;\" \n");
                return_flag = false; 
                emit(ret, NULL, NULL, $expr);  
                $returnstmt = new stmt_l();
                $returnstmt->breaklist = newList(nextQuadLabel()); 
                emit(jump);
            }
            | RETURN { 
                // //printf("returnstmt->return; \n");
                if(last_func.top() == -1) yyerror("Use of \"Return\"outside of function");
                emit(ret);
            } semicolon {
                $returnstmt = new stmt_l();
                $returnstmt->breaklist = newList(nextQuadLabel()); 
                emit(jump);
            };

%%

int yyerror(char* yaccProvidedMessage){
    printQuads();
    printf("\033[1;31m");
    printf("\n_____________ERROR:line %d, before token: \"%s\" message: %s____________\n"
        ,yylineno,yytext,yaccProvidedMessage);
        printf("\033[0m");
    error = true;
   
}

int main(int argc, char* argv[]){
    
    FILE* fp;
    
    if( !( fp = fopen(argv[1],"r") ) ){

        //printf("An error occured while openning the file\n");
        exit(-1);
    }
    yyin = fp;
    yyparse();
    
    //symbol_table.print();
    printQuads();
    if(!error){
        generateAll();
        printInstructions();
        printf("\nWe fucking did it\nYAAAAAAAAAAAAAAAAAS\n");
    }
    return 0;
}
