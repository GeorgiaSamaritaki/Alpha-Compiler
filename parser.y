%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "quad.hpp"

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
    // iopcode op;
}

%type <exprNode> lvalue 
%type <exprNode> tableitem 
%type <exprNode> primary 
%type <exprNode> assignexpr 
%type <exprNode> expr 
%type <exprNode> elist 
%type <exprNode> elist_l 
%type <exprNode> call 
%type <exprNode> objectdef 
%type <exprNode> indexedelem 
%type <exprNode> indexed 
%type <exprNode> term 
%type <exprNode> arithexpr 
%type <exprNode> relexpr 
%type <exprNode> boolexpr 

%type <call_l> methodcall 
%type <call_l> normcall 
%type <call_l> callsuffix 

%type <stringValue> func_name
%type <intValue> func_body
%type <symbol> func_prefix
%type <symbol> funcdef

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


program:    statements;

stmt:       expr         {printf("stmt->expr");      }  semicolon {printf("';' \n");}
            | ifstmt     {printf("stmt->ifstmt \n\n");    }
            | whilestmt  {printf("stmt->whilestmt\n\n");  }
            | forstmt    {printf("stmt->forstmt   \n\n");   }
            | returnstmt {printf("stmt->returnstmt \n\n");}
            | { scope++;} block {printf("stmt->block2");} //maybe scope++ after left curly
            | funcdef    {printf("stmt->funcdef ");   }
            | BREAK      {printf("stmt->Break ");     } semicolon {printf(" ';'  \n\n");}
            | CONTINUE   {printf("stmt->Continue");  } semicolon {printf(" ';'  \n\n");}
            | semicolon  {printf("';' \n\n");       };
            

statements: statements   stmt 
            | /*empty*/ ;

expr:       assignexpr    { $$ = $1; printf("expr->assignexpr \n");}
            |  boolexpr   { $$ = $1; }
            |  arithexpr  { $$ = $1;}
            |  relexpr    { $$ = $1;}
            | term        { assert($1); $$ = $1; printf("expr->term \n");};     

arithexpr:     expr plus expr     { 
                if($1->type != constnum_e && $3->type !=constnum_e && 
                    $3->type != arithexpr_e && $1->type != arithexpr_e ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = NULL;
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
                printf("opexr->expr+expr \n");
            }  
            | expr minus expr   { 
                if($1->type != constnum_e && $3->type !=constnum_e && 
                    $3->type != arithexpr_e && $1->type != arithexpr_e ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = NULL;
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
                printf("opexr->expr-expr \n");
            }
            | expr mul expr      {
                if($1->type != constnum_e && $3->type !=constnum_e && 
                    $3->type != arithexpr_e && $1->type != arithexpr_e ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = NULL;
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
             printf("opexr->expr*expr \n");
            }
            | expr division expr { 
                if($1->type != constnum_e && $3->type !=constnum_e && 
                    $3->type != arithexpr_e && $1->type != arithexpr_e ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = NULL;
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
                printf("opexr->expr/expr \n");
            }
            | expr mod expr      { 
                if($1->type != constnum_e && $3->type !=constnum_e && 
                    $3->type != arithexpr_e && $1->type != arithexpr_e ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = NULL;
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
                    printf("expr->expr arithexpr expr \n");
                }printf("opexr->expr\%expr \n");};

relexpr:      expr b_greater expr      {
                if($1->type != constnum_e && $3->type !=constnum_e && 
                    $3->type != arithexpr_e && $1->type != arithexpr_e ){
                    yyerror("Invalid arithmetic expressions");
                    $$ = NULL;
                }else {
                    if($1->type == constbool_e && $3->type == constbool_e){
                        $$ = newExpr(constbool_e);
                        $$->sym = new_tmp(yylineno);
                        $$->boolConst = compute_rel(if_greater, $1->numConst , $3->numConst);
                    }else{
                        $$ = newExpr(boolexpr_e);
                        $$->sym = new_tmp(yylineno);
                        emit(
                            if_greater, $1 , $3, $$, nextQuadLabel()+3);
                        emit(
                            assign_op, newExpr_constBool(0), NULL, $$);
                        emit(
                            jump, NULL, NULL, $$, nextQuadLabel()+2);
                        emit(
                            assign_op, newExpr_constBool(1), NULL, $$);
                    }
                }
                printf("opexr->expr>expr \n");
        }
        |  expr b_less expr         {
            if($1->type != constnum_e && $3->type !=constnum_e && 
                $3->type != arithexpr_e && $1->type != arithexpr_e ){
                yyerror("Invalid arithmetic expressions");
                $$ = NULL;
            }else {
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_less, $1->numConst , $3->numConst);
                }else{
                    $$ = newExpr(boolexpr_e);
                    $$->sym = new_tmp(yylineno);
                    emit(
                        if_less, $1 , $3, $$, nextQuadLabel()+3);
                    emit(
                        assign_op, newExpr_constBool(0), NULL, $$);
                    emit(
                        jump, NULL, NULL, $$, nextQuadLabel()+2);
                    emit(
                        assign_op, newExpr_constBool(1), NULL, $$);
                }
            }
            printf("opexr->expr<expr \n");
        }         
        |  expr b_greater_eq expr   {
            if($1->type != constnum_e && $3->type !=constnum_e && 
                $3->type != arithexpr_e && $1->type != arithexpr_e ){
                yyerror("Invalid arithmetic expressions");
                $$ = NULL;
            }else {
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_greater_eq, $1->numConst , $3->numConst);
                }else{
                    $$ = newExpr(boolexpr_e);
                    $$->sym = new_tmp(yylineno);
                    emit(
                        if_greater_eq, $1 , $3, $$, nextQuadLabel()+3);
                    emit(
                        assign_op, newExpr_constBool(0), NULL, $$);
                    emit(
                        jump, NULL, NULL, $$, nextQuadLabel()+2);
                    emit(
                        assign_op, newExpr_constBool(1), NULL, $$);
                }
                printf("opexr->expr>=expr \n");
            }
        }
        |  expr b_less_eq expr      {
            if($1->type != constnum_e && $3->type !=constnum_e && 
                $3->type != arithexpr_e && $1->type != arithexpr_e ){
                yyerror("Invalid arithmetic expressions");
                $$ = NULL;
            }else {
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_lesseq, $1->numConst , $3->numConst);
                }else{
                    $$ = newExpr(boolexpr_e);
                    $$->sym = new_tmp(yylineno);
                    emit(
                        if_lesseq, $1 , $3, $$, nextQuadLabel()+3);
                    emit(
                        assign_op, newExpr_constBool(0), NULL, $$);
                    emit(
                        jump, NULL, NULL, $$, nextQuadLabel()+2);
                    emit(
                        assign_op, newExpr_constBool(1), NULL, $$);
                }
                printf("opexr->expr<=expr \n");}
            }       
        |  expr b_equals expr       {
            if(!is_same($1->type,$3->type) ){
                yyerror("Invalid operands to boolean expression");
                $$ = NULL;
            }else {
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute(if_eq, (bool)$1->boolConst ,(bool) $3->boolConst);
                }else if($1->type == constnum_e && $3->type == constnum_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_eq, $1->numConst , $3->numConst);
                }else if(($1->type == newtable_e || $3->type == nil_e) && 
                                ($3->type == newtable_e || $1->type == nil_e)){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = false;
                } else{
                    $$ = newExpr(boolexpr_e);
                    $$->sym = new_tmp(yylineno);
                    emit(
                        if_eq, $1 , $3, $$, nextQuadLabel()+3);
                    emit(
                        assign_op, newExpr_constBool(0), NULL, $$);
                    emit(
                        jump, NULL, NULL, $$, nextQuadLabel()+2);
                    emit(
                        assign_op, newExpr_constBool(1), NULL, $$);
                }
                printf("opexr->expr==expr \n");}
            }            
        |  expr b_not_equal expr    {
            if(!is_same($1->type,$3->type) ){
                yyerror("Invalid operands to boolean expression");
                $$ = NULL;
            }else {
                if($1->type == constbool_e && $3->type == constbool_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute(if_noteq, (bool)$1->boolConst ,(bool)$3->boolConst);
                }else if($1->type == constnum_e && $3->type == constnum_e){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = compute_rel(if_noteq, $1->numConst , $3->numConst);
                }else if(($1->type == newtable_e || $3->type == nil_e) && 
                                ($3->type == newtable_e || $1->type == nil_e)){
                    $$ = newExpr(constbool_e);
                    $$->sym = new_tmp(yylineno);
                    $$->boolConst = true;
                } else{
                    $$ = newExpr(boolexpr_e);
                    $$->sym = new_tmp(yylineno);
                    emit(
                        if_noteq, $1 , $3, $$, nextQuadLabel()+3);
                    emit(
                        assign_op, newExpr_constBool(0), NULL, $$);
                    emit(
                        jump, NULL, NULL, $$, nextQuadLabel()+2);
                    emit(
                        assign_op, newExpr_constBool(1), NULL, $$);
                }
                printf("opexr->expr!=expr \n");
            }};

boolexpr:      expr AND expr  {
                $$ = newExpr(boolexpr_e);
                $$->sym = new_tmp(yylineno);
                $$->boolConst = false;
                emit(and_op, $1 , $3, $$);
                printf("opexr->expr&&expr \n");
                    
                }
            | expr OR  expr           {      
                $$ = newExpr(boolexpr_e);
                $$->sym = new_tmp(yylineno);
                $$->boolConst = true;
                emit(or_op, $1 , $3, $$);
                printf("opexr->expr || expr \n");
                    
                } ;

term:       left_parenthesis expr  right_parenthesis {
                $term = $expr;
                printf("term->(expr) \n");
                } 
            | NOT expr {
                $term = newExpr(boolexpr_e);
                $term->sym = new_tmp(yylineno);
                emit(not_op, $expr,NULL, $term);
                printf("term->NOTexpr \n");
                }
            | minus expr %prec uminus {
                checkUminus($expr);
                $term = newExpr(arithexpr_e);
                $term->sym = new_tmp(yylineno);
                emit(uminus_op, $expr, NULL , $term);
                printf("term->-expr \n");
                }
            | increment {printf("term->++lvalue \n");} lvalue { 
                if($3 != NULL) {
                    if( (int)symbol_table.get_scope($3->sym) <= last_func.top() && (int)symbol_table.get_scope($3->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        $$ = NULL;
                    }else {
                        if($3->sym->type == USERFUNC) 
                            yyerror("Cannot increment function");
                        else if ($3->sym->type == LIBFUNC) 
                            yyerror("Cannot increment libfunc");
                        else{
                            if ($lvalue->type == tableitem_e) {
                                $term = emit_ifTableItem($lvalue);
                                emit(
                                    add, $term, newExpr_constNum(1), $term);
                                emit( tablesetelem, $lvalue, $lvalue->index, $term);
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
                    yyerror("variable undefined");
                }
            } //lookup maybe
            | lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top() && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else{
                        if($1->sym->type == USERFUNC) yyerror("Cannot increment function");
                        else if($1->sym->type == LIBFUNC) yyerror("Cannot increment libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                } 
            } increment {
                assert($1); //FIXME:check scopes
                $term = newExpr(var_e);
                $term->sym = new_tmp(yylineno);

                if( $lvalue->type == tableitem_e ) {
                    expr* value = emit_ifTableItem($lvalue);
                    emit(assign_op, value, NULL, $term);
                    emit(
                        add, value, newExpr_constNum(1), value);
                    emit( tablesetelem, $lvalue, $lvalue->index, value);
                }else {
                    emit(assign_op, $lvalue, NULL, $term);
                    emit(
                        add, $lvalue, newExpr_constNum(1), $lvalue);
                }
                printf("term->lvalue++ \n");
                
                } 
            | decrement {printf("term->--lvalue \n");} lvalue {
                if($3 != NULL) {
                    if( (int)symbol_table.get_scope($3->sym) <= last_func.top() && (int)symbol_table.get_scope($3->sym)!=0 ){
                        yyerror("Cant reference variable out of scope");
                    }else {   
                        if($3->sym->type == USERFUNC) yyerror("Cannot increment function");
                        else if($3->sym->type == LIBFUNC) yyerror("Cannot increment libfunc");
                        else{
                            if ($lvalue->type == tableitem_e) {
                                $term = emit_ifTableItem($lvalue);   
                                emit(
                                    sub, $term, newExpr_constNum(1), $term);
                                emit( tablesetelem, $lvalue, $lvalue->index, $term);
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
                }
                } //lookup maybe
            | lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top() && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else {   
                        if($1->sym->type == USERFUNC) yyerror("Cannot increment function");
                        else if($1->sym->type == LIBFUNC) yyerror("Cannot increment libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
                } decrement {
                    assert($1);
                    if ($lvalue->type == tableitem_e) {
                        $term = emit_ifTableItem($lvalue);   
                        emit(
                            sub, $term, newExpr_constNum(1), $term);
                        emit( tablesetelem, $lvalue, $lvalue->index, $term);
                    } else {
                        emit(
                            sub, $lvalue, newExpr_constNum(1), $lvalue);
                        $term = newExpr(arithexpr_e);
                        $term->sym = new_tmp(yylineno);
                        emit(assign_op, $lvalue, NULL, $term);
                    }
                    printf("term->lvalue-- \n");
                    }  //lookup maybe (before --?)
            | primary {
                $term = $primary;
                printf("term->primary \n");
                };

assignexpr: lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top() && (int)symbol_table.get_scope($1->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else { 
                        if($1->sym->type == USERFUNC) yyerror("Cannot assign to function");
                        else if($1->sym->type == LIBFUNC) yyerror("Cannot assign to libfunc");
                    }
                }else{ //define as new var
                    printf("inserting in assign\n");
                    symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                }    
                } assign expr {
                    if($lvalue->type == tableitem_e){
                        emit( //that is lvalue[index] = expr;
                            tablesetelem,
                            $lvalue,
                            $lvalue->index,
                            $expr
                        );
                        //The value f the assignment expression should be gained
                        $assignexpr = emit_ifTableItem($lvalue);
                        $assignexpr->type = assignexpr_e;
                    }else{
                        emit(
                            assign_op,
                            $expr,
                            (expr*) 0,
                            $lvalue
                        );
                        $assignexpr = newExpr(assignexpr_e);
                        $assignexpr->sym = new_tmp(yylineno);
                        emit(
                            assign_op, 
                            $lvalue,
                            (expr*)0, $assignexpr);
                    }
                    printf("assignexpr->lvalue=expr \n");
                    }; //lookup (before assign?)

primary:    lvalue  {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  && (int)symbol_table.get_scope($1->sym)!=0){
                            yyerror("Cant reference variable out of scope");
                    }
                }else{ //define as new var
                    if(return_flag) yyerror("return values undefined in this scope");
                    else symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                }
                $primary = emit_ifTableItem($lvalue);
                printf("primary->lvalue \n");
                }
            | call  {
                printf("primary->call \n");
                }
            | objectdef{ 
                printf("primary->objectdef \n");
                }
            | left_parenthesis funcdef right_parenthesis {
                $primary = newExpr(programfunc_e);
                $primary->sym = $funcdef;                
                printf("primary->(funcdef) \n");
                }
            | const {printf("primary->const \n");}
            ;

lvalue:     id {
                printf("lvalue->ids '%s'\n",yylval.stringValue);
                SymbolTableEntry *tmp = symbol_table.lookUp_allscope(yylval.stringValue); 
                if(tmp == NULL){
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    assert(tmp);
                    tmp->space = currScopeSpace();
                    tmp->offset = currScopeOffset();
                    inCurrScopeOffset();
                }
                $lvalue = lvalue_expr(tmp);

            } //lookup
            | local id {     
                printf("local id %s \n",yylval.stringValue);
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

                printf("lvalue->::id \n");
                }
            | member { $$ = NULL; printf("lvalue->member \n");}; 

member:     tableitem
            | call dot id {printf("member->call().id \n");}
            | call left_bracket expr right_bracket {printf("member->[expr] \n");};

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
                    printf("member->lvalue.id \n");
                    $tableitem = member_item($lvalue, yylval.stringValue);
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
                printf("member->lvalue[expr] \n");
                }
            ;

call:       call left_parenthesis elist right_parenthesis {
                $$ = make_call($$, $elist);
                printf("call->call(elist) \n");
                }
            | lvalue{
                if($1 != NULL) {
                    if($1->sym->type == USERFUNC || $1->sym->type == LIBFUNC) { } 
                    else{
                        if( (int)symbol_table.get_scope($1->sym) <= last_func.top()  && (int)symbol_table.get_scope($1->sym)!=0){
                            yyerror("Cant reference variable out of scope");
                        }else if($1->sym->type == LOCAL||$1->sym->type == GLOBAL) {
                            yyerror("cant use variable as function");
                        }
                    }
                }else{ //define as new var
                    yyerror("function not found");
                }
            } callsuffix {

                if($callsuffix->method){
                    expr* self = $lvalue;
                    $lvalue = emit_ifTableItem(member_item(self,$callsuffix->name));
                    self->next = $callsuffix->elist;
                    $callsuffix->elist = self;   
                }
                $call = make_call($lvalue, $callsuffix->elist);
                printf("call->lvaluecallsuffix \n");
                } 
            | left_parenthesis funcdef right_parenthesis left_parenthesis elist right_parenthesis {
                expr* func = newExpr(programfunc_e);
                func->sym = $funcdef;
                $call = make_call(func, $elist);
                printf("call->(funcdef)(elist) \n");
                };

callsuffix: normcall  {    
                $callsuffix = $normcall; 
                printf("callsuffix->normcall \n");
            }
            | methodcall { 
                $callsuffix = $methodcall; 
                printf("callsuffix->methodcall \n");
            };

normcall:   left_parenthesis elist right_parenthesis {
                $normcall->elist = $elist;
                $normcall->method = false;
                $normcall->name = NULL;

                printf("normcall->(elist) \n");
                };

methodcall: double_dot id {//maybe needs code
            }left_parenthesis elist right_parenthesis {
                $methodcall->elist = $elist;
                $methodcall->method = true;
                $methodcall->name = yylval.stringValue;

                printf("methodcall->..id(elist) \n");
                }; 

elist_l:    expr {
                $expr->next = $elist_l;
                $$ = $expr;
                printf("elist_l->expr \n");
                }
            | elist_l comma expr {
                $expr->next = $1;
                $$ = $expr;
                printf("elist_l->elist_l,expr \n");
                };

elist:      elist_l {
                $elist  = $elist_l;
                printf("elist->elist_l \n");
                }
            |/*empty*/  {
                $elist = NULL;
                printf("elist->empty \n");
                };

objectdef:  left_bracket elist right_bracket {
                expr* t = newExpr(newtable_e);
                t->sym = new_tmp(yylineno);
                emit(tablecreate,NULL,NULL, t);
                double i =0;

                expr* x = $elist;
                while(x){
                    emit(tablesetelem, t, newExpr_constNum(i++), x);
                    x = x->next;
                }
                $objectdef = t;
                printf("objectdef->[elist]\n");
                }
            |left_bracket indexed right_bracket {
                expr* t = newExpr(newtable_e);
                t->sym = new_tmp(yylineno);
                emit(tablecreate,NULL,NULL, t);
                double i =0;

                expr* x = $indexed;
                while(x){
                    emit(tablesetelem, t, x->index, x);
                    x = x->next;
                }
                $objectdef = t;
                
                printf("objectdef->[indexed] \n");
                
                };

indexedelem: left_curly expr colon expr right_curly {
                $indexedelem = $4;
                $indexedelem->index = $2;
                $indexedelem -> next = NULL;
                printf("indexedelem->{expr:expr} \n");
            }; 
            // { expr {printf("expr");} : expr {printf("expr");}}

indexed:    indexedelem {
                $$ = $indexedelem;
                printf("indexed->indexedelem \n");
            } 
            | indexed comma indexedelem {
                $1->next = $3;
                printf("indexed->indexed,indexedelem \n");
            };

block_l:    block_l stmt
            |/*empty*/;

block:      left_curly { printf("\n\n-----enter block ------ \n"); } block_l right_curly { printf("\n-----exit block ------\n\n"); symbol_table.hide(scope--);};

func_name:  id {
                $func_name = strdup(yylval.stringValue);
                printf("func_name->func_id \n");
                }  //lookup
            | /*empty*/{
                char name[100]; 
                sprintf(name, "%s%d","$anonymous",  anonymous_count );
                anonymous_count++;
                $func_name = strdup(name);
                }; //probably insert with $_name(anonymous)

funcdef:  func_prefix func_args func_body{
                exitScopeSpace();
                $func_prefix->value.funcVal->totalLocals  = functionLocalOffset;
                functionLocalOffset = functionLocalsStack.top();
                functionLocalsStack.pop();
                $funcdef = $func_prefix;
                emit_function(funcend, lvalue_expr($func_prefix));
            };

func_prefix:  function func_name  { 
                last_func.push(scope); 
                scope++; 
                
                $func_prefix =symbol_table.lookUp_curscope($func_name); 
                if($func_prefix ==  NULL) {//undefined
                    symbol_table.insert($func_name, yylineno, USERFUNC);
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
                
                $func_prefix->value.funcVal->iaddress = nextQuadLabel();
                emit_function(funcstart,lvalue_expr($func_prefix));
                functionLocalsStack.push(functionLocalOffset);
                enterScopeSpace();
                resetFormalArgsOffset();
                printf("funcdef->( ");
                }; 

func_args:  left_parenthesis idlist right_parenthesis {
                    printf("funcdef->) \n");
                    enterScopeSpace();
                    resetFunctionLocalOffset();
                };

func_body:  block { 
                    last_func.pop(); 

                    exitScopeSpace();
                    };

number:     integer     {printf("number->int \n");}
            | real      {printf("number->real \n");};

const:      number      {printf("const->number \n");}
            | STRING    {printf("const->string \n");}
            | NIL       {printf("const->nil \n");}
            | TRUE      {printf("const->true \n");}
            | FALSE     {printf("const->false \n");};

//idlist {printf("'id'");} can be empty
idlist_l:   id {  
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    symbol_table.insert(yylval.stringValue, yylineno, FORMAL);
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
                printf("idlist_l->id1 \n");
            
            } //lookup
            |idlist_l comma id {
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    symbol_table.insert(yylval.stringValue, yylineno, FORMAL);
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

idlist:     idlist_l {printf("idlist->idlist_l \n");} 
            | /*empty*/ {printf("idlist->emptyidlist \n");};

ifstmt:     IF left_parenthesis expr right_parenthesis  stmt ELSE stmt { printf("ifstmt->\"if(expr) stmt else stmt\" \n"); } 
            | IF left_parenthesis expr right_parenthesis stmt { printf("ifstmt->\"if(expr) stmt\" \n");};

whilestmt:  WHILE left_parenthesis expr right_parenthesis stmt {printf("whilestmt->\"while(expr) stmt else stmt\" \n");};

forstmt:    FOR left_parenthesis elist semicolon expr semicolon elist right_parenthesis {printf("forstmt->\"for(elist; expr; elist)\" \n");} stmt;

returnstmt: RETURN {
                return_flag = true;
                if(last_func.top() == -1) yyerror("return statement without function");
            }expr semicolon {return_flag = false; printf("returnstmt=>\"return expr;\" \n");}
            | RETURN{ 
                if(last_func.top() == -1) yyerror("return statement without function");
            }semicolon {printf("returnstmt->return; \n");};
%%

int yyerror(char* yaccProvidedMessage){
    printf("\033[1;31m");
    printf("\n_____________ERROR:line %d, before token: \"%s\" message: %s____________\n"
        ,yylineno,yytext,yaccProvidedMessage);
        printf("\033[0m");
   
}

int main(int argc, char* argv[]){
    
    FILE* fp;
    
    if( !( fp = fopen(argv[1],"r") ) ){

        printf("An error occured while openning the file\n");
        exit(-1);
    }
    yyin = fp;
    yyparse();
    
    symbol_table.print();
    return 0;
}
