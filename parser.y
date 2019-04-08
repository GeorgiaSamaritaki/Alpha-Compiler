%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "symtable.hpp"
    #include "quad.hpp"

    int yyerror(char* yaccProvidedMessage);
    int yylex(void);

    SymTable symbol_table = *new SymTable();


    extern int yylineno;
    extern char* yytext;
    extern FILE* yyin;


%}

%defines

%union { 
    char* stringValue; 
    int intValue;   
    double realValue; 
    struct SymbolTableEntry* exprNode;
}
%type <exprNode> lvalue

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

expr:       assignexpr  {printf("expr->assignexpr \n");}
            | opexpr    {printf("expr->opexpr \n");}
            | term      {printf("expr->term \n");};     

opexpr:       expr plus         expr {printf("opexr->expr+expr \n");}  
            | expr minus        expr {printf("opexr->expr-expr \n");}
            | expr mul          expr {printf("opexr->expr*expr \n");}
            | expr division     expr {printf("opexr->expr/expr \n");}
            | expr mod          expr {printf("opexr->expr\%expr \n");}
            | expr b_greater    expr {printf("opexr->expr>expr \n");}
            | expr b_less       expr {printf("opexr->expr<expr \n");}
            | expr b_greater_eq expr {printf("opexr->expr>=expr \n");}
            | expr b_less_eq    expr {printf("opexr->expr<=expr \n");}
            | expr b_equals     expr {printf("opexr->expr==expr \n");}
            | expr b_not_equal  expr {printf("opexr->expr!=expr \n");}
            | expr AND          expr {printf("opexr->expr&&expr \n");}
            | expr OR           expr {printf("opexr->expr||expr \n");} ;

term:       left_parenthesis expr  right_parenthesis {printf("term->(expr) \n");} 
            | NOT expr {
                printf("term->NOTexpr \n");
                }
            | minus expr %prec uminus {
                printf("term->-expr \n");
                }
            | increment {printf("term->++lvalue \n");} lvalue { 
                if($3 != NULL) {
                    if( (int)symbol_table.get_scope($3) <= last_func.top() && (int)symbol_table.get_scope($3)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else {
                        if($3->type == USERFUNC) yyerror("Cannot increment function");
                        else if($3->type == LIBFUNC) yyerror("Cannot increment libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
            } //lookup maybe
            | lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top() && (int)symbol_table.get_scope($1)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else{
                        if($1->type == USERFUNC) yyerror("Cannot increment function");
                        else if($1->type == LIBFUNC) yyerror("Cannot increment libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                } 
            } increment {printf("term->lvalue++ \n");} //lookup maybe(before ++?)
            | decrement {printf("term->--lvalue \n");} lvalue {
                if($3 != NULL) {
                    if( (int)symbol_table.get_scope($3) <= last_func.top() && (int)symbol_table.get_scope($3)!=0 ){
                        yyerror("Cant reference variable out of scope");
                    }else {   
                        if($3->type == USERFUNC) yyerror("Cannot increment function");
                        else if($3->type == LIBFUNC) yyerror("Cannot increment libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
                } //lookup maybe
            | lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top() && (int)symbol_table.get_scope($1)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else {   
                        if($1->type == USERFUNC) yyerror("Cannot increment function");
                        else if($1->type == LIBFUNC) yyerror("Cannot increment libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
                } decrement {printf("term->lvalue-- \n");}  //lookup maybe (before --?)
            | primary {printf("term->primary \n");};

assignexpr: lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top() && (int)symbol_table.get_scope($1)!=0){
                        yyerror("Cant reference variable out of scope");
                    }else { 
                        if($1->type == USERFUNC) yyerror("Cannot assign to function");
                        else if($1->type == LIBFUNC) yyerror("Cannot assign to libfunc");
                    }
                }else{ //define as new var
                printf("inserting in assign\n");
                    symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                }

                printf("assignexpr->lvalue \n");
                
                } assign expr {printf("assignexpr->=expr \n");}; //lookup (before assign?)

primary:    lvalue  {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top()  && (int)symbol_table.get_scope($1)!=0){
                            printf("fuck? %d %d\n",(int)symbol_table.get_scope($1),last_func.top() );
                            yyerror("Cant reference variable out of scope");
                    }
                }else{ //define as new var
                    if(return_flag) yyerror("return values undefined in this scope");
                    else symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                }
                //LVALUE
                // switch( symbol_table.lookUp_allscope(yylval.stringValue,(scope?LOCAL:GLOBAL)) ){
                //         case 2: { printf("Function Found"); break;}
                //         case 1: { printf("-Var found top: %d scope %d-",last_func.top()); break; }
                //         case -1: {
                //             break;
                //         } 
                //         case 0: { 
                //             printf("primary:");
                //             if(return_flag) yyerror("return values undefined in this scope");
                //             else symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                //          }  
                //     }
                printf("primary->lvalue \n");}
            | call  {printf("primary->call \n");}
            | objectdef     {printf("primary->objectdef \n");}
            | left_parenthesis funcdef right_parenthesis {printf("primary->(funcdef) \n");}
            | const {printf("primary->const \n");}
            ;

lvalue:     id {
                printf("lvalue->ids '%s'\n",yylval.stringValue);
                switch(symbol_table.lookUp_allscope(yylval.stringValue) ){
                    case 0:{
                        $$ = NULL;
                        break;
                        }
                    case 1:{
                        $$ = symbol_table.find_node(yylval.stringValue,LOCAL);
                        if($$ == NULL)
                            $$ = symbol_table.find_node(yylval.stringValue,GLOBAL);
                        break;}
                    case 2:{
                        $$ = symbol_table.find_node(yylval.stringValue,USERFUNC); break;}
                    case -2:{}
                    case -1:{
                        $$ = symbol_table.find_node(yylval.stringValue,LIBFUNC); break; }
                }
            } //lookup
            | local id {     
                printf("local id %s \n",yylval.stringValue);
                switch( symbol_table.lookUp_curscope(yylval.stringValue) ){
                    case 0: {//undefined
                        $$ = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                        break;
                    }
                    case 1:{
                        //symbol_table.change_value()
                        $$ = symbol_table.find_node(yylval.stringValue,LOCAL);
                        break;
                    } //defined as var
                    case 2:{
                        $$ = symbol_table.find_node(yylval.stringValue,USERFUNC);
                        break;
                    } //defined as fgunc
                    case -2:{}
                    case -1:{
                        $$ = symbol_table.find_node(yylval.stringValue,LIBFUNC);
                       yyerror("shadowing of library functions not allowed");
                    break;
                    }
                }
            }
            | double_colon id {
                unsigned int scope_tmp = scope;
                scope = 0; 

                switch( symbol_table.lookUp_curscope(yylval.stringValue)  ){
                    case 0: {//undefined
                        yyerror("global variable not found");
                        $$ = NULL;
                        break;
                    }
                    case 1:{//ok var found
                        //symbol_table.change_value()
                       $$ = symbol_table.find_node(yylval.stringValue,GLOBAL);
                       break;
                    } 
                    case 2:{//ok func found 
                        $$ = symbol_table.find_node(yylval.stringValue,USERFUNC);
                        break;
                    }
                    case -2:{}
                    case -1:{
                       $$ = symbol_table.find_node(yylval.stringValue,LIBFUNC);
                        break;
                    }
                    default:{
                        assert(false);
                    }
                }

                scope = scope_tmp;
                printf("lvalue->::id \n");
                }
            | member { $$ = NULL; printf("lvalue->member \n");}; 

member:     lvalue{
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top()  && (int)symbol_table.get_scope($1)!=0){
                        yyerror("Cant reference variable out of scope");
                    }
                    else {
                        if($1->type == USERFUNC) yyerror("cannot member function");
                        else if($1->type == LIBFUNC) yyerror("cannot member libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
                printf("idlist_l->id1+ \n");
            }dot id {printf("member->lvalue.id \n");}
            | lvalue {
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top()  && (int)symbol_table.get_scope($1)!=0){
                            yyerror("Cant reference variable out of scope");
                    }else{
                        if($1->type == USERFUNC) yyerror("cannot use function as array");
                        else if($1->type == LIBFUNC) yyerror("cannot use libfunc as array");
                    }
                }else{ //define as new var
                    yyerror("array undefined");
                }
            }left_bracket expr right_bracket {printf("member->lvalue[expr] \n");}
            | call dot id {printf("member->call().id \n");}
            | call left_bracket expr right_bracket {printf("member->[expr] \n");};

call:       call left_parenthesis elist right_parenthesis {printf("call->call(elist) \n");}
            | lvalue{
                if($1 != NULL) {
                    if( (int)symbol_table.get_scope($1) <= last_func.top()  && (int)symbol_table.get_scope($1)!=0){
                            yyerror("Cant reference variable out of scope");
                    }else{
                        if($1->type == USERFUNC) {}
                        else if($1->type == LIBFUNC) {}
                        else if($1->type == LOCAL||$1->type == GLOBAL) {
                            yyerror("cant use variable as function");
                            }
                    }
                }else{ //define as new var
                    yyerror("function not found");
                }
                printf("idlist_l->id1+ \n");
            } callsuffix {
                printf("call->lvaluecallsuffix \n");
                } 
            | left_parenthesis funcdef right_parenthesis left_parenthesis elist right_parenthesis {printf("call->(funcdef)(elist) \n");};

callsuffix: normcall  {printf("callsuffix->normcall \n");} 
            | methodcall {printf("callsuffix->methodcall \n");} ;

normcall:   left_parenthesis elist right_parenthesis {printf("normcall->(elist) \n");};

methodcall: double_dot id {//maybe needs code
            }left_parenthesis elist right_parenthesis {printf("methodcall->..id(elist) \n");} ; 

elist_l:    expr {printf("elist_l->expr \n");}
            | elist_l comma expr {printf("elist_l->elist_l,expr \n");};

elist:      elist_l {printf("elist->elist_l \n");}
            |/*empty*/  {printf("elist->empty \n");};

objectdef:  left_bracket elist right_bracket {printf("objextdef->[elist]\n");}
            |left_bracket indexed right_bracket {printf("objectdef->[indexed] \n");};

indexedelem: left_curly expr colon expr right_curly {printf("indexedelem->{expr:expr} \n");}; 
            // { expr {printf("expr");} : expr {printf("expr");}}

indexed:    indexedelem {printf("indexed->indexedelem \n");} 
            | indexed comma indexedelem {printf("indexed->indexed,indexedelem \n");};

block_l:    block_l stmt
            |/*empty*/;

block:      left_curly { printf("\n\n-----enter block ------ \n"); } block_l right_curly { printf("\n-----exit block ------\n\n"); symbol_table.hide(scope--);};

func_name:  id {
                switch( symbol_table.lookUp_curscope(yylval.stringValue)  ){
                    case 1 :{
                        yyerror("function name already used as var");// error: var redefined as a function
                        break;
                    } 
                    case 2 :{
                        yyerror("function name already used as func");// error: var redefined as a function
                        break;
                    }
                    case -1:{}
                    case -2:{
                        yyerror("shadowing of library functions not allowed");
                        break;
                    }
                    case 0: {//undefined
                        symbol_table.insert(yylval.stringValue, yylineno, USERFUNC);
                    }
                }
                
                printf("func_name->func_id \n");
                }  //lookup
            | /*empty*/{
                char name[100]; 
                sprintf(name, "%s%d","$anonymous",  anonymous_count );
                anonymous_count++;
                symbol_table.insert(name, yylineno, USERFUNC); printf("func_name->annonymousfunc \n");}; //probably insert with $_name(anonymous)

funcdef:    function func_name left_parenthesis { last_func.push(scope); scope++; printf("funcdef->( ");} 
                        idlist right_parenthesis {printf("funcdef->) \n");} block { last_func.pop(); };

number:     integer     {printf("number->int \n");}
            | real      {printf("number->real \n");};

const:      number      {printf("const->number \n");}
            | STRING    {printf("const->string \n");}
            | NIL       {printf("const->nil \n");}
            | TRUE      {printf("const->true \n");}
            | FALSE     {printf("const->false \n");};

//idlist {printf("'id'");} can be empty
idlist_l:   id {  symbol_table.insert(yylval.stringValue,yylineno, FORMAL); printf("idlist_l->id1 \n");} //lookup
            |idlist_l comma id {
                switch(  symbol_table.lookUp_curscope(yylval.stringValue)){ 
                    case 0:{ 
                        symbol_table.insert(yylval.stringValue,yylineno, FORMAL);
                        break;  
                    }
                    case 1:{    
                        yyerror("variable redefined in same scope");
                        break;
                    }
                    case -1:{
                        yyerror("formal arguement trying to shadow library func");
                        break;
                    }
                    default:{ yyerror("unknown error occured"); }
                }
                printf("idlist_l->id1+ \n");
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
