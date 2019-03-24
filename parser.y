%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "symtable.hpp"

    int yyerror(char* yaccProvidedMessage);
    int yylex(void);

    SymTable symbol_table = *new SymTable();


    extern int yylineno;
    extern char* yytext;
    extern FILE* yyin;

    void lvalue_check(const char* name){
        int switchl = symbol_table.lookUp_allscope(name); 
        switch( switchl ){
            case 2: {
                //Function
                yyerror("Attempting to use function as variale");
                break;
            }
            case 1: {
                //Var found
                printf("Var found\n");
                break;
            }
            case 0: {
                yyerror("variable undefined");
                break;
            }  
        }  
    }

%}

%defines

%union { 
    char* stringValue; 
    int intValue;   
    double realValue; 
}

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

stmt:       expr         {printf("stmt->expr");      }  semicolon {printf("';' \n\n");}
            | ifstmt     {printf("stmt->ifstmt \n\n");    }
            | whilestmt  {printf("stmt->whilestmt\n\n");  }
            | forstmt    {printf("stmt->forstmt   \n\n");   }
            | returnstmt {printf("stmt->returnstmt \n\n");}
            | {printf("\n\n-----\n\nstmt->block1 "); scope++;} block      {printf("stmt->block2 \n\n----\n\n");} //maybe scope++ after left curly
            | funcdef    {printf("stmt->funcdef \n\n");   }
            | BREAK      {printf("stmt->Break ");     } semicolon {printf(" ';'  \n\n");}
            | CONTINUE   {printf("stmt->Continue");  } semicolon {printf(" ';'  \n\n");}
            | semicolon  {printf("';'  \n\n");       };
            

statements: statements  stmt 
            | /*empty*/;

expr:       assignexpr  {printf(" expr->assignexpr ");}
            | opexpr    {printf(" expr->opexpr ");}
            | term      {printf(" expr->term ");};     

opexpr:       expr plus         expr {printf(" opexr->expr+expr ");}  
            | expr minus        expr {printf(" opexr->expr-expr ");}
            | expr mul          expr {printf(" opexr->expr*expr ");}
            | expr division     expr {printf(" opexr->expr/expr ");}
            | expr mod          expr {printf(" opexr->expr%expr ");}
            | expr b_greater    expr {printf(" opexr->expr>expr ");}
            | expr b_less       expr {printf(" opexr->expr<expr ");}
            | expr b_greater_eq expr {printf(" opexr->expr>=expr ");}
            | expr b_less_eq    expr {printf(" opexr->expr<=expr ");}
            | expr b_equals     expr {printf(" opexr->expr==expr ");}
            | expr b_not_equal  expr {printf(" opexr->expr!=expr ");}
            | expr AND          expr {printf(" opexr->expr&&expr ");}
            | expr OR           expr {printf(" opexr->expr||expr ");} ;

term:       left_parenthesis expr  right_parenthesis {printf(" term->(expr) ");} 
            | NOT expr {
                printf(" term->NOTexpr ");
                }
            | minus expr %prec uminus {
                printf(" term->-expr ");
                }
            | increment {printf(" term->++lvalue ");} lvalue { 
                lvalue_check(yylval.stringValue); 
                } //lookup maybe
            | lvalue {
                lvalue_check(yylval.stringValue);
                } increment {printf(" term->lvalue++ ");} //lookup maybe(before ++?)
            | decrement {printf(" term->--lvalue ");} lvalue {
                lvalue_check(yylval.stringValue);
                } //lookup maybe
            | lvalue {
                lvalue_check(yylval.stringValue);
                } decrement {printf(" term->lvalue-- ");}  //lookup maybe (before --?)
            | primary {printf(" term->primary ");};

assignexpr: lvalue {printf(" assignexpr->lvalue ");} assign expr {printf(" assignexpr->=expr ");}; //lookup (before assign?)

primary:    lvalue  {printf(" primary->lvalue ");}
            | call  {printf(" primary->call ");}
            | objectdef     {printf(" primary->objectdef ");}
            | left_parenthesis funcdef right_parenthesis {printf(" primary->(funcdef) ");}
            | const {printf(" primary->const ");}
            ;

lvalue:     id {printf(" lvalue->id ");} //lookup
            | local id { 
                int switchl = symbol_table.lookUp_curscope(yylval.stringValue, LOCAL);       
                switch( switchl ){
                    case 0: {//undefined
                        symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    }
                    case 1:{
                        //symbol_table.change_value()
                    } //defined as var
                    case 2:{} //defined as fgunc
                    default:{}
                }
                printf("'local id'");
            }
            | double_colon id {
                unsigned int scope_tmp =scope;
                scope = 0; 

                switch( symbol_table.lookUp_curscope(yylval.stringValue, LOCAL)  ){
                    case 0: {//undefined
                        yyerror("global variable not found");
                    }
                    case 1:{//ok var found
                        //symbol_table.change_value()
                    } 
                    case 2:{//ok func found 
                    }
                    default:{}
                }

                scope = scope_tmp;
                printf(" lvalue->id ");
                }
            | member {printf(" lvalue->member ");}; 

member:     lvalue dot id {printf(" member->lvalue.id ");}
            | lvalue left_bracket expr right_bracket {printf(" member->lvalue[expr] ");}
            | call dot id {printf(" member->call().id ");}
            | call left_bracket expr right_bracket {printf(" member->[expr] ");};

call:       call left_parenthesis elist right_parenthesis {printf(" call->call(elist) ");}
            | lvalue callsuffix {
                printf(" call->lvaluecallsuffix ");
                } 
            | left_parenthesis funcdef right_parenthesis left_parenthesis elist right_parenthesis {printf(" call->(funcdef)(elist) ");};

callsuffix: normcall  {printf(" callsuffix->normcall ");} 
            | methodcall {printf(" callsuffix->methodcall ");} ;

normcall:   left_parenthesis elist right_parenthesis {printf(" normcall->(elist) ");};

methodcall: double_dot id left_parenthesis elist right_parenthesis {printf(" methodcall->..id(elist) ");} ; 

elist_l:    expr {printf(" elist_l->expr ");}
            | elist_l comma expr {printf(" elist_l->elist_l,expr ");};

elist:      elist_l {printf(" elist->elist_l ");}
            |/*empty*/  {printf(" elist->empty ");};

objectdef:  left_bracket elist right_bracket {printf(" objextdef->[elist]");}
            |left_bracket indexed right_bracket {printf(" objectdef->[indexed] ");};

indexedelem: left_curly expr colon expr right_curly {printf(" indexedelem->{expr:expr} ");}; 
            // { expr {printf("expr");} : expr {printf("expr");}}

indexed:    indexedelem {printf(" indexed->indexedelem ");} 
            | indexed comma indexedelem {printf(" indexed->indexed,indexedelem ");};

block:      left_curly {printf(" block->{ ");} statements right_curly {printf(" block->statements} "); symbol_table.hide(scope--);};

func_name:  id {
                if(symbol_table.lookUp_curscope(yylval.stringValue, USERFUNC) == -1) yyerror("function name already used");
                else symbol_table.insert(yyval.stringValue, yylineno, USERFUNC);
                printf(" func_name->func_id ");

                }  //lookup
            | /*empty*/{
                char* name =(char*) "$sanonymous" +( char*)anonymous_count;
                symbol_table.insert(name, yylineno, USERFUNC); printf(" func_name->annonymousfunc ");}; //probably insert with $_name(anonymous)

funcdef:    function  func_name left_parenthesis { last_func.push(scope); scope++; printf(" funcdef->( ");} idlist right_parenthesis {printf(" funcdef->) ");} block { last_func.pop(); };

number:     integer     {printf(" number-> int ");}
            | real      {printf(" number-> real ");};

const:      number      {printf(" const->number ");}
            | STRING    {printf(" const->string ");}
            | NIL       {printf(" const->nil ");}
            | TRUE      {printf(" const->true ");}
            | FALSE     {printf(" const->false ");};

//idlist {printf("'id'");} can be empty
idlist_l:   id {printf(" idlist_l->id1 ");} //lookup
            |idlist_l comma id {printf(" idlist_l->id1+ ");};

idlist:     idlist_l {printf(" idlist->idlist_l ");} 
            | /*empty*/ {printf(" idlist->emptyidlist ");};

ifstmt:     IF left_parenthesis expr right_parenthesis  stmt ELSE stmt { printf(" ifstmt->\"if(expr) stmt else stmt\" "); } 
            | IF left_parenthesis expr right_parenthesis stmt { printf(" ifstmt->\"if(expr) stmt\" ");};

whilestmt:  WHILE left_parenthesis expr right_parenthesis stmt {printf(" whilestmt->\"while(expr) stmt else stmt\" ");};

forstmt:    FOR left_parenthesis elist semicolon expr semicolon elist right_parenthesis {printf(" forstmt->\"for(elist; expr; elist)\" ");} stmt;

returnstmt: RETURN expr semicolon {printf("returnstmt=>\"return expr;\" ");}
            | RETURN semicolon {printf(" returnstmt->return; ");};
%%

int yyerror(char* yaccProvidedMessage){
    printf("%s: at line %d, before token: %s\n",yaccProvidedMessage,yylineno,yytext);
    printf("INPUT NOT VALID\n");
}

int main(int argc, char* argv[]){
    
    FILE* fp;
    
    if( !( fp = fopen(argv[1],"r") ) ){
        printf("An error occured while openning the file\n");
        exit(-1);
    }
    yyin = fp;
    // initialize();

    yyparse();
    symbol_table.print();
    return 0;
}
