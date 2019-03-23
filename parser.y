%{
    #include <stdio.h>
    #include "symtable.hpp"

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
            | {printf("\n\n-----\n\nstmt->block1 "); scope++;} block      {printf("stmt->block2 \n\n----\n\n");}
            | funcdef    {printf("stmt->funcdef \n\n");   }
            | BREAK      {printf("stmt->Break ");     } semicolon {printf(" ';'  \n\n");}
            | CONTINUE   {printf("stmt->Continue");  } semicolon {printf(" ';'  \n\n");}
            | semicolon  {printf("';'  \n\n");       };
            

statements: statements  stmt 
            | /*empty*/;

expr:       assignexpr  {printf("assignexpr ");}
            | opexpr    {printf("opexpr ");}
            | term      {printf("term ");};     

opexpr:       expr plus         expr {printf("'expr + expr' ");}  
            | expr minus        expr {printf("'expr - expr' ");}
            | expr mul          expr {printf("'expr * expr' ");}
            | expr division     expr {printf("'expr / expr' ");}
            | expr mod          expr {printf("'expr % expr' ");}
            | expr b_greater    expr {printf("'expr > expr' ");}
            | expr b_less       expr {printf("'expr < expr' ");}
            | expr b_greater_eq expr {printf("'expr >= expr' ");}
            | expr b_less_eq    expr {printf("'expr <= expr' ");}
            | expr b_equals     expr {printf("'expr == expr' ");}
            | expr b_not_equal  expr {printf("'expr != expr' ");}
            | expr AND          expr {printf("'expr && expr' ");}
            | expr OR           expr {printf("'expr || expr' ");} ;

term:       left_parenthesis {printf("'('");} expr {printf("expr");}  right_parenthesis {printf("')'");}
            | NOT {printf("not");} expr {printf("expr");}
            | minus {printf("'-'");} expr {printf("expr");}  %prec uminus
            | increment {printf("'++'");} lvalue {printf("lvalue ");}
            | lvalue {printf("lvalue ");} increment {printf("'++'");} 
            | decrement {printf("'--'");} lvalue {printf("lvalue ");}
            | lvalue {printf("lvalue ");} decrement {printf("'--'");} 
            | primary {printf("primary ");};

assignexpr: lvalue {printf("lvalue ");} assign {printf("'='");} expr {printf("expr");};

primary:    lvalue {printf("lvalue ");}
            | call
            | objectdef
            | left_parenthesis funcdef right_parenthesis {printf("'(' funcdef ')'");}
            | const {printf("const ");}
            ;

lvalue:     id {printf("'id'");}
            | local id {printf("'id'");}
            | double_colon id {printf("'id'");}
            | member {printf("'member'");};

member:     lvalue {printf("lvalue ");} dot id {printf("'id'");}
            | lvalue left_bracket expr right_bracket {printf("lvalue '[' expr ']'");}
            | call dot id {printf("'id'");}
            | call left_bracket expr right_bracket {printf("'[' expr ']'");};

call:       call left_parenthesis elist right_parenthesis {printf("'(' elist ')'");}
            | lvalue {printf("lvalue ");} callsuffix {printf("callsuffix ");} 
            | left_parenthesis funcdef right_parenthesis left_parenthesis elist right_parenthesis {printf("'(' funcdef ')''(' elist ')'");};

callsuffix: normcall  {printf("normcall ");} 
            | methodcall {printf("methodcall ");} ;

normcall:   left_parenthesis elist right_parenthesis {printf("'(' elist ')'");};

methodcall: double_dot id left_parenthesis elist right_parenthesis {printf("..id '(' elist ')'");} ; 

elist_l:    expr 
            | elist_l comma expr;

elist:      elist_l {printf("elist ");}
            |/*empty*/  {printf("emptyelist ");};

objectdef:  left_bracket elist right_bracket {printf("'[' elist ']'");}
            |left_bracket indexed right_bracket {printf("'[' indexed ']'");};

indexedelem: left_curly expr colon expr right_curly {printf("'{' expr: expr'}'");}; 
            // { expr {printf("expr");} : expr {printf("expr");}}

indexed:    indexedelem {printf("indexedelem ");} 
            | indexed comma indexedelem {printf("indexed , indexedelem ");};


block:      left_curly {printf("'{' block ");} statements right_curly {printf("'}'"); hide(scope--);};

func_name:  id {printf("'func_id'");} | /*empty*/{printf("'annonymousfunc' ");};

funcdef:    function {printf("function ");} func_name left_parenthesis {scope++; func_scope++; printf("'('");} idlist right_parenthesis {printf("')'");} block { func_scope--; };

number:     integer     {printf("'int'");}
            | real      {printf("'real'");};

const:      number      {printf("'number'");}
            | STRING    {printf("'string'");}
            | NIL       {printf("'nil'");}
            | TRUE      {printf("'true'");}
            | FALSE     {printf("'false'");};

//idlist {printf("'id'");} can be empty
idlist_l:   id {printf("'idlist_id1'");}
            |idlist_l comma id {printf("'idlsit id1+'");};

idlist:     idlist_l {printf("idlist ");} 
             | /*empty*/ {printf("emptyidlist ");};

ifstmt:     IF left_parenthesis expr right_parenthesis  stmt ELSE stmt { printf(" \"if(expr) stmt else stmt\" "); } 
            | IF left_parenthesis expr right_parenthesis stmt ;

whilestmt:  WHILE left_parenthesis expr right_parenthesis stmt {printf(" \"while(expr) stmt else stmt\" ");};

forstmt:    FOR left_parenthesis {printf("'('");} elist {printf("elist");} semicolon expr {printf("expr");} semicolon elist {printf("elist");} right_parenthesis {printf("')'");} stmt;

returnstmt: RETURN { printf("return "); } expr {printf("expr");} semicolon {printf("';'");}
            | RETURN { printf("return "); } semicolon {printf("';'");};
%%
int yyerror(char* yaccProvidedMessage){
    fprintf(stderr, "%s: at line %d, before token: %s\n",yaccProvidedMessage,yylineno,yytext);
    fprintf(stderr,"INPUT NOT VALID\n");
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
    return 0;
}
