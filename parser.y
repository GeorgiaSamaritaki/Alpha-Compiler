%{
    #include <stdio.h>
    int yyerror(char *yaccProvidedMessage);
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
%token and         
%token or          
%token not         
%token true        
%token false  
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
%left	    or
%left	    and 
%nonassoc   b_not_equal b_equals
%nonassoc	b_greater   b_greater_eq  b_less  b_less_eq 

%left       plus    minus
%left       mul     division    mod
%right      not     increment   decrement uminus

%left	    dot     double_dot
%left       comma

%left       left_bracket        right_bracket
%left       left_parenthesis      right_parenthesis
%left       left_curly          right_curly


%%


program:    statements;

stmt:       expr  semicolon
            | ifstmt
            | whilestmt
            | forstmt
            | returnstmt
            | BREAK semicolon
            | CONTINUE semicolon
            | block
            | funcdef
            | semicolon;

statements: statements stmt
            | /*empty*/;

expr:       assignexpr
            | opexpr
            | term;

opexpr:       expr plus         expr 
            | expr minus        expr 
            | expr mul          expr
            | expr division     expr 
            | expr mod          expr
            | expr b_greater    expr 
            | expr b_greater_eq expr 
            | expr b_less       expr
            | expr b_less_eq    expr 
            | expr b_equals     expr 
            | expr b_not_equal  expr
            | expr and          expr 
            | expr or           expr;


term:       left_parenthesis  expr  right_parenthesis
            | not expr
            | minus expr  %prec uminus
            |  increment lvalue
            | lvalue increment 
            |  decrement lvalue
            | lvalue decrement 
            | primary;

assignexpr: lvalue assign expr;

primary:    lvalue
            | call
            | objectdef
            | left_parenthesis  funcdef  right_parenthesis
            | const;

lvalue:     id
            | local id
            | double_colon id
            | member;

member:     lvalue dot id
            | lvalue left_bracket expr  right_bracket
            | call dot id
            | call left_bracket expr  right_bracket;

call:       call left_parenthesis  elist  right_parenthesis
            | lvalue callsuffix
            | left_parenthesis  funcdef right_parenthesis left_parenthesis  elist  right_parenthesis;

callsuffix: normcall
            | methodcall;

normcall:   left_parenthesis  elist  right_parenthesis;

methodcall: real id left_parenthesis  elist  right_parenthesis ; 
            // equivalent to lvalue.id(lvalue, elist)

elist_l:    expr
            | elist_l comma expr;

elist:      elist_l
            |/*empty*/;

objectdef:  left_bracket elist right_bracket
            |left_bracket indexed  right_bracket;

indexedelem:left_curly expr colon expr right_curly; // { expr : expr}

indexed:    indexedelem 
            | indexed comma indexedelem;

block_l:    stmt | block_l stmt ;

block:      left_curly block_l right_curly 
            | left_curly right_curly;

funcdef_l:  id | /*empty*/;

funcdef:    function funcdef_l left_parenthesis idlist right_parenthesis block;

number:     integer | real ;
const:      number | STRING | NIL | true | false;

//id can be empty
idlist:     id
            |idlist comma id;

elsestmt:   ELSE stmt 
            | /*empty*/;

ifstmt:     IF left_parenthesis  expr right_parenthesis elsestmt;

whilestmt:  WHILE left_parenthesis  expr right_parenthesis stmt;

forstmt:    FOR left_parenthesis  elist semicolon expr semicolon elist right_parenthesis stmt;

returnstmt: RETURN expr semicolon
            | RETURN semicolon;


%%
int yyerror(char* yaccProvidedMessage){
    fprintf(stderr, "%s: at line %d, before token: %s\n",yaccProvidedMessage,yylineno,yytext);
    fprintf(stderr,"INPUT NOT VALID\n");
}

