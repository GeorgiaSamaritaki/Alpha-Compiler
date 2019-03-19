%{
    #include <stdio.h>
    int yyerror(char *yaccProvidedMessage);
    int alpha_yylex(void);

    extern int yylineno;
    extern char* yytext;
    extern FILE* yyin;

%}


%union { 
    char* stringValue; 
    int intValue;   
    double realValue; 
}

%start program
%token ID INTEGER
%token new_line    
%token whitespace  
%token EOFile  
    
/*Keywords*/
%token and         
%token or          
%token not         
%token true        
%token false       
%token nil         
%token if          
%token else        
%token while       
%token for         
%token break       
%token continue    
%token function    
%token return      
%token local       

/*Operators*/
%token assign      
%token plus        
%token minus       
%token mul         
%token division    
%token mod         
%token increament  
%token decreament  
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
%token string  

/*Punstuation marks*/
%token left_curly          
%token right_curly         
%token left_bracket        
%token right_bracket       
%token left_parenthesis    
%token right_parenthesis   
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
%nonassoc   b_not_equal b_equal
%nonassoc	b_greater   b_greater_eq  b_less  b_less_eq 

%left       plus    minus
%left       mul     division    mod
%right      not     increment   decreament UMINUS

%left	    dot     double_dot
%left       comma

%left       left_bracket        right_bracket
%left       left_parenthesis    right_parenthesis
%left       left_curly          right_curly


%%

/*
    print
    input
    objectmemberkeys
    objecttotalmembers
    objectcopy
    totalarguments
    argument
    typeof
    strtonum
    sqrt
    cos
    sin
*/

program:    statements
            | 
            ;

stmt:       expr ;
            | ifstmt
            | whilestmt
            | forstmt
            | returnstmt
            | break;
            | continue;
            | block
            | funcdef
            | /*empty*/;

statements: statements stmt
            | /*empty*/;

expr:       assignexpr
            | expr op expr
            | term ;

op:         + | - | * | / | % | > | >= | < | <= | == | != | and | or ;

term:       ( expr )
            | not expr
            | - expr
            | ++lvalue
            | lvalue++
            | --lvalue
            | lvalue--
            | primary;

assginexpr: lvalue = expr;

primary:    lvalue
            | call
            | objectdef
            | ( funcdef )
            | const;

lvalue:     id
            | local id
            | :: id
            | member;

member:     lvalue . id
            | lvalue [ expr ]
            | call . id
            | call [ expr ];

call:       call ( elist )
            | lvalue callsuffix
            | ( funcdef) ( elist );

callsuffix: normcall
            | methodcall;

normcall:   ( elist );

methodcall: .. id ( elist ) // equivalent to lvalue.id(lvalue, elist);

elist_l:    expr
            | elist_l ',' expr;
elist:      elist_l 
            | /*empty*/;


objectdef:  [ elist | indexed | /*empty*/ ];

indexedelem:{ expr : expr };

indexed_l:  indexedelem
            | indexed_l ',' indexedelem;

indexed:    indexed_l 
            | /*empty*/;


block_l:    block_l stmt | /*empty*/;

block:      { block_l };

funcdef_l:  id | /*empty*/;

funcdef:    function funcdef_l (idlist) block;

const:      number | string | nil | true | false;

idlist_l:   id
            | idlist_l ',' id;
idlist:     idlist_l 
            | /*empty*/;

elsestmt:   else stmt | /*empty*/;

ifstmt:     if ( expr ) stmt elsestmt;

whilestmt:  while ( expr ) stmt;

forstmt:    for ( elist; expr; elist) stmt;

returnexpr: expr | /*empty*/;

returnstmt: return returnexpr;

%%