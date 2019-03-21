%{
    #include <stdio.h>
    
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

stmt:       expr         {printf("expr ");      }  semicolon {printf("';'\n");}
            | ifstmt     {printf("'ifstmt' ");  }
            | whilestmt  {printf("'whilestmt'");}
            | forstmt    {printf("'forstmt' "); }
            | returnstmt {printf("returnstmt ");}
            | BREAK      {printf("Break ");     } semicolon {printf(" ';'\n ");}
            | CONTINUE   {printf("Continue ");  } semicolon {printf(" ';'\n ");}
            | block      {printf("'block' ");   }
            | funcdef    {printf("'funcdef' "); }
            | semicolon  {printf("';'\n ");     };
            

statements: statements {printf("statements ");} stmt {printf("stmt ");}
            | /*empty*/{printf("emptystatements ");};

expr:       assignexpr  {printf("assignexpr ");}
            | opexpr    {printf("opexpr ");}
            | term      {printf("term ");};     

opexpr:       expr plus         expr {printf("expr + expr");}  
            | expr minus        expr {printf("expr - expr");}
            | expr mul          expr {printf("expr * expr");}
            | expr division     expr {printf("expr / expr");}
            | expr mod          expr {printf("expr % expr");}
            | expr b_greater    expr {printf("expr > expr");}
            | expr b_less       expr {printf("expr < expr");}
            | expr b_greater_eq expr {printf("expr >= expr");}
            | expr b_less_eq    expr {printf("expr <= expr");}
            | expr b_equals     expr {printf("expr == expr");}
            | expr b_not_equal  expr {printf("expr != expr");}
            | expr AND          expr {printf("expr && expr");}
            | expr OR           expr {printf("expr || expr");} ;

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

elist_l:    expr {printf("elist_lexpr");}
            | elist_l comma expr {printf("elist_l , elist_lexpr");};

elist:      elist_l {printf("elist ");}
            |/*empty*/  {printf("emptyelist ");};

objectdef:  left_bracket elist right_bracket {printf("'[' elist ']'");}
            |left_bracket indexed right_bracket {printf("'[' indexed ']'");};

indexedelem: left_curly expr colon expr right_curly {printf("'{' expr: expr'}'");}; 
            // { expr {printf("expr");} : expr {printf("expr");}}

indexed:    indexedelem {printf("indexedelem ");} 
            | indexed comma indexedelem {printf("indexed , indexedelem ");};

block_l:    stmt {printf("stmt ");} 
            | block_l {printf("block_l ");} stmt {printf("stmt ");} ;

block:      left_curly {printf("'{'");} block_l right_curly {printf("'}'");} 
            | left_curly {printf("'{'");} right_curly {printf("'}'");};

funcdef_l:  id {printf("'id'");} | /*empty*/{printf("emptyfuncdef_l ");};

funcdef:    function {printf("function ");} funcdef_l  {printf("funcdef_l ");} left_parenthesis {printf("'('");} idlist right_parenthesis {printf("')'");} block;

number:     integer {printf("'int'");}
            | real  {printf("'real'");};
const:      number {printf("'number'");}
            | STRING {printf("'string'");}
            | NIL {printf("'nil'");}
            | TRUE {printf("'true'");}
            | FALSE{printf("'false'");};

//idlist {printf("'id'");} can be empty
idlist:     id {printf("'id'");}
            |idlist comma {printf("','");} id {printf("'id'");};

// idlist:     idlist_l {printf("idlist ");} 
//             | /*empty*/ {printf("emptyidlist ");};

elsestmt:   ELSE {printf("else ");} stmt {printf("stmt ");}
            | /*empty*/ {printf("emptyelse ");};

ifstmt:     IF left_parenthesis {printf("'('");} expr {printf("expr");} right_parenthesis {printf("')'");} elsestmt {printf("elsestmt");};

whilestmt:  WHILE left_parenthesis {printf("'('");} expr {printf("expr");} right_parenthesis {printf("')'");} stmt {printf("stmt");};

forstmt:    FOR left_parenthesis {printf("'('");} elist {printf("elist");} semicolon expr {printf("expr");} semicolon elist {printf("elist");} right_parenthesis {printf("')'");} stmt;

returnstmt: RETURN { printf("return "); } expr {printf("expr");} semicolon {printf("';'\n");}
            | RETURN { printf("return "); } semicolon {printf("';'\n");};
%%
int yyerror(char* yaccProvidedMessage){
    fprintf(stderr, "%s: at line %d, before token: %s\n",yaccProvidedMessage,yylineno,yytext);
    fprintf(stderr,"INPUT NOT VALID\n");
}

int main(int argc, char* argv[]){
    
    FILE* fp;
    
    // if( !( fp = fopen(argv[1],"r") ) ){
    //     printf("An error occured while openning the file\n");
    //     exit(-1);
    // }

    yyin = stdin;

    yyparse();
    return 0;
}
