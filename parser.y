%{
    #include <stdio.h>
    int yyerror(char *yaccProvidedMessage);
    int alpha_yylex(void);

    extern int yylineno;
    extern char* yytext;
    extern FILE* yyin;

%}

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
%token /*Operators*/
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
%token /*Comments*/
%token start_comment           
%token end_comment             
%token line_comment        






%right      '='
%left       ','
%left       '+' '-'
%left       '*' '/'
%nonassoc   UMINUS
%left       '(' ')'


%%