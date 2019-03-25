%{  
    //Τμήμα ορισμών
    /*includes*/
    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>
    #include "parser.hpp"

    /*Define yylex*/
    #define YY_DECL int yylex(void)
    #define MAX_STR_CONST 2048

    /*For keeping track of nested comments*/
    int comment_nesting = 0;

    /*Variables for keeping strings*/
    char string_buf[MAX_STR_CONST];
    char *string_buf_ptr;

    /*token struct*/
    struct alpha_token_t {
        int token_num;
        char* content;
        char* category;
        int line_no;
        struct alpha_token_t *next;
    };

    /*head of a list that will store all our token*/
    struct alpha_token_t *token_head = NULL;

    /*Fuctions to use*/
    /*Add a new token to the list of tokens*/
    void add_new_token(char* content, char* category, int line_no) {
        struct alpha_token_t *tmp=token_head;
        struct alpha_token_t *new_token = (struct alpha_token_t*)malloc(sizeof(struct alpha_token_t));
        //printf("(token:\'%s\" \"%s\" \"lineno:%d\")", content, category, line_no);

        /*Initialize fields*/
        new_token->line_no = line_no;
        new_token->next = NULL;
        new_token->content = strdup(content);
        new_token->category = strdup(category);

        if(tmp == NULL){
            /*new_token token is the first*/
            new_token->token_num = 1;
            token_head = new_token;
            return;
        }
        while( tmp->next != NULL){
            tmp=tmp->next;
        }
        /*tmp now points to the last token in the list*/
        new_token->token_num = tmp->token_num + 1;
        tmp->next = new_token;
        return;
    }

    /*Prints all tokens. If dest equals 0 prints tokens to stdout, else if its 1 to an output file*/
    void print_all_tokens(int dest, char* output_file){
        struct alpha_token_t* to_print = token_head;
        printf("--------------------------------------------------------\n");
        printf("%6s: %6s %20s %20s\n", "Line#", "Token#", "Category", "Content");
        switch(dest){
            case 0: {
                while(to_print != NULL) {
                    printf("%6d: %5d# %20s %20s\n", to_print->line_no, to_print->token_num, to_print->category, to_print->content);
                    to_print = to_print->next;
                }
                break;
            } 
            case 1:{
                FILE* fp;
                if( !( fp = fopen(output_file,"w") ) ){
                    printf("An error occured while openning the output file\n");
                    exit(-1);
                }
                while( to_print != NULL){
                    fprintf(fp, "%d: #%d \t %s \t %s\n", to_print->line_no, to_print->token_num, to_print->category, to_print->content);
                    to_print = to_print->next;
                }
                break;
            }
        }
        printf("--------------------------------------------------------\n");
    }
    
%}

/*options*/ 
%option noyywrap 
%option yylineno 

/*Regular expressions*/
/*Whitespaces*/
new_line    [\n]
whitespace  [\t\r' ']
EOFile      "<<EOF>>"
/*Keywords*/
AND         "and"
OR          "or"
NOT         "not"
TRUE        "true"
FALSE       "false"
nil         "nil"
if          "if"
else        "else"
while       "while"
for         "for"
break       "break"
continue    "continue"
function    "function"
return      "return"
local       "local"
/*Operators*/
assign      "="
plus        "+"
minus       "-"
mul          "*"
division    "/"
mod         "%"
increment  "++"
decrement   "--"
b_equals      "=="
b_not_equal   "!="
b_greater     ">"
b_greater_eq  ">="
b_less        "<"
b_less_eq     "<="
/*Constants-Identifiers*/
digit       [0-9]
letter      [a-zA-Z]    
underscore  "_"
integer     {digit}+
real        ({digit})+\.({digit})+
id          {letter}({digit}|{letter}|{underscore})*
/*String have to be implemented in code*/
string      \"
/*Punstuation marks*/
left_curly          \{
right_curly         \}
left_bracket        \[
right_bracket       \]
left_parenthesis    \(
right_parenthesis   \)
semicolon           \;
comma               \,
colon               \:
double_colon        \:\:
dot                 \.
double_dot          \.\. 
/*Comments*/
start_comment       "/*"    
end_comment         "*/"    
line_comment        "//".*


/*Conditions for comments and strings*/
%x SC_COMMENT SRULE

%%


{whitespace}*       {/*Ignore it*/}
{new_line}+         { 
                        // return new_line;
                    }
{AND}               {
                        add_new_token("and", "KEYWORD", yylineno);
                        return AND;
                    }
{OR}                {
                        add_new_token("or", "KEYWORD", yylineno);
                        return OR;
                    }
{NOT}               {
                        add_new_token("not", "KEYWORD", yylineno);
                        return NOT;
                    }
{TRUE}              {
                        add_new_token("true", "KEYWORD", yylineno);
                        return TRUE;
                    }
{FALSE}             {
                        add_new_token("false", "KEYWORD", yylineno);
                        return FALSE;
                    }
{nil}               {
                        add_new_token("nil", "KEYWORD", yylineno);
                        return NIL;
                    }
{if}                {
                        add_new_token("if", "KEYWORD", yylineno);
                        return IF;
                    }
{else}              {
                        add_new_token("else", "KEYWORD", yylineno);
                        return ELSE;
                    }
{while}             {
                        add_new_token("while", "KEYWORD", yylineno);
                        return WHILE;
                    }
{for}               {
                        add_new_token("for", "KEYWORD", yylineno);
                        return FOR;
                    }
{break}             {
                        add_new_token("break", "KEYWORD", yylineno);
                        return BREAK;
                    }
{continue}          {
                        add_new_token("continue", "KEYWORD", yylineno);
                        return CONTINUE;
                    }
{function}          {
                        add_new_token("function", "KEYWORD", yylineno);
                        return function;
                    }
{return}            {
                        add_new_token("return", "KEYWORD", yylineno);
                        return RETURN;
                    }
{local}             {
                        add_new_token("local", "KEYWORD", yylineno);
                        return local;
                    }
{assign}            {
                        add_new_token("=", "OPERATOR", yylineno);
                        return assign;
                    }
{plus}              {
                        add_new_token("+", "OPERATOR", yylineno);
                        return plus;
                    }
{minus}             {    
                        add_new_token("-", "OPERATOR", yylineno);
                        return minus;
                    }
{mul}               {
                        add_new_token("*", "OPERATOR", yylineno);
                        return mul;
                    }
{division}          {
                        add_new_token("/", "OPERATOR", yylineno);
                        return division;
                    }
{mod}               {
                        add_new_token("%", "OPERATOR", yylineno);
                        return mod;
                    }
{increment}         {
                        add_new_token("++", "OPERATOR", yylineno);
                        return increment;
                    }
{decrement}         {
                        add_new_token("--", "OPERATOR", yylineno);
                        return decrement;
                    }
{b_equals}          {
                        add_new_token("==", "OPERATOR", yylineno);
                        return b_equals;
                    }
{b_not_equal}       {
                        add_new_token("!=", "OPERATOR", yylineno);
                        return b_not_equal;
                    }
{b_greater}         {
                        add_new_token(">", "OPERATOR", yylineno);
                        return b_greater;
                    }
{b_greater_eq}      {
                        add_new_token(">=", "OPERATOR", yylineno);
                        return b_greater_eq;
                    }
{b_less}            {
                        add_new_token("<", "OPERATOR", yylineno);
                        return b_less;
                    }
{b_less_eq}         {
                        add_new_token("<=", "OPERATOR", yylineno);
                        return b_less_eq;
                    }
{integer}           {
                        add_new_token(yytext, "INT_CONST", yylineno);
                        yylval.intValue = atoi(yytext);
                        return integer;
                    }
{real}              {
                        add_new_token(yytext, "REAL_CONST", yylineno);
                        return real;
                    }
{id}                {
                        add_new_token(yytext, "IDENTIFIER", yylineno);
                        yylval.stringValue = strdup(yytext);
                        return id;
                    }
{left_curly}        {
                        add_new_token("{", "PUNCTUATION_MARK", yylineno);
                        return left_curly;
                    }
{right_curly}       {
                        add_new_token("}", "PUNCTUATION_MARK", yylineno);
                        return right_curly;
                    }
{left_bracket}      {
                        add_new_token("[", "PUNCTUATION_MARK", yylineno);
                        return left_bracket;
                    }
{right_bracket}     {
                        add_new_token("]", "PUNCTUATION_MARK", yylineno);
                        return right_bracket;
                    }
{left_parenthesis}  {
                        add_new_token("(", "PUNCTUATION_MARK", yylineno);
                        return left_parenthesis;
                    }
{right_parenthesis} {
                        add_new_token(")", "PUNCTUATION_MARK", yylineno);
                        return right_parenthesis;
                    }
{semicolon}         {
                        add_new_token(";", "PUNCTUATION_MARK", yylineno);
                        return semicolon;
                    }
{comma}             {
                        add_new_token(",", "PUNCTUATION_MARK", yylineno);
                        return comma;
                    }
{colon}             {
                        add_new_token(":", "PUNCTUATION_MARK", yylineno);
                        return colon;
                    }
{double_colon}      {
                        add_new_token("::", "PUNCTUATION_MARK", yylineno);
                        return double_colon;
                    }
{dot}               {
                        add_new_token(".", "PUNCTUATION_MARK", yylineno);
                        return dot;
                    }
{double_dot}        {
                        add_new_token("..", "PUNCTUATION_MARK", yylineno);
                        return double_dot;
                    }
{start_comment}     {
                        BEGIN(SC_COMMENT);
                    }
<SC_COMMENT>{start_comment}     { 
                                    ++comment_nesting; 
                                    add_new_token("/**/", "NESTED_COMMENT", yylineno); 
                                }
<SC_COMMENT>"*"+"/"             { 
                                    if (comment_nesting)
                                        --comment_nesting;
                                    else{
                                        BEGIN(INITIAL); 
                                        add_new_token("/**/", "MULTILINE_COMMENT", yylineno);
                                    } 
                                }
<SC_COMMENT>"*"+                {/*Ignore it*/} 
<SC_COMMENT>[^/*\n]+            {/*Ignore it*/} 
<SC_COMMENT>[/]                 {/*Ignore it*/} 
<SC_COMMENT>\n                  {/*Ignore it*/} 

<SC_COMMENT><<EOF>>             {   BEGIN(INITIAL);        
                                    printf("ERROR: comment doesn't end lineno:%d\n",yylineno);
                                }  
                             

{line_comment}                  {
                                    add_new_token("//", "ONE_LINE_COMMENT", yylineno);
                                }
{string}            {
                        string_buf_ptr = string_buf; 
                        BEGIN(SRULE);
                        
                    }
<SRULE>\"      { 
                        BEGIN(INITIAL);
                        *string_buf_ptr = '\0';
                        add_new_token(string_buf, "string", yylineno);
                        
                        return STRING;
                    }
<SRULE>\\[0-7]{1,3} {
                        int result;
                        (void) sscanf( yytext + 1, "%o", &result );
                        if ( result > 0xff )
                            *string_buf_ptr++ = result;
                    }
<SRULE>\\[0-9]+    {
                        printf("Bad escape sequence\n");
                    }
<SRULE>\\n         {
                        *string_buf_ptr++ = '\n';
                    }
<SRULE>\\t         {
                        *string_buf_ptr++ = '\t';
                    }         
<SRULE>\\r         {
                        *string_buf_ptr++ = '\r';
                    }
<SRULE>\\b         {   
                        *string_buf_ptr++ = '\b';
                    }
<SRULE>\\f         {
                        *string_buf_ptr++ = '\f';
                    }
<SRULE>\\(.|\n)    {
                        *string_buf_ptr++ = yytext[1];
                        printf("[%s]\n", string_buf);
                    }
<SRULE>[^\\\n\"]+  {
                        char *yptr = yytext;
                        while ( *yptr ) *string_buf_ptr++ = *yptr++;
                    }

<SRULE><<EOF>>     {   printf("ERROR: string doesn't end lineno:%d\n",yylineno);}  

.                   { printf("ERROR: Other token\n");}

%%

/*int main(int argc, char *argv[]){
    
    FILE* fp;

    if( argc < 2 || argc > 3 ){
        printf("Invalid arguments number given %d\n", argc);
        exit(-1);
    }
*/
    /*Read text from file*/
/*    if( !( fp = fopen(argv[1],"r") ) ){
        printf("An error occured while openning the file\n");
        exit(-1);
    }
*/
    /*Set the open file as the input file of lex*/
//    yyin = fp;

    /*Call lex function*/
 /*   yylex();

    if( argc == 2 ) {
*/        /*Output to stdout*/
 /*       print_all_tokens(0, NULL);
    }
    if( argc == 3 ){
   */     /*Output to file*/
    /*    print_all_tokens(1, argv[2]);
    } 
    return 0;
}*/