%{  
    /*includes*/
    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    /*Define alpha_yylex*/
    #define YY_DECL int alpha_yylex (void* ylval)
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
        struct alpha_token_t *new = (struct alpha_token_t*)malloc(sizeof(struct alpha_token_t));
       
        /*Initialize fields*/
        new->line_no = line_no;
        new->next = NULL;
        new->content = strdup(content);
        new->category = strdup(category);

        if(tmp == NULL){
            /*New token is the first*/
            new->token_num = 1;
            token_head = new;
            return;
        }
        while( tmp->next != NULL){
            tmp=tmp->next;
        }
        /*tmp now points to the last token in the list*/
        new->token_num = tmp->token_num + 1;
        tmp->next = new;
        return;
    }

    /*Prints all tokens. If dest equals 0 prints tokens to stdout, else if its 1 to an output file*/
    void print_all_tokens(int dest, char* output_file){
        struct alpha_token_t* to_print = token_head;
        printf("-------------------------------\n");
        switch(dest){
            case 0: {
                while(to_print != NULL) {
                    printf("%d: #%d \t %s \t %s\n", to_print->line_no, to_print->token_num, to_print->category, to_print->content);
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
    }
    
%}

/*options*/ 
%option noyywrap 
%option yylineno 

/*Regular expressions*/
/*Whitespaces*/
new_line    [\n]
whitespace  [\t\r' ']
/*Keywords*/
and         "and"
or          "or"
not         "not"
true        "true"
false       "false"
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
mul         "*"
division    "/"
mod         "%"
increament  "++"
decreament  "--"
b_equals      "=="
b_not_equal   "!="
b_greater     ">"
b_greater_eq  ">="
b_less        "<"
b_less_eq     "<="
/*Constants-Identifiers*/
digit       [0-9]
letter      [A-Za-z]    
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
%x SC_COMMENT STRING

%%
{new_line}          {/*Ignore it*/}
{whitespace}*       {/*Ignore it*/}
{and}               {
                        add_new_token("and", "KEYWORD", yylineno);
                    }
{or}                {
                        add_new_token("or", "KEYWORD", yylineno);
                    }
{not}               {
                        add_new_token("not", "KEYWORD", yylineno);
                    }
{true}              {
                        add_new_token("true", "KEYWORD", yylineno);
                    }
{false}             {
                        add_new_token("false", "KEYWORD", yylineno);
                    }
{nil}               {
                        add_new_token("nil", "KEYWORD", yylineno);
                    }
{if}                {
                        add_new_token("if", "KEYWORD", yylineno);
                    }
{else}              {
                        add_new_token("else", "KEYWORD", yylineno);
                    }
{while}             {
                        add_new_token("while", "KEYWORD", yylineno);
                    }
{for}               {
                        add_new_token("for", "KEYWORD", yylineno);
                    }
{break}             {
                        add_new_token("break", "KEYWORD", yylineno);
                    }
{continue}          {
                        add_new_token("continue", "KEYWORD", yylineno);
                    }
{function}          {
                        add_new_token("function", "KEYWORD", yylineno);
                    }
{return}            {
                        add_new_token("return", "KEYWORD", yylineno);
                    }
{local}             {
                        add_new_token("local", "KEYWORD", yylineno);
                    }
{assign}            {
                        add_new_token("=", "OPERATOR", yylineno);
                    }
{plus}              {
                        add_new_token("+", "OPERATOR", yylineno);
                    }
{minus}             {    
                        add_new_token("-", "OPERATOR", yylineno);
                    }
{mul}               {
                        add_new_token("*", "OPERATOR", yylineno);
                    }
{division}          {
                        add_new_token("/", "OPERATOR", yylineno);
                    }
{mod}               {
                        add_new_token("%", "OPERATOR", yylineno);
                    }
{increament}        {
                        add_new_token("++", "OPERATOR", yylineno);
                    }
{decreament}        {
                        add_new_token("--", "OPERATOR", yylineno);
                    }
{b_equals}          {
                        add_new_token("==", "OPERATOR", yylineno);
                    }
{b_not_equal}       {
                        add_new_token("!=", "OPERATOR", yylineno);
                    }
{b_greater}         {
                        add_new_token(">", "OPERATOR", yylineno);
                    }
{b_greater_eq}      {
                        add_new_token(">=", "OPERATOR", yylineno);
                    }
{b_less}            {
                        add_new_token("<", "OPERATOR", yylineno);
                    }
{b_less_eq}         {
                        add_new_token("<=", "OPERATOR", yylineno);
                    }
{integer}           {
                        add_new_token(yytext, "INT_CONST", yylineno);
                    }
{real}              {
                        add_new_token(yytext, "REAL_CONST", yylineno);
                    }
{id}                {
                        add_new_token(yytext, "IDENTIFIER", yylineno);
                    }
{left_curly}        {
                        add_new_token("{", "PUNCTUATION_MARK", yylineno);
                    }
{right_curly}       {
                        add_new_token("}", "PUNCTUATION_MARK", yylineno);
                    }
{left_bracket}      {
                        add_new_token("[", "PUNCTUATION_MARK", yylineno);
                    }
{right_bracket}     {
                        add_new_token("]", "PUNCTUATION_MARK", yylineno);
                    }
{left_parenthesis}  {
                        add_new_token("(", "PUNCTUATION_MARK", yylineno);
                    }
{right_parenthesis} {
                        add_new_token(")", "PUNCTUATION_MARK", yylineno);
                    }
{semicolon}         {
                        add_new_token(";", "PUNCTUATION_MARK", yylineno);
                    }
{comma}             {
                        add_new_token(",", "PUNCTUATION_MARK", yylineno);
                    }
{colon}             {
                        add_new_token(":", "PUNCTUATION_MARK", yylineno);
                    }
{double_colon}      {
                        add_new_token("::", "PUNCTUATION_MARK", yylineno);
                    }
{dot}               {
                        add_new_token(".", "PUNCTUATION_MARK", yylineno);
                    }
{double_dot}        {
                        add_new_token("..", "PUNCTUATION_MARK", yylineno);
                    }
{start_comment}     {
                        add_new_token("/**/", "COMMENT", yylineno);
                        BEGIN(SC_COMMENT);
                    }
<SC_COMMENT>{start_comment}     { 
                                    ++comment_nesting; 
                                    add_new_token("/**/", "NESTED_COMMENT", yylineno);
                                }
<SC_COMMENT>"*"+"/"             { 
                                    if (comment_nesting)
                                        --comment_nesting;
                                    else BEGIN(INITIAL); 
                                }
<SC_COMMENT>"*"+                {/*Ignore it*/} 
<SC_COMMENT>[^/*\n]+            {/*Ignore it*/} 
<SC_COMMENT>[/]                 {/*Ignore it*/} 
<SC_COMMENT>\n                  {/*Ignore it*/} 

{line_comment}                  {
                                    add_new_token("//", "COMMENT", yylineno);
                                }
{string}            {
                        string_buf_ptr = string_buf; 
                        BEGIN(STRING);
                    }
<STRING>\"          { 
                        BEGIN(INITIAL);
                        *string_buf_ptr = '\0';
                        add_new_token(string_buf, "STRING", yylineno);
                    }
<STRING>\\[0-7]{1,3}        {
                                int result;
                                (void) sscanf( yytext + 1, "%o", &result );
                                if ( result > 0xff )
                                    *string_buf_ptr++ = result;
                            }
<STRING>\\[0-9]+    {
                        printf("Bad escape sequence\n");
                    }
<STRING>\\n         {
                        *string_buf_ptr++ = '\n';
                    }
<STRING>\\t         {
                        *string_buf_ptr++ = '\t';
                    }         
<STRING>\\r         {
                        *string_buf_ptr++ = '\r';
                    }
<STRING>\\b         {   
                        *string_buf_ptr++ = '\b';
                    }
<STRING>\\f         {
                        *string_buf_ptr++ = '\f';
                    }
<STRING>\\(.|\n)    {
                        *string_buf_ptr++ = yytext[1];
                        printf("[%s]\n", string_buf);
                    }
<STRING>[^\\\n\"]+  {
                        char *yptr = yytext;
                        while ( *yptr ) *string_buf_ptr++ = *yptr++;
                    }

%%

int main(int argc, char *argv[]){
    
    FILE* fp;

    if( argc < 2 || argc > 3 ){
        printf("Invalid arguments number given %d\n", argc);
        exit(-1);
    }

    /*Read text from file*/
    if( !( fp = fopen(argv[1],"r") ) ){
        printf("An error occured while openning the file\n");
        exit(-1);
    }

    /*Set the open file as the input file of lex*/
    yyin = fp;

    /*Call lex function*/
    alpha_yylex(token_head);

    if( argc == 2 ) {
        /*Output to stdout*/
        print_all_tokens(0, NULL);
    }
    if( argc == 3 ){
        /*Output to file*/
        print_all_tokens(1, argv[2]);
    } 
    return 0;
}