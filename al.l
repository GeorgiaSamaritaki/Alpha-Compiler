%{  
    /*includes*/
    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    /*Define alpha_yylex*/
    #define YY_DECL int alpha_yylex (void* ylval)

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
        strcpy(new->content, content);
        strcpy(new->category, category);
        new->line_no = line_no;
        new->next = NULL;

        if(tmp == NULL){
            /*New token is the first*/
            new->token_num = 1;
            return;
        }
        while( tmp->next == NULL){
            tmp=tmp->next;
        }
        /*tmp now points to the last token in the list*/
        new->token_num = tmp->token_num + 1;
        strcpy(new->content, content);
        strcpy(new->category, category);
        new->line_no = line_no;
        new->next = NULL;
        return;
    }

    /*Prints all tokens. If dest equals 0 prints tokens to stdout, else if its 1 to an output file*/
    void print_all_tokens(int dest, char* output_file){
        struct alpha_token_t* to_print = token_head;
        switch(dest){
            case 0: {
                printf("to stdout\n");
                while(to_print != NULL) {
                    printf("%d: #%d \t %s \t %s\n", to_print->line_no, to_print->token_num, to_print->category, to_print->content);
                }
                break;
            } 
            case 1:{
                FILE* fp;
                printf("to file\n");
                if( !( fp = fopen(output_file,"r") ) ){
                    printf("An error occured while openning the output file\n");
                    exit(-1);
                }
                fprintf("%d: #%d \t %s \t %s\n", to_print->line_no, to_print->token_num, to_print->category, to_print->content);
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
new_line    "\n"
whitespace  [\t\r' ' ']
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
equals      "=="
not_equal   "!="
greater     ">"
greater_eq  ">="
less        "<"
less_eq     "<="
/*Constants-Identifiers*/
digit       [0-9]
letter      [a-zA-Z]    
underscore  [_]
interger    {digit}+
real        ({digit})*\.({digit})*
id          {letter}{ (letter) | (digit) | (underscore) }*
/*String*/
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

%%


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

    if( argc == 2 ) {
        /*Output to stdout*/
        print_all_tokens(0);
    }
    if( argc == 3 ){
        /*Output to file*/
        print_all_tokens(1);
    } 

    return 0;
}