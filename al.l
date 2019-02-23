%{  
    /*includes*/
    #include <stdio.h>
    #include <stdlib.h>

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

int main(int argc, int *argv[]){

    FILE* fp;
    if( argc < 2 || argc > 3 ){
        printf("Invalid arguments number given %d\n", argc);
    }

    if( argc == 2 ) {
        /*Output to stdout*/
    }
    if( argc == 3 ){
        /*Output to file*/
    } 

}