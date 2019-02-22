%{  
    /*includes*/
%}

/*options*/ 
%option noyywrap 
%option yylineno 

/*Regular expressions*/
digit       [0-9]
letter      [a-zA-Z]
underscore  [_]
interger    {digit}+
real        ({digit})*\.({digit})*
id          {letter}{ (letter) | (digit) | (underscore) }*
