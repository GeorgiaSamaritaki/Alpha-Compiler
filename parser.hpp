/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_HPP_INCLUDED
# define YY_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    new_line = 258,
    whitespace = 259,
    EOFile = 260,
    AND = 261,
    OR = 262,
    NOT = 263,
    TRUE = 264,
    FALSE = 265,
    NIL = 266,
    IF = 267,
    ELSE = 268,
    WHILE = 269,
    FOR = 270,
    BREAK = 271,
    CONTINUE = 272,
    function = 273,
    RETURN = 274,
    local = 275,
    assign = 276,
    plus = 277,
    minus = 278,
    uminus = 279,
    mul = 280,
    division = 281,
    mod = 282,
    increment = 283,
    decrement = 284,
    b_equals = 285,
    b_not_equal = 286,
    b_greater = 287,
    b_greater_eq = 288,
    b_less = 289,
    b_less_eq = 290,
    digit = 291,
    letter = 292,
    underscore = 293,
    integer = 294,
    real = 295,
    id = 296,
    STRING = 297,
    left_curly = 298,
    right_curly = 299,
    left_bracket = 300,
    right_bracket = 301,
    left_parenthesis = 302,
    right_parenthesis = 303,
    semicolon = 304,
    comma = 305,
    colon = 306,
    double_colon = 307,
    dot = 308,
    double_dot = 309,
    start_comment = 310,
    end_comment = 311,
    line_comment = 312,
    other = 313
  };
#endif
/* Tokens.  */
#define new_line 258
#define whitespace 259
#define EOFile 260
#define AND 261
#define OR 262
#define NOT 263
#define TRUE 264
#define FALSE 265
#define NIL 266
#define IF 267
#define ELSE 268
#define WHILE 269
#define FOR 270
#define BREAK 271
#define CONTINUE 272
#define function 273
#define RETURN 274
#define local 275
#define assign 276
#define plus 277
#define minus 278
#define uminus 279
#define mul 280
#define division 281
#define mod 282
#define increment 283
#define decrement 284
#define b_equals 285
#define b_not_equal 286
#define b_greater 287
#define b_greater_eq 288
#define b_less 289
#define b_less_eq 290
#define digit 291
#define letter 292
#define underscore 293
#define integer 294
#define real 295
#define id 296
#define STRING 297
#define left_curly 298
#define right_curly 299
#define left_bracket 300
#define right_bracket 301
#define left_parenthesis 302
#define right_parenthesis 303
#define semicolon 304
#define comma 305
#define colon 306
#define double_colon 307
#define dot 308
#define double_dot 309
#define start_comment 310
#define end_comment 311
#define line_comment 312
#define other 313

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 17 "parser.y" /* yacc.c:1909  */
 
    char* stringValue; 
    int intValue;   
    double realValue; 
    struct expr* exprNode;
    struct SymbolTableEntry* symbol;
    struct call_l* call_l;
    struct stmt_l* stmt_l;
    struct for_prefix* for_prefix;
    // iopcode op;

#line 182 "parser.hpp" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_HPP_INCLUDED  */
