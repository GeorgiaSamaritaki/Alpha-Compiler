/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "parser.y" /* yacc.c:339  */

    #include <stdio.h>
    #include <stdlib.h>
    #include "quad.hpp"

    int yyerror(char* yaccProvidedMessage);
    int yylex(void);

    extern int yylineno;
    extern char* yytext;
    extern FILE* yyin; 


#line 80 "parser.cpp" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.hpp".  */
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
#line 17 "parser.y" /* yacc.c:355  */
 
    char* stringValue; 
    int intValue;   
    double realValue; 
    struct expr* exprNode;
    struct SymbolTableEntry* symbol;
    struct call_l* call_l;
    struct stmt_l* stmt_l;
    struct for_prefix* for_prefix;
    // iopcode op;

#line 248 "parser.cpp" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_HPP_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 263 "parser.cpp" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  70
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   649

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  59
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  61
/* YYNRULES -- Number of rules.  */
#define YYNRULES  120
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  205

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   313

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   176,   176,   178,   182,   186,   190,   194,   198,   198,
     202,   206,   216,   226,   232,   240,   242,   245,   248,   251,
     254,   259,   276,   293,   311,   328,   347,   365,   383,   401,
     419,   444,   469,   469,   506,   506,   546,   550,   560,   568,
     568,   601,   637,   637,   670,   704,   710,   756,   774,   779,
     784,   789,   792,   806,   821,   836,   838,   841,   848,   856,
     856,   875,   875,   899,   903,   942,   949,   953,   958,   966,
     970,   978,   982,   988,   992,   997,  1012,  1031,  1039,  1044,
    1050,  1053,  1057,  1057,  1064,  1068,  1077,  1091,  1125,  1131,
    1137,  1154,  1160,  1161,  1166,  1171,  1176,  1183,  1210,  1239,
    1240,  1242,  1253,  1258,  1263,  1267,  1271,  1281,  1290,  1295,
    1299,  1310,  1324,  1326,  1328,  1332,  1337,  1341,  1341,  1352,
    1352
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "new_line", "whitespace", "EOFile",
  "AND", "OR", "NOT", "TRUE", "FALSE", "NIL", "IF", "ELSE", "WHILE", "FOR",
  "BREAK", "CONTINUE", "function", "RETURN", "local", "assign", "plus",
  "minus", "uminus", "mul", "division", "mod", "increment", "decrement",
  "b_equals", "b_not_equal", "b_greater", "b_greater_eq", "b_less",
  "b_less_eq", "digit", "letter", "underscore", "integer", "real", "id",
  "STRING", "left_curly", "right_curly", "left_bracket", "right_bracket",
  "left_parenthesis", "right_parenthesis", "semicolon", "comma", "colon",
  "double_colon", "dot", "double_dot", "start_comment", "end_comment",
  "line_comment", "other", "$accept", "program", "stmt", "$@1",
  "statements", "expr", "arithexpr", "relexpr", "boolexpr", "$@2", "$@3",
  "term", "$@4", "$@5", "assignexpr", "primary", "lvalue", "member",
  "tableitem", "$@6", "$@7", "call", "callsuffix", "normcall", "method_id",
  "methodcall", "elist_l", "elist", "objectdef", "indexedelem", "indexed",
  "block_l", "block", "$@8", "func_name", "funcdef", "func_prefix",
  "func_args", "func_body", "number", "const", "idlist_l", "idlist",
  "ifprefix", "elseprefix", "ifstmt", "whilestart", "whilecond",
  "whilestmt", "N", "M", "forprefix", "forstmt", "loopstart", "loopend",
  "loopstmt", "func_block_start", "func_block_end", "returnstmt", "$@9",
  "$@10", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313
};
# endif

#define YYPACT_NINF -119

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-119)))

#define YYTABLE_NINF -120

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-120)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     249,   320,  -119,  -119,  -119,   -37,  -119,   -33,   -26,    -9,
      -1,    13,   320,  -119,  -119,  -119,  -119,  -119,  -119,   294,
     320,  -119,    19,    64,  -119,    25,   159,    40,  -119,  -119,
    -119,  -119,  -119,  -119,    79,  -119,  -119,    -4,  -119,  -119,
    -119,  -119,   249,  -119,    22,  -119,    69,  -119,  -119,  -119,
    -119,   320,   320,  -119,  -119,   320,    41,  -119,  -119,    -2,
      -2,   320,   572,    42,    45,  -119,   -39,   408,    46,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,  -119,   320,  -119,
    -119,   320,    52,    43,    54,  -119,  -119,  -119,   320,   320,
      56,    88,   320,  -119,    62,    57,   320,   438,    60,   348,
    -119,  -119,   -32,    -4,   -32,     2,   320,  -119,  -119,    63,
    -119,    58,  -119,  -119,  -119,    31,    31,  -119,  -119,  -119,
     600,   600,   203,   203,   203,   203,   572,    65,  -119,    67,
      70,   320,   498,    71,  -119,  -119,   249,   468,   249,  -119,
    -119,  -119,    76,  -119,    73,  -119,  -119,  -119,    74,   320,
     572,  -119,   320,   204,   320,   320,  -119,   320,  -119,   520,
    -119,  -119,  -119,  -119,  -119,  -119,    68,    80,  -119,    25,
    -119,   320,    58,   542,    81,  -119,  -119,   614,   586,    82,
    -119,  -119,    86,  -119,  -119,  -119,   378,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       8,     0,    95,    96,    94,     0,   105,     0,     0,     0,
     117,     0,     0,    39,    42,    90,    91,    52,    93,    74,
     108,    13,     0,     0,    15,     0,     2,     0,    18,    19,
      17,    20,    16,    45,    47,    55,    56,    48,    49,    10,
      92,    51,     8,     4,     0,     5,     0,   108,     6,     7,
      37,     0,    74,    11,    12,     0,     0,    53,    38,     0,
       0,     0,    71,    73,     0,    78,     0,     0,     0,    54,
       1,    82,     9,    14,    34,    32,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     3,     0,    41,
      44,    74,     0,     0,     0,    64,    66,    67,     0,    74,
       0,   104,     0,   112,    85,     0,    74,     0,     0,     0,
     120,   108,    40,     0,    43,     0,     0,    75,    76,     0,
      36,    50,    81,   109,   109,    21,    22,    23,    24,    25,
      30,    31,    26,    28,    27,    29,    46,     0,    69,     0,
       0,     0,     0,     0,    57,   102,     8,     0,     8,   107,
      84,    87,   100,   115,     0,   101,   109,   118,     0,     0,
      72,    79,    74,     8,     0,     0,    68,    74,    60,     0,
      58,    63,   103,   106,   113,    97,    99,     0,    86,     0,
     108,     0,     0,     0,     0,    83,    80,    35,    33,     0,
      62,   114,     0,    88,   116,   112,     0,    77,    65,    70,
      98,    89,   108,   110,   111
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -119,  -119,   -23,  -119,  -119,     0,  -119,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,   -43,  -119,  -119,  -119,
    -119,   -29,  -119,  -119,  -119,  -119,  -119,   -47,  -119,    -7,
    -119,  -119,   -69,  -119,  -119,   -16,  -119,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,   -45,
    -118,  -119,  -119,  -119,  -119,   -64,  -119,  -119,  -119,  -119,
    -119
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    23,    24,    25,    26,    62,    28,    29,    30,   124,
     123,    31,    59,    60,    32,    33,    34,    35,    36,    93,
      94,    37,    95,    96,   139,    97,    63,    64,    38,    65,
      66,   163,    72,   122,   151,    39,   105,   153,   178,    40,
      41,   176,   177,    42,   146,    43,    44,   103,    45,    46,
     164,    47,    48,   148,   191,   149,   179,   201,    49,    55,
      56
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      27,    50,   106,    73,    68,   108,   165,   118,    74,    75,
      51,   119,    58,   -61,    52,    91,   112,   114,    11,   101,
      67,   -59,    92,    53,    76,    77,    27,    78,    79,    80,
     113,   113,    81,    82,    83,    84,    85,    86,   181,    17,
      54,    98,    27,    99,   137,   111,    74,    75,  -119,   100,
      22,   107,   143,   159,    57,   109,    78,    79,    80,   154,
      69,   115,    76,    77,    70,    78,    79,    80,    71,   102,
      81,    82,    83,    84,    85,    86,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   104,   136,    87,
     110,   117,   116,   138,   121,   158,   140,   144,   142,   141,
      88,   145,   147,   150,   152,   162,    61,    89,    90,   156,
     194,   168,   161,   166,   167,   184,   160,   175,   192,   171,
     189,   180,   182,   172,   -61,   174,    91,   200,   193,   198,
     199,   202,   -59,    92,     0,   195,     0,     0,     0,     0,
     186,   169,     0,     0,     0,     0,    27,     0,    27,     0,
       0,     0,     0,     0,     0,     0,     0,   204,     0,   183,
       0,     0,     0,    27,   187,   188,     0,     1,     2,     3,
       4,     5,     0,     6,     7,     8,     9,  -108,    10,    11,
       0,   196,    12,     0,     0,     0,     0,    13,    14,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    15,    16,
      17,    18,    -8,     0,    19,     0,    20,     0,    21,     0,
       0,    22,     1,     2,     3,     4,     5,     0,     6,     7,
       8,     9,  -108,    10,    11,    76,    77,    12,    78,    79,
      80,     0,    13,    14,     0,  -120,  -120,  -120,  -120,     0,
       0,     0,     0,    15,    16,    17,    18,     0,   185,    19,
       0,    20,     0,    21,     0,     0,    22,     1,     2,     3,
       4,     5,     0,     6,     7,     8,     9,  -108,    10,    11,
       0,     0,    12,     0,     0,     0,     0,    13,    14,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    15,    16,
      17,    18,     0,     0,    19,     0,    20,     0,    21,     0,
       0,    22,     1,     2,     3,     4,     0,     0,     0,     0,
       0,     0,     0,     0,    11,     0,     0,    12,     0,     0,
       0,     0,    13,    14,     0,     0,     0,     0,     1,     2,
       3,     4,     0,    15,    16,    17,    18,    61,     0,    19,
      11,    20,     0,    12,     0,     0,    22,     0,    13,    14,
       0,     0,     0,     0,    74,    75,     0,     0,     0,    15,
      16,    17,    18,     0,     0,    19,     0,    20,     0,     0,
      76,    77,    22,    78,    79,    80,     0,     0,    81,    82,
      83,    84,    85,    86,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   157,     0,     0,
      76,    77,     0,    78,    79,    80,     0,     0,    81,    82,
      83,    84,    85,    86,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   203,     0,     0,
      76,    77,     0,    78,    79,    80,     0,     0,    81,    82,
      83,    84,    85,    86,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
      76,    77,     0,    78,    79,    80,     0,     0,    81,    82,
      83,    84,    85,    86,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   155,     0,     0,     0,
      76,    77,     0,    78,    79,    80,     0,     0,    81,    82,
      83,    84,    85,    86,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   173,     0,     0,     0,
      76,    77,     0,    78,    79,    80,    74,    75,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,   170,    78,    79,    80,    74,    75,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,   190,    78,    79,    80,
       0,     0,    81,    82,    83,    84,    85,    86,    74,    75,
       0,     0,     0,     0,     0,     0,   197,     0,     0,     0,
       0,     0,    74,     0,    76,    77,     0,    78,    79,    80,
       0,     0,    81,    82,    83,    84,    85,    86,    76,    77,
       0,    78,    79,    80,     0,     0,    81,    82,    83,    84,
      85,    86,    76,    77,     0,    78,    79,    80,     0,     0,
    -120,  -120,    83,    84,    85,    86,    76,    77,     0,    78,
      79,    80,     0,     0,    81,    82,    83,    84,    85,    86
};

static const yytype_int16 yycheck[] =
{
       0,     1,    47,    26,    20,    52,   124,    46,     6,     7,
      47,    50,    12,    45,    47,    47,    59,    60,    20,    42,
      20,    53,    54,    49,    22,    23,    26,    25,    26,    27,
      59,    60,    30,    31,    32,    33,    34,    35,   156,    41,
      49,    45,    42,    47,    91,    47,     6,     7,    49,    53,
      52,    51,    99,    51,    41,    55,    25,    26,    27,   106,
      41,    61,    22,    23,     0,    25,    26,    27,    43,    47,
      30,    31,    32,    33,    34,    35,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    18,    88,    49,
      49,    46,    50,    41,    48,   111,    53,    41,    98,    45,
      21,    13,   102,    41,    47,    47,    43,    28,    29,    49,
     179,    41,   119,    48,    47,   162,   116,    41,    50,    48,
     167,    48,    48,   146,    45,   148,    47,    41,    48,    48,
      48,   195,    53,    54,    -1,   180,    -1,    -1,    -1,    -1,
     163,   141,    -1,    -1,    -1,    -1,   146,    -1,   148,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,   159,
      -1,    -1,    -1,   163,   164,   165,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,    16,    17,    18,    19,    20,
      -1,   181,    23,    -1,    -1,    -1,    -1,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    39,    40,
      41,    42,    43,    -1,    45,    -1,    47,    -1,    49,    -1,
      -1,    52,     8,     9,    10,    11,    12,    -1,    14,    15,
      16,    17,    18,    19,    20,    22,    23,    23,    25,    26,
      27,    -1,    28,    29,    -1,    32,    33,    34,    35,    -1,
      -1,    -1,    -1,    39,    40,    41,    42,    -1,    44,    45,
      -1,    47,    -1,    49,    -1,    -1,    52,     8,     9,    10,
      11,    12,    -1,    14,    15,    16,    17,    18,    19,    20,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    39,    40,
      41,    42,    -1,    -1,    45,    -1,    47,    -1,    49,    -1,
      -1,    52,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    23,    -1,    -1,
      -1,    -1,    28,    29,    -1,    -1,    -1,    -1,     8,     9,
      10,    11,    -1,    39,    40,    41,    42,    43,    -1,    45,
      20,    47,    -1,    23,    -1,    -1,    52,    -1,    28,    29,
      -1,    -1,    -1,    -1,     6,     7,    -1,    -1,    -1,    39,
      40,    41,    42,    -1,    -1,    45,    -1,    47,    -1,    -1,
      22,    23,    52,    25,    26,    27,    -1,    -1,    30,    31,
      32,    33,    34,    35,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      22,    23,    -1,    25,    26,    27,    -1,    -1,    30,    31,
      32,    33,    34,    35,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      22,    23,    -1,    25,    26,    27,    -1,    -1,    30,    31,
      32,    33,    34,    35,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      22,    23,    -1,    25,    26,    27,    -1,    -1,    30,    31,
      32,    33,    34,    35,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      22,    23,    -1,    25,    26,    27,    -1,    -1,    30,    31,
      32,    33,    34,    35,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      22,    23,    -1,    25,    26,    27,     6,     7,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    22,    23,    46,    25,    26,    27,     6,     7,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    22,    23,    46,    25,    26,    27,
      -1,    -1,    30,    31,    32,    33,    34,    35,     6,     7,
      -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,     6,    -1,    22,    23,    -1,    25,    26,    27,
      -1,    -1,    30,    31,    32,    33,    34,    35,    22,    23,
      -1,    25,    26,    27,    -1,    -1,    30,    31,    32,    33,
      34,    35,    22,    23,    -1,    25,    26,    27,    -1,    -1,
      30,    31,    32,    33,    34,    35,    22,    23,    -1,    25,
      26,    27,    -1,    -1,    30,    31,    32,    33,    34,    35
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     8,     9,    10,    11,    12,    14,    15,    16,    17,
      19,    20,    23,    28,    29,    39,    40,    41,    42,    45,
      47,    49,    52,    60,    61,    62,    63,    64,    65,    66,
      67,    70,    73,    74,    75,    76,    77,    80,    87,    94,
      98,    99,   102,   104,   105,   107,   108,   110,   111,   117,
      64,    47,    47,    49,    49,   118,   119,    41,    64,    71,
      72,    43,    64,    85,    86,    88,    89,    64,    94,    41,
       0,    43,    91,    61,     6,     7,    22,    23,    25,    26,
      27,    30,    31,    32,    33,    34,    35,    49,    21,    28,
      29,    47,    54,    78,    79,    81,    82,    84,    45,    47,
      53,    61,    47,   106,    18,    95,   108,    64,    86,    64,
      49,    47,    75,    80,    75,    64,    50,    46,    46,    50,
      48,    48,    92,    69,    68,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    86,    41,    83,
      53,    45,    64,    86,    41,    13,   103,    64,   112,   114,
      41,    93,    47,    96,    86,    48,    49,    49,    94,    51,
      64,    88,    47,    90,   109,   109,    48,    47,    41,    64,
      46,    48,    61,    48,    61,    41,   100,   101,    97,   115,
      48,   109,    48,    64,    86,    44,    61,    64,    64,    86,
      46,   113,    50,    48,    91,   108,    64,    44,    48,    48,
      41,   116,   114,    49,   108
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    59,    60,    61,    61,    61,    61,    61,    62,    61,
      61,    61,    61,    61,    63,    63,    64,    64,    64,    64,
      64,    65,    65,    65,    65,    65,    66,    66,    66,    66,
      66,    66,    68,    67,    69,    67,    70,    70,    70,    71,
      70,    70,    72,    70,    70,    70,    73,    74,    74,    74,
      74,    74,    75,    75,    75,    75,    76,    76,    76,    78,
      77,    79,    77,    80,    80,    80,    81,    81,    82,    83,
      84,    85,    85,    86,    86,    87,    87,    88,    89,    89,
      90,    90,    92,    91,    93,    93,    94,    95,    96,    97,
      98,    98,    99,    99,    99,    99,    99,   100,   100,   101,
     101,   102,   103,   104,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   118,   117,   119,
     117
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     0,     2,
       1,     2,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     0,     5,     0,     5,     3,     2,     2,     0,
       3,     2,     0,     3,     2,     1,     3,     1,     1,     1,
       3,     1,     1,     2,     2,     1,     1,     3,     4,     0,
       4,     0,     5,     4,     2,     6,     1,     1,     3,     1,
       5,     1,     3,     1,     0,     3,     3,     5,     1,     3,
       2,     0,     0,     4,     1,     0,     4,     2,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       0,     4,     1,     4,     2,     1,     3,     3,     0,     0,
       7,     7,     0,     0,     3,     0,     0,     0,     4,     0,
       3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 176 "parser.y" /* yacc.c:1646  */
    {printf("Finished\n");}
#line 1593 "parser.cpp" /* yacc.c:1646  */
    break;

  case 3:
#line 178 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();
                printf("stmt->expr';' \n");
                }
#line 1602 "parser.cpp" /* yacc.c:1646  */
    break;

  case 4:
#line 182 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();  
                printf("stmt->ifstmt \n\n");
                }
#line 1611 "parser.cpp" /* yacc.c:1646  */
    break;

  case 5:
#line 186 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();  
                printf("stmt->whilestmt\n\n");
                }
#line 1620 "parser.cpp" /* yacc.c:1646  */
    break;

  case 6:
#line 190 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();  
                printf("stmt->forstmt   \n\n");
                }
#line 1629 "parser.cpp" /* yacc.c:1646  */
    break;

  case 7:
#line 194 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = (yyvsp[0].stmt_l);  
                printf("stmt->returnstmt \n\n");
                }
#line 1638 "parser.cpp" /* yacc.c:1646  */
    break;

  case 8:
#line 198 "parser.y" /* yacc.c:1646  */
    { scope++;}
#line 1644 "parser.cpp" /* yacc.c:1646  */
    break;

  case 9:
#line 198 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = (yyvsp[0].stmt_l);  
                printf("stmt->block2\n");
                }
#line 1653 "parser.cpp" /* yacc.c:1646  */
    break;

  case 10:
#line 202 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l(); 
                printf("stmt->funcdef \n");   
                }
#line 1662 "parser.cpp" /* yacc.c:1646  */
    break;

  case 11:
#line 206 "parser.y" /* yacc.c:1646  */
    {
                printf("BREAK  ';'  \n\n");
                (yyval.stmt_l) = new stmt_l();
                if(loopcnt==0){
                    yyerror("Use of \"Break\" outside of loop");
                }else{
                    (yyval.stmt_l)->breaklist = newList(nextQuadLabel()); 
                    emit(jump);
                }
                }
#line 1677 "parser.cpp" /* yacc.c:1646  */
    break;

  case 12:
#line 216 "parser.y" /* yacc.c:1646  */
    {
                printf("CONTINUE ';'  \n\n");
                (yyval.stmt_l) = new stmt_l();
                if(loopcnt==0){
                    yyerror("Use of \"Continue\" outside of loop");
                }else{
                    (yyval.stmt_l)->contlist = newList(nextQuadLabel()); 
                    emit(jump);
                }
                }
#line 1692 "parser.cpp" /* yacc.c:1646  */
    break;

  case 13:
#line 226 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();
                printf("';' \n\n");       
                }
#line 1701 "parser.cpp" /* yacc.c:1646  */
    break;

  case 14:
#line 232 "parser.y" /* yacc.c:1646  */
    { 
                printf("statements ->statements stmt\n");
                error = 0; 
                reset_tmp(); 

                (yyval.stmt_l) = merge((yyvsp[-1].stmt_l),(yyvsp[0].stmt_l));
                //free $2?
                }
#line 1714 "parser.cpp" /* yacc.c:1646  */
    break;

  case 15:
#line 240 "parser.y" /* yacc.c:1646  */
    { (yyval.stmt_l) =(yyvsp[0].stmt_l); reset_tmp(); printf("statements->empty\n");}
#line 1720 "parser.cpp" /* yacc.c:1646  */
    break;

  case 16:
#line 242 "parser.y" /* yacc.c:1646  */
    { 
                assert((yyvsp[0].exprNode)); 
                (yyval.exprNode) = (yyvsp[0].exprNode); printf("expr->assignexpr \n");}
#line 1728 "parser.cpp" /* yacc.c:1646  */
    break;

  case 17:
#line 245 "parser.y" /* yacc.c:1646  */
    { 
                assert((yyvsp[0].exprNode)); 
                (yyval.exprNode) = (yyvsp[0].exprNode); printf("expr->boolexpr \n");}
#line 1736 "parser.cpp" /* yacc.c:1646  */
    break;

  case 18:
#line 248 "parser.y" /* yacc.c:1646  */
    { 
                assert((yyvsp[0].exprNode)); 
                (yyval.exprNode) = (yyvsp[0].exprNode); printf("expr->arithexpr \n");}
#line 1744 "parser.cpp" /* yacc.c:1646  */
    break;

  case 19:
#line 251 "parser.y" /* yacc.c:1646  */
    { 
                assert((yyvsp[0].exprNode)); 
                (yyval.exprNode) = (yyvsp[0].exprNode); printf("expr->relexpr \n");}
#line 1752 "parser.cpp" /* yacc.c:1646  */
    break;

  case 20:
#line 254 "parser.y" /* yacc.c:1646  */
    { 
                assert((yyvsp[0].exprNode)); 
                (yyval.exprNode) = (yyvsp[0].exprNode); printf("expr->term \n");
                }
#line 1761 "parser.cpp" /* yacc.c:1646  */
    break;

  case 21:
#line 259 "parser.y" /* yacc.c:1646  */
    { 
                printf("opexr->expr+expr \n");
                if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type) ){
                    yyerror("Invalid arithmetic expressions");
                    (yyval.exprNode) = nil_expr;
                }else {
                    if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                        (yyval.exprNode) = newExpr(constnum_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        (yyval.exprNode)->numConst = compute(add, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                    }else{
                        (yyval.exprNode) = newExpr(arithexpr_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        emit(add, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode), (yyval.exprNode));
                    }
                }    
            }
#line 1783 "parser.cpp" /* yacc.c:1646  */
    break;

  case 22:
#line 276 "parser.y" /* yacc.c:1646  */
    { 
                printf("opexr->expr-expr \n");
                if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type) ){
                    yyerror("Invalid arithmetic expressions");
                    (yyval.exprNode) = nil_expr;
                }else {
                    if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                        (yyval.exprNode) = newExpr(constnum_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        (yyval.exprNode)->numConst = compute(sub, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                    }else{
                        (yyval.exprNode) = newExpr(arithexpr_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        emit(sub, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode), (yyval.exprNode));
                    }
                }    
            }
#line 1805 "parser.cpp" /* yacc.c:1646  */
    break;

  case 23:
#line 293 "parser.y" /* yacc.c:1646  */
    {
                printf("opexr->expr*expr \n");
                assert((yyvsp[-2].exprNode) && (yyvsp[0].exprNode));
                if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type)){
                    yyerror("Invalid arithmetic expressions");
                    (yyval.exprNode) = nil_expr;
                }else {
                    if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                        (yyval.exprNode) = newExpr(constnum_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        (yyval.exprNode)->numConst = compute(mul_op, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                    }else{
                        (yyval.exprNode) = newExpr(arithexpr_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        emit(mul_op, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode), (yyval.exprNode));
                    }
                }
            }
#line 1828 "parser.cpp" /* yacc.c:1646  */
    break;

  case 24:
#line 311 "parser.y" /* yacc.c:1646  */
    { 
                printf("opexr->expr/expr \n");
                if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type)){
                    yyerror("Invalid arithmetic expressions");
                    (yyval.exprNode) = nil_expr;
                }else {
                    if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                        (yyval.exprNode) = newExpr(constnum_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        (yyval.exprNode)->numConst = compute(div_op, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                    }else{
                        (yyval.exprNode) = newExpr(arithexpr_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        emit(div_op, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode), (yyval.exprNode));
                    }
                }
            }
#line 1850 "parser.cpp" /* yacc.c:1646  */
    break;

  case 25:
#line 328 "parser.y" /* yacc.c:1646  */
    { 
                printf("opexr->expr\%expr \n");
                if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type)){
                    yyerror("Invalid arithmetic expressions");
                    (yyval.exprNode) = nil_expr;
                }else {
                    if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                        (yyval.exprNode) = newExpr(constnum_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        (yyval.exprNode)->numConst = compute(mod_op, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                    }else{
                        (yyval.exprNode) = newExpr(arithexpr_e);
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        emit(mod_op, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode), (yyval.exprNode));
                    }
                    printf("expr->expr arithexpr expr \n");
                }
                }
#line 1873 "parser.cpp" /* yacc.c:1646  */
    break;

  case 26:
#line 347 "parser.y" /* yacc.c:1646  */
    {
                printf("opexr->expr>expr \n");
                (yyval.exprNode) = newExpr(constbool_e);
                (yyval.exprNode)->truelist = newList(nextQuadLabel());
                (yyval.exprNode)->falselist = newList(nextQuadLabel()+1);
                printf("t %d f%d\n", (yyval.exprNode)->truelist[0], (yyval.exprNode)->falselist[0]);
                if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type)){
                    yyerror("Invalid arithmetic expressions");
                    (yyval.exprNode) = nil_expr;
                }else {
                    if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                        (yyval.exprNode)->sym = new_tmp(yylineno);
                        (yyval.exprNode)->boolConst = compute_rel(if_greater, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                    }
                    emit(if_greater, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode));
                    emit(jump);
                }
        }
#line 1896 "parser.cpp" /* yacc.c:1646  */
    break;

  case 27:
#line 365 "parser.y" /* yacc.c:1646  */
    {
            printf("opexr->expr<expr \n");
            (yyval.exprNode) = newExpr(constbool_e);
            (yyval.exprNode)->truelist = newList(nextQuadLabel());
            (yyval.exprNode)->falselist = newList(nextQuadLabel()+1);
            printf("t %d f%d\n", (yyval.exprNode)->truelist[0], (yyval.exprNode)->falselist[0]);
            if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type) ){
                yyerror("Invalid arithmetic expressions");
                (yyval.exprNode) = nil_expr;
            }else {
                if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute_rel(if_less, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                }
                emit(if_less, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode));
                emit(jump);
            }
        }
#line 1919 "parser.cpp" /* yacc.c:1646  */
    break;

  case 28:
#line 383 "parser.y" /* yacc.c:1646  */
    {
            printf("opexr->expr>=expr \n");
            (yyval.exprNode) = newExpr(constbool_e);
            (yyval.exprNode)->truelist = newList(nextQuadLabel());
            (yyval.exprNode)->falselist = newList(nextQuadLabel()+1);
            printf("t %d f%d\n", (yyval.exprNode)->truelist[0], (yyval.exprNode)->falselist[0]);
            if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type)){
                yyerror("Invalid arithmetic expressions");
                (yyval.exprNode) = nil_expr;
            }else {
                if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute_rel(if_greater_eq, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                }
                emit(if_greater_eq, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode));
                emit(jump);
            }
        }
#line 1942 "parser.cpp" /* yacc.c:1646  */
    break;

  case 29:
#line 401 "parser.y" /* yacc.c:1646  */
    {
            printf("opexr->expr<=expr \n");
            (yyval.exprNode) = newExpr(constbool_e);
            (yyval.exprNode)->truelist = newList(nextQuadLabel());
            (yyval.exprNode)->falselist = newList(nextQuadLabel()+1);
            printf("t %d f%d\n", (yyval.exprNode)->truelist[0], (yyval.exprNode)->falselist[0]);
            if(!isvalid_arithmeticCheck((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type) ){
                yyerror("Invalid arithmetic expressions");
                (yyval.exprNode) = nil_expr;
            }else {
                if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute_rel(if_lesseq, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                }
                emit(if_lesseq, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode));
                emit(jump);
            }
        }
#line 1965 "parser.cpp" /* yacc.c:1646  */
    break;

  case 30:
#line 419 "parser.y" /* yacc.c:1646  */
    {
            printf("opexr->expr==expr \n");
            if(!is_same((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type) ){
                yyerror("Invalid operands to boolean expression");
                (yyval.exprNode) = nil_expr;
            }else {
                (yyval.exprNode) = newExpr(constbool_e);
                (yyval.exprNode)->truelist = newList(nextQuadLabel());
                (yyval.exprNode)->falselist = newList(nextQuadLabel()+1);
                printf("t %d f%d\n", (yyval.exprNode)->truelist[0], (yyval.exprNode)->falselist[0]);
                if((yyvsp[-2].exprNode)->type == constbool_e && (yyvsp[0].exprNode)->type == constbool_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute(if_eq, (bool)(yyvsp[-2].exprNode)->boolConst ,(bool) (yyvsp[0].exprNode)->boolConst);
                }else if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute_rel(if_eq, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                }else if(((yyvsp[-2].exprNode)->type == newtable_e && (yyvsp[0].exprNode)->type == nil_e) || 
                                ((yyvsp[0].exprNode)->type == newtable_e && (yyvsp[-2].exprNode)->type == nil_e)){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = false;
                }
                emit(if_eq, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode));
                emit(jump);
            }
        }
#line 1995 "parser.cpp" /* yacc.c:1646  */
    break;

  case 31:
#line 444 "parser.y" /* yacc.c:1646  */
    {
            if(!is_same((yyvsp[-2].exprNode)->type,(yyvsp[0].exprNode)->type) ){
                yyerror("Invalid operands to boolean expression");
                (yyval.exprNode) = nil_expr;
            }else {
                printf("opexr->expr!=expr \n");
                (yyval.exprNode) = newExpr(constbool_e);
                (yyval.exprNode)->truelist = newList(nextQuadLabel());
                (yyval.exprNode)->falselist = newList(nextQuadLabel()+1);
                printf(" t %d f%d\n", (yyval.exprNode)->truelist[0], (yyval.exprNode)->falselist[0]);
                if((yyvsp[-2].exprNode)->type == constbool_e && (yyvsp[0].exprNode)->type == constbool_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute(if_noteq, (bool)(yyvsp[-2].exprNode)->boolConst ,(bool)(yyvsp[0].exprNode)->boolConst);
                }else if((yyvsp[-2].exprNode)->type == constnum_e && (yyvsp[0].exprNode)->type == constnum_e){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = compute_rel(if_noteq, (yyvsp[-2].exprNode)->numConst , (yyvsp[0].exprNode)->numConst);
                }else if(((yyvsp[-2].exprNode)->type == newtable_e && (yyvsp[0].exprNode)->type == nil_e) || 
                                ((yyvsp[0].exprNode)->type == newtable_e && (yyvsp[-2].exprNode)->type == nil_e)){
                    (yyval.exprNode)->sym = new_tmp(yylineno);
                    (yyval.exprNode)->boolConst = true;
                } 
                emit(if_noteq, (yyvsp[-2].exprNode) , (yyvsp[0].exprNode));
                emit(jump);
            }}
#line 2024 "parser.cpp" /* yacc.c:1646  */
    break;

  case 32:
#line 469 "parser.y" /* yacc.c:1646  */
    {
                if((yyvsp[-1].exprNode)->truelist.empty() && (yyvsp[-1].exprNode)->falselist.empty()){
                    (yyvsp[-1].exprNode)->truelist = newList(nextQuadLabel());
                    (yyvsp[-1].exprNode)->falselist = newList(nextQuadLabel()+1);
                    emit(
                        if_eq, (yyvsp[-1].exprNode) ,newExpr_constBool(1));
                    emit(jump);
                }

            }
#line 2039 "parser.cpp" /* yacc.c:1646  */
    break;

  case 33:
#line 478 "parser.y" /* yacc.c:1646  */
    {      
                printf("opexr->expr || expr \n");
                
                patchLabel((yyvsp[-4].exprNode)->falselist, (yyvsp[-1].intValue)); // -1 for and quad
                if((yyvsp[0].exprNode)->truelist.empty() && (yyvsp[0].exprNode)->falselist.empty()){
                    (yyvsp[0].exprNode)->truelist = newList(nextQuadLabel());
                    (yyvsp[0].exprNode)->falselist = newList(nextQuadLabel()+1);
                    emit(
                        if_eq, (yyvsp[0].exprNode) ,newExpr_constBool(1));
                    emit(jump);
                }

                (yyval.exprNode) = newExpr(boolexpr_e);
                (yyval.exprNode)->sym = new_tmp(yylineno);
                // $$->boolConst = true;
                assert((yyvsp[-4].exprNode) && (yyvsp[0].exprNode));
                emit( assign_op, newExpr_constBool(0), NULL, (yyval.exprNode));
                
                emit( jump, NULL, NULL, NULL, nextQuadLabel()+2);
                
                emit(assign_op, newExpr_constBool(1), NULL, (yyval.exprNode));
                // printf("f %d\n", $1->falselist[0]);
                printf("patching false with %d: ------------------",(yyvsp[-1].intValue));
                (yyval.exprNode)->truelist = merge((yyvsp[-4].exprNode)->truelist, (yyvsp[0].exprNode)->truelist);
                (yyval.exprNode)->falselist = (yyvsp[0].exprNode)->falselist;
                patchLabel((yyval.exprNode)->truelist, nextQuadLabel()-1);
                    
                }
#line 2072 "parser.cpp" /* yacc.c:1646  */
    break;

  case 34:
#line 506 "parser.y" /* yacc.c:1646  */
    {
                
                if((yyvsp[-1].exprNode)->truelist.empty() && (yyvsp[-1].exprNode)->falselist.empty()){
                    (yyvsp[-1].exprNode)->truelist = newList(nextQuadLabel());
                    (yyvsp[-1].exprNode)->falselist = newList(nextQuadLabel()+1);
                    emit(if_eq, (yyvsp[-1].exprNode) ,newExpr_constBool(1));
                    emit(jump);
                }
                
                }
#line 2087 "parser.cpp" /* yacc.c:1646  */
    break;

  case 35:
#line 515 "parser.y" /* yacc.c:1646  */
    {
                printf("opexr->expr&&expr \n");

                patchLabel((yyvsp[-4].exprNode)->truelist, (yyvsp[-1].intValue));
                if((yyvsp[0].exprNode)->truelist.empty() && (yyvsp[0].exprNode)->falselist.empty()){
                    (yyvsp[0].exprNode)->truelist = newList(nextQuadLabel());
                    (yyvsp[0].exprNode)->falselist = newList(nextQuadLabel()+1);
                    emit(
                        if_eq, (yyvsp[0].exprNode) ,newExpr_constBool(1));
                        emit(jump);
                }

                (yyval.exprNode) = newExpr(boolexpr_e);
                (yyval.exprNode)->sym = new_tmp(yylineno);
                // $$->boolConst = false;
                assert((yyvsp[-4].exprNode)&&(yyvsp[0].exprNode));
                emit(
                        assign_op, newExpr_constBool(0), NULL, (yyval.exprNode));
                emit(
                        jump, NULL, NULL, NULL,nextQuadLabel()+2);
                emit(
                        assign_op, newExpr_constBool(1), NULL, (yyval.exprNode));

                printf("patching true with %d: ----------------",(yyvsp[-1].intValue));
                (yyval.exprNode)->truelist = (yyvsp[0].exprNode)->truelist;
                (yyval.exprNode)->falselist = merge((yyvsp[-4].exprNode)->falselist, (yyvsp[0].exprNode)->falselist);
                patchLabel((yyval.exprNode)->falselist, nextQuadLabel()-3);

                }
#line 2121 "parser.cpp" /* yacc.c:1646  */
    break;

  case 36:
#line 546 "parser.y" /* yacc.c:1646  */
    {
                printf("term->(expr) \n");
                (yyval.exprNode) = (yyvsp[-1].exprNode);
                }
#line 2130 "parser.cpp" /* yacc.c:1646  */
    break;

  case 37:
#line 550 "parser.y" /* yacc.c:1646  */
    {
                printf("term->NOTexpr \n");
                assert((yyvsp[0].exprNode));
                (yyval.exprNode) = newExpr(boolexpr_e);
                (yyval.exprNode)->sym = new_tmp(yylineno);

                (yyval.exprNode)->truelist = (yyvsp[0].exprNode)->falselist;
                (yyval.exprNode)->falselist = (yyvsp[0].exprNode)->truelist;
                emit(not_op, (yyvsp[0].exprNode), NULL, (yyval.exprNode));
                }
#line 2145 "parser.cpp" /* yacc.c:1646  */
    break;

  case 38:
#line 560 "parser.y" /* yacc.c:1646  */
    {
                assert((yyvsp[0].exprNode));
                printf("term->-expr \n");
                checkUminus((yyvsp[0].exprNode));
                (yyval.exprNode) = newExpr(arithexpr_e);
                (yyval.exprNode)->sym = new_tmp(yylineno);
                emit(uminus_op, (yyvsp[0].exprNode), NULL , (yyval.exprNode));
                }
#line 2158 "parser.cpp" /* yacc.c:1646  */
    break;

  case 39:
#line 568 "parser.y" /* yacc.c:1646  */
    {}
#line 2164 "parser.cpp" /* yacc.c:1646  */
    break;

  case 40:
#line 568 "parser.y" /* yacc.c:1646  */
    {
                printf("term->++lvalue \n"); 
                if((yyvsp[0].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym) <= last_func.top() && (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        (yyval.exprNode) = nil_expr;
                    }else {
                        if((yyvsp[0].exprNode)->sym->type == USERFUNC){ 
                            (yyval.exprNode) = nil_expr;
                            yyerror("Using ProgramFunc as an lvalue");
                        }else if ((yyvsp[0].exprNode)->sym->type == LIBFUNC){ 
                            (yyval.exprNode) = nil_expr;
                            yyerror("Using LibFunc as an lvalue");
                        }else{
                            if ((yyvsp[0].exprNode)->type == tableitem_e) {
                                (yyval.exprNode) = emit_ifTableItem((yyvsp[0].exprNode));
                                emit(
                                    add, (yyval.exprNode), newExpr_constNum(1), (yyval.exprNode));
                                emit( tablesetelem, (yyvsp[0].exprNode)->index, (yyvsp[0].exprNode), (yyval.exprNode));
                            } else {
                                emit(
                                    add, (yyvsp[0].exprNode), newExpr_constNum(1), (yyvsp[0].exprNode));
                                (yyval.exprNode) = newExpr(arithexpr_e);
                                (yyval.exprNode)->sym = new_tmp(yylineno);
                                emit(assign_op, (yyvsp[0].exprNode), NULL, (yyval.exprNode));
                            }
                        }                
                    }
                }else{ //define as new var
                    (yyval.exprNode) = nil_expr;
                    yyerror("variable undefined");
                }
            }
#line 2202 "parser.cpp" /* yacc.c:1646  */
    break;

  case 41:
#line 601 "parser.y" /* yacc.c:1646  */
    {
                printf("term->lvalue++ \n");
                if((yyvsp[-1].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym) <= last_func.top() && (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        (yyval.exprNode) = nil_expr;
                    }else{
                        if((yyvsp[-1].exprNode)->sym->type == USERFUNC){ 
                            (yyval.exprNode) = nil_expr;
                            yyerror("Using ProgramFunc as an lvalue");
                        }else if((yyvsp[-1].exprNode)->sym->type == LIBFUNC){ 
                            (yyval.exprNode) = nil_expr;
                            yyerror("Using LibFunc as an lvalue");
                        }else{
                            (yyval.exprNode) = newExpr(var_e);
                            (yyval.exprNode)->sym = new_tmp(yylineno);

                            if( (yyvsp[-1].exprNode)->type == tableitem_e ) {
                                expr* value = emit_ifTableItem((yyvsp[-1].exprNode));
                                emit(assign_op, value, NULL, (yyval.exprNode));
                                emit(
                                    add, value, newExpr_constNum(1), value);
                                emit( tablesetelem, (yyvsp[-1].exprNode)->index, value, (yyvsp[-1].exprNode));
                            }else {
                                emit(assign_op, (yyvsp[-1].exprNode), NULL, (yyval.exprNode));
                                emit(
                                    add, (yyvsp[-1].exprNode), newExpr_constNum(1), (yyvsp[-1].exprNode));
                            }
                        }
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                    (yyval.exprNode) = nil_expr;
                } 
                
                }
#line 2243 "parser.cpp" /* yacc.c:1646  */
    break;

  case 42:
#line 637 "parser.y" /* yacc.c:1646  */
    {}
#line 2249 "parser.cpp" /* yacc.c:1646  */
    break;

  case 43:
#line 637 "parser.y" /* yacc.c:1646  */
    {
                printf("term->--lvalue \n");
                if((yyvsp[0].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym) <= last_func.top() && (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym)!=0 ){
                        yyerror("Cant reference variable out of scope");
                        (yyval.exprNode) = nil_expr;
                    }else {   
                        if((yyvsp[0].exprNode)->sym->type == USERFUNC){ 
                            yyerror("Using ProgramFunc as an lvalue");
                            (yyval.exprNode) = nil_expr;
                        }else if((yyvsp[0].exprNode)->sym->type == LIBFUNC){ 
                            yyerror("Using LibFunc as an lvalue");
                            (yyval.exprNode) = nil_expr;
                        }else{
                            if ((yyvsp[0].exprNode)->type == tableitem_e) {
                                (yyval.exprNode) = emit_ifTableItem((yyvsp[0].exprNode));   
                                emit(
                                    sub, (yyval.exprNode), newExpr_constNum(1), (yyval.exprNode));
                                emit( tablesetelem, (yyvsp[0].exprNode)->index, (yyvsp[0].exprNode), (yyval.exprNode));
                            } else {
                                emit(
                                    sub, (yyvsp[0].exprNode), newExpr_constNum(1), (yyvsp[0].exprNode));
                                (yyval.exprNode) = newExpr(arithexpr_e);
                                (yyval.exprNode)->sym = new_tmp(yylineno);
                                emit(assign_op, (yyvsp[0].exprNode), NULL, (yyval.exprNode));
                            }
                        }
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                    (yyval.exprNode) = nil_expr;
                }
                }
#line 2287 "parser.cpp" /* yacc.c:1646  */
    break;

  case 44:
#line 670 "parser.y" /* yacc.c:1646  */
    {
                printf("term->lvalue-- \n");
                if((yyvsp[-1].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym) <= last_func.top() && (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        (yyval.exprNode) = nil_expr;
                    }else {   
                        if((yyvsp[-1].exprNode)->sym->type == USERFUNC){
                            yyerror("Using ProgramFunc as an lvalue");
                            (yyval.exprNode) = nil_expr;
                        }else if((yyvsp[-1].exprNode)->sym->type == LIBFUNC){ 
                            yyerror("Using LibFunc as an lvalue");
                            (yyval.exprNode) = nil_expr;
                        }else{    
                            if ((yyvsp[-1].exprNode)->type == tableitem_e) {
                                (yyval.exprNode) = emit_ifTableItem((yyvsp[-1].exprNode));
                                emit(assign_op, (yyval.exprNode), NULL, (yyval.exprNode));
                                emit(
                                    sub, (yyval.exprNode), newExpr_constNum(1), (yyval.exprNode));
                                emit( tablesetelem, (yyvsp[-1].exprNode)->index, (yyvsp[-1].exprNode), (yyval.exprNode));
                            } else {
                                emit(assign_op, (yyvsp[-1].exprNode), NULL, (yyval.exprNode));
                                (yyval.exprNode) = newExpr(arithexpr_e);
                                (yyval.exprNode)->sym = new_tmp(yylineno);
                                emit(
                                    sub, (yyvsp[-1].exprNode), newExpr_constNum(1), (yyvsp[-1].exprNode));
                            }
                        }
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                    (yyval.exprNode) = nil_expr;
                }
            }
#line 2326 "parser.cpp" /* yacc.c:1646  */
    break;

  case 45:
#line 704 "parser.y" /* yacc.c:1646  */
    {
                printf("term->primary \n");
                assert((yyvsp[0].exprNode));
                (yyval.exprNode) = (yyvsp[0].exprNode);
                }
#line 2336 "parser.cpp" /* yacc.c:1646  */
    break;

  case 46:
#line 710 "parser.y" /* yacc.c:1646  */
    {
                printf("assignexpr->lvalue=expr \n");
                if((yyvsp[-2].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[-2].exprNode)->sym) <= last_func.top() && (int)symbol_table.get_scope((yyvsp[-2].exprNode)->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        (yyval.exprNode) = nil_expr;
                    }else if((yyvsp[-2].exprNode)->sym->type == USERFUNC){ 
                        yyerror("Cannot assign to function");
                        (yyval.exprNode) = nil_expr;
                    }else if((yyvsp[-2].exprNode)->sym->type == LIBFUNC){
                        yyerror("Cannot assign to libfunc");
                        (yyval.exprNode) = nil_expr;
                    }else{ 
                        if((yyvsp[-2].exprNode)->type == tableitem_e){
                            emit( //that is lvalue[index] = expr;
                                tablesetelem, (yyvsp[-2].exprNode)->index, (yyvsp[0].exprNode), (yyvsp[-2].exprNode));
                            //The value f the assignment expression should be gained
                            (yyval.exprNode) = emit_ifTableItem((yyvsp[-2].exprNode));
                            (yyval.exprNode)->type = assignexpr_e;
                        }else{
                            //change_type($lvalue,$expr);

                            // $lvalue->sym->type = $expr->sym->type;
                            emit(assign_op, (yyvsp[0].exprNode), NULL, (yyvsp[-2].exprNode));
                            printf("lvalue %s\n",symbol_table.get_name((yyvsp[-2].exprNode)->sym));
                            // if($lvalue->type == constnum_e)
                            //     $assignexpr = newExpr(constnum_e);
                            // if($lvalue->type == constbool_e)
                            //     $assignexpr = newExpr(constbool_e);
                            // if($lvalue->type == conststring_e)
                            //     $assignexpr = newExpr(conststring_e);
                            // else
                            (yyval.exprNode) = newExpr(assignexpr_e);
                            (yyval.exprNode)->sym = new_tmp(yylineno);
                            emit(
                                assign_op, (yyvsp[-2].exprNode), NULL, (yyval.exprNode));
                        }
                
                    }
                }else{ //define as new var
                    printf("member undefined in : assignexpr->lvalue=expr \n");
                    (yyval.exprNode) = nil_expr;
                    // assert(false);
                }    
                    }
#line 2386 "parser.cpp" /* yacc.c:1646  */
    break;

  case 47:
#line 756 "parser.y" /* yacc.c:1646  */
    {
                printf("primary->lvalue \n");
                if((yyvsp[0].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym) <= last_func.top()  && (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                        (yyvsp[0].exprNode) = nil_expr;
                    }
                }else{ //define as new var
                    if(return_flag){
                        yyerror("return values undefined in this scope");
                        (yyvsp[0].exprNode) = nil_expr;
                    }else{
                        (yyvsp[0].exprNode) = newExpr(var_e); 
                        (yyvsp[0].exprNode)->sym = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    }     
                }
                (yyval.exprNode) = emit_ifTableItem((yyvsp[0].exprNode));
                }
#line 2409 "parser.cpp" /* yacc.c:1646  */
    break;

  case 48:
#line 774 "parser.y" /* yacc.c:1646  */
    {
                printf("primary->call \n");
                assert((yyvsp[0].exprNode));
                (yyval.exprNode) = (yyvsp[0].exprNode);
                }
#line 2419 "parser.cpp" /* yacc.c:1646  */
    break;

  case 49:
#line 779 "parser.y" /* yacc.c:1646  */
    { 
                printf("primary->objectdef \n");
                assert((yyvsp[0].exprNode));
                (yyval.exprNode) = (yyvsp[0].exprNode);
                }
#line 2429 "parser.cpp" /* yacc.c:1646  */
    break;

  case 50:
#line 784 "parser.y" /* yacc.c:1646  */
    {
                printf("primary->(funcdef) \n");
                (yyval.exprNode) = newExpr(programfunc_e);
                (yyval.exprNode)->sym = (yyvsp[-1].symbol);                
                }
#line 2439 "parser.cpp" /* yacc.c:1646  */
    break;

  case 51:
#line 789 "parser.y" /* yacc.c:1646  */
    {(yyval.exprNode) = (yyvsp[0].exprNode);printf("primary->const \n");}
#line 2445 "parser.cpp" /* yacc.c:1646  */
    break;

  case 52:
#line 792 "parser.y" /* yacc.c:1646  */
    {
                printf("lvalue->ids '%s'\n",yylval.stringValue);
                SymbolTableEntry *tmp = symbol_table.lookUp_allscope(yylval.stringValue); 
                if(tmp == NULL){
                    printf("didnt find in sym\n");
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    assert(tmp);
                    tmp->space = currScopeSpace();
                    tmp->offset = currScopeOffset();
                    inCurrScopeOffset();
                }
                (yyval.exprNode) = lvalue_expr(tmp);

            }
#line 2464 "parser.cpp" /* yacc.c:1646  */
    break;

  case 53:
#line 806 "parser.y" /* yacc.c:1646  */
    {     
                printf("local id %s \n",yylval.stringValue);
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, (scope?LOCAL:GLOBAL));
                    assert(tmp);
                    tmp->space = currScopeSpace();
                    tmp->offset = currScopeOffset();
                    inCurrScopeOffset();
                }else{
                    if(tmp->type == LIBFUNC)
                        yyerror("shadowing of library functions not allowed");
                }
                (yyval.exprNode) = lvalue_expr(tmp);
            }
#line 2484 "parser.cpp" /* yacc.c:1646  */
    break;

  case 54:
#line 821 "parser.y" /* yacc.c:1646  */
    {
                unsigned int scope_tmp = scope;
                scope = 0; 
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 

                if(tmp ==  NULL){
                    yyerror("global variable not found");
                    (yyval.exprNode) = NULL;                    
                }else{
                    (yyval.exprNode) = lvalue_expr(tmp);                    
                } //undefined
                
                scope = scope_tmp;  
                printf("lvalue->::id \n");
                }
#line 2504 "parser.cpp" /* yacc.c:1646  */
    break;

  case 55:
#line 836 "parser.y" /* yacc.c:1646  */
    { (yyval.exprNode) = (yyvsp[0].exprNode); printf("lvalue->member \n");}
#line 2510 "parser.cpp" /* yacc.c:1646  */
    break;

  case 56:
#line 838 "parser.y" /* yacc.c:1646  */
    {
                (yyval.exprNode) = (yyvsp[0].exprNode);
            }
#line 2518 "parser.cpp" /* yacc.c:1646  */
    break;

  case 57:
#line 841 "parser.y" /* yacc.c:1646  */
    { 
                printf("member->call().id \n"); 
               
                assert((yyval.exprNode)); 
               
                (yyval.exprNode) = member_item((yyval.exprNode), strdup(yylval.stringValue)); 
            }
#line 2530 "parser.cpp" /* yacc.c:1646  */
    break;

  case 58:
#line 848 "parser.y" /* yacc.c:1646  */
    { 
                printf("member->[expr] \n"); 
 
                char* name = (char*)symbol_table.get_name((yyvsp[-1].exprNode)->sym); 
                (yyval.exprNode) = member_item((yyval.exprNode), name); 
            }
#line 2541 "parser.cpp" /* yacc.c:1646  */
    break;

  case 59:
#line 856 "parser.y" /* yacc.c:1646  */
    {
                if((yyvsp[0].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym) <= last_func.top()  && (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym)!=0){
                        yyerror("Cant reference variable out of scope");
                    }
                    else {
                        if((yyvsp[0].exprNode)->sym->type == USERFUNC) 
                            yyerror("cannot member function");
                        else if((yyvsp[0].exprNode)->sym->type == LIBFUNC) 
                            yyerror("cannot member libfunc");
                    }
                }else{ //define as new var
                    yyerror("variable undefined");
                }
            }
#line 2561 "parser.cpp" /* yacc.c:1646  */
    break;

  case 60:
#line 871 "parser.y" /* yacc.c:1646  */
    { 
                    printf("member->lvalue.id \n");
                    (yyval.exprNode) = member_item((yyvsp[-3].exprNode), strdup(yylval.stringValue));
                }
#line 2570 "parser.cpp" /* yacc.c:1646  */
    break;

  case 61:
#line 875 "parser.y" /* yacc.c:1646  */
    {
                if((yyvsp[0].exprNode) != NULL) {
                    if( (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym) <= last_func.top()  && (int)symbol_table.get_scope((yyvsp[0].exprNode)->sym)!=0){
                            yyerror("Cant reference variable out of scope");
                    }else{
                        if((yyvsp[0].exprNode)->sym->type == USERFUNC){
                            yyerror("cannot use function as array");
                        } else if((yyvsp[0].exprNode)->sym->type == LIBFUNC) {
                            yyerror("cannot use libfunc as array");
                        }
                    }
                }else{ 
                    //define as new var
                    yyerror("array undefined");
                }
            }
#line 2591 "parser.cpp" /* yacc.c:1646  */
    break;

  case 62:
#line 890 "parser.y" /* yacc.c:1646  */
    { 
                (yyvsp[-4].exprNode) = emit_ifTableItem((yyvsp[-4].exprNode));
                (yyval.exprNode) = newExpr(tableitem_e);
                (yyval.exprNode)->sym = (yyvsp[-4].exprNode)->sym;
                (yyval.exprNode)->index = (yyvsp[-1].exprNode);
                printf("member->lvalue[expr] \n");
                }
#line 2603 "parser.cpp" /* yacc.c:1646  */
    break;

  case 63:
#line 899 "parser.y" /* yacc.c:1646  */
    {
                printf("call->call(elist) \n");
                (yyval.exprNode) = make_call((yyval.exprNode), (yyvsp[-1].exprNode));
                }
#line 2612 "parser.cpp" /* yacc.c:1646  */
    break;

  case 64:
#line 903 "parser.y" /* yacc.c:1646  */
    {
                printf("call->lvaluecallsuffix \n");
                if((yyvsp[-1].exprNode) != NULL) {
                    if((yyvsp[0].call_l)->method){ //method call
                        expr* self = (yyvsp[-1].exprNode);
                        (yyvsp[-1].exprNode) = emit_ifTableItem(member_item(self,(yyvsp[0].call_l)->name));
                        self->next = (yyvsp[0].call_l)->elist;
                        (yyvsp[0].call_l)->elist = self;   
                        (yyval.exprNode) = make_call((yyvsp[-1].exprNode), (yyvsp[0].call_l)->elist);
                    }else{ //normcall
                        assert((yyvsp[-1].exprNode)->sym);                                        //make sure its defined
                        if((yyvsp[-1].exprNode)->type == var_e){
                            if( (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym) <= last_func.top()  &&
                             (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym)!=0){
                                yyerror("Cant reference variable out of scope");
                                (yyval.exprNode) = NULL;
                            }
                            (yyval.exprNode) = make_call((yyvsp[-1].exprNode), (yyvsp[0].call_l)->elist);
                        }else if((yyvsp[-1].exprNode)->sym->type != USERFUNC && (yyvsp[-1].exprNode)->sym->type != LIBFUNC  && (yyvsp[-1].exprNode)->type !=tableitem_e) {
                            if( (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym) <= last_func.top()  &&
                             (int)symbol_table.get_scope((yyvsp[-1].exprNode)->sym)!=0){
                                yyerror("Cant reference variable out of scope");
                                (yyval.exprNode) = NULL;
                            }else if((yyvsp[-1].exprNode)->sym->type == LOCAL||(yyvsp[-1].exprNode)->sym->type == GLOBAL||(yyvsp[-1].exprNode)->sym->type == FORMAL) {
                                printf("\n%s",symbol_table.get_name((yyvsp[-1].exprNode)->sym));
                                yyerror("cant use variable as function");
                                (yyval.exprNode) = NULL;
                            }else{
                                assert(false);
                            }
                        }else{
                            (yyval.exprNode) = make_call((yyvsp[-1].exprNode), (yyvsp[0].call_l)->elist);
                        }
                    }

                }else{ //define as new var
                    yyerror("function not found");
                }
            }
#line 2656 "parser.cpp" /* yacc.c:1646  */
    break;

  case 65:
#line 942 "parser.y" /* yacc.c:1646  */
    {
                printf("call->(funcdef)(elist) \n");
                expr* func = newExpr(programfunc_e);
                func->sym = (yyvsp[-4].symbol);
                (yyval.exprNode) = make_call(func, (yyvsp[-1].exprNode));
                }
#line 2667 "parser.cpp" /* yacc.c:1646  */
    break;

  case 66:
#line 949 "parser.y" /* yacc.c:1646  */
    {    
                printf("callsuffix->normcall \n");
                (yyval.call_l) = (yyvsp[0].call_l); 
            }
#line 2676 "parser.cpp" /* yacc.c:1646  */
    break;

  case 67:
#line 953 "parser.y" /* yacc.c:1646  */
    { 
                printf("callsuffix->methodcall \n");
                (yyval.call_l) = (yyvsp[0].call_l); 
            }
#line 2685 "parser.cpp" /* yacc.c:1646  */
    break;

  case 68:
#line 958 "parser.y" /* yacc.c:1646  */
    {
                printf("normcall->(elist) \n");
                (yyval.call_l) = new call_l();
                (yyval.call_l)->elist = (yyvsp[-1].exprNode);
                (yyval.call_l)->method = false;
                (yyval.call_l)->name = NULL;

                }
#line 2698 "parser.cpp" /* yacc.c:1646  */
    break;

  case 69:
#line 966 "parser.y" /* yacc.c:1646  */
    { 
                (yyval.stringValue) = strdup(yylval.stringValue);
            }
#line 2706 "parser.cpp" /* yacc.c:1646  */
    break;

  case 70:
#line 970 "parser.y" /* yacc.c:1646  */
    {
                (yyval.call_l) = new call_l();
                printf("methodcall->..id(elist) \n");
                (yyval.call_l)->elist = (yyvsp[-1].exprNode);
                (yyval.call_l)->name = strdup((yyvsp[-3].stringValue));
                (yyval.call_l)->method = true;
                }
#line 2718 "parser.cpp" /* yacc.c:1646  */
    break;

  case 71:
#line 978 "parser.y" /* yacc.c:1646  */
    {
                printf("elist_l->expr \n");
                (yyval.exprNode) = (yyvsp[0].exprNode);
                }
#line 2727 "parser.cpp" /* yacc.c:1646  */
    break;

  case 72:
#line 982 "parser.y" /* yacc.c:1646  */
    {
                printf("elist_l->elist_l,expr \n");
                (yyvsp[0].exprNode)->next = (yyvsp[-2].exprNode);
                (yyval.exprNode) = (yyvsp[0].exprNode);
                }
#line 2737 "parser.cpp" /* yacc.c:1646  */
    break;

  case 73:
#line 988 "parser.y" /* yacc.c:1646  */
    {
                printf("elist->elist_l \n");
                (yyval.exprNode)  = (yyvsp[0].exprNode);
                }
#line 2746 "parser.cpp" /* yacc.c:1646  */
    break;

  case 74:
#line 992 "parser.y" /* yacc.c:1646  */
    {
                printf("elist->empty \n");
                (yyval.exprNode) = nil_expr;
                }
#line 2755 "parser.cpp" /* yacc.c:1646  */
    break;

  case 75:
#line 997 "parser.y" /* yacc.c:1646  */
    {
                printf("objectdef->[elist]\n");
                expr* t = newExpr(newtable_e);
                t->sym = new_tmp(yylineno);
                emit(tablecreate,NULL,NULL, t);
                double i =0;

                expr* x = (yyvsp[-1].exprNode);
                reverse_list(&x);
                while(x!=NULL && x->type != nil_e){
                    emit(tablesetelem, newExpr_constNum(i++), x, t);
                    x = x->next;
                }
                (yyval.exprNode) = t;
                }
#line 2775 "parser.cpp" /* yacc.c:1646  */
    break;

  case 76:
#line 1012 "parser.y" /* yacc.c:1646  */
    {
                assert((yyvsp[-1].exprNode));
                printf("objectdef->[indexed] \n");
                expr* t = newExpr(newtable_e);
                t->sym = new_tmp(yylineno);
                emit(tablecreate,NULL,NULL, t);
                double i =0;

                expr* x = (yyvsp[-1].exprNode);
                reverse_list(&x);
                while(x!=NULL &&x->type != nil_e){
                    emit(tablesetelem, x->index, x, t);
                    x = x->next;
                }
                (yyval.exprNode) = t;
                
                
                }
#line 2798 "parser.cpp" /* yacc.c:1646  */
    break;

  case 77:
#line 1031 "parser.y" /* yacc.c:1646  */
    {
                printf("indexedelem->{expr:expr} \n");
                assert((yyvsp[-3].exprNode) && (yyvsp[-1].exprNode));
                (yyval.exprNode) = (yyvsp[-1].exprNode);
                (yyval.exprNode)->index = (yyvsp[-3].exprNode);
            }
#line 2809 "parser.cpp" /* yacc.c:1646  */
    break;

  case 78:
#line 1039 "parser.y" /* yacc.c:1646  */
    {
                printf("indexed->indexedelem \n");
                assert((yyvsp[0].exprNode));
                (yyval.exprNode) = (yyvsp[0].exprNode);
            }
#line 2819 "parser.cpp" /* yacc.c:1646  */
    break;

  case 79:
#line 1044 "parser.y" /* yacc.c:1646  */
    {
                printf("indexed->indexed,indexedelem \n");
                (yyvsp[0].exprNode)->next = (yyvsp[-2].exprNode);
                (yyval.exprNode) = (yyvsp[0].exprNode);
            }
#line 2829 "parser.cpp" /* yacc.c:1646  */
    break;

  case 80:
#line 1050 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = merge((yyvsp[-1].stmt_l), (yyvsp[0].stmt_l));
            }
#line 2837 "parser.cpp" /* yacc.c:1646  */
    break;

  case 81:
#line 1053 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();
            }
#line 2845 "parser.cpp" /* yacc.c:1646  */
    break;

  case 82:
#line 1057 "parser.y" /* yacc.c:1646  */
    { 
                printf("\n\n-----enter block ------ \n"); }
#line 2852 "parser.cpp" /* yacc.c:1646  */
    break;

  case 83:
#line 1059 "parser.y" /* yacc.c:1646  */
    { 
                    printf("\n-----exit block ------\n\n"); 
                    (yyval.stmt_l) = (yyvsp[-1].stmt_l);
                symbol_table.hide(scope--);}
#line 2861 "parser.cpp" /* yacc.c:1646  */
    break;

  case 84:
#line 1064 "parser.y" /* yacc.c:1646  */
    {
                printf("func_name->func_id \n");
                (yyval.stringValue) = strdup(yylval.stringValue);
                }
#line 2870 "parser.cpp" /* yacc.c:1646  */
    break;

  case 85:
#line 1068 "parser.y" /* yacc.c:1646  */
    {
                printf("func_name->anonymous \n");
                std::ostringstream ss;
                ss << "$anonymous" << anonymous_count;
                anonymous_count++;

                (yyval.stringValue) = strdup(ss.str().c_str());
                }
#line 2883 "parser.cpp" /* yacc.c:1646  */
    break;

  case 86:
#line 1077 "parser.y" /* yacc.c:1646  */
    {
                printf("funcdef->prefix args body\n");
                exitScopeSpace();
                (yyvsp[-2].symbol)->value.funcVal->totalLocals  = functionLocalOffset;
                functionLocalOffset = functionLocalsStack.top();
                functionLocalsStack.pop();
                (yyval.symbol) = (yyvsp[-2].symbol);
                emit_function(funcend, lvalue_expr((yyvsp[-2].symbol)));
                patchLabel((yyvsp[-3].intValue), nextQuadLabel());
                if((yyvsp[0].stmt_l) != NULL ){
                    patchLabel((yyvsp[0].stmt_l)->breaklist, nextQuadLabel() - 1);
                }
            }
#line 2901 "parser.cpp" /* yacc.c:1646  */
    break;

  case 87:
#line 1091 "parser.y" /* yacc.c:1646  */
    { 
                printf("funcdef->( ");
                last_func.push(scope); 
                
                (yyval.symbol) =symbol_table.lookUp_curscope((yyvsp[0].stringValue)); 
                if((yyval.symbol) ==  NULL) {//undefined
                    (yyval.symbol) = symbol_table.insert((yyvsp[0].stringValue), yylineno, USERFUNC);
                    (yyval.symbol)->value.funcVal->iaddress = nextQuadLabel();
                    emit_function(funcstart,lvalue_expr((yyval.symbol)));
                }else{
                    switch( (yyval.symbol)->type ){
                        case LIBFUNC:{
                            yyerror("shadowing of library functions not allowed");
                            break;
                        }
                        case USERFUNC:{
                            yyerror("function name already used as func");// error: var redefined as a function
                            break;
                        }
                        case GLOBAL :{}
                        case FORMAL :{}
                        case LOCAL :{
                            yyerror("function name already used as var");// error: var redefined as a function
                            break;
                        } 
                    }
                }
                
                functionLocalsStack.push(functionLocalOffset);
                enterScopeSpace();
                resetFormalArgsOffset();
                scope++; 
                }
#line 2939 "parser.cpp" /* yacc.c:1646  */
    break;

  case 88:
#line 1125 "parser.y" /* yacc.c:1646  */
    {
                    printf("funcdef->) \n");
                    enterScopeSpace();
                    resetFunctionLocalOffset();
                }
#line 2949 "parser.cpp" /* yacc.c:1646  */
    break;

  case 89:
#line 1131 "parser.y" /* yacc.c:1646  */
    { 
                    last_func.pop(); 
                    exitScopeSpace();
                    (yyval.stmt_l) = (yyvsp[-1].stmt_l);
                    }
#line 2959 "parser.cpp" /* yacc.c:1646  */
    break;

  case 90:
#line 1137 "parser.y" /* yacc.c:1646  */
    {
                // std::string name = "^" + yylval.intValue;
                // SymbolTableEntry *tmp = symbol_table.lookUp_allscope(name.c_str()); 
                // if(tmp == NULL){
                //     tmp = symbol_table.insert(name.c_str(), 0, LOCAL, 0);
                //     assert(tmp);
                //     tmp->space = currScopeSpace();
                //     tmp->offset = currScopeOffset();
                //     inCurrScopeOffset();
                // }
                // $$ = newExpr(constnum_e);
                // $$->sym = tmp;
                // $$->numConst = yylval.intValue;
                printf("number->integer \n");
                (yyval.exprNode) = newExpr(constnum_e);
                (yyval.exprNode)->numConst = (double)yylval.intValue;
                }
#line 2981 "parser.cpp" /* yacc.c:1646  */
    break;

  case 91:
#line 1154 "parser.y" /* yacc.c:1646  */
    {
                printf("number->real \n");
                (yyval.exprNode) = newExpr(constnum_e);
                (yyval.exprNode)->numConst = yylval.realValue;
                }
#line 2991 "parser.cpp" /* yacc.c:1646  */
    break;

  case 92:
#line 1160 "parser.y" /* yacc.c:1646  */
    {(yyval.exprNode) = (yyvsp[0].exprNode); printf("const->number \n");}
#line 2997 "parser.cpp" /* yacc.c:1646  */
    break;

  case 93:
#line 1161 "parser.y" /* yacc.c:1646  */
    {
                printf("const->string \n");
                (yyval.exprNode) = newExpr(conststring_e);
                (yyval.exprNode)->strConst = strdup(yylval.stringValue);
            }
#line 3007 "parser.cpp" /* yacc.c:1646  */
    break;

  case 94:
#line 1166 "parser.y" /* yacc.c:1646  */
    {
                printf("const->nil \n");
                (yyval.exprNode) = nil_expr;
                (yyval.exprNode)->boolConst = false;
            }
#line 3017 "parser.cpp" /* yacc.c:1646  */
    break;

  case 95:
#line 1171 "parser.y" /* yacc.c:1646  */
    {
                printf("const->true \n");
                (yyval.exprNode) = newExpr(constbool_e);
                (yyval.exprNode)->boolConst = true;
            }
#line 3027 "parser.cpp" /* yacc.c:1646  */
    break;

  case 96:
#line 1176 "parser.y" /* yacc.c:1646  */
    {
                printf("const->false \n");
                (yyval.exprNode) = newExpr(constbool_e);
                (yyval.exprNode)->boolConst = false;
            }
#line 3037 "parser.cpp" /* yacc.c:1646  */
    break;

  case 97:
#line 1183 "parser.y" /* yacc.c:1646  */
    {  
                printf("idlist_l->id1 \n");
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, FORMAL);
                    assert(tmp);
                    // $idlist_l = newExpr(var_e);
                    // $idlist_l->sym = tmp;
                }else{
                    switch( tmp->type ){
                        case GLOBAL:{} 
                        case FORMAL:{} //first arguement can only be global
                        case LOCAL:{
                            yyerror("variable redefined in same scope");
                            break;
                        }
                        case LIBFUNC:{
                            yyerror("formal arguement trying to shadow library func");
                            break;
                        }
                        default:{
                            yyerror("unknown error occured"); 
                        }
                    }
                }
            
            }
#line 3069 "parser.cpp" /* yacc.c:1646  */
    break;

  case 98:
#line 1210 "parser.y" /* yacc.c:1646  */
    {
                printf("idlist_l->idlist_l , id \n");
                SymbolTableEntry* tmp =symbol_table.lookUp_curscope(yylval.stringValue); 
                if(tmp ==  NULL) {//undefined
                    tmp = symbol_table.insert(yylval.stringValue, yylineno, FORMAL);
                    assert(tmp);
                    // expr* e = newExpr(var_e);
                    // e->sym = tmp;
                    // e->next = $idlist_l;
                    // $idlist_l = e;
                }else{
                    switch( tmp->type ){
                        case GLOBAL:{}
                        case FORMAL:{}
                        case LOCAL:{
                            yyerror("variable redefined in same scope");
                            break; 
                        }
                        case LIBFUNC:{
                            yyerror("formal arguement trying to shadow library func");
                            break;
                        }
                        default:{
                            yyerror("unknown error occured"); 
                        }
                    }
                }
            }
#line 3102 "parser.cpp" /* yacc.c:1646  */
    break;

  case 99:
#line 1239 "parser.y" /* yacc.c:1646  */
    {  printf("idlist->idlist_l \n");}
#line 3108 "parser.cpp" /* yacc.c:1646  */
    break;

  case 100:
#line 1240 "parser.y" /* yacc.c:1646  */
    {printf("idlist->emptyidlist \n");}
#line 3114 "parser.cpp" /* yacc.c:1646  */
    break;

  case 101:
#line 1242 "parser.y" /* yacc.c:1646  */
    {
                if(!(yyvsp[-1].exprNode)->truelist.empty() && !(yyvsp[-1].exprNode)->falselist.empty()){
                    patchLabel((yyvsp[-1].exprNode)->truelist,nextQuadLabel()+2);
                    patchLabel((yyvsp[-1].exprNode)->falselist,nextQuadLabel()+1);
                }
                emit(if_eq, (yyvsp[-1].exprNode), newExpr_constBool(true), NULL, nextQuadLabel()+2);
                (yyval.intValue) = (int)nextQuadLabel();
                emit(jump);
                //maybe patch expression
            }
#line 3129 "parser.cpp" /* yacc.c:1646  */
    break;

  case 102:
#line 1253 "parser.y" /* yacc.c:1646  */
    {
                (yyval.intValue) = (int)nextQuadLabel();
                emit(jump);
            }
#line 3138 "parser.cpp" /* yacc.c:1646  */
    break;

  case 103:
#line 1258 "parser.y" /* yacc.c:1646  */
    { 
                printf("ifstmt->\"if(expr) stmt else stmt\" \n"); 
                patchLabel((yyvsp[-3].intValue), (yyvsp[-1].intValue) + 1);
                patchLabel((unsigned int)(yyvsp[-1].intValue), nextQuadLabel());
            }
#line 3148 "parser.cpp" /* yacc.c:1646  */
    break;

  case 104:
#line 1263 "parser.y" /* yacc.c:1646  */
    { 
                printf("ifstmt->\"if(expr) stmt\" \n");
                patchLabel((unsigned int)(yyvsp[-1].intValue), nextQuadLabel());
            }
#line 3157 "parser.cpp" /* yacc.c:1646  */
    break;

  case 105:
#line 1267 "parser.y" /* yacc.c:1646  */
    {
                (yyval.intValue) = (int)nextQuadLabel();   
            }
#line 3165 "parser.cpp" /* yacc.c:1646  */
    break;

  case 106:
#line 1271 "parser.y" /* yacc.c:1646  */
    {
                if(!(yyvsp[-1].exprNode)->truelist.empty() && !(yyvsp[-1].exprNode)->falselist.empty()){
                    patchLabel((yyvsp[-1].exprNode)->truelist,nextQuadLabel()+2);
                    patchLabel((yyvsp[-1].exprNode)->falselist,nextQuadLabel()+1);
                }
                emit(if_eq, (yyvsp[-1].exprNode), newExpr_constBool(true), NULL, nextQuadLabel()+2);
                (yyval.intValue) = (int)nextQuadLabel();
                emit(jump);
            }
#line 3179 "parser.cpp" /* yacc.c:1646  */
    break;

  case 107:
#line 1281 "parser.y" /* yacc.c:1646  */
    {
                printf("whilestmt->\"while(expr) stmt else stmt\" \n");
                emit(jump,NULL, NULL, NULL, (yyvsp[-2].intValue));
                patchLabel((unsigned int)(yyvsp[-1].intValue), nextQuadLabel());
                if((yyvsp[0].stmt_l)!=NULL){
                    patchLabel((yyvsp[0].stmt_l)->breaklist, nextQuadLabel());
                    patchLabel((yyvsp[0].stmt_l)->contlist,  (yyvsp[-2].intValue));
                }
            }
#line 3193 "parser.cpp" /* yacc.c:1646  */
    break;

  case 108:
#line 1290 "parser.y" /* yacc.c:1646  */
    {
        (yyval.intValue) = (int)nextQuadLabel();
        emit(jump);
    }
#line 3202 "parser.cpp" /* yacc.c:1646  */
    break;

  case 109:
#line 1295 "parser.y" /* yacc.c:1646  */
    {
        (yyval.intValue) = (int)nextQuadLabel();
    }
#line 3210 "parser.cpp" /* yacc.c:1646  */
    break;

  case 110:
#line 1299 "parser.y" /* yacc.c:1646  */
    {
                if(!(yyvsp[-1].exprNode)->truelist.empty() && !(yyvsp[-1].exprNode)->falselist.empty()){
                    patchLabel((yyvsp[-1].exprNode)->truelist,nextQuadLabel()+2);
                    patchLabel((yyvsp[-1].exprNode)->falselist,nextQuadLabel()+1);
                }
                (yyval.for_prefix) = new for_prefix();
                (yyval.for_prefix)->test = (yyvsp[-2].intValue);
                (yyval.for_prefix)->enter = (int)nextQuadLabel();
                emit(if_eq, (yyvsp[-1].exprNode), newExpr_constBool(true));
            }
#line 3225 "parser.cpp" /* yacc.c:1646  */
    break;

  case 111:
#line 1310 "parser.y" /* yacc.c:1646  */
    { 
                printf("forstmt->\"for(elist; expr; elist)\" \n");
                assert((yyvsp[-6].for_prefix));
                printf("\n%d %d\n", (yyvsp[-6].for_prefix)->enter, (yyvsp[-2].intValue));
                patchLabel((yyvsp[-6].for_prefix)->enter, (yyvsp[-2].intValue) + 1);
                patchLabel((yyvsp[-5].intValue), nextQuadLabel());
                patchLabel((yyvsp[-2].intValue), (unsigned int)(yyvsp[-6].for_prefix)->test);
                patchLabel((yyvsp[0].intValue), (unsigned int)((yyvsp[-5].intValue) + 1));
                if((yyvsp[-1].stmt_l) != NULL ){
                    patchLabel((yyvsp[-1].stmt_l)->breaklist, nextQuadLabel());
                    patchLabel((yyvsp[-1].stmt_l)->contlist, (yyvsp[-5].intValue) + 1);
                }
            }
#line 3243 "parser.cpp" /* yacc.c:1646  */
    break;

  case 112:
#line 1324 "parser.y" /* yacc.c:1646  */
    { ++loopcnt; }
#line 3249 "parser.cpp" /* yacc.c:1646  */
    break;

  case 113:
#line 1326 "parser.y" /* yacc.c:1646  */
    { --loopcnt; }
#line 3255 "parser.cpp" /* yacc.c:1646  */
    break;

  case 114:
#line 1328 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = (yyvsp[-1].stmt_l);
            }
#line 3263 "parser.cpp" /* yacc.c:1646  */
    break;

  case 115:
#line 1332 "parser.y" /* yacc.c:1646  */
    {
                        loopcntStack.push(loopcnt);
                        loopcnt = 0;
                    }
#line 3272 "parser.cpp" /* yacc.c:1646  */
    break;

  case 116:
#line 1337 "parser.y" /* yacc.c:1646  */
    {
                        loopcntStack.pop();
                    }
#line 3280 "parser.cpp" /* yacc.c:1646  */
    break;

  case 117:
#line 1341 "parser.y" /* yacc.c:1646  */
    {
                return_flag = true;
                if(last_func.top() == -1) yyerror("Use of \"Return\"outside of function");
            }
#line 3289 "parser.cpp" /* yacc.c:1646  */
    break;

  case 118:
#line 1344 "parser.y" /* yacc.c:1646  */
    {
                printf("returnstmt=>\"return expr;\" \n");
                return_flag = false; 
                emit(ret, NULL, NULL, (yyvsp[-1].exprNode));  
                (yyval.stmt_l) = new stmt_l();
                (yyval.stmt_l)->breaklist = newList(nextQuadLabel()); 
                emit(jump);
            }
#line 3302 "parser.cpp" /* yacc.c:1646  */
    break;

  case 119:
#line 1352 "parser.y" /* yacc.c:1646  */
    { 
                printf("returnstmt->return; \n");
                if(last_func.top() == -1) yyerror("Use of \"Return\"outside of function");
                emit(ret);
            }
#line 3312 "parser.cpp" /* yacc.c:1646  */
    break;

  case 120:
#line 1356 "parser.y" /* yacc.c:1646  */
    {
                (yyval.stmt_l) = new stmt_l();
                (yyval.stmt_l)->breaklist = newList(nextQuadLabel()); 
                emit(jump);
            }
#line 3322 "parser.cpp" /* yacc.c:1646  */
    break;


#line 3326 "parser.cpp" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1362 "parser.y" /* yacc.c:1906  */


int yyerror(char* yaccProvidedMessage){
    printQuads();
    printf("\033[1;31m");
    printf("\n_____________ERROR:line %d, before token: \"%s\" message: %s____________\n"
        ,yylineno,yytext,yaccProvidedMessage);
        printf("\033[0m");
    error = true;
   
}

int main(int argc, char* argv[]){
    
    FILE* fp;
    
    if( !( fp = fopen(argv[1],"r") ) ){

        printf("An error occured while openning the file\n");
        exit(-1);
    }
    yyin = fp;
    yyparse();
    
    //symbol_table.print();
    printQuads();
    return 0;
}
