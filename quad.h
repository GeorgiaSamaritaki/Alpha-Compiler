



typedef enum expr_t {
    var_e,
    tableitem_e,

    programfunc_e,
    libraryfunc_e,

    arithexpr_e,
    boolexpr_e,
    assignexpr_e,
    newtable_e,

    constnum_e,
    constbool_e,
    conststring_e,

    nil_e,
}expr_t;

typedef struct expr{ 
	expr_t type;
	SymbolTableEntry* sym;
	struct expr* index;
	double numConst;
    char* strConst;
    unsigned char boolConst;
	struct expr* next;
}expr;