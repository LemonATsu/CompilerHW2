
/* This is a simpled gcc grammar */
/* Copyright (C) 1987 Free Software Foundation, Inc. */
/* BISON parser for a simplied C by Jenq-kuen Lee  Sep 20, 1993    */

%{
#include <stdio.h>
#include <string.h>
#include "symbol.h"
//#include "SymbolTable.h"
#define REDU(A) strcpy(str, A)

extern int lineno;
extern FILE *f_asm;
char   *install_symbol();

void output_c(char *t1, char *t2, char *str);
void output_m(char *repeat, char *mid, char *str);
void output_d(char *t1, char *mid, char *t2, char *str);
void singleton(char *single, char *str);
%}

%start program
%union { 
         int       token ;
         char      charv ;
         char      *ident;
       }
/* all identifiers   that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER INTEGER FLOAT

/* reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token SCSPEC

/* reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPESPEC ENUM STRUCT UNION

/* reserved words that modify type: "const" or "volatile".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPEMOD

%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
/*%token STRING*/


/* the reserved words */
%token SIZEOF  IF ELSE WHILE DO FOR SWITCH CASE DEFAULT_TOKEN
%token BREAK CONTINUE RETURN GOTO ASM

%type <ident> notype_declarator IDENTIFIER primary expr_no_commas

%type <token> CONSTANT

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */
%left  <charv> ';'
%left IDENTIFIER  SCSPEC TYPESPEC TYPEMOD
%left  <charv> ','
%right <charv> '='
%right <token> ASSIGN 
%right <charv> '?' ':'
%left <charv> OROR
%left <charv> ANDAND
%left <charv> '|'
%left <charv> '^'
%left <charv> '&'
%left <token> EQCOMPARE
%left <token> ARITHCOMPARE  '>' '<' 
%left <charv> LSHIFT RSHIFT
%left <charv> '+' '-'
%left <token> '*' '/' '%'
%right <token> UNARY PLUSPLUS MINUSMINUS 
%left HYPERUNARY 
%left <token> POINTSAT '.'
%nonassoc IFX
%nonassoc ELSE


%{
/* external function is defined here */
void error();
int TRACEON = 100;
%}     


%%

program: /* empty */ { singleton("NULL", "program"); }
	| extdefs        { singleton("extdefs", "program"); }
	;

extdefs:
    extdef              { singleton("extdef", "extdefs"); }
	| extdefs  extdef   { output_c("extdefs", "extdef", "extdefs"); }
	;

extdef:
	 TYPESPEC notype_declarator ';' { output_d("TYPESPEC", "notype_declarator", "';'", "extdef"); }
        | notype_declarator
          '{'  xdecls 
               stmts
           '}'                      { printf("notype_declarator '{' xdecls stmts '}' -> extdef\n"); }
        | error ';'                 { output_c("error", "';'", "extdef"); }
	| ';'                           { singleton("';'", "extdef"); }
	;

/* Must appear precede expr for resolve precedence problem */
/* A nonempty list of identifiers.  */

/* modified */
expr_no_commas:
    primary                                         { singleton("primary", "expr_no_commas"); } 
	| expr_no_commas '+' expr_no_commas             { output_m("expr_no_commas", "'+'", "expr_no_commas"); }
	| expr_no_commas '=' expr_no_commas             { output_m("expr_no_commas", "'='", "expr_no_commas"); }
	| expr_no_commas '*' expr_no_commas             { output_m("expr_no_commas", "'*'", "expr_no_commas"); }
	| expr_no_commas ARITHCOMPARE expr_no_commas    { output_m("expr_no_commas", "ARITHCOMPARE", "expr_no_commas"); }
	;


// finished, but not a subset 
// grammar for block
block :
    '{' stmts '}' { output_d("'{'", "stmts", "'}'", "block");}
    ;

// lable, using for complete switch/goto subset.
// For IDENTIFIER ':' is for goto label.
// And the other two rules is for switch
label_stmt:
    IDENTIFIER ':'          { output_c("IDENTIFIER", "':'", "label_stmt"); }
    | CASE CONSTANT ':'     { output_d("CASE", "CONSTANT", "':'", "label_stmt"); }
    | DEFAULT_TOKEN ':'     { output_c("DEFAULT_TOKEN", "':'", "label_stmt"); } 
    ;

// grammar for selection.
// In this part, I complete subsets if and switch.
// in the rule 3, I define the precedence to avoid shift/reduce conflicts happens in if/if else statement
select_stmt:
    SWITCH '(' expr_no_commas ')' stmt         { printf("SWITCH '(' expr_no_commas ')' stmt -> select_stmt\n"); }
    | IF '(' expr_no_commas ')' stmt ELSE stmt { printf("IF '(' expr_no_commas ')' block ELSE block -> select_stmt\n"); } 
    | IF '(' expr_no_commas ')' stmt %prec IFX { printf("IF '(' expr_no_commas ')' block -> select_stmt\n"); }
    ;

// grammar for iteration
// In this part, I complete subset while and for.
// It covers both do while and for with any expression, i.e. for(;;)
iter_stmt:
    WHILE '(' expr_no_commas ')' stmt            { printf("WHILE '(' expr_no_commas ')' stmt -> iter_stmt\n"); }
    | DO stmt WHILE '(' expr_no_commas ')' ';'   { printf("DO stmt WHILE '(' expr_no_commas ')' -> iter_stmt\n"); } 
    | FOR '(' stmt stmt expr_no_commas ')' stmt  { printf("FOR '(' stmt stmt expr_no_commas ')' stmt -> iter_stmt\n"); }
    | FOR '(' stmt stmt ')' stmt                 { printf("FOR '(' stmt stmt ')' stmt -> iter_stmt\n"); }
    ;

// In the part, it completes the subset of switch and goto, and iteration.
jump_stmt:
    GOTO IDENTIFIER         { output_c("GOTO", "IDENTIFIER", "jump"); }
    | CONTINUE              { singleton("CONTINUE", "jump_stmt"); }
    | BREAK                 { singleton("BREAK", "jump_stmt"); }
    | RETURN                { singleton("RETURN", "jump_stmt"); }
    | RETURN expr_no_commas { output_c("RETURN", "expr_no_commas", "jump_stmt"); }
    ;

/* modified */
primary:
        IDENTIFIER      { singleton("IDENTIFIER", "primary"); }
	| CONSTANT          { singleton("CONSTANT", "primary"); }
	| primary PLUSPLUS  { output_c("primary", "PLUSPLUS", "primary"); }
        ;


notype_declarator: 
	  notype_declarator '(' parmlist ')'  %prec '.' { printf("notype_declarator '(' parmlist ')' -> notype_declarator\n"); }
        | IDENTIFIER                                { singleton("IDENTIFIER", "notype_declarator"); }
	    //| IDENTIFIER '=' expr_no_commas           { output_d("IDENTIFIER", "'='", "expr_no_commas", "notype_declarator"); };
    ;

/* This is what appears inside the parens in a function declarator.
   Is value is represented in the format that grokdeclarator expects.  */
parmlist:  /* empty */ {singleton("NULL", "parmlist");}
	| parms            {singleton("parms", "parmlist");}
	;

/* A nonempty list of parameter declarations or type names.  */
parms:	
	parm                {singleton("parm", "parms");}
	| parms ',' parm    {output_d("parms", "','", "parm", "parms");}
	;

parm:                
	  TYPESPEC notype_declarator    {output_c("TYPESPEC", "notype_declarator", "parm");}
   ;


/* at least one statement, the first of which parses without error.  */
/* stmts is used only after decls, so an invalid first statement
   is actually regarded as an invalid decl and part of the decls.  */

stmts:
	stmt    {singleton("stmt", "stmts");}
	| stmts stmt    {output_c("stmts", "stmt", "stmts");}
	;


/* modified */
// add select, iter, label, jump, ';' and block
stmt:
    ';'                   { singleton("';'", "stmt"); } // added to handle stmt with only one ';'
    | select_stmt         { singleton("select_stmt", "stmt"); }
    | iter_stmt           { singleton("iter_stmt", "stmt"); }
    | label_stmt          { singleton("label_stmt", "stmt"); }
    | jump_stmt ';'       { output_c("jump_stmt", "';'", "stmt"); }
  	| expr_no_commas ';'  { output_c("expr_no_commas", "';'", "stmt"); }
    | block               { singleton("block", "stmt"); }
    ;


xdecls: /* empty */ { singleton("NULL", "decls"); }
	| decls         { singleton("decls", "xdecls"); }
	;

decls:
	decl           { singleton("decl", "decls"); }
	| decls decl   { output_c("decls", "decl", "decls"); }
	;

decl:
    TYPESPEC notype_declarator ';'  { output_d("TYPESPEC", "notype_declarator", "';'", "decl"); }


%%


/*
 *	  s - the error message to be printed
 */


void output_c(char *t1, char *t2, char *str)
{
    printf("%s %s -> %s\n", t1, t2, str);
}

void singleton(char *single, char *str)
{
    printf("%s -> %s\n", single, str);
}

void output_m(char *repeat, char *mid, char *str)
{
    printf("%s %s %s -> %s\n", repeat, mid, repeat, str);
}

void output_d(char *t1, char *mid, char *t2, char *str)
{
    printf("%s %s %s -> %s\n", t1, mid, t2, str);
}

int main(void)
{
    yyparse();
    return 0;
}

int yyerror(char *s)
{
    fprintf(stderr,"Error on line %d \n",lineno);
    return 0;
}




