%{
    open Interpreter;;
%}

%token <int> INT
%token <string> VAR CONST
%token LPAREN RPAREN LBRACKET RBRAKET COMMA DOT CUT PIPE EQ N_EQ PLUS MINUS TIMES DIV GT LT EOF

%left COMMA
%nonassoc PIPE EQ GT LT
%left PLUS MINUS
%left TIMES DIV
%nonassoc DOT

%start program goal
%type <Interpreter.program> program
%type <Interpreter.goal> goal
%%

