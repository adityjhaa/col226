%{

  type term =
    | Variable of string
    | Constant of string
    | Function of string * term list

  type atomic_formula = {
    predicate: string;
    terms: term list;
  }

  type body = atomic_formula list

  type head = atomic_formula

  type fact = {
    head: head;
  }

  type rule = {
    head: head;
    body: body;
  }

  type clause =
    | Fact of fact
    | Rule of rule

  type program = clause list

  type goal = atomic_formula list 
%}

%token <int> INT
%token <string> VAR CONST
%token LPAREN RPAREN LBRACKET RBRACKET ADD SUB MUL DIV EQUAL GT LT COMMA CUT ARROW DOT EOF PIPE

%left COMMA
%nonassoc EQUAL LT GT PIPE
%left ADD SUB MUL DIV
%nonassoc DOT

%start program
%type <program> program
%type <goal> goal
%%

program:
    | program clause

clause:
    | fact
    | rule

fact:
    | head DOT { Fact { head = $1; body = None } }

rule:
    | head ARROW body DOT { Rule { head = $1; body = $3 } }

head:
    | atomic_formula { $1 }

body:
    | atomic_formula { [$1] }
    | atomic_formula COMMA body { $1 :: $3 }

atomic_formula:
    | VAR LPAREN term_list RPAREN { { predicate = $1; terms = $3 } }

term_list:
    | /* Empty */
    | term
    | term COMMA term_list

term:
    | VAR { Constant $1 }
    | VAR LPAREN term_list RPAREN { Function ($1, $3) }
    | VAR { Variable $1 }
