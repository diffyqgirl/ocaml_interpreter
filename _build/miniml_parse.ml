type token =
  | EOF
  | OPEN
  | CLOSE
  | LET
  | DOT
  | IN
  | REC
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | LESSTHAN
  | EQUALS
  | IF
  | THEN
  | ELSE
  | FUNCTION
  | RAISE
  | ID of (string)
  | INT of (int)
  | TRUE
  | FALSE

open Parsing;;
let _ = parse_error;;
# 8 "miniml_parse.mly"
  open Printf ;;
  open Expr ;;
# 31 "miniml_parse.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* OPEN *);
  258 (* CLOSE *);
  259 (* LET *);
  260 (* DOT *);
  261 (* IN *);
  262 (* REC *);
  263 (* NEG *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* LESSTHAN *);
  268 (* EQUALS *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* FUNCTION *);
  273 (* RAISE *);
  276 (* TRUE *);
  277 (* FALSE *);
    0|]

let yytransl_block = [|
  274 (* ID *);
  275 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\002\000\006\000\006\000\007\000\
\004\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\007\000\004\000\005\000\006\000\020\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yydgoto = "\002\000\
\013\000\028\000\029\000"

let yysindex = "\003\000\
\122\000\000\000\122\000\027\255\122\000\122\000\251\254\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\104\255\
\252\254\010\255\209\255\125\255\019\255\000\000\122\000\122\000\
\122\000\122\000\122\000\209\255\000\000\000\000\012\255\122\000\
\122\000\122\000\255\254\255\254\230\255\101\000\122\000\122\000\
\146\255\167\255\209\255\188\255\122\000\122\000\122\000\209\255\
\209\255\209\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\070\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\054\000\062\000\032\000\043\000\021\000\000\000\
\000\000\000\000\076\000\000\000\000\000\000\000\000\000\078\000\
\084\000\092\000"

let yygindex = "\000\000\
\000\000\002\000\063\000"

let yytablesize = 399
let yytable = "\003\000\
\022\000\004\000\014\000\001\000\016\000\005\000\019\000\020\000\
\025\000\026\000\027\000\006\000\021\000\031\000\007\000\008\000\
\009\000\010\000\011\000\012\000\011\000\032\000\034\000\040\000\
\035\000\036\000\037\000\038\000\039\000\000\000\000\000\010\000\
\017\000\041\000\042\000\043\000\000\000\000\000\000\000\000\000\
\000\000\044\000\012\000\000\000\018\000\000\000\048\000\049\000\
\050\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\015\000\
\000\000\015\000\000\000\015\000\015\000\013\000\000\000\000\000\
\000\000\000\000\000\000\017\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\015\000\015\000\015\000\
\015\000\015\000\000\000\016\000\000\000\000\000\015\000\015\000\
\015\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\003\000\030\000\004\000\015\000\015\000\015\000\005\000\023\000\
\024\000\025\000\026\000\027\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\003\000\000\000\004\000\
\000\000\000\000\000\000\005\000\023\000\024\000\025\000\026\000\
\027\000\006\000\033\000\000\000\007\000\008\000\009\000\010\000\
\011\000\012\000\003\000\000\000\004\000\000\000\045\000\000\000\
\005\000\023\000\024\000\025\000\026\000\027\000\006\000\000\000\
\000\000\007\000\008\000\009\000\010\000\011\000\012\000\003\000\
\000\000\004\000\000\000\000\000\000\000\005\000\023\000\024\000\
\025\000\026\000\027\000\006\000\000\000\046\000\007\000\008\000\
\009\000\010\000\011\000\012\000\003\000\000\000\004\000\000\000\
\047\000\000\000\005\000\023\000\024\000\025\000\026\000\027\000\
\006\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\012\000\003\000\000\000\004\000\000\000\000\000\000\000\005\000\
\023\000\024\000\025\000\026\000\027\000\006\000\000\000\000\000\
\007\000\008\000\009\000\010\000\011\000\012\000\003\000\000\000\
\004\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
\026\000\027\000\006\000\000\000\000\000\007\000\008\000\009\000\
\010\000\011\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\004\000\000\000\000\000\000\000\005\000\
\023\000\024\000\025\000\026\000\027\000\006\000\000\000\000\000\
\007\000\008\000\009\000\010\000\011\000\012\000\011\000\000\000\
\000\000\011\000\000\000\000\000\011\000\011\000\011\000\011\000\
\000\000\010\000\011\000\011\000\010\000\000\000\000\000\010\000\
\010\000\010\000\000\000\000\000\012\000\010\000\010\000\012\000\
\000\000\000\000\012\000\012\000\012\000\000\000\000\000\008\000\
\012\000\012\000\008\000\000\000\000\000\008\000\008\000\009\000\
\000\000\000\000\009\000\008\000\008\000\009\000\009\000\013\000\
\000\000\000\000\013\000\009\000\009\000\017\000\000\000\015\000\
\017\000\000\000\015\000\013\000\013\000\014\000\000\000\000\000\
\014\000\017\000\017\000\015\000\015\000\016\000\000\000\000\000\
\016\000\014\000\014\000\000\000\000\000\003\000\000\000\004\000\
\000\000\016\000\016\000\005\000\000\000\000\000\000\000\000\000\
\027\000\006\000\000\000\000\000\007\000\008\000\009\000\010\000\
\011\000\012\000\003\000\000\000\004\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\007\000\008\000\009\000\010\000\011\000\012\000"

let yycheck = "\001\001\
\000\000\003\001\001\000\001\000\003\000\007\001\005\000\006\000\
\010\001\011\001\012\001\013\001\018\001\018\001\016\001\017\001\
\018\001\019\001\020\001\021\001\000\000\012\001\004\001\012\001\
\023\000\024\000\025\000\026\000\027\000\255\255\255\255\000\000\
\006\001\032\000\033\000\034\000\255\255\255\255\255\255\255\255\
\255\255\040\000\000\000\255\255\018\001\255\255\045\000\046\000\
\047\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\001\000\
\255\255\003\000\255\255\005\000\006\000\000\000\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\023\000\024\000\025\000\
\026\000\027\000\255\255\000\000\255\255\255\255\032\000\033\000\
\034\000\255\255\255\255\255\255\255\255\255\255\040\000\255\255\
\001\001\002\001\003\001\045\000\046\000\047\000\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\001\001\255\255\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\255\255\003\001\255\255\005\001\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\001\001\
\255\255\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\001\001\255\255\003\001\255\255\
\005\001\255\255\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\001\001\255\255\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\255\255\
\003\001\255\255\255\255\255\255\007\001\255\255\255\255\255\255\
\011\001\012\001\013\001\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\002\001\255\255\
\255\255\005\001\255\255\255\255\008\001\009\001\010\001\011\001\
\255\255\002\001\014\001\015\001\005\001\255\255\255\255\008\001\
\009\001\010\001\255\255\255\255\002\001\014\001\015\001\005\001\
\255\255\255\255\008\001\009\001\010\001\255\255\255\255\002\001\
\014\001\015\001\005\001\255\255\255\255\008\001\009\001\002\001\
\255\255\255\255\005\001\014\001\015\001\008\001\009\001\002\001\
\255\255\255\255\005\001\014\001\015\001\002\001\255\255\002\001\
\005\001\255\255\005\001\014\001\015\001\002\001\255\255\255\255\
\005\001\014\001\015\001\014\001\015\001\002\001\255\255\255\255\
\005\001\014\001\015\001\255\255\255\255\001\001\255\255\003\001\
\255\255\014\001\015\001\007\001\255\255\255\255\255\255\255\255\
\012\001\013\001\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\255\255\003\001\255\255\255\255\255\255\
\007\001\255\255\255\255\255\255\255\255\255\255\013\001\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001"

let yynames_const = "\
  EOF\000\
  OPEN\000\
  CLOSE\000\
  LET\000\
  DOT\000\
  IN\000\
  REC\000\
  NEG\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  LESSTHAN\000\
  EQUALS\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FUNCTION\000\
  RAISE\000\
  TRUE\000\
  FALSE\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 36 "miniml_parse.mly"
                 ( _1 )
# 243 "miniml_parse.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expnoapp) in
    Obj.repr(
# 38 "miniml_parse.mly"
                       ( App(_1, _2) )
# 251 "miniml_parse.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expnoapp) in
    Obj.repr(
# 39 "miniml_parse.mly"
             ( _1 )
# 258 "miniml_parse.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 41 "miniml_parse.mly"
                ( Num _1 )
# 265 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "miniml_parse.mly"
          ( Bool true )
# 271 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "miniml_parse.mly"
           ( Bool false )
# 277 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "miniml_parse.mly"
        ( Var _1 )
# 284 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 45 "miniml_parse.mly"
                 ( Binop("+", _1, _3) )
# 292 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 46 "miniml_parse.mly"
                  ( Binop("-", _1, _3) )
# 300 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 47 "miniml_parse.mly"
                  ( Binop("*", _1, _3) )
# 308 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 48 "miniml_parse.mly"
                  ( Binop("=", _1, _3) )
# 316 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 49 "miniml_parse.mly"
                    ( Binop("<", _1, _3) )
# 324 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 50 "miniml_parse.mly"
            ( Unop("~", _2) )
# 331 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 52 "miniml_parse.mly"
                      ( Conditional(_2, _4, _6) )
# 340 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 53 "miniml_parse.mly"
                            ( Let(_2, _4, _6) )
# 349 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 54 "miniml_parse.mly"
                                ( Letrec(_3, _5, _7) )
# 358 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 55 "miniml_parse.mly"
                       ( Fun(_2, _4) )
# 366 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "miniml_parse.mly"
           ( Raise )
# 372 "miniml_parse.ml"
               : 'expnoapp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 57 "miniml_parse.mly"
                  ( _2 )
# 379 "miniml_parse.ml"
               : 'expnoapp))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
;;
