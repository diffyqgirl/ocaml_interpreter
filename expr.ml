
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;
  
type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Num _ -> SS.empty
  | Var v -> SS.singleton (v)
  | Bool _ -> SS.empty
  | Unop (op, exp1) -> free_vars exp1
  | Binop (op, exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  | Conditional (exp1, exp2, exp3) -> SS.union (free_vars exp1) 
      (SS.union (free_vars exp2) (free_vars exp3))
  | Fun (v, exp1) -> SS.remove v (free_vars exp1) 
  | Let (v, exp1, exp2) -> SS.union (SS.remove v (free_vars exp2)) 
      (free_vars exp1)
  | Letrec (v, exp1, exp2) -> SS.remove v 
      (SS.union (free_vars exp1) (free_vars exp2))
  | Raise -> SS.empty
  | Unassigned -> SS.empty
  | App (exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
;;
  
(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname () : varid =
  let start = ref 0 in
  let temp = !start in
  (start := !start + 1;
  "var" ^ (string_of_int temp));;
  
(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  match exp with 
  | Num _ -> exp
  | Bool _ -> exp
  | Unop (op, exp1) -> Unop (op, subst var_name repl exp1)
  | Binop (op, exp1, exp2) -> 
    Binop (op, subst var_name repl exp1, subst var_name repl exp2)
  | Var v -> if var_name = v then repl else exp
  | Conditional (exp1, exp2, exp3) -> Conditional (subst var_name repl exp1, 
      subst var_name repl exp2, subst var_name repl exp3)
  | Fun (v, exp1) -> if var_name = v then exp
      else if SS.mem v (free_vars repl) 
        then let z = new_varname () in 
        Fun (z, subst var_name repl (subst v (Var (z)) exp1))
      else Fun (v, subst var_name repl exp1)
  | Let (v, exp1, exp2) -> if var_name = v 
      then Let (v, subst var_name repl exp1, exp2)
      else if SS.mem v (free_vars repl) 
      then let z = new_varname () in
        Let (z, subst var_name repl (subst v (Var(z)) exp1),
          subst var_name repl exp2)
      else Let (v, subst var_name repl exp1, subst var_name repl exp2)
  | Letrec (v, exp1, exp2) -> if var_name = v then exp
      else if SS.mem v (free_vars repl) 
      then let z = new_varname () in
        Letrec (z, subst var_name repl (subst v (Var(z)) exp1),
          subst var_name repl exp2)
      else Letrec (v, subst var_name repl exp1, subst var_name repl exp2)
  | Raise -> exp
  | Unassigned -> exp
  | App (exp1, exp2) -> App (subst var_name repl exp1, 
      subst var_name repl exp2)
;;

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
  match exp with
  | Var (v) -> "Var(" ^ v ^ ")"
  | Num (i) -> "Num(" ^ (string_of_int i) ^ ")"
  | Bool (b) -> string_of_bool b
  | Unop (op, exp1) -> "Unop(" ^ op ^ ", " ^ (exp_to_string exp1) ^ ")"
  | Binop (op, exp1, exp2) -> "Binop(" ^ op ^ ", " ^ (exp_to_string exp1) ^ 
      ", " ^ (exp_to_string exp2) ^ ")"
  | Conditional (exp1, exp2, exp3) -> "Conditional(" ^ (exp_to_string exp1) ^ 
      ", " ^ (exp_to_string exp2) ^ ", " ^ (exp_to_string exp3) ^ ")"
  | Fun (v, exp1) -> "Fun(" ^ v ^ ", " ^ (exp_to_string exp1) ^ ")"
  | Let (v, exp1, exp2) -> "Let(" ^ v ^ ", " ^ (exp_to_string exp1) ^ ", " ^ 
      (exp_to_string exp2) ^ ")"
  | Letrec (v, exp1, exp2) -> "Letrec(" ^ v ^ ", " ^ (exp_to_string exp1) ^ 
      ", " ^ (exp_to_string exp2) ^ ")"
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (exp1, exp2) -> "App(" ^ (exp_to_string exp1) ^ ", " ^ 
      (exp_to_string exp2) ^ ")"
;;
