
(* TYPE DEFINITIONS *)

(* Type definition of types in lambda interpreter *)
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyPair of ty * ty
  | TyRecord of (string*ty) list
;;

(* Type definition of terms in lambda interpreter *)
type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term

  | TmFix of term

  | TmString of string
  | TmConcat of term * term

  | TmPair of term * term
  | TmFst of term
  | TmSnd of term

  | TmRecord of (string*term) list
  | TmProjection of term*string
;;

(* Type definition of operations allowed in lambda interpreter 
        --> Term Evaluation
        --> Variable Binding
*)
type operations =
    Eval of term
    | Bind of string * term
  

(* CONTEXT MANAGEMENT *)

(* aproaches tried: 
    three elements list --> wont work because ocaml does not work well with three elements tupels
    record --> wont work because we would need to reimplement the get_binding function into a less efficient one
    var_name, (var_term, var_type) --> ok
    *)
type context =
  (string*(term*ty)) list
;;

let emptyctx =
  []
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyPair (ty1, ty2)-> 
      "Pair("^string_of_ty ty1 ^","^ string_of_ty ty2 ^")"
  | TyRecord t ->
    let content_string = 
      (let rec aux t acc = match t with
        | (field_name, field_term)::tl -> aux tl (acc^field_name^":"^string_of_ty field_term^", ")
        | _ -> String.sub acc 0 ((String.length acc)-2) 
      in aux t "")
    in "Record{"^content_string^"}"
;;


exception Type_error of string
;;

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      s
  | TmConcat (t1,t2) ->
      string_of_term t1 ^ string_of_term t2
  | TmPair (t1, t2) ->
      "Pair("^string_of_term t1^","^string_of_term t2^")"
  | TmFst t -> 
    (match t with 
      | TmPair(t1, t2) ->  string_of_term t1
      | _ -> raise (Type_error "[String of Term Error] Fst argument must be of type Tuple"))
  | TmSnd t -> 
    (match t with 
      | TmPair(t1, t2) ->  string_of_term t2
      | _ -> raise (Type_error "[String of Term Error] Snd argument must be of type Tuple"))
  | TmRecord t ->
    let content_string = 
      (let rec aux t acc = match t with
        | (field_name, field_term)::tl -> aux tl (acc^field_name^":"^string_of_term field_term^", ")
        | _ -> String.sub acc 0 ((String.length acc)-2) 
      in aux t "")
    in "Record{"^content_string^"}"
  | TmProjection (t, field_name) -> 
    (match t with 
      | TmRecord t ->  string_of_term (List.assoc field_name t)
      | _ -> raise (Type_error "[String of Term Error] Projection argument must be of type Record"))
;;

let addbinding ctx var_name var_term var_type =
  (*print_endline("adding bind of variable "^var_name^":"^string_of_ty var_type);*)
  (var_name, (var_term, var_type)) :: ctx
;;

let getbinding ctx var_name =
  (*List assoc searches for <var_name> in the list of pairs <context>
    Returns a tuple (var_term, var_type
    print_endline("retrieving bind of variable "^var_name^":"^(string_of_ty (snd(List.assoc var_name ctx))^" >> "^(string_of_term (fst(List.assoc var_name ctx)))));
  *)

  List.assoc var_name ctx
;;


let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t_cond, t_then, t_else) ->
      if typeof ctx t_cond = TyBool then
        let tyT2 = typeof ctx t_then in
          if typeof ctx t_else = tyT2 
            then tyT2
            else raise (Type_error "[Type of Error] Branches of the conditional have different types")
      else
        raise (Type_error "[Type of Error] The condition of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "[Type of Error] The argument of succ is not a Natural")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "[Type of Error] The argument of pred is not a Natural")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "[Type of Error] The argument of iszero is not a Natural")

    (* T-Var *)
  | TmVar x ->
      (try (snd(getbinding ctx x)) with
       _ -> raise (Type_error ("[Type of Error] No binding type found for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x t2 tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "[Type of Error] Parameter type mismatch")
         | _ -> raise (Type_error "[Type of Error] Arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x t1 tyT1 in
      typeof ctx' t2
    
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "[Type of Error] Result of body not compatible with domain")
          | _ -> raise (Type_error "[Type of Error] Arrow type expected"))
      
    (* T-String*)
  | TmString _ ->
      TyString
  
    (* T-Concat*)
  | TmConcat (t1,t2) ->
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
      (match (tyT1,tyT2) with
        (TyString,TyString) -> TyString
        | _ -> raise (Type_error "[Type of Error] Concat arguments must be of type String"))
    (* T-Tuple*)
  | TmPair (t1, t2) ->
      TyPair((typeof ctx t1), (typeof ctx t2))
    
    (* T-Fst*)
  | TmFst t -> 
      let type_of_term = typeof ctx t in
        (match type_of_term with 
          | TyPair(t1, t2) -> t1
          | _ -> raise (Type_error "[Type of Error] Fst argument must be of type Tuple"))
    
    (* T-Snd*)
  | TmSnd t ->
    let type_of_term = typeof ctx t in
    (match type_of_term with 
        | TyPair(t1, t2) ->  t2 
        | _ -> raise (Type_error "[Type of Error] Snd argument must be of type Tuple"))
    
    (* T-Record *)
  | TmRecord t ->
      let rec aux acc t =
      match t with
        | (field_name, field_term)::tl -> aux ((field_name,typeof ctx field_term)::acc) tl
        | _ -> (TyRecord (List.rev acc))
      in aux [] t

    (*T-Projection*)
  | TmProjection (t, field_name) -> 
    let type_of_term = typeof ctx t in
    (match type_of_term with 
      | TyRecord t -> (List.assoc field_name t)
      | _ -> raise (Type_error "[Type of Error] Projection argument must be of type Record"))
;;
     
(* TERMS MANAGEMENT *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      s
  | TmConcat (t1,t2) ->
      string_of_term t1 ^ string_of_term t2
  | TmPair (t1, t2) ->
      "Pair("^string_of_term t1^","^string_of_term t2^")"
  | TmFst t ->     
    (match t with 
      | TmPair(t1, t2) ->  string_of_term t1
      | _ -> raise (Type_error "[String of Term Error] Snd argument must be of type Tuple"))
  | TmSnd t -> 
    (match t with 
      | TmPair(t1, t2) ->  string_of_term t2
      | _ -> raise (Type_error "[String of Term Error] Snd argument must be of type Tuple"))
  | TmRecord t ->
    let content_string = 
      (let rec aux t acc = match t with
        | (field_name, field_term)::tl -> aux tl (acc^field_name^":"^string_of_term field_term^", ")
        | _ -> String.sub acc 0 ((String.length acc)-2) 
      in aux t "")
    in "Record{"^content_string^"}"
  | TmProjection (t, field_name) -> 
    (match t with 
      | TmRecord t ->  string_of_term (List.assoc field_name t)
      | _ -> raise (Type_error "[String of Term Error] Projection argument must be of type Record"))
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmString s ->
      [s]
  | TmFix t ->
      free_vars t
  | TmConcat (t1, t2) -> 
    [string_of_term t1 ^ string_of_term t2]
  | TmPair (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmFst t ->
    (match t with 
      | TmPair(t1, t2) ->  free_vars t1
      | _ -> raise (Type_error "[Free Vars Error] Fst argument must be of type Tuple"))
  | TmSnd t ->
    (match t with 
      | TmPair(t1, t2) ->  free_vars t1
      | _ -> raise (Type_error "[Free Vars Error] Snd argument must be of type Tuple"))
  | TmRecord t->
      let rec aux accum t = match t with 
        | (field_name, field_term)::tl -> aux ((free_vars field_term)@accum) tl
        | _ -> (List.rev accum)
      in aux [] t
  | TmProjection (t, field_name) -> 
    (match t with 
      | TmRecord t ->  free_vars (List.assoc field_name t)
      | _ -> raise (Type_error "[Free Vars Error] Projection argument must be of type Record"))
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString s ->
      TmString s
  | TmConcat (t1,t2) ->
        TmString (string_of_term t1 ^ string_of_term t2)
  | TmPair (t1, t2) ->
      TmPair (subst x s t1, subst x s t2)
  | TmFst t ->
    (match t with 
      | TmVar vn -> (TmFst (subst x s t))
      | TmPair(t1, t2) -> subst x s t1
      | _ -> raise (Type_error "[Substitution Error] Fst argument must be of type Tuple"))
  | TmSnd t ->
    (match t with 
      | TmVar vn -> (TmSnd (subst x s t))
      | TmPair(t1, t2) ->  subst x s t2
      | _ -> raise (Type_error "[Substitution Error] Snd argument must be of type Tuple"))
  | TmRecord t ->
    let rec aux accum t = match t with
      | (field_name,field_term)::tl -> aux ((field_name,(subst x s field_term))::accum) tl
      | _ -> TmRecord (List.rev accum)
    in aux [] t
  | TmProjection (t, field_name) -> 
    (match t with 
      | TmVar vn -> TmProjection ((subst x s t), field_name)
      | TmRecord t -> subst x s (List.assoc field_name t)
      | _ -> raise (Type_error "[Substitution Error] Projection argument must be of type Record"))
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
  | TmPair(_,_)->true
  | TmRecord(_)->true
  | _ -> false
;;

exception NoRuleApplies
;;
let rec print_list = function 
[] -> ()
| e::l -> print_endline e ; print_string " " ; print_list l
(*auxiliar function to resolve variable naming in the context*)
let eval_ctx ctx t = 
  let rec solve_context l tm = match tm with
    (*S-True*)
    | TmTrue -> 
        TmTrue
    (*S-False*)
    | TmFalse -> 
        TmFalse
    (*S-If*)
    | TmIf(cond, thn, els) -> 
        TmIf (solve_context l cond, solve_context l thn, solve_context l els)
    (*S-Zero*)
    | TmZero ->
        TmZero
    (*S-Succ*)
    | TmSucc t ->
        TmSucc (solve_context l t)
    (*S-Pred*)
    | TmPred t ->
        TmPred (solve_context l t)
    (*S-IsZero*)
    | TmIsZero t ->
        TmIsZero (solve_context l t)
    (*S-Var*)
    (*Check if variable already in var_list l, otherwise retrieve*)
    | TmVar var_name ->
        if List.mem var_name l then 
          TmVar var_name
        else 
          (fst(getbinding ctx var_name))
    (*S-Abs*)
    | TmAbs(s, t, t1)->
        TmAbs (s,t,(solve_context(s::l) t1))
    (*S-App*)
    | TmApp(t1,t2)->
        TmApp (solve_context l t1, solve_context l t2)
    (*S-LetIn*)
    (*Solve context for the term that is getting binded in t2 and solve context
    in t2 but with the variable name of t1 already being added*)
    | TmLetIn(var_name, t1, t2)->
      print_list l;
        TmLetIn(var_name, solve_context l t1, (solve_context(var_name::l) t2))
    (*S-Fix*)
    | TmFix t ->
        TmFix (solve_context l t)
    (*S-Str*)
    | TmString s ->
        TmString s
    (*S-Concat*)
    | TmConcat (t1,t2) ->
        TmConcat(solve_context l t1, solve_context l t2)
    (*S-Pair*)
    | TmPair (t1, t2) ->
        TmPair (solve_context l t1, solve_context l t2)
    (*S-Fst*)
    | TmFst t ->
      let context_solved_term = solve_context l t in
      (match context_solved_term with 
          |TmPair(t1,t2) -> solve_context l t1
          |_ ->raise(Type_error "[Solving Context Error] Fst argument must be of type Tuple"))
    (*S-Snd*)
    | TmSnd t ->
      let context_solved_term = solve_context l t in
      (match context_solved_term with 
          |TmPair(t1,t2) -> solve_context l t2
          |_ ->raise(Type_error "[Solving Context Error] Snd argument must be of type Tuple"))
    (*S-Record*)
    | TmRecord t ->
      let rec aux accum t =
        match t with
          (field_name,field_term)::tl -> aux ((field_name,(solve_context l field_term))::accum) tl
          | _ -> TmRecord (List.rev accum)
      in aux [] t
    | TmProjection (t, field_name) -> 
      let context_solved_term = solve_context l t in
      (match context_solved_term with 
        | TmRecord t -> solve_context l (List.assoc field_name t)
        | _ -> raise (Type_error "[Solving Context Error] Projection argument must be of type Record"))

    
  in solve_context [] t
;;

(*now we need to eval according to context, so whenever a rule does not apply we can search in the context
in order to retrieve var name*)
let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

  (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t12)) ->
      subst x tm t12

  (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'
  
  (* E-Concat *)
  | TmConcat (t1,t2) ->
    let t1' = eval1 ctx t1 in
    let t2' = eval1 ctx t2 in
    TmString ( string_of_term t1' ^ string_of_term t2')

  | TmFst t -> 
    (let evaluation_of_term = eval1 ctx t in 
    (match evaluation_of_term with  
        | TmPair(t1, t2) ->  eval1 ctx t1 
        | _ -> raise (Type_error "[Error] Fst argument must be of type Tuple")))
  
  | TmSnd t -> 
    (let evaluation_of_term = eval1 ctx t in 
    (match evaluation_of_term with 
        | TmPair(t1, t2) ->  eval1 ctx t2 
        | _ -> raise (Type_error "[Error] Snd argument must be of type Tuple")))
  
  | TmVar s ->
      (*getbinding string -> (term, type)*)
      eval1 ctx (fst (getbinding ctx s))

  | TmRecord t ->
    let rec aux accum t =
      match t with
        (field_name,field_term)::tl -> aux ((field_name,(eval1 ctx field_term))::accum) tl
        | _ -> TmRecord (List.rev accum)
    in aux [] t
  
  | TmProjection (t, field_name) ->
    let evaluation_of_term = eval1 ctx t in 
    (match evaluation_of_term with 
      | TmRecord t -> eval1 ctx (List.assoc field_name t)
      | _ -> raise (Type_error "[Evaluation Error] Projection argument must be of type Record"))

  | _ ->
      raise NoRuleApplies
;;

(*Whenever we are evaluating a term and we find a term that applies no rule
we evaluate context for variable solving*)
let rec eval ctx tm =
  try
    let evaluated_tm = eval1 ctx tm in
    eval ctx evaluated_tm
  with
    NoRuleApplies -> eval_ctx ctx tm
;;

