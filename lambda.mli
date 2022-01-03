
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyPair of ty * ty
  | TyRecord of (string*ty) list
  | TyList of ty
  | TyEmptyList
;;

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

  | TmList of term list
  | TmEmptyList 
  | TmHead of term
  | TmTail of term
  | TmIsEmpty of term
;;

type operations =
    Eval of term
    | Bind of string * term


type context = (string * (term * ty)) list

val emptyctx : context;;
val addbinding : context -> string -> term -> ty -> context;;
val getbinding : context -> string -> (term * ty);;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;
val do_operation: context -> operations -> context;;

