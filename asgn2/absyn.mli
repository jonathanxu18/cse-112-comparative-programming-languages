(* Zain Shafique Jonathan Xu *)
(* $Id: absyn.mli,v 1.3 2020-01-22 16:07:24-08 - - $ *)

(*
* Abstract syntax definitions for SB.
*)

(* new type name called linenr which is of type int *)
type linenr    = int

(* new type name called ident which is of type string *)
type ident     = string

(* new type name called label which is of type string *)
type label     = string

(* new type name called number which is of type float *)
type number    = float

(* new type name called oper which is of type string *)
type oper      = string

(* mutually recursive type memref 
   cases:
         - arrayref of type (ident, expr)
         - variable of type ident

 *)
and  memref    = Arrayref of ident * expr
               | Variable of ident

(* mutually recursive type expr
   cases:
         - number of type number
         - memref of type memref
         - unary  of type (oper, expr)
         - binary of type (oper, expr, expr)

 *)
and  expr      = Number of number
               | Memref of memref
               | Unary of oper * expr
               | Binary of oper * expr * expr

(* type called printable
   cases:
         - Printexpr with argument of type expr
         - String with argument of type string

 *)
type printable = Printexpr of expr
               | String of string

(* type called stmt
   cases:
         - dim of type (ident, expr)
         - let of type (memref, expr)
         - goto of type label
         - if of type (expr, label)
         - print of type list of printables
         - input of type list of memrefs

 *)
type stmt      = Dim of ident * expr
               | Let of memref * expr
               | Goto of label
               | If of expr * label
               | Print of printable list
               | Input of memref list

(* 
   type called progline which is a tuple of
   (linenr, label option, stmt option) 
*)
type progline  = linenr * label option * stmt option

(* 
   type called program which is a list of proglines
*)
type program   = progline list

