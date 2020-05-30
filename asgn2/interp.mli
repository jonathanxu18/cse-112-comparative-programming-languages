(* Zain Shafique Jonathan Xu *)
(* $Id: interp.mli,v 1.7 2020-01-24 12:57:06-08 - - $ *)

(*
  Interpreter for Silly Basic
*)

(* variable which is a pointer to a location 
   in memory that is a bool (boolean) 
*)
val want_dump : bool ref

(* function that takes in parameter of type 
   Absyn.program and returns nothing 
*)
val interpret_program : Absyn.program -> unit

