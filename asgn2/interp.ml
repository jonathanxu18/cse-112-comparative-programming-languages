(* Zain Shafique Jonathan Xu *)
(* $Id: interp.ml,v 1.9 2020-01-28 13:33:00-08 - - $ *)

(*opens absyn.mli file *)
open Absyn
open Format

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

(* recursive function eval_expr
   parameter: expr - type Absyn.expr
*)
let rec eval_expr (expr : Absyn.expr) : float = 
    match expr with
    | Number number -> number
    | Memref memref -> (match memref with 
       | Variable ident -> Hashtbl.find Tables.variable_table ident
       | Arrayref (ident, expr) -> let num = eval_expr expr in 
          let index = int_of_float num in 
               let array = Hashtbl.find Tables.array_table ident in 
                    Array.get array index)

    | Unary (oper, expr) -> 

      let op = Hashtbl.find Tables.unary_fn_table oper 
      and arg = eval_expr expr
      in op arg

    | Binary (oper, expr1, expr2) -> 
      let op = Hashtbl.find Tables.binary_fn_table oper
      and arg1 = eval_expr expr1 
      and arg2 = eval_expr expr2
      in op arg1 arg2
      

(* recursive function interpret
   parameter: program - type Absyn.program 
   Absyn.program: list of (linenr, label option, stmt option) tuples
*)
let rec interpret (program : Absyn.program) = match program with
    
    (* 
       - case: reached end of file
       - returns unit
    *)
    | [] -> ()

    (* 
       - takes first tuple (line from file) and 
       uses pattern matching on it 
    *)
    | firstline::continuation -> match firstline with
      
      (* 
       - case: line doesn't include statement 
       - calls function interpret on rest of file
         parameter: continuation
      *)
      | _, _, None -> interpret continuation

      (* 
       - case: line includes statement 
       - calls function interpret_stmt
         parameters: stmt and continuation
      *)
      | _, _, Some stmt -> (interp_stmt stmt continuation)

(* 
   - mutally recursive function interpret_stmt with 
     interpret, interp_print, and interp_input
   - parameters: stmt       - type Absyn.stmt
             : continuation - type Absyn.program
*)
and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with

    (* 
       - case: statement = dim 
       - calls function interp_dim 
         parameters:
    *)
    | Dim (ident, expr) -> interp_dim ident expr continuation

    (* 
       - case: statement = let 
       - calls function interp_let 
         parameters:
    *)
    | Let (memref, expr) -> interp_let memref expr continuation

    (* 
       - case: statement = goto 
       - calls function interp_goto 
         parameters:
    *)
    | Goto label -> interp_goto label continuation

    (* 
       - case: statement = if 
       - calls function interp_if
         parameters: 
    *)
    | If (expr, label) -> interp_if expr label continuation

    (* 
       - case: statement = print 
       - calls function interp_print
         parameters: print_list continuation
    *)
    | Print print_list -> interp_print print_list continuation

    (* 
       - case: statement = input 
       - calls function interp_input
         parameters: memref_list continuation
    *)
    | Input memref_list -> interp_input memref_list continuation

and interp_dim (ident : string) (expr : Absyn.expr) 
               (continuation : Absyn.program) = 
     (* print_string ident;
     print_newline ();
     print_string (Dumper.string_of_expr expr);
     print_newline (); *)
     let arr_name = ident in (match expr with 
        | Number number -> 
        let size = int_of_float number in 
        let array = Array.make size 0.0 
        in Hashtbl.add Tables.array_table ident array;
        | Memref memref -> (match memref with
               | Variable ident -> let size_float = 
                    Hashtbl.find Tables.variable_table ident in 
                       let size = int_of_float size_float in
                           let array = Array.make size 0.0 in 
                    Hashtbl.add Tables.array_table arr_name array
               | _ -> ())
        | _ -> ());
     interpret continuation 

and interp_let (memref : Absyn.memref) (expr : Absyn.expr) 
               (continuation : Absyn.program) = 
     (* print_string (Dumper.string_of_memref memref);
     print_newline ();
     print_string (Dumper.string_of_expr expr);
     print_newline (); *)
     (* print_string (Dumper.string_of_memref memref) *)
     (* print_string (Dumper.string_of_expr expr); *)
     (* let test = eval_expr expr in print_float test; *)
     let calc = eval_expr expr in 
     (match memref with 
        | Variable ident -> 
          Hashtbl.add Tables.variable_table ident calc;

        | Arrayref (ident, expr) -> let num = eval_expr expr in
          let index = int_of_float num in 
               let array = Hashtbl.find Tables.array_table ident in 
                    Array.set array index calc);
             (* print_string ident; *)
     interpret continuation

and interp_goto (label : string) (continuation : Absyn.program) = 
     (* print_string label;
     print_newline (); *)
     let jump = Hashtbl.find Tables.label_table label in interpret jump;

and interp_if (expr : Absyn.expr) (label : string) 
              (continuation: Absyn.program) =
     (* print_string (Dumper.string_of_expr expr); 
     print_newline (); *)
     match expr with 
          | Binary (oper, expr1, expr2) -> 
          let op = Hashtbl.find Tables.boolean_fn_table oper in
          let arg1 = eval_expr expr1 in 
          let arg2 = eval_expr expr2 in 
          let bool = op arg1 arg2 in (match bool with
          
               | true -> interp_goto label continuation
               | _ -> interpret continuation
          )
          | _ -> ()
     (* let calc = eval_expr expr in 
     let bool = string_of_bool calc in 
     let print_bool = print_string bool; *)

     (* let eval = eval_expr expr in (match eval with 
          | _ -> interpret continuation
     ); *)

(* 
   - mutally recursive function interpret_stmt with 
     interpret, interp_stmt, and interp_input
   - parameters: print_list   - type Absyn.printable list
             : continuation - type Absyn.program
*)
and interp_print (print_list : Absyn.printable list) 
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with

         (* 
            - case: argument is a String of type string
            - calls function print_string and prints the string
         *)
         | String string -> let regex = Str.regexp "\"\\(.*\\)\"" 
            in print_string (Str.replace_first regex "\\1" string)

         (* 
            - case: argument is a Printexpr with argument expr
            - calls function eval_expr on expr
         *)
         | Printexpr expr -> print_float (eval_expr expr))

         (* 
            - call print_item on each item in print_list
            - parameter: print_list
         *)
    in (List.iter print_item print_list; 

        (* prints newline character *)
        print_newline ());
    (* 
       - call function interpret 
       - parameter: continuation
    *)
    interpret continuation

(* 
   - mutally recursive function interpret_stmt with 
     interpret, interp_stmt, and interp_print
   - parameters: memref_list  - type Absyn.memref list
             : continuation - type Absyn.program
*)
and interp_input (memref_list : Absyn.memref list) 
                 (continuation : Absyn.program) =
    let input_number memref =
             (* checks if user-input is a readable number and 
                assings to a variable if it is *)
        try  let number = Etc.read_number ()

             (* prints user-inputted number and a new-line char *)
        in (match memref with 
          | Variable ident -> 
               Hashtbl.add Tables.variable_table ident number
                 (* print_float number; print_newline () *)
                 
               | _ -> ())
        with End_of_file -> 
               Hashtbl.replace Tables.variable_table "eof" 1.0;
    in List.iter input_number memref_list;
    interpret continuation

(* 
   function interpret_program
   parameter: program
*)
let interpret_program program =
     (* 
        - calls function init_label_table in tables.ml 
        - init_label_table adds label positions to label table
        - parameter: program
     *)
    (Tables.init_label_table program; 

     (* used for debugging *)
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;

     (* 
        - calls function interpret 
        - parameter: program
     *)
     interpret program)
