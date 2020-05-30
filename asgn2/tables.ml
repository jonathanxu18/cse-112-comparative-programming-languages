(* Zain Shafique Jonathan Xu *)
(*five hash tables needed for implementation *)

(* creating a hash table type (string, float) Hashtbl.t *)
type variable_table_t = (string, float) Hashtbl.t

(* creating a hash table type (string, float array) Hashtbl.t *)
type array_table_t = (string, float array) Hashtbl.t

(* 
   - creating a hash table type (string, float -> float) Hashtbl.t 
   - second paramter is a function that 
     takes in a float and returns a float 
*)
type unary_fn_table_t = (string, float -> float) Hashtbl.t

(* 
   - creating a hash table type 
     (string, float -> float -> float) Hashtbl.t 
   - second paramter is a function that takes 
     in two floats and returns a float
*)
type binary_fn_table_t = (string, float -> float -> float) Hashtbl.t

(* creating a hash table type 
   (string, Absyn.program) Hashtbl.t *)
type label_table_t = (string, Absyn.program) Hashtbl.t

(* 
   - creating a hash table type 
     (string, float -> float -> bool) Hashtbl.t 
*)
type boolean_table_t = (string, float -> float -> bool) Hashtbl.t

(* 
   - initializing hash table called variable_table 
   - used to store variables from sb file
   - initial size = 16
*)
let variable_table : variable_table_t = Hashtbl.create 16

(* inserting elements into variable_table *)
let _ = List.iter (fun (label, value) -> 
      Hashtbl.add variable_table label value)

                 ["e"  , exp 1.0;
                  "eof", 0.0;
                  "pi" , acos ~-.1.0;
                  "nan", nan]

(* 
   - initializing hash table called array_table
   - used to store arrays from sb file
   - initial size = 16
*)
let array_table : array_table_t = Hashtbl.create 16

(* 
   - initializing hash table called unary_fn_table
   - used to store unary operators
   - initial size = 16
*)
let unary_fn_table : unary_fn_table_t = Hashtbl.create 16

(* inserting operators into unary_fn_table *)
let _ = List.iter (fun (label, value) -> 
      Hashtbl.add unary_fn_table label value)

                 ["+"    , (~+.);
                  "-"    , (~-.);
                  "abs"  , abs_float;
                  "acos" , acos;
                  "asin" , asin;
                  "atan" , atan;
                  "ceil" , ceil;
                  "cos"  , cos;
                  "exp"  , exp;
                  "floor", floor;
                  "log"  , log;
                  "log10", log10;
                  "log2" , (fun x -> log x /. log 2.0);
                  "round", (fun x -> floor (x +. 0.5));
                  "sin"  , sin;
                  "sqrt" , sqrt;
                  "tan"  , tan]

(* 
   - initializing hash table called binary_fn_table
   - used to store binary operators
   - initial size = 16
*)
let binary_fn_table : binary_fn_table_t = Hashtbl.create 16

(* inserting operators into binary_fn_table *)
let _ = List.iter (fun (label, value) -> 
      Hashtbl.add binary_fn_table label value)
                 
                 ["+", (+.);
                  "-", (-.);
                  "*", ( *.);
                  "/", (/.);
                  "%", mod_float;
                  "^", ( ** )]

let boolean_fn_table : boolean_table_t = Hashtbl.create 16

let _ = List.iter (fun (label, value) -> 
      Hashtbl.add boolean_fn_table label value)
                 
                 ["=" , (=);
                  "!=", (<>);
                  "<" , (<);
                  ">" , (>);
                  "<=", (<=);
                  ">=", (>=); 
                 ]
(* 
   - initializing hash table called label_table
   - used to store labels from sb file
   - initial size = 16
*)
let label_table : label_table_t = Hashtbl.create 16

(* 
   - recursive function that finds labels from sb file and 
     adds their positions to the label table 
*)

let rec init_label_table program =
    let rec init program =  match program with
        
        (* 
           - case: empty line 
           - returns unit
        *)
        | [] -> ()

        (* 
           - case: has label 
           - adds label and rest of file after position of label as 
             (key, value) into label table
           - calls function init on rest of file
        *)
        | (_, Some label, _)::rest -> 
            (Hashtbl.add label_table label program; init rest)

        (* 
           - case: non-empty line that doesn't have label
           - calls function init on rest of file
        *)
        | _::rest -> init rest   
    in (Hashtbl.reset label_table; init program)

(* only used for debugging *)
let dump_label_table () =
    let dump key value = match value with
        | [] -> ()
        | (line, _, _)::_ -> Printf.fprintf stderr
              "label_table: \"%s\" -> line %d\n%!" key line
    in Hashtbl.iter dump label_table 
