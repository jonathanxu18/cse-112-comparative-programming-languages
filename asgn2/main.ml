(* Zain Shafique Jonathan Xu *)
(* $Id: main.ml,v 1.3 2020-01-24 12:57:06-08 - - $ *)

(*
* Main program reads a file and prints to stdout.
*)

(* 
   - function called interpret-source 
   - interprets input sb file
   - parameter: filename
*)
let interpret_source filename =

         (* tests if the input file matches *)
    try (let sourcefile =
             if filename = "-"
             then stdin
             (* 
                - else opens input file if not null
                - uses Lexing, Parser, and Scanner files when 
                  running Interp.interpret_program
                  as the parameter abstract_syntax
             *)
             else open_in filename in
             
         (* Lexing starts up the scanner *)
         let lexbuf = Lexing.from_channel sourcefile in

         (* 
            - initializes abstract_syntax
            - variable that is result of parsing input file
         *)
         let abstract_syntax = Parser.program Scanner.token lexbuf in

         (* 
            - calls interpret_program in interp.ml 
            - parameter: abstract_syntax
         *)
         Interp.interpret_program abstract_syntax)

    (* catches Sys_error (string) and directs to Etc.die [string] *)
    with Sys_error (string) -> Etc.die [string]

(* used for interactive mode *)
  let _ = if !Sys.interactive

        (* return unit (null) / do nothing *)
        then ()
        else match Array.length Sys.argv with
             | 1 -> interpret_source "-"
             | 2 -> interpret_source Sys.argv.(1)
             | _ -> Etc.usage_exit ["[filename.sb]"] 
