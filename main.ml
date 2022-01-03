open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


(*Debug print function*)
let debug = true
let dprint s = if debug then Printf.printf "[Debug]: %s\n" s

(*Regexp to check if a line is final line*)
let is_final_line l = let r = Str.regexp ".*;;" in Str.string_match r l 0;;

(*Function to concatenate strings with a blank space in the middle using the ocaml build-in function: String.concat separator str_list*)
let concat a b = String.concat " " [a;b];;

(*Function to concatenate multiple input lines until a line ending with a regexp defined in is_final_line is found*)
let read_input_i () =
  let rec aux acc =
    let line = read_line () in
    if (is_final_line line) then 
      let substring = (List.hd (String.split_on_char ';' line)) in 
        concat acc substring
    else 
      let next_iteration = (concat  acc line) in
        aux next_iteration;
  in aux ""

(* Function to read lines from a file. It keeps track of the number of line (nL) in order to raise errors*)
let read_input_f inC nLine =
  let rec aux acc_Text acc_lineN =
    let line = input_line inC in
    if (is_final_line line) then 
      let substring = (List.hd (Str.split (Str.regexp ";;") line)) in
        ((concat acc_Text substring), acc_lineN+1)
    else 
        aux (concat acc_Text line) (acc_lineN+1)
  in aux "" nLine
;;



(*Function to read lambda expresions from a file.
  -- The expresions will be read sequentially, from the first line to the last
  -- We will input a expression to the main loop whenever we find a ;;
  -- If an error is found, we will indicate the fist line and last line of the expression that raised the exception*)

let loop_f () = 
  let inC = open_in Sys.argv.(1) in
  try
    print_endline "[Debug] Starting lambda evaluator in File Mode";
    while true do
      let rec loop ctx nLine =
        try
          let inp = (read_input_f inC nLine) in
            let op = s token (from_string (fst inp)) in
          loop (do_operation ctx op) (snd inp)
        with
           Lexical_error ->
             print_endline ("[Error] Lexical error in line: " ^ string_of_int nLine);
             exit 0;
         | Parse_error ->
             print_endline ("[Error] Syntax error in line: " ^ string_of_int nLine);
             exit 0;
         | Type_error e ->
             print_endline ("[Error] Type error: " ^ e ^ " in line: " ^ string_of_int nLine);
             exit 0;
      in
        loop emptyctx 1
    done
  with End_of_file ->
    close_in inC
  ;;
;;

(*Function to read lambda expresions from an interactive console.*)
let loop_i () =
  print_endline "[Debug] Starting lambda evaluator in Interactive Mode";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let inp = read_input_i () in
        let op = s token (from_string (inp)) in
      loop (do_operation ctx op)
    with
       Lexical_error ->
         print_endline "[Error] Lexical error";
         loop ctx
     | Parse_error ->
         print_endline "[Error] Syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("[Error] Type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "[Debug] Exiting "
  in
    loop emptyctx
  ;;

  

if (Array.length Sys.argv >= 2) then
  loop_f ()
else
  loop_i ()
;;

