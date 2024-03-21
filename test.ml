open Ast

let _ =
  let input_string = input_line stdin in
  print_endline "Input from stdin:";
  print_endline input_string;
  let lexbuf = Lexing.from_string input_string in
  let program = Parser.program_rule Scanner.token lexbuf in
  print_endline "\nParsed program:";
  print_endline (string_of_program program)