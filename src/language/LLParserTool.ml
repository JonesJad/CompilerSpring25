(*
This file is the main entry point for the LL parser of Pyrrhuloxia.
*)
(** This exception is raised if the parser tool fails to parse a file. *)
exception ParseFailure of string;;

(** A function to parse some text as a program.  If the parsing process fails, a
    ParseFailure is raised containing a message describing why.
*)
let parse (filename : string) (text : string) =
  try
    let tokens = LLLexer.lex text in
    let program = LLParser.parse tokens in
    program
  with
  | LLLexer.LexerError msg ->
    raise (ParseFailure("Lexer error parsing " ^ filename ^ ": " ^ msg ))
  | LLParser.ParserError msg ->
    raise (ParseFailure("Parser error parsing " ^ filename ^ ": " ^ msg))
;;
