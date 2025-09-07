(*
This file defines a lexer for an LL parser generator for Pyrrhuloxia.
*)

(*What to ask at office hours / lab time:

need to do:  

&& / || expressions: parsing  (???)
 
if / let expressions: parsing

make sure that I need to parse all of the things that I have identified so far
*)



open Batteries;;

(** An exception type which should be raised when lexing fails. *)
exception LexerError of string;;

(** The type of tokens produced by this lexer. *)
type token =
  | TokMult
  | TokPlus
  | TokInt of int
  | TokIden of string
  | TokMinus
  | TokOpenParen
  | TokCloseParen
  | TokLet
  | TokIn
  | TokAfter
  | TokBefore
  | TokPrint
  | TokIsBool
  | TokIsInt
  | TokTrue 
  | TokFalse 
  | TokAnd
  | TokOr
  | TokIf
  | TokThen 
  | TokElse
  | TokLess
  | TokGreater
  | TokEqual

  

  (* TODO: add more token types here *)
[@@deriving eq, ord, show];;

(** A helper function which attempts to lex the prefix of a string into a token.
    It takes as an argument a list of mini-lexers: functions which *try* to
    recognize a specific token and raise a LexerError if they fail. *)
let rec tokenize_first_of
    (tokenizers : (char list -> token * char list) list)
    (input : char list)
  : token * char list =
  match tokenizers with
  | [] ->
    (* We have no routines to use to find a token.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 20 in
    let first_part_of_string =
      String.of_list (List.take max_error_length input)
    in
    raise (LexerError(Printf.sprintf "Unrecognized token starting at: \"%s\""
                        first_part_of_string))
  | tokenizer :: tokenizers' ->
    try
      (* If this tokenizer successfully produces a result, use it! *)
      tokenizer input
    with
    | LexerError _ ->
      (* This tokenizer failed.  Let's try the next one. *)
      tokenize_first_of tokenizers' input
;;

(** This routine determines whether a particular character is an identifier
    character. *)
let is_identifier_character (ch : char) : bool =
  Char.is_digit ch || Char.is_letter ch || ch == '_' || ch == '\''
;;

(** This routine discards whitespace from the input stream. *) 

let discard_whitespace (input : char list) : char list =
  List.drop_while Char.is_whitespace input
;;

(** This routine attempts to lex a single numeric token from the input.
    Note that this routine does NOT handle negative numbers. *)
let tokenize_int (input : char list) : token * char list =
  (* Get all of the digits from the front of the input that we can. *)
  let digits = List.take_while Char.is_digit input in
  let rest = List.drop_while Char.is_digit input in
  (* If we didn't get any digits, then the next token isn't an integer. *)
  if List.is_empty digits then
    raise (LexerError "Could not tokenize integer")
  else
    (* We error on "123abc" since it is not a valid integer or identifier. *)
    match rest with
    | ch::_ when is_identifier_character ch ->
      raise (LexerError ("Identifier character following integer"))
    | _ ->
      (* Convert the list of digits into a string. *)
      let digit_string = String.of_list digits in
      (* Turn that into a number. *)
      let number = int_of_string digit_string in
      (* Return a token with that number along with the rest of the input. *)
      (TokInt number, rest)
;;

let tokenize_iden (input : char list) : token * char list = 

 let chars = List.take_while Char.is_letter input in (*all letters*)
 let rest = List.drop_while Char.is_letter input in (*remaining*)

 if List.is_empty chars then 
  raise (LexerError "Could not tokenize and identifier")
else 
  match rest with 
  |ch::_ when (is_identifier_character ch ) -> 
    raise (LexerError ("Following chracter not an identifier"))
  
  |_ -> 
    let string = String.of_list chars in 
    (TokIden string, rest)

  ;;


(** This routine attempts to lex a single plus token from the input. *)
let tokenize_plus (input : char list) : token * char list =
  match input with
  | '+'::rest -> (TokPlus, rest)
  | _ -> raise (LexerError "Could not tokenize plus")
;;

(** This routine attempts to lex a single minus token from the input. *)
let tokenize_minus (input : char list) : token * char list =
  match input with
  | '-'::rest -> (TokMinus, rest)
  | _ -> raise (LexerError "Could not tokenize minus")
;;

let tokenize_mult (input : char list) : token * char list = 
  match input with 
  | '*' ::rest -> (TokMult, rest)
  |_ -> raise (LexerError "Could not tokenize mult")
;;

(** This routine attemps to lex a single open parenthesis from the input. *)
let tokenize_open_paren (input : char list) : token * char list =
  match input with
  | '('::rest -> (TokOpenParen, rest)
  | _ -> raise (LexerError "Could not tokenize open parenthesis")
;;

(** This routine attemps to lex a single close parenthesis from the input. *)
let tokenize_close_paren (input : char list) : token * char list =
  match input with
  | ')'::rest -> (TokCloseParen, rest)
  | _ -> raise (LexerError "Could not tokenize close parenthesis")
;;


(* TODO: write more lexer routines here *)

(*tries to lex a boolean value*)
let tokenize_true (input : char list) : token * char list = 
  match input with 
  (*PROBLEM: WHEN A TRUE OR FALSE IS FOLLOWED BY AN THEN OR ELSE STATEMENT*)
  |'t'::'r'::'u'::'e'::ch::rest when not (is_identifier_character ch)  -> (TokTrue, ch::rest)
  |'t'::'r'::'u'::'e'::[] -> (TokTrue, []) (*special case: end of file*)
  |_ -> raise (LexerError "Could not tokenize true")
;;

let tokenize_false (input : char list) : token * char list = 
  match input with 
  |'f'::'a'::'l'::'s'::'e'::ch::rest when not (is_identifier_character ch) -> (TokFalse, ch::rest)
  |'f'::'a'::'l'::'s'::'e'::[] -> (TokFalse, []) (*special case: end of file*)
  |_ -> raise (LexerError "Could not tokenize false")
;;

let tokenize_and (input : char list) : token * char list = 
  match input with 
  | '&'::'&'::rest -> (TokAnd, rest)
  |_ -> raise (LexerError "Could not tokenize &&")
;;

let tokenize_less (input : char list) : token * char list = 
  match input with 
  | '<'::rest -> (TokLess, rest) 
  |_ -> raise (LexerError "Could not tokenize <")
;;

let tokenize_greater (input : char list) : token * char list = 
  match input with 
  | '>'::rest -> (TokGreater, rest) 
  |_ -> raise (LexerError "Could not tokenize >")
;;

let tokenize_equal (input : char list) : token * char list = 
  match input with 
  | '='::rest -> (TokEqual, rest) 
  |_ -> raise (LexerError "Could not tokenize =")
;;


let tokenize_is_bool (input : char list) : token * char list = 
  match input with 
  |'i'::'s'::'b'::'o'::'o'::'l'::ch::rest when not (is_identifier_character ch) -> 
    (TokIsBool, ch::rest)
    |_ -> raise (LexerError "Could not tokenize IsBool")

;;

let tokenize_is_int (input : char list) : token * char list = 
  match input with 
  |'i'::'s'::'i'::'n'::'t'::ch::rest when not (is_identifier_character ch) -> 
    (TokIsInt, ch::rest)
    |_ -> raise (LexerError "Could not tokenize IsInt")

;;

let tokenize_print (input : char list) : token * char list = 
  match input with 
  |'p'::'r'::'i'::'n'::'t'::ch::rest when not (is_identifier_character ch) -> 
    (TokPrint, ch::rest)
  |_ -> raise (LexerError "Could not tokenize print()")


  ;;

let tokenize_after (input : char list) : token * char list = 
  match input with 
  |'a'::'f'::'t'::'e'::'r'::ch::rest when not (is_identifier_character ch) -> 
    (TokAfter, ch::rest)
    |_ -> raise (LexerError "Could not tokenize After()")

;;

let tokenize_before (input : char list) : token * char list = 
  match input with 
  |'b'::'e'::'f'::'o'::'r'::'e'::ch::rest when not (is_identifier_character ch) -> 
    (TokBefore, ch::rest)
    |_ -> raise (LexerError "Could not tokenize Before()")

;;

let tokenize_or (input : char list) : token * char list = 
  match input with 
  |'|'::'|'::rest -> 
    (TokOr, rest) 
    |_ -> raise (LexerError "Could not tokenize or")

  ;;


(*needs to get a *)
let tokenize_if (input : char list) : token * char list = 
  match input with 
  |'i'::'f'::ch::rest when not (is_identifier_character ch) -> 
  (TokIf, ch::rest)
  |_ -> raise (LexerError "Could not tokenize if")
  
;;

let tokenize_then (input : char list) : token * char list = 
  match input with 
  |'t'::'h'::'e'::'n'::ch::rest when not (is_identifier_character ch) -> 
  (TokThen, ch::rest) 
  |_ -> raise (LexerError "Could not tokenize then")
  
;;

let tokenize_else (input : char list) : token * char list = 
  match input with 
  |'e'::'l'::'s'::'e'::ch::rest when not (is_identifier_character ch) -> 
  (TokElse, ch::rest)
  |_ -> raise (LexerError "Could not tokenize else")
;;

let tokenize_let (input : char list) : token * char list = 
  match input with 
  |'l'::'e'::'t'::ch::rest when not (is_identifier_character ch) -> 
  (TokLet, ch::rest)
  |_ -> raise (LexerError "Could not tokenize let")
  ;;

let tokenize_in (input  : char list) : token * char list = 
 match input with 
 |'i'::'n'::ch::rest  when not (is_identifier_character ch) -> (TokIn, ch::rest)
  |_ -> raise (LexerError "Could not tokenize in")
 ;;

(** This routine attempts to take a single token from the input stream.  If an
    unrecoverable error occurs, a LexerError is raised. *)
let tokenize (input : char list) : token * char list =
  (* TODO: modify this function as you write more lexer routines. *)
  tokenize_first_of
    [ 
      tokenize_before;
      tokenize_after;
      tokenize_int;
      tokenize_plus;
      tokenize_minus;
      tokenize_mult;

      tokenize_if;
      tokenize_then;
      tokenize_else;
      tokenize_false;
      tokenize_true;
      
      tokenize_open_paren;
      tokenize_close_paren;
      tokenize_and;
      tokenize_or;
      tokenize_less;

      tokenize_let;
      tokenize_in;
      tokenize_greater;
      tokenize_equal;
      tokenize_is_bool;
      tokenize_is_int;
      tokenize_print;

      (*must do this last*)
      tokenize_iden;
      
    ]
    input
;;

(** A function to lex a string.  If lexing is successful, a list of tokens is
    returned.  Otherwise, a LexerError is raised. *)
let lex (text : string) : token list =
  let input = String.to_list text in
  (*
    This function recursively takes a token from the input stream.  It builds up
    a list in *reverse* order because this allows it to tail recurse; the
    list is reversed once the routine is complete.
  *)
  let rec take_tokens (tokens_so_far : token list) (input : char list)
    : token list =
    (* Get rid of any leading whitespace. *)
    let input' = discard_whitespace input in
    (* If we're out of input, then return the list of tokens we have. *)
    if List.is_empty input' then
      tokens_so_far
    else
      (* Take a token from the input. *)
      let (token, input'') = tokenize input' in
      (* Recurse to take more tokens.  Note that the new token is put on the
         front of the list, resulting in the list being constructed in backwards
         order.  This is reversed later.  Doing things this way allows us to
         tail recurse here!  :-D *)
      take_tokens (token :: tokens_so_far) input''
  in
  List.rev (take_tokens [] input)
;;
