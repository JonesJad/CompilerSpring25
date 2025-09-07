(*
This file defines an LL parser for Pyrrhuloxia.
*)

open Batteries;;

open Asts;;
open LLLexer;;

(** This exception should be raised if an error occurs during parsing. *)
exception ParserError of string;;

(** A helper function which attempts to parse the prefix of a token stream into
    an expression.  It takes as an argument a list of mini-parsers: functions
    which *try* to recognize a specific expression and raise a ParserError if
    they fail. *)
let rec parse_first_of
    (parsers : (token list -> expr * token list) list)
    (input : token list)
  : expr * token list =
  match parsers with
  | [] ->
    (* We have no routines to use to find a parser.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 8 in
    let first_part_of_token_stream = List.take max_error_length input in
    let stream_string =
      String.join ", " (List.map show_token first_part_of_token_stream)
    in
    raise (ParserError(
        Printf.sprintf "Failed to parse expression from tokens: %s"
          stream_string))
  | parser :: parsers' ->
    try
      (* If this parser successfully produces a result, use it! *)
      parser input
    with
    | ParserError _ ->
      (* This parser failed.  Let's try the next one. *)
      parse_first_of parsers' input
;;

(** A routine which attempts to parse a general expression. *)
let rec parse_expr (input : token list) : expr * token list =
  parse_first_of 
  [
  parse_let;
  parse_if;
  parse_or_expr;
  (*parse_additive_expr;*)
  ]
input


(** A routine which attempts to parse an additive expression. *)
(*takes care of addition, subtraction, and multiplication (Is this okay???), 
 how is this working???*)
and parse_additive_expr (input : token list) : expr * token list =
  (* Read the first primary expr. *)
  let (e, input') = parse_multiplicative_expr input in
  (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
     return that along with the remaining tokens. *)
  let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with
    | TokPlus :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_multiplicative_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpPlus, e) :: rest_exprs, rest_input)

    | TokMinus :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_multiplicative_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpMinus, e) :: rest_exprs, rest_input)
      
    | _ ->
      (* We couldn't find a "+ expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')


and parse_multiplicative_expr (input: token list) : expr * token list = 
  let (e, input') = parse_primary_expr input in
  let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with

    | TokMult :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_primary_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpTimes, e) :: rest_exprs, rest_input)

      | _ ->
        ([], loop_input)
    in
    (* Find all of the operations we want to attach to this expr. *)
    let (op_exprs, input'') = loop input' in
    let result =
      List.fold_left
        (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
        e
        op_exprs
    in
    (result, input'')

and parse_comp_expr (input : token list) : expr * token list = 
  let (e, input') = parse_additive_expr input in
  let rec loop (loop_input : token list)
  : (binary_operator * expr) list * token list =
  match loop_input with

  | TokGreater :: loop_input' ->
    (* A + is next.  See if we can get an expression after it. *)
    let (e, loop_input'') = parse_additive_expr loop_input' in
    let (rest_exprs, rest_input) = loop loop_input'' in
    (* We found a + and an expression.  Store them and keep trying. *)
    ((OpGreaterThan, e) :: rest_exprs, rest_input) 

| TokLess :: loop_input' ->
    (* A + is next.  See if we can get an expression after it. *)
    let (e, loop_input'') = parse_additive_expr loop_input' in
    let (rest_exprs, rest_input) = loop loop_input'' in
    (* We found a + and an expression.   Store them and keep trying. *)
    ((OpLessThan, e) :: rest_exprs, rest_input)

  | TokEqual :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
    let (e, loop_input'') = parse_additive_expr loop_input' in
    let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
    ((OpEqualTo, e) :: rest_exprs, rest_input)

    | _ ->
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')

and parse_and_expr (input: token list) : expr * token list = 
      let (e, input') = parse_comp_expr input in
      let rec loop (loop_input : token list)
        : (binary_operator * expr) list * token list =
        match loop_input with
    
        | TokAnd :: loop_input' ->
          (* A + is next.  See if we can get an expression after it. *)
          let (e, loop_input'') = parse_comp_expr loop_input' in
          let (rest_exprs, rest_input) = loop loop_input'' in
          (* We found a + and an expression.  Store them and keep trying. *)
          ((OpAnd, e) :: rest_exprs, rest_input) 
    
          | _ ->
            ([], loop_input)
        in
        (* Find all of the operations we want to attach to this expr. *)
        let (op_exprs, input'') = loop input' in
        let result =
          List.fold_left
            (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
            e
            op_exprs
        in
        (result, input'')

(** A routine which attempts to parse a primary expression. *)
and parse_primary_expr (input : token list) : expr * token list =
  parse_first_of
    [ 
      parse_is_bool;
      parse_is_int;
      parse_print;
      parse_before;
      parse_after;
      parse_int_expr;
      parse_paren_expr;
      parse_bool_expr;

      (*again, try this last*)
      parse_iden;
    ]
    input
(** A routine which attempts to parse the integer production of a primary
    expression. *)
and parse_int_expr (input : token list) : expr * token list =
  match input with
  | TokInt n :: input' ->
    (EInt n, input')
  | TokMinus :: TokInt n :: input' ->
    (EInt (-n), input')
  | _ ->
    raise (ParserError("Failed to parse integer"))

and parse_let (input : token list) : expr * token list = 

  match input with 
  |TokLet::TokIden(e)::rest -> 
    (*loop through the next tokens until we get to a "then" token*)
    (* let (_, rest2) = parse_primary_expr rest in  *)
      begin 
      match rest with 

      |TokEqual::rest2 -> 
      let (value, rest3) = parse_expr rest2 in 
        begin 
        match rest3 with  
        |TokIn::rest4 -> 
        let (result, restfin) = parse_expr rest4
        in 
        (ELet(e, value, result), restfin)
        |_ -> raise (ParserError("Failed to parse if expression"))
        end
      |_ -> raise (ParserError("Failed to parse if expression")) 
      end
    |_ -> raise (ParserError("Failed to parse if expression"))

and parse_if (input : token list) : expr * token list = 
  
  match input with 
  |TokIf::rest -> 
    (*loop through the next tokens until we get to a "then" token*)
    let (cond_expr, rest2) = parse_expr rest in 
      begin 
      match rest2 with 
      |TokThen::rest3 -> 
      let (then_expr, rest4) = parse_expr rest3 in 
        begin 
        match rest4 with  
        |TokElse::rest5 -> 
        let (else_expr, restfin) = parse_expr rest5
        in 
        (EIf(cond_expr, then_expr, else_expr), restfin)
        |_ -> raise (ParserError("Failed to parse if expression"))
        end
      |_ -> raise (ParserError("Failed to parse if expression")) 
      end
    |_ -> raise (ParserError("Failed to parse if expression"))

and parse_or_expr (input : token list) : expr * token list = 

  let (e, input') = parse_and_expr input in
  let rec loop (loop_input : token list)
  : (binary_operator * expr) list * token list =
  match loop_input with

  | TokOr :: loop_input' ->
    (* A + is next.  See if we can get an expression after it. *)
    let (e, loop_input'') = parse_and_expr loop_input' in
    let (rest_exprs, rest_input) = loop loop_input'' in
    (* We found a + and an expression.  Store them and keep trying. *)
    ((OpOr, e) :: rest_exprs, rest_input) 

    | _ ->
      (* We couldn't find a "+ expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')


and parse_iden (input : token list) : expr * token list = 
      match input with 
      | TokIden s ::rest -> 
        (EVar s , rest)
      |_ -> raise (ParserError("Failed to parse identifier")) 

(** A routine which attempts to parse the parenthesis production of a primary
    expression. *)
and parse_paren_expr (input : token list) : expr * token list =
  match input with
  | TokOpenParen :: input' ->
    begin
      let (e, input'') = parse_expr input' in
      match input'' with
      | TokCloseParen :: input''' ->
        (e, input''')
      | _ ->
        raise (ParserError("Failed to parse expression: missing close paren"))
    end
  | _ ->
    raise (ParserError("Failed to parse expression: missing open paren"))

(* TODO: define your parsing helper functions here *)
and parse_bool_expr (input : token list) : expr * token list = 
  match input with 
  |TokTrue :: rest -> 
    (EBool(true), rest)
  |TokFalse :: rest -> 
    (EBool(false), rest)
  |_ -> raise (ParserError("Failed to parse expression: no bool"))

  (*doesnt work yet*)
and parse_is_bool (input : token list) : expr * token list = 
  match input with 
  |TokIsBool :: TokOpenParen :: rest1 -> 
    let (e, rest2) = parse_expr rest1 in (*get the expression in the parentheses*)
      begin 
      match rest2 with 
      |TokCloseParen :: rest3 -> 
        (EUnaryOp(OpIsBool, e), rest3)
      |_ -> raise (ParserError("Failed to parse IsBool"))
      end

    |_ -> raise (ParserError("Failed to parse IsBool"))

and parse_is_int (input : token list) : expr * token list = 
  match input with 
  |TokIsInt :: TokOpenParen :: rest1 -> 
    let (e, rest2) = parse_expr rest1 in
      begin 
      match rest2 with 
      |TokCloseParen :: rest3 -> 
        (EUnaryOp(OpIsInt, e), rest3)
      |_ -> raise (ParserError("Failed to parse IsInt"))
      end

  |_ -> raise (ParserError("Failed to parse IsInt"))

and parse_after (input : token list) : expr * token list = 
  match input with 
  |TokAfter:: TokOpenParen :: rest1 -> 
    (*get the thing that is in the expression *)
    let (e, rest2) = parse_expr rest1 in 

    begin 
    match rest2 with 
    |TokCloseParen :: rest3 -> (*must be enclosed with parens*)
      (EUnaryOp(OpAfter, e), rest3)
    |_ -> raise (ParserError("Failed to parse After()"))
    end 
  |_ -> raise (ParserError("Failed to parse After()"))

and parse_before (input : token list) : expr * token list = 
  match input with 
  
  |TokBefore :: TokOpenParen :: rest1 -> 
    (*get the thing that is in the expression *)
    let (e, rest2) = parse_expr rest1 in 

    begin 
    match rest2 with 
    |TokCloseParen :: rest3 -> (*must be enclosed with parens*)
    (EUnaryOp(OpBefore, e), rest3)
    |_ -> raise (ParserError("Failed to parse Before()"))
    end 
  |_ -> raise (ParserError("Failed to parse Before()"))

and parse_print (input : token list) : expr * token list = 
  match input with
  |TokPrint :: TokOpenParen :: rest1 -> 

  let (e, rest2) = parse_expr rest1 in 
  begin 
    match rest2 with 
    |TokCloseParen :: rest3 -> 
      (EUnaryOp(OpPrint, e), rest3)
    |_ -> raise (ParserError("Failed to parse print()"))
    end 
  |_ -> raise (ParserError("Failed to parse print()"))


(** This function attempts to transform a list of tokens into a program.  If
    this process is unsuccessful, a ParserError is raised. *)
let parse (tokens : token list) : program =
  let (e, extras) = parse_expr tokens in
  if List.is_empty extras then
    Program([],e)
  else
    let stream_string = String.join ", " (List.map show_token extras) in
    raise (ParserError(Printf.sprintf "Extraneous tokens during parse: %s"
                         stream_string))
;;
