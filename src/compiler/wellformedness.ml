open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;
type set = Set.String.t;;

(* This function produces a list of compile-time errors found in a program. *)
(*ERRORS: 

  1) tried to call a function that was not declared (not in the declaration list)

  2) tried to call a function with the wrong number of arguments

  3) tried to call a function with duplicate parameter names (ECall)

  4) created two functions with the same name (decalrationn list)

  5) tried to use an undefined variable

*)


(*program = declaration list (the functions that are defined) + expr (the actual things that are runnning) *)
(*declaration = function name + parameter names + expression explaining what the function does*)

(*given a program, will check the syntax to see if there are any errors in the code*)
let check_program_for_errors (p : program) : string list =
  match p with 

  (*4: Duplicate function names*)
  | Program(dec, e) -> 
    (*get a list of all the function names*)
    let funcs = (List.map (fun x -> match x with |DFunction(func, _, _)->func) dec) in  

    let args = (List.map (fun x -> match x with |DFunction(_,args, _)->args) dec) in

    (*checks for functions with the same name (Type 4 error)*)
    
    let rec get_dupe_functions (f_list : string list) : string list = 
      match f_list with 
      |[] -> []
      |a::b -> 
      if List.mem a b = true then [a] @ get_dupe_functions b else 
        get_dupe_functions b
      in 
    (* let rec create_valid_funcs (func_names) (func_args): (string*int) list =
      match func_names with
      | [] -> []
      | name::tail -> [(name, List.length(List.hd func_args))] @ create_valid_funcs tail (List.tl func_args)
    in 
    let funcs_dict = (create_valid_funcs funcs args) in *)
    (*returns a list of all of the duplicate functions found by thre function above*)
    let rec enum_dupes (dup_functions : string list): string list =
    match dup_functions with 
    | []->[]
    | a::b-> begin 
      ["Tried to create duplicate definition of function " ^a] @ enum_dupes b 
              end
    
    (*3: Duplicate parameter names*)    
    in      (*list of each of the functions parameters*)                                                     

    let rec check_args (args: string list list) (funcs): string list =
      match args with
        | [] -> []
        | a::b ->  
          (*which ones are duplicates*)
          let rec get_dupe_args(arg_list : string list) : string list = 
            match arg_list with 
            |[] -> []
            |a::b -> 
              if List.mem a b = true then [a] @ get_dupe_args b else get_dupe_args b
            in
          (*let errors = List.filter (fun x -> not (List.mem x a)) (List.sort_uniq compare a) in*)

          let rec check_dup (errors) (funcs): string list =
            match errors with 
            | []->[]
            | a::b-> 
              ["Tried to call a function " ^ List.hd funcs ^" with duplicate parameter name " ^a] @ check_dup b funcs
            
          in check_dup (get_dupe_args a) funcs @ check_args b (List.tl funcs)
    in
     
    (*checking how the user is calling the functions*)
    let rec check_e (valid_funcs : string list) (valid_args: string list) (e: expr):string list=
    
        match e with
      |EAppl(a,b) ->  check_e valid_funcs valid_args a @ check_e valid_funcs valid_args  b  (*Implement this*)
      |EBool(_)|EInt(_)-> []
      |EVar(x) -> if List.mem x (valid_args@valid_funcs) = false then ["Tried to use an undefined variable " ^x] else []
      |EUnaryOp(_,e1) -> check_e valid_funcs valid_args e1
      |EBinaryOp(_,e1,e2) -> (check_e valid_funcs valid_args e1) @ (check_e valid_funcs valid_args e2)
      |ELet(x,e1,e2) -> check_e valid_funcs valid_args e1@ check_e valid_funcs (valid_args@[x]) e2
      |EIf(e1,e2,e3) -> (check_e valid_funcs valid_args e1) @ (check_e valid_funcs valid_args e2) @ (check_e valid_funcs valid_args e3)
      |ETuple(e) -> 
        match e with
        | [] -> []
        | e1::e2 -> check_e valid_funcs valid_args e1 @ check_e valid_funcs valid_args (ETuple(e2))

    in
    let rec check_dec (dec: declaration list) : string list=
      match dec with
      | [] -> []
      | DFunction(_,args1,expr1)::d2 -> 
          check_e funcs args1 expr1 @
          check_dec d2
      in 
  check_e funcs [] e @ check_args args funcs @ check_dec dec @ enum_dupes (get_dupe_functions funcs)
  
      
;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else raise (IllFormed errors);
;;

