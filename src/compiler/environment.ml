(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_variable (name : string) (env : environment) : environment =
   let address, dict = env in (address-8, Map.String.add name (ArgMemory(AddrByRegisterOffset(RBP, address))) dict)
;;

(*for the initial binding of funciton names to closures at the beginning of a falcon program*)
let allocate_closure (name : string) (env : environment) : environment = 
   let address, dict = env in (address-8, Map.String.add name (ArgMemory(AddrByLabelOffset(("closure_of_fn_"^name,"1")))) dict)
(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
   match env with
   |(_,b) -> try (Map.String.find name b) with Not_found -> raise (UnboundVariable (name, env))
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
   let address, dict = env in (ArgMemory(AddrByRegisterOffset(RBP, address)), (address-8, dict))
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;
(* let x = allocate_named_variable "var1" empty_environment in
print_endline (string_of_environment (allocate_named_variable "var1" empty_environment));
print_endline (code_of_argument(find_named_variable "var1" x)) *)