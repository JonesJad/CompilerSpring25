(** This file contains the definition of the compiler: the tool that translates
    an AST in 
    our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
open Wellformedness;;

(*
type expr =
  | EInt of int
  | EVar of string
  | EUnaryOp of unary_operator * expr
  | EBinaryOp of binary_operator * expr * expr
  | ELet of string * expr * expr
[@@deriving eq, ord, show]
show_expr
;;

*)
let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))
;;


  (*this function should be called in the code for each expression that expects to take integers as operands 
    ex) before, after, plus, minus... etc.

*)
let check_rax_int (l1 : string) (l2 : string) : instruction list = 

  [AsmCmt("START ERROR CHECK");
   AsmMov(ArgRegister(R11),ArgRegister(RAX))    (*move copy of RAX into R11 for checking later*)

  ;AsmNot(ArgRegister(R11))
  ;AsmShl(ArgRegister(R11), ArgConstant("63"))
  ;AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF"))
  ;AsmOr(ArgRegister(R11), ArgRegister(R10))
  ;AsmMov(ArgRegister(R10), ArgConstant("0xFFFFFFFFFFFFFFFF")) (*move true to compare it*)
  ;AsmCmp(ArgRegister(R11), ArgRegister(R10))]@   (*compare the result with TRUE*)
  let f = fresh_name(l1) in 
  let d = fresh_name(l2) in 
  [AsmJne(f)  (*jump if false*)
  ;AsmJmp(d)  (*when true, jump straight to DONE, we dont need to do anything*)     
  ;AsmLabel(f)
  ;AsmMov(ArgRegister(RDI), ArgConstant("1"))  (*CALL WITH ERROR CODE 1*)
  ;AsmCall("stopWithError")
  ;AsmLabel(d)
  ;AsmCmt("END ERROR CHECK")]  (*DONE label, the program may continue from here*)
;;

(*Does the same thing but for boolean values (exit condition 2)*)
let check_rax_bool (l1: string) (l2: string): instruction list = 
  [AsmCmt("START ERROR CHECK");
  AsmMov(ArgRegister(R11),ArgRegister(RAX)) ] @  (*move copy of RAX into R11 for checking later*)
  let l=fresh_name ("chk_l") in 
  let d1 =fresh_name ("done_chk") in
  [AsmShl(ArgRegister(R11), ArgConstant("62"));
   AsmMov(ArgRegister(R10), ArgConstant("0xC000000000000000"));
   AsmXor(ArgRegister(R11), ArgRegister(R10));
   AsmJz(l);
   AsmMov(ArgRegister(R11), ArgConstant("0x7FFFFFFFFFFFFFFF"));
   AsmJmp(d1);
   AsmLabel(l);
   AsmMov(ArgRegister(R11), ArgConstant("0xFFFFFFFFFFFFFFFF"));
   AsmLabel(d1);]
  @AsmCmp(ArgRegister(R11), ArgConstant("0xFFFFFFFFFFFFFFFF"))   (*compare the result with TRUE*)
  ::let f = fresh_name(l1) in AsmJne(f)   (*jump if false*)
  ::AsmJne(f)    (*jump if false*)
  ::let d = fresh_name(l2) in AsmJmp(d)       
  ::AsmLabel(f)
  (*CALL WITH ERROR CODE 2*)
  ::AsmMov(ArgRegister(RDI), ArgConstant("2"))
  ::AsmCall("stopWithError")
  ::AsmLabel(d)::
  AsmCmt("END ERROR CHECK")::[]
;;

let check_rax_tuple (l1: string) (l2: string) (l3: string): instruction list = 
  [AsmCmt("START ERROR CHECK");
  AsmMov(ArgRegister(R11),ArgRegister(RAX));
  AsmMov(ArgRegister(R15),ArgRegister(RAX))] @  (*move copy of RAX into R11 for checking later*)
  let l=fresh_name (l1) in 
  let d1 =fresh_name (l2) in
  let label_3 = fresh_name(l3) in
  [AsmShl(ArgRegister(R11), ArgConstant("61"));
   AsmMov(ArgRegister(R10), ArgConstant("0x2000000000000000"));
   AsmXor(ArgRegister(R11), ArgRegister(R10));
   AsmJz(l);
   AsmMov(ArgRegister(R11), ArgConstant("0x7FFFFFFFFFFFFFFF"));
   AsmJmp(d1);
   AsmLabel(l);

   AsmSub(ArgRegister(R15), ArgConstant("1"));
   AsmMov(ArgRegister(R12), ArgMemory(AddrByRegister(R15)));
   AsmShr(ArgRegister(R12), ArgConstant("63"));
   AsmMov(ArgRegister(R11), ArgConstant("0x0000000000000000"));
   AsmCmp(ArgRegister(R12), ArgRegister(R11));
   AsmJe(label_3);
   AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
   AsmJmp(d1);
   AsmLabel(label_3);

   AsmMov(ArgRegister(R11), ArgConstant("0xFFFFFFFFFFFFFFFF"));
   AsmLabel(d1);]
  @AsmCmp(ArgRegister(R11), ArgConstant("0xFFFFFFFFFFFFFFFF"))   (*compare the result with TRUE*)
  ::let f = fresh_name(l1) in AsmJne(f)   (*jump if false*)
   (*jump if false*)
  ::let d = fresh_name(l2) in AsmJmp(d)       
  ::AsmLabel(f)
  (*CALL WITH ERROR CODE 3*)
  ::AsmMov(ArgRegister(RDI), ArgConstant("3"))
  ::AsmCall("stopWithError")
  ::AsmLabel(d)::
  AsmCmt("END ERROR CHECK")::[]
;;
(*The two functions above should be used AFTER computing the two operands*)

(*gets register offset or 0*)
let stack_memory_of_argument (arg : argument) : int = 
  match arg with 
  |ArgConstant(_) -> 0
  |ArgRegister(_) -> 0
  |ArgLabelOffset(_, _) -> 0
  |ArgMemory(add) ->
    begin 
      match add with 
      |AddrByRegister(_) -> 0
      |AddrByRegisterOffset(_, offset) -> -offset
      |AddrByLabel(_) -> 0
      |AddrByRegisterProductOffset(_, _, _) -> 0
      |AddrByLabelOffset(_,_) -> 0 (*do we need more memory here???*)
    end 

;;

(*Gets largest offset from an instruction*)
let rec stack_memory_of_instruction (instr : instruction) : int = 
  match instr with 
  |AsmAdd (a, b)
  |AsmIMul (a, b)
  |AsmMov (a, b)
  |AsmSub (a, b)
  |AsmShl (a, b)
  |AsmShr (a, b)
  |AsmSar (a, b)
  |AsmAnd (a, b)
  |AsmOr(a,b)
  |AsmXor (a, b)
  |AsmCmp (a, b)
  |AsmSal (a, b) -> if stack_memory_of_argument(a) > stack_memory_of_argument(b) then 
      stack_memory_of_argument(a) else stack_memory_of_argument(b) 
  |AsmLabel (_)
  |AsmJmp (_)
  |AsmJe (_)
  |AsmJne (_)
  |AsmJl (_)
  |AsmJg (_)
  |AsmAlign(_)
  |AsmDq(_)
  |AsmSection(_)
  |AsmJz (_)
  |AsmCall (_) -> 0
  |AsmNot (a)
  |AsmPop (a)
  |AsmPush (a) -> stack_memory_of_argument(a)
  |AsmRet -> 0
  |AsmCmt(_) -> 0
  |AsmRep -> 0
;;

let rec calculate_stack_size (instr : instruction list) : int = 
  match instr with 
  | [] -> 0
  | a :: [] -> stack_memory_of_instruction(a)
  | a :: b :: c  -> if stack_memory_of_instruction(a) >= stack_memory_of_instruction(b) 
    then calculate_stack_size(a::c) else calculate_stack_size(b::c)

;;

(*compiles a function into assembly code (Handles steps 2~4 of the calling conventions, 
  including the code itself)
*)



(*creates a new starting environment that contains the parameters stored in registers*)
let get_params (params:string list): environment =
  let empty_env : environment = (16, Map.String.empty) in
  let rec get_parameters (env: environment) (params : string list): environment =
    match params with
    (*does the base case work now???*)
    |[] -> let _, dict = env in (-8, dict)
    |e1 :: [] -> let address, dict = env in (-8, Map.String.add e1 (ArgMemory(AddrByRegisterOffset(RBP, address))) dict)
    |e1::e2 -> 
      let address, dict = env in get_parameters (address+8, Map.String.add e1 (ArgMemory(AddrByRegisterOffset(RBP, address))) dict) e2
  in
  get_parameters empty_env params
;;

let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  match e with
  | EInt(a) -> 
    AsmMov(ArgRegister(RAX), ArgConstant(string_of_twice_int a)) ::[]

  | EUnaryOp(OpAfter, a) -> 
    compile_expression env a @ 
    check_rax_int "f" "d"@
    AsmAdd(ArgRegister(RAX), ArgConstant("2"))::[]

  | EUnaryOp(OpBefore, a) -> 
    compile_expression env a @ 
    check_rax_int "f1" "d1"@
    AsmSub(ArgRegister(RAX), ArgConstant("2"))::[]

  | EVar(a) -> 
    AsmMov(ArgRegister(RAX), find_named_variable a env)::[]

  | ELet(var, e1, e2) -> 
    compile_expression env e1
    @let env2  = (allocate_named_variable var env) in 
    [AsmMov(find_named_variable var env2, ArgRegister(RAX))]
    @compile_expression env2 e2

  | EBinaryOp(OpPlus, e1,e2) -> 
    compile_expression env e1@

    check_rax_int "f" "d"
    @let arg, env2 = (allocate_temp_variable env) in 

    [AsmMov(arg, ArgRegister(RAX))] @

    compile_expression env2 e2 @


    check_rax_int "f" "d" @
    AsmAdd(ArgRegister(RAX), arg)::[]

  | EBinaryOp(OpMinus, e1,e2) ->
    compile_expression env e1 @ 
    check_rax_int "f" "d" 
    @let arg, env2 = (allocate_temp_variable env) in 
    AsmMov(arg, ArgRegister(RAX))::[]
    @compile_expression env2 e2 @ 
    check_rax_int "f" "d"@ 
    [AsmSub(ArgRegister(RAX), arg);AsmIMul(ArgRegister(RAX), ArgConstant("-1"))]

  | EBinaryOp(OpTimes, e1,e2) -> 
    compile_expression env e1 @ 
    check_rax_int "f" "d" 
    @let arg, env2 = (allocate_temp_variable env) in 
    [AsmMov(arg, ArgRegister(RAX))]
    @compile_expression env2 e2 @ 
    check_rax_int "f" "d" @
    [AsmSar(ArgRegister(RAX), ArgConstant("1"));AsmIMul(ArgRegister(RAX), arg)]

  | EBool(true) -> 
    AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"))::[]

  | EBool(false) -> 
    AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"))::[]

  | EUnaryOp(OpIsBool, a) -> 
    compile_expression env a@ 
    let l=fresh_name ("chk_l") in 
    let d =fresh_name ("done_chk") in
    [AsmShl(ArgRegister(RAX), ArgConstant("62"));
     AsmMov(ArgRegister(R10), ArgConstant("0xC000000000000000"));
     AsmXor(ArgRegister(RAX), ArgRegister(R10));
     AsmJz(l);
     AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
     AsmJmp(d);
     AsmLabel(l);
     AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
     AsmLabel(d);]

  | EUnaryOp(OpIsInt, a) -> 
    compile_expression env a
    @[AsmNot(ArgRegister(RAX));
      AsmShl(ArgRegister(RAX), ArgConstant("63"));
      AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF"));
      AsmOr(ArgRegister(RAX), ArgRegister(R10))]

  | EUnaryOp(OpIsTuple, a) -> compile_expression env a @
    let l=fresh_name ("chk_l") in 
    let l2 = fresh_name("chk_l2") in
    let d =fresh_name ("done_chk") in
    [AsmMov(ArgRegister(R15), ArgRegister(RAX));
    AsmShl(ArgRegister(RAX), ArgConstant("61"));
    AsmMov(ArgRegister(R10), ArgConstant("0x2000000000000000"));
    AsmXor(ArgRegister(RAX), ArgRegister(R10));
    AsmJz(l);
    AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
    AsmJmp(d);
    AsmLabel(l);
    
    AsmSub(ArgRegister(R15), ArgConstant("1"));
    AsmMov(ArgRegister(R12), ArgMemory(AddrByRegister(R15)));
    AsmShr(ArgRegister(R12), ArgConstant("63"));
    AsmMov(ArgRegister(R11), ArgConstant("0x0000000000000000"));
    AsmCmp(ArgRegister(R12), ArgRegister(R11));
    AsmJe(l2);
    AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
    AsmJmp(d);

    AsmLabel(l2);
    AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
    AsmLabel(d);]

  | EBinaryOp(OpOr, e1, e2) -> 
    compile_expression env e1
    @ check_rax_bool "f" "d" 
    @let arg, env2 = (allocate_temp_variable env) in 
    AsmMov(arg, ArgRegister(RAX))
    ::[]@compile_expression env2 e2@ 
    check_rax_bool "f" "d" @
    AsmOr(ArgRegister(RAX), arg)::[]

  | EBinaryOp(OpAnd, e1, e2) -> 
    compile_expression env e1
    @ check_rax_bool "f" "d" 
    @let arg, env2 = (allocate_temp_variable env) in 
    AsmMov(arg, ArgRegister(RAX))
    ::[]@compile_expression env2 e2@ 
    check_rax_bool "f" "d" @
    AsmAnd(ArgRegister(RAX), arg)::[]

  | EBinaryOp(OpEqualTo, e1, e2) -> 
    compile_expression env e1
    @ check_rax_int "f" "d" 
    @let arg, env2 = (allocate_temp_variable env) in 
    AsmMov(arg, ArgRegister(RAX))::[]
    @compile_expression env2 e2@ 
    check_rax_int "f" "d" @[AsmCmp(ArgRegister(RAX), arg)]
    @let label = fresh_name("L") in 
    [AsmJe(label)]
    @let d = fresh_name("done") in 
    [AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
     AsmJmp(d);
     AsmLabel(label);
     AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
     AsmLabel(d)]

  | EBinaryOp(OpLessThan, e1, e2) -> 
    compile_expression env e1@ 
    check_rax_int "f" "d" 
    @ let arg, env2 = (allocate_temp_variable env) in 
    AsmMov(arg, ArgRegister(RAX))::[]
    @compile_expression env2 e2@ 
    check_rax_int "f" "d" 
    @let label = fresh_name("L") in  
    [AsmCmp(ArgRegister(RAX), arg);
     AsmJg(label)]
    @let d = fresh_name("done") in
    [AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
     AsmJmp(d);
     AsmLabel(label);
     AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
     AsmLabel(d)]

  | EBinaryOp(OpGreaterThan, e1, e2) -> 
    compile_expression env e1@ 
    check_rax_int "f" "d" 
    @let arg, env2 = (allocate_temp_variable env) in 
    AsmMov(arg, ArgRegister(RAX))::[]
    @compile_expression env2 e2@ 
    check_rax_int "f" "d" @ let label = fresh_name("L") in
    [AsmCmp(ArgRegister(RAX), arg);
     AsmJl(label)]
    @let d = fresh_name("done") in 
    [AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
     AsmJmp(d);
     AsmLabel(label);
     AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
     AsmLabel(d)]

 | EBinaryOp(OpTupleIndex, tuple, index) -> 
    compile_expression env tuple @ (*get pointer to tuple*)
    check_rax_tuple "chk_l" "done_chk" "chk_l2" @ (*check value*)
    let arg, env2 = (allocate_temp_variable env) in
    let error = fresh_name("error") in  (*defning labels*)
    let chk_done = fresh_name("chk_done") in

    [AsmMov(arg, ArgRegister(RAX))]@ (*store tuple in rbx*)
    compile_expression env2 index @ (*store index value in rax*)
    check_rax_int "c_l" "d_chk" @

    (*basically this is checking if index is out of range, first one checks if it's negative and i                                                                                                                                                                      got halfway
      through checking if it's less than length of the tuple. rcx is machine value of size of tuple, rax is the bird size 
        of the index. exit code is 4*)
    [AsmMov(ArgRegister(RBX), arg);
    AsmSub(ArgRegister(RBX), ArgConstant("1")); (*get address value for start of *)
    AsmMov(ArgRegister(RCX), ArgMemory(AddrByRegister(RBX))); 
    AsmCmp(ArgRegister(RAX), ArgConstant("0"));
    AsmJl(error); (*index value is less than zero*)

    AsmShr(ArgRegister(RAX), ArgConstant("1")); 
    AsmCmp(ArgRegister(RAX), ArgRegister(RCX)); (*check that not too big*)
    AsmJl(chk_done);
    (*error code 4*)
    AsmLabel(error);
    AsmMov(ArgRegister(RDI), ArgConstant("4"));
    AsmCall("stopWithError");
    AsmLabel(chk_done)] @

    [AsmAdd(ArgRegister(RAX), ArgConstant("1"));
    AsmMov(ArgRegister(RDX), ArgMemory ( (AddrByRegisterProductOffset ("rbx", "rax", "8")) ) );
    AsmMov(ArgRegister(RAX), ArgRegister(RDX))]

  | EIf(e1, e2, e3) ->
    compile_expression env e1@ 
    check_rax_bool "f" "d"
    @let label= fresh_name("L") in 
    [AsmMov(ArgRegister(R10), ArgConstant("0xFFFFFFFFFFFFFFFF"));
     AsmCmp(ArgRegister(RAX), ArgRegister(R10));
     AsmJne(label)] @
    compile_expression env e2
    @let d = fresh_name("done") in 
    [AsmJmp(d);
     AsmLabel(label)] @
    compile_expression env e3@
    AsmLabel(d)::[]

  | EUnaryOp(OpPrint, e) ->  
    compile_expression env e@ 
    [AsmMov(ArgRegister(RDI), ArgRegister(RAX)); 
     AsmPush(ArgRegister(RDI)); 
     AsmCall("printValue"); 
     AsmPop(ArgRegister(RDI)); 
     AsmMov(ArgRegister(RAX), ArgRegister(RDI))]
  
  (*Once we get here, we are GROWING already existing closures*)
  (*"apply" e1 to e2, where both expressions are both expressions that can be applied to one another*)
  | EAppl (e1, e2) -> 
    
    (*check that the first expression is a closure, assuming it is in R10*)
    let check_closure : instruction list =
      let label_true1 = fresh_name("true_pointer") in 
      let label_true2 = fresh_name("true_closure") in
      
      (*to check, first make sure it is a pointer, and then
        check what it points to*)
      [
      (*check if pointer*)
      AsmCmt("<<BEGIN EAPPL>>");
      AsmCmt("<<CHECK POINTER>>");
      AsmShl(ArgRegister(R10), ArgConstant("61"));
      AsmMov(ArgRegister(R11), ArgConstant("0x2000000000000000"));
      AsmXor(ArgRegister(R10), ArgRegister(R11));
      AsmJz(label_true1);
      (*false condition (not a pointer) *)
      AsmMov(ArgRegister(RDI), ArgConstant("5"));    
      AsmCall("stopWithError");
      AsmLabel(label_true1);

      (*follow pointer to first word of closure, and check the tag bit*)
      AsmCmt("<<CHECK CLOSURE>>");
      AsmSub(ArgRegister(RAX), ArgConstant("1"));
      (*AsmSub(ArgRegister(R10), ArgConstant("1"));*) (*a is the pointer to the closure*)
      AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(RAX))); (*follow pointer to first thing in closure*)
      
      AsmShr(ArgRegister(R10), ArgConstant("63")); (*check the tag bit*)
      AsmMov(ArgRegister(R11), ArgConstant("0x0000000000000001"));
      AsmCmp(ArgRegister(R10), ArgRegister(R11));
      AsmJe(label_true2);
      AsmMov(ArgRegister(RDI), ArgConstant("5")); 
      AsmCall("stopWithError");
      AsmLabel(label_true2);
      ] 
    in 
    (*compile left side and check that it is a closure*)
    compile_expression env e1 @ 

    [AsmMov(ArgRegister(RDX), ArgRegister(RAX));] @ (*store original pointer in a temp variable*)
    (*move into R10 to be checked*)
    [AsmMov(ArgRegister(R10), ArgRegister(RAX))] @  
    check_closure @ 
    let env2 = allocate_named_variable "closure" env in

    [AsmMov ((find_named_variable "closure" env2), ArgRegister(RDX))]@

    (*evaluate right expression*)
    compile_expression env2 e2 @ (*new argument stored in RAX*)

    [AsmMov(ArgRegister(RDX), find_named_variable "closure" env2)]@ (*bird pointer closure stored in RDX*)
     
    let label_break = fresh_name("break_loop") in
    let label_equal = fresh_name("equal") in 


    (*begin comparison*)
    [ AsmSub(ArgRegister(RDX), ArgConstant("1")); (*turn bird pointer into memory address*)
      AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(RDX)));  (*give R10 first thing in closure, POINTER*)

     AsmMov(ArgRegister(RBX), ArgConstant("0x8000000000000000"));
     AsmXor(ArgRegister(R10), ArgRegister(RBX)); (*get rid of tag bit, this is # of arguments*)
     AsmAdd(ArgRegister(R10), ArgConstant("1")); (*add one to arg (the argument we are about to add)*)
     AsmMov(ArgRegister(R11), ArgMemory(AddrByRegisterOffset(RDX, 8))); (*get expected # of parameters (the next thing), POINTER*)

     (*By this point, R10-># of current arguments (a+1), R11-># of parameters*)

     AsmCmp(ArgRegister(R10), ArgRegister(R11)); (*compare number of arguments with parameters*)
     AsmJe(label_equal);
     (*when a + 1 < p:*)
    
     (*set up rep movsq*)
     AsmMov(ArgRegister(RSI), ArgRegister(RDX)); (*source: the original closure*)
     AsmMov(ArgRegister(RDI), ArgMemory(AddrByLabel("heap_cursor"))); (*destination (new memory)*)
     AsmMov(ArgRegister(R13), ArgRegister(RDI)); (*save ptr to new closure VERY SMART*)
     AsmMov(ArgRegister(R12), ArgConstant("3")); (*length of zero closure*)
     AsmSub(ArgRegister(R10), ArgConstant("1")); (*b/c copying original, does not have new arg*)
     AsmAdd(ArgRegister(R12), ArgRegister(R10)); (*R12 has the length of the original closure (3+a)*)
     AsmMov(ArgRegister(RCX), ArgRegister(R12)); (*number of words*)
     AsmRep; (*rep movsq*)
     
     (*copy e2 to end of new closure*)
     (*AsmIMul(ArgRegister(R12), ArgConstant("8")); (*get amount of memory for all arguments, offset*)
    
     AsmMov(ArgRegister(RBX), ArgMemory(AddrByLabel("heap_cursor"))); (*storing, R13 also has this*) 
     AsmAdd(ArgRegister(RBX), ArgRegister(R12));*) (*increment heap cursor address*)
     AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister(RAX)); (*move new argument into memory, POINTER*) 
     
     AsmAdd(ArgRegister(RDI), ArgConstant("8")); (*move heap cursor to the very end*)

     AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(RDI)); (*exchange value, inc heap*)
     
     AsmCmt("<<INCREMENT ARGUMENTS BELOW>>");
     (*increment number of arguments*)

     
     AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(R13)));  (*pointer to closure put in R10*)
     (*AsmShr(ArgRegister(R10), ArgConstant("3"));*)  (*get machine address of closure (remove bird tag)*)
     AsmAdd(ArgRegister(R10), ArgConstant("1")); (*increment the value at that location*)
     AsmMov(ArgMemory(AddrByRegister(R13)), ArgRegister(R10)); (*replace the original value, treat R10 like memory*)

     (*undo shifting and produce result in RAX*)
     AsmAdd(ArgRegister(R13), ArgConstant("1"));
     AsmMov(ArgRegister(RAX), ArgRegister(R13)); (*return pointer to new closure*)
     AsmCmt("<<DONE UPDATING CLOSURE>>");

     AsmJmp(label_break); (*break from Appl*)
     
     (*number of params stored in r10,r11*)
     AsmLabel(label_equal); (*when we have enough arguments: GO TIME, RDX has pointer to closure*)
     
     AsmIMul(ArgRegister(R10), ArgConstant("8")); (*get memory need for all parameters R10 = number of parameters*)
     AsmMov(ArgRegister(R14), ArgRegister(R10)); 
     (*AsmMov(ArgRegister(R12), ArgRegister(RSP));
     AsmAdd(ArgRegister(R12), ArgConstant("8"));*)
     AsmSub(ArgRegister(RSP), ArgRegister(R10));(*make room on stack (move down)*)

     (*set up another rep movsq, copy all of arguments onto stack*)
     AsmMov(ArgRegister(RBX), ArgRegister(RDX));
     AsmAdd(ArgRegister(RBX), ArgConstant("24"));
     AsmMov(ArgRegister(RSI), ArgRegister(RBX)); (*beginning of the arguments*)
     AsmMov(ArgRegister(RDI), ArgRegister(RSP));(*destination*)
     AsmSub(ArgRegister(R11), ArgConstant("1"));
     AsmMov(ArgRegister(RCX), ArgRegister(R11)); (*copying all but the last parameter (which we do manually)*)
     AsmRep;

     AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister(RAX)); (*move final argument onto stack*)

     (*AsmMov(ArgRegister(R13), ArgRegister(RSP)); (*top of the stack*)

     AsmSub(ArgRegister(R13), ArgRegister(R14)); (*bottom of stack*)
     AsmAdd(ArgRegister(R13), ArgConstant("8")); (*one up from bottom of stack*)
     AsmMov(ArgMemory(AddrByRegister(R13)), ArgRegister(RAX)); (*move final argument onto stack*) *)
     
     AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(RDX, 16))); (**) 
     
     AsmCall("rax");

     AsmAdd(ArgRegister(RSP), ArgRegister(R14)); (*remove arguments from stack*)

    ] @ 

    (*go here after either growing the closure or calling the function (if we have enough arguments)*)
    [ AsmCmt("<<END EAPPL>>");
      AsmLabel(label_break)]


  (*the expression here is the arguments for the function*)
  (*| ECall(name, e) -> 
    (*save caller-saved regs: *)

    (*must do in opposite order*)
    (List.concat (List.map (fun x-> (compile_expression env x @ [AsmPush(ArgRegister(RAX))])) (e))) @

    [
      AsmCall("fn_"^name); (*call the function label*)

      (*remove arguments from stack, the PROPER NUMBER*)
      AsmAdd(ArgRegister(RSP), ArgConstant(string_of_int(8*List.length e)))

    ]
    *)
  | ETuple(e) -> 
      [AsmMov(ArgRegister(R13), ArgMemory(AddrByLabel("heap_cursor"))); (*store cursor in R10*)
      AsmMov(ArgRegister(R14), ArgRegister(R13)); (*save original address to store in rax at end*)

      (*AsmAdd(ArgRegister(R11), ArgConstant("1"));*) (*convert to pointer???*)

      AsmAdd(ArgRegister(R13), ArgConstant(string_of_int(8*((List.length e)+1)))); (*allocate space*)
      (*r10 now contains end of needed memory*)
      AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R13)); (*advance heap cursor*)
      AsmCmt("^^ HEAP CURSOR ADVANCED (BEGINNING POINTED TO BY R14) ^^")
      ]@


      (*first item is the size of the tuple*)
      [ 
        AsmMov(ArgRegister(R12), ArgConstant(string_of_int(List.length e)));
        (*when transferring directly to memory, use register as intermediary*)
        AsmMov(ArgMemory(AddrByRegister(R14)), ArgRegister(R12));
        (*AsmAdd(ArgRegister(R11), ArgConstant("8"));*)
        AsmCmt("^^ LENGTH OF TUPLE ^^")]  (*advance to next memory slot*)@

      
      (*fill in the rest of the tuple recursively*)

      begin
      let rec get_values (exprs : expr list) (env:environment): instruction list = 
      match exprs with 
      |[] -> []
      (*move each expression into the memory pointed to by the heap cursor*)
      |a::b -> [AsmPush(ArgRegister(R14))] @
                compile_expression env a @                 
                [AsmPop(ArgRegister(R14));
                  AsmMov(ArgMemory(AddrByRegisterOffset(R14, 8*(((List.length e)+1) - (List.length exprs)))), ArgRegister(RAX));
                (*AsmAdd(ArgRegister(R11), ArgConstant("8"))*)] @ 
                (get_values b env)    
      in
      (*recursive call gets and adds all of the items to the tuple*)
      (get_values e env) 
      end 

      (*convert return value to a pointer*)
      @ [
      AsmAdd(ArgRegister(R14), ArgConstant("1"));
      AsmMov(ArgRegister(RAX), ArgRegister(R14));
      ] 
  ;;
;;

(*Creates environment with closures included???*)
let rec create_env (decl:string list) (env1: environment) : environment =
  match decl with
  | [] -> env1 (*empty starting environment*)
  | d1::[] -> allocate_closure (d1) env1 
  | d1::d2 -> 
    let env2 = allocate_closure (d1) env1 in
    create_env d2 env2
  ;;

let compile_function_declaration (dec : declaration) (str_dec): instruction list= 
  match dec with 
  | DFunction(iden, params, e) -> 
    let params_env = get_params (params) in
    let env_final = create_env str_dec params_env in
    [AsmLabel("fn_"^iden);(*function identifier*)
     AsmPush(ArgRegister(RBP));
     AsmMov(ArgRegister(RBP), ArgRegister(RSP));
     AsmSub(ArgRegister(RSP), ArgConstant(string_of_int(calculate_stack_size
                                                          (compile_expression  (env_final)  e))))] @   (*setup function stack*)

    [AsmPush(ArgRegister(RBX)); (*push callee-saved*)
     AsmPush(ArgRegister(R12));
     AsmPush(ArgRegister(R13));
     AsmPush(ArgRegister(R14));
     AsmPush(ArgRegister(R15));
     ] @ 

     (*reversing here ensures that that they are evaluated in the right order*)
    (compile_expression (env_final) (e) ) @(*run function contents*)

    [(*pop callee-saved*)
      AsmPop(ArgRegister(R15));
      AsmPop(ArgRegister(R14));
      AsmPop(ArgRegister(R13));
      AsmPop(ArgRegister(R12));
      AsmPop(ArgRegister(RBX)); 

      (*tear down stack frame & ret*)
      AsmMov(ArgRegister(RSP), ArgRegister(RBP));
      AsmPop(ArgRegister(RBP));
      AsmRet
    ]

;;

let compile_program (p : program) : instruction list = 
  (*check for errors in the program*)
  check_well_formed p;
  match p with 
  | Program(decl,expr) -> 
    (* let rec create_env (decl:declaration list) (env1: environment) : environment =
      match decl with
      | [] -> []
      | DFunction(iden, _, _)::d2 -> 
        let env2 = allocate_named_variable ("fn_"^ iden) env1 in
        create_env d2 env2
    in*)
    let decl_strings = List.map(fun x-> match x with |DFunction(iden, _, _)->iden) decl in
    let starting_env = create_env decl_strings empty_environment in 
    let rec compile_decs (decl: declaration list) (env: environment) : instruction list=
      match decl with
      | [] -> []
      | dec::d2 -> 
          compile_function_declaration dec decl_strings@
          compile_decs d2 env
    in 
    let declaration = compile_decs decl starting_env in  (*The user-defined functions*)
    let instructions = compile_expression starting_env expr in (*The expressions that make up the program*)
    [ 
      AsmPush(ArgRegister(RBP));
      AsmMov(ArgRegister(RBP), ArgRegister(RSP));
      AsmSub(ArgRegister(RSP), ArgConstant(string_of_int (calculate_stack_size instructions)));
      AsmPush(ArgRegister(RBX)); (*push callee-saved*)
      AsmPush(ArgRegister(R12));
      AsmPush(ArgRegister(R13));
      AsmPush(ArgRegister(R14));
      AsmPush(ArgRegister(R15));
      (*get malloc ptr from c driver file*)
      AsmMov(ArgRegister(R14), ArgRegister(RDI));
      AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R14))

    ] @ 
    instructions @ 
    [
      AsmPop(ArgRegister(R15));
      AsmPop(ArgRegister(R14));
      AsmPop(ArgRegister(R13));
      AsmPop(ArgRegister(R12));
      AsmPop(ArgRegister(RBX)); 
      AsmMov(ArgRegister(RSP), ArgRegister(RBP));
      AsmPop(ArgRegister(RBP));
      AsmRet] @
    declaration (*should the function call be at the very end?*)

;;

let compile_to_assembly_code (p : program): string =

  (*generates text for the remaining declarations in the list*)
  let rec make_text (d : declaration list) : string = 
    match d with 
    |[] -> ""
    |DFunction(name, args, _)::b -> 
      "closure_of_" ^ "fn_" ^ name ^ ":\n" ^
    "dq 0x8000000000000000, " ^ (*same for every closure*)
    string_of_int(List.length args) ^ ", " ^  (*expected number of arguments*)
    "fn_" ^name ^ "\n" 
    ^ make_text b
    in

  let instructions = compile_program p in
  let instruction_code = code_of_instruction_list instructions in
  "extern stopWithError\n" ^
  "extern printValue\n" ^
  "section .data\n" ^
  "align 8\n" ^
  "heap_cursor:\n" ^
  "dq 0\n" ^
  (*zero closures for functions*)
  let add_closures (prog : program) : string= 
  
    match prog with 
  |Program(dec, e) -> 
    ignore e;
  match dec with 
  |[] -> ""
  |DFunction(name, args, _)::b -> 
    "closure_of_" ^ "fn_" ^ name ^  ":\n" ^
    "dq 0x8000000000000000, " ^ (*same for every closure*)
    string_of_int(List.length args) ^ ", " ^  (*expected number of arguments*)
    "fn_" ^name ^ "\n" 
    ^ make_text b


  in 
    add_closures p ^ (*recurse for rest of functions*)


  "section .text\n" ^
  "global bird_main\n" ^
  "bird_main:\n" ^
  instruction_code ^
  "\n"
;;

