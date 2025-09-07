(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;
open Freshening;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | RBP
  | R10
  | RDI
  | RSI
  | RBX
  | RDX
  | RCX
  | R8
  | R9
  | R11
  | R12
  | R13
  | R14
  | R15
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  (*Eagle addition*)
  | AddrByLabel of string (*for heap cursor*)
  | AddrByRegisterProductOffset of string * string * string (*for tuple indexing in Eagle*)
  | AddrByLabelOffset of string * string (*for getting closures in Falcon*)
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * string
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmRet
  (*Bluebird additions*)
  | AsmShl of argument * argument 
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmNot of argument
  | AsmJl of string
  | AsmJg of string
  (*Cardinal Additions*)
  |AsmPush of argument
  |AsmPop of argument
  |AsmCall of string
  |AsmCmt of string
  (*Eagle additions*)
  |AsmSection of string 
  |AsmAlign of string
  |AsmDq of string (*string list?*)
  |AsmJz of string 
  |AsmRep

;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =

  match register with 
  | RAX -> "rax"
  | RSP -> "rsp"
  | R10 -> "r10"
  | RBP -> "rbp"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RBX -> "rbx"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | R8 -> "r8"
  | R9 -> "r9"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14" 
  | R15 -> "r15"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with 
  |AddrByRegister(reg) ->  "[" ^code_of_register(reg) ^"]"
  |AddrByRegisterOffset(reg, off) -> 
    if off > 0 then 
      "[" ^ code_of_register(reg) ^ "+" ^string_of_int(off) ^ "]"
  else if off = 0 then
    "[" ^ code_of_register(reg) ^ "]"
  else
    "[" ^ code_of_register(reg) ^ string_of_int(off) ^ "]"
  |AddrByLabel(str) -> 
    "[" ^ str ^ "]"
  |AddrByRegisterProductOffset(reg1, reg2, product) -> 
    "[" ^ reg1 ^ " + " ^ reg2 ^ " * " ^ product ^ "]" 
  |AddrByLabelOffset(str1, str2) -> 
   str1 ^ " + " ^ str2 

;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with 

  |ArgConstant(str) -> str

  |ArgRegister(reg) -> code_of_register(reg) 

  |ArgMemory(add) -> code_of_address(add)

  |ArgLabelOffset(label, offset) -> "closure_of_fn" ^ label ^ " + " ^offset
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
  *)
let code_of_instruction (instruction : instruction) : string =
  
  match instruction with 

  |AsmAdd(arg1,arg2) -> 
    "add " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)
    
  |AsmIMul(arg1,arg2) -> 
    "imul " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmMov(arg1,arg2) -> 
    "mov " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmSub(arg1,arg2) -> 

    "sub " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmRet -> 
    "\nret"
  |AsmShl(arg1,arg2) -> 
    "shl " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmShr(arg1,arg2) -> 
    "shr " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmSal(arg1,arg2) -> 
    "sal " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmSar(arg1,arg2) -> 
    "sar " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmAnd(arg1,arg2) -> 
    "and " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmOr(arg1,arg2) -> 
    "or " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmXor(arg1,arg2) -> 
    "xor " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmLabel(str) ->
    (str) ^ ":"

  |AsmCmp(arg1,arg2) -> 
    "cmp " ^ code_of_argument(arg1) ^ ", " ^ code_of_argument(arg2)

  |AsmJmp(str) -> 
    "jmp " ^ str

  |AsmJe(str) -> 
    "je " ^ str
  
  |AsmCmt(str) -> 
      ";" ^ str
  
    |AsmSection(str) -> 
      "section ." ^ str
  
    |AsmAlign(str)-> 
    "align " ^ str
  
    |AsmDq(str) -> 
    "je " ^ str

  | AsmJne(str) -> 
    "jne " ^ str

  | AsmNot(arg) -> 
    "not " ^ code_of_argument arg
  | AsmJl(str) -> 
    "jl " ^ str

  | AsmJg(str) -> 
    "jg " ^ str

  |AsmPush(arg) -> 
    "push " ^ code_of_argument arg
  
  |AsmPop(arg) -> 
    "pop " ^ code_of_argument arg

  |AsmCall(str) -> 
    "call " ^ str


  (**|AsmSection(str) -> 
    "section ." ^ str

  |AsmAlign(str)-> 
  "align " ^ str

  |AsmDq(str) -> 
    "dq " ^ str*)

  |AsmJz(lbl) ->
    "jz " ^ lbl
   
  |AsmRep -> 
    "rep movsq"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with 
  |[] -> ""
  |instr :: rest -> 
  code_of_instruction(instr) ^ "\n" ^code_of_instruction_list(rest)
;;
