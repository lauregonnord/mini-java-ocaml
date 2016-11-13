(* This file defines the Abstract Syntax Tree for "Simplified Java", *)
(* a minimalist subset of Java.                                      *)
(* We define only data-types in this file; functions modifying       *)
(* these will appear in separate files.                              *)

(* This module allows to track positions in the source files.   *)
open Localizing

(* Keys (to associate to variables). *)
type s_uniqueId = int

(* Data-types: we only support int, bool, void, and intarray *)
type s_type =
  | St_int
  | St_bool
  | St_void
  | St_intarray of int 

let type2string = function
  | St_int -> "int"
  | St_bool -> "bool"
  | St_void  -> "void"
  | St_intarray(n) -> "(int)array (size="^(string_of_int n)^")"
      
(* Constants. *)
type s_constant =
  | Sc_int of int64
  | Sc_bool of bool

let constant2string = function
  | Sc_int(v64) -> Printf.sprintf "%Ld" v64 
  | Sc_bool(b) -> if b then "true" else "false"
		 
(* Operators. *)
(* Unary operators. *)
type s_unary_op =
  | Su_neg

let uop2string = function
 | Su_neg -> "-"
    
(* Binary operators. *)
type s_binary_op =
  | Sb_add | Sb_sub | Sb_mul | Sb_div
  | Sb_or
  | Sb_lt
      
let bop2string = function
  | Sb_add -> "+"
  | Sb_sub -> "-"
  | Sb_mul -> "*"
  | Sb_div -> "/"
  | Sb_or -> "||"
  | Sb_lt   -> "<"

(* A variable *)
type s_var =
    { s_var_name:     string;     (* name *)
      s_var_extent:   extent;     (* position in the source file *)
      s_var_type:     s_type;     (* type *)
      mutable s_var_arraysize: int;  (* size = ct for the moment, 0 if non array*)
      s_var_uniqueId: s_uniqueId; (* key *) }

      
 (* Variable declaration: variable + initializer, if any *)
 and s_var_decl = s_var * (s_expr_e list) option

(* A procedure, with no parameter:
 *  - it should not return a value
 *  - it should not take any parameter *)
and s_proc =
    { s_proc_name:   string;     (* name *)
      s_proc_body:   s_block;    (* code of the function *) }

(* A function call *)
and  s_proc_call =
    { s_proc_call_class:  string;        (* class of the function called *)
      s_proc_call_name:   string;        (* its name *) }
      
(* Arithmetic and boolean expressions + intarrays *)
and s_expr =
  | Se_const of s_constant
  | Se_random of int64 * int64
  | Se_var of s_var
  | Se_unary of s_unary_op * s_expr_e
  | Se_binary of s_binary_op * s_expr_e * s_expr_e
  | Se_arrayaccess of s_var * s_var (* a[var] only*)
					    
(* Expressions annotated with a position in the source file *)
and s_expr_e = s_expr * extent
			  
(* Instructions (assignment, if, while, etc... *)
and s_command =
  | Sc_assign of s_var * s_expr_e
  | Sc_if of s_expr_e * s_block * s_block
  | Sc_while of s_expr_e * s_block
  | Sc_proc_call of s_proc_call
  | Sc_assert of s_expr_e

(* Instructions annotated with a position in the source file. *)
and s_command_e = s_command * extent

(* A block is a sequence of instructions. *)
and s_block = s_command_e list

(* Declaration of a class member: could be a global variable or a function. *)
and s_declaration =
  | Sd_var of s_var_decl
  | Sd_function of s_proc

(* A class = a name + a sequence of declarations *)
type s_class =
    { s_class_name: string;
      s_class_body: s_declaration list; }

(* A program = a list of classes *)
type s_program = s_class list



(*warning only print expression, not location info*)
let rec sexpr2string se = match se with
  | Se_const(sct) -> constant2string sct
  | Se_random(lb,ub) -> Printf.sprintf "rand[%Ld,%Ld]" lb ub
  | Se_var(svar) -> svar.s_var_name
  | Se_unary(uop,se1) -> (uop2string uop)^"("^(sexpr2string (fst se1))^")"
  | Se_binary(bop,se1,se2) -> "("^(sexpr2string (fst se1))^")"^(bop2string bop)^"("^(sexpr2string (fst se2))^")"
  | Se_arrayaccess(svar,i) -> svar.s_var_name^"["^i.s_var_name^"]"
			 
let pp_var_decl avar =
  let vname,other = avar in
  Printf.printf "var decl : name(+id) =%s(%s), type=%s, pos=%s" vname.s_var_name
		(string_of_int vname.s_var_uniqueId)
		(type2string vname.s_var_type)
		(Localizing.extent_to_string vname.s_var_extent);
  match other with (*initialization*)
  | None -> Printf.printf "\n"
  | Some(sel) ->
     begin
       Printf.printf ", initial value(s) = ";
       List.iter (fun se -> Printf.printf "%s," (sexpr2string (fst se))) sel;
       Printf.printf "\n";
     end
    
(*pp a block , remember a proc body is a block*)
let rec pp_block funb (printpos:bool) =  (*iter on block list = list of scommands*)
  List.iter (fun sc -> pp_s_command_e sc printpos) funb
  and
    pp_s_command_e sce (printpos:bool) =
    let stmt,ext=sce in
    if printpos then Printf.printf("Smt @ pos=%s") (Localizing.extent_to_string ext);
    match stmt with 
    | Sc_assign(v,e) -> Printf.printf ("(assign) %s := %s\n") v.s_var_name (sexpr2string (fst e))
    | Sc_if(e,b1,b2) -> Printf.printf ("(test) if(%s) then \n  ")  (sexpr2string (fst e));
			pp_block b1 printpos;
			Printf.printf ("else ");
			pp_block b2 printpos;
			Printf.printf ("endif\n ");
    | Sc_while(e,b) -> Printf.printf ("(while) while(%s) do \n  ")  (sexpr2string (fst e));
		       pp_block b printpos;
		       Printf.printf (" done \n")
    | Sc_proc_call(call)-> Printf.printf ("(call) %s \n")  "calltodo"
    | Sc_assert(e) -> Printf.printf ("(assert) %s \n")  (sexpr2string (fst e))
				    
  
let pp_proc afun =
  Printf.printf "fun decl : name =%s\n" afun.s_proc_name;
  pp_block afun.s_proc_body false;
  Printf.printf "\n"
				 
let pp_s_decl  = function
  | Sd_var(avar) -> pp_var_decl avar
  | Sd_function(afun) -> pp_proc afun
			 
let pp_s_program (prog:s_program) =
  Printf.printf "--------------------- Pretty Print -----------------------\n";
  List.iter (
      fun  aclass -> (*print a given class*)
      Printf.printf "****Class %s****\n" aclass.s_class_name;
      List.iter pp_s_decl aclass.s_class_body
    ) prog
