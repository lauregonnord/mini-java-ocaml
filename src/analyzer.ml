open Simple_java_syntax
open Format

(*** DEMO FILE: Lex and Parse the file, translate into mini_java,
     pretty print ***)


(****** Command line + Software configuration *****)
type config_t = {
  f_name:string;
  debug:bool;
  version:string;
}

exception Version
exception Usage of string
exception NotFound of string
			
		  
let set_debug config () = config := {!config with debug=true}
let set_fname config (s:string) =  
  if Sys.file_exists s then config := {!config with f_name=s}	else
    raise (NotFound s) 					   
let make_default_config () = {f_name="";debug=true;version="0.1";}
			       
let print_config cf =
  Printf.printf "inputfile=%s\n" cf.f_name 
			       
let read_args () =
  let cf = ref (make_default_config()) in
  let speclist = 
    [
      ("--version",Arg.Unit (fun () -> fprintf std_formatter "Analyzer Version %s@." !cf.version ; raise(Version)),": print version and exit");
      ("-debug", Arg.Unit (set_debug cf) ,": all debug info");
    ] in
  let usage_msg = "Usage : ./analyser [options] file \n " in
  try (Arg.parse speclist (set_fname cf) usage_msg; 
       if !cf.f_name = "" then begin Arg.usage speclist usage_msg ; raise (Usage usage_msg) end; 
       !cf 
      )
  with
  | Version -> exit(1);;
 

(****** Parsing -> IR *****)
       
let parse_and_translate config =
  if String.compare config.f_name "" = 0 then failwith "no program file given";
    Localizing.current_file_name := config.f_name;
    let f_desc = open_in config.f_name in
    let lexbuf = Lexing.from_channel f_desc in
    let java_prog =
      try Java_parser.program Java_lexer.token lexbuf
      with
      | e ->
         Printf.printf "Exception during parsing: %s\n"
		       (Printexc.to_string e);
         failwith "Stopped" in
    Simple_java_translate.tr_java_prog java_prog 


(****** MAIN *****)
				       
let _  =
    (* Parsing arguments *)
  let cf = read_args() in 
  
  (* Parsing of the source file *)
  let simple_java_prog,env = parse_and_translate cf  in
  print_config cf;
  (*Pretty print*)
  Simple_java_syntax.pp_s_program simple_java_prog;
  Printf.printf "finished...\n"
		

