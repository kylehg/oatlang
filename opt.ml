(* These flags are set by the driver to indicate which optimizations
 * should be run.  The flag for opt_asm must live in occ.ml. *)
let opt_ast_flag = ref false 
let opt_il_flag = ref false


(* Choose one of the following functions to implement.  You
 * optimization pass should strive to improve the performance 
 * of the generated code. *)

let opt_ast (prog:Range.t Ast.prog) : (Range.t Ast.prog) = 
  (* currently just the identity transformation *)
  prog

let opt_il (prog:Ll.prog) : (Ll.prog) = 
  (* currently just the identity transformation *)
  prog

let opt_asm (prog:Cunit.cunit) : (Cunit.cunit) = 
  (* currently just the identity transformation *)
  prog
