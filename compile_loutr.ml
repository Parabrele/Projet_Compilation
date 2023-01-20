
let space_s = alloc_string " "




and expr env e = match e.expr_desc with
  | TEskip -> nop
  | TEconstant (Cbool true) -> movq (imm 1) (reg rax)
  | TEconstant (Cbool false) -> movq (imm 0) (reg rax)
  | TEconstant (Cint x) -> movq (imm64 x) (reg rax)
  | TEnil -> xorq (reg rax) (reg rax)
  | TEconstant (Cstring s) ->
      let label = alloc_string s in
      movq (ilab label) (reg rax)

  | TEbinop (Band | Bor as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++ expr env e2 ++
      (if op = Band then andq else orq)
        (ind rsp) (reg rax) ++
      popq rcx

  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++ expr env e2 ++
      cmpq (reg rax) (ind rsp) ++
      (function
        | Blt -> setl | Ble -> setle
        | Bgt -> setg | _ -> setge) op
        (reg al) ++ movzbq (reg al) rax ++
      popq rcx
      
  | TEbinop (Badd | Bsub | Bmul as op, e1, e2) ->
      expr env e2 ++ pushq (reg rax) ++ expr env e1 ++ 
      (function
        | Badd -> addq | Bsub -> subq
        | _ -> imulq) op (ind rsp) (reg rax) ++
      popq rcx

  | TEbinop (Bdiv | Bmod as op, e1, e2) ->
      expr env e2 ++ pushq (reg rax) ++ expr env e1 ++
      cqto ++ idivq (ind rsp) ++
      movq (reg (if Bdiv = op then rax else rdx)) (reg rax) ++
      popq rcx

  | TEbinop (Beq | Bne as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++ movq (reg rax) (reg rdi) ++
      popq rsi ++
      compare e1.expr_typ ++
      (if op = Beq then nop else notq (reg rax))

  | TEunop (Uneg, e1) -> expr env e1 ++ negq (reg rax)
  | TEunop (Unot, e1) -> expr env e1 ++ notq (reg rax) ++ andq (imm 1) (reg rax)

  | TEunop (Uamp, e1) -> expr_address env e1 

  | TEunop (Ustar, e1) ->
      expr env e1 ++ (match e1.expr_typ with
        | Tptr (Tstruct _) -> nop
        | _ -> movq (ind rax) (reg rax)
      )

  | TEprint el -> add_print_capability Tstring; expr_print false env el
      
  | TEident x -> movq (ind ~ofs:x.v_addr rbp) (reg rax)

  | TEassign (lvl, el) -> expr_stack env el ++ List.fold_left (fun d lv -> d ++ (match lv.expr_typ with
        | Tstruct s -> expr_address env lv ++ popq rsi ++ movq (reg rax) (reg rdi) ++
            movq (imm s.s_size) (reg rdx) ++ callc "memcpy"
        | Twild -> popq rsi
        | _ -> expr_address env lv ++ popq rsi ++ movq (reg rsi) (ind rax)
      )) nop lvl
      (* NOTE what about using free to remove useless temporary structures ? *)
      
  | TEblock el -> let env' = copy env and nb_glob = env.nb_locals in
      let t1 = List.fold_left (++) nop (List.map (expr env') el) in
      t1 ++ (* haha! ++ evaluation order is not left-to-right! *)
      (if env'.nb_locals > nb_glob
        then addq (imm ((env'.nb_locals - nb_glob) * 8)) (reg rsp)
        else nop)

  | TEif (b, e1, e2) -> let l_end = new_label () and l_false = new_label () in
      expr env b ++ cmpq (imm 0) (reg rax) ++ je l_false ++
      expr env e1 ++ jmp l_end ++
      label l_false ++ expr env e2 ++ label l_end

  | TEfor (b, e) -> let l_cond = new_label () and l_begin = new_label () in
      jmp l_cond ++ label l_begin ++ expr env e ++
      label l_cond ++ expr env b ++ cmpq (imm 1) (reg rax) ++ je l_begin

  | TEnew ty -> malloc (sizeof ty) 

  | TEcall (f, el) -> ev_fun env (f, el) ++ begin match (List.length f.fn_typ) with
        | 0 -> nop
        | 1 -> popq rax
        | _ -> raise (Anomaly "trying to evaluate invalid function within an expression")
      end

  | TEdot _ -> expr_address env e ++ (match e.expr_typ with
        | Tstruct _ -> nop
        | _ -> movq (ind rax) (reg rax)
      )

  | TEvars (varlist, initlist) ->
      List.iter (fun var -> addv env; var.v_addr <- -8 * env.nb_locals) (List.rev varlist);
      expr_stack env initlist

  | TEreturn el -> 
      (* NOTE this is incorrect when the evaluations have side-effects, because the elements
      are evaluated in reverse order, which does not match the semantics *)
      expr_stack env el ++ (List.fold_left (++) nop (List.mapi (fun i _ ->
        popq rax ++ movq (reg rax) (ind ~ofs:(env.ofs_this + 8 * i) rbp)) el)) ++ leave ++ ret

  | TEincdec (e1, op) -> expr_address env e1 ++
      (if op = Inc then incq else decq) (ind rax)
    (* NOTE optimisations are possible when e1 is more specific, like TEident.
       In that case, a command like incq -16(%rbp). This has not been done here
       and is beyond the scope of this project. *)


(* evaluates a list of expressions and put the results on the stack in reverse order of appearance.
   This function is used within TEassign, TEvars and function calls *)
and expr_stack env = function
  | [] -> nop
  | { expr_desc = TEcall (f, arglist) } :: el' -> 
      expr_stack env el' ++ evaluating env (f, arglist)
  | e :: el' -> expr_stack env el' ++ expr env e ++ (match e.expr_typ with
      | Tstruct s -> (* structures need to be copied when evaluated, as to allow x, y = y, x *)
          movq (reg rax) (reg r10) ++ malloc (s.s_size) ++ 
          movq (reg rax) (reg rdi) ++ movq (reg r10) (reg rsi) ++
          movq (imm s.s_size) (reg rdx) ++ callc "memcpy"
      | _ -> nop) ++ pushq (reg rax)

(* calls a function and leave the results on the stack *)
and evaluating env (f, arglist) =
  subq (imm (8 * (List.length f.fn_typ))) (reg rsp) ++
  expr_stack env arglist ++ call ("F_" ^ f.fn_name) ++
  (* function arguments must be removed from the stack once the call is done *)
  (if arglist = [] then nop else addq (imm (8 * (List.length arglist))) (reg rsp))




let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* why 2? both return address and previous %rbp value are on the stack *)
  let env = empty_env (8 * (2 + (List.length f.fn_params))) in
  label ("F_" ^ f.fn_name) ++ pushq (reg rbp) ++ movq (reg rsp) (reg rbp) ++
  expr env e ++ (if f.fn_typ = [] then leave ++ ret else nop)

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b=false) dl =
  debug := b;
  let funs = List.fold_left decl nop dl in
  let print_funs = Hashtbl.fold (fun l s d -> label l ++ s ++ d) print_functions nop in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++ print_funs
    ;
    data = (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }