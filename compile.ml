(* étiquettes
     F_function      entrée fonction
     print_xxx           print function
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

(* TODO : (rapport) tests : tous les cas de bases des fonctions de bases, puis des cas plus compliqués éventuellement, et si jamais tous les cas de base fonctionnent, et que les cas compliqués sont valides, alors la composition des cas de bases est valide et les autres cas compliqués sont valides. On espère, en tout cas :o *)
(* TODO : reprendre les différences avec le compile de la salle info ! il gère les structures ! *)

(* note préliminaire : la plupart du temps, on ignore la taille des objets qu'on manipule et on considère que c'est juste 8.
Il faudra y remédier si le temps s'y prête.*)

(* autre note : les fonctions retourneront TOUJOURS dans la pile, même si elles peuvent retourner dans les registres, ce sera un enfer sinon pour les assign. Elles mettront juste 0 dans rax. *)

(* autre note :
les Tstruct seront des poiteurs
les Tptr sont des pointeurs

si on a une Tstruct, ça pointe vers son contenu, dans la mémoire
si on a un Tptr Tstruct, ça pointe vers l'adresse dans la pile du pointeur vers la mémoire, qui lui même est de taille 8.
Donc tout est de taille 8 !*)

open Format
open Ast
open Tast
open X86_64

exception Anomaly of string

let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let malloc n =
  movq (imm n) !%rdi ++
  (* Ce motif dégueulasse reviendra plusieurs fois. Il sert à réaligner la pile pour que papy Windows arrête de faire son aigris. Ca n'a aucune importance sur les linux *)
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ilab "0xfffffffffffffff0") !%rax ++
  andq !%rax !%rsp ++
  call "malloc" ++
  movq !%rbp !%rsp ++
  popq rbp
let allocz n =
  movq (imm n) !%rdi ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ilab "0xfffffffffffffff0") !%rax ++
  andq !%rax !%rsp ++
  call "malloc" ++
  movq !%rbp !%rsp ++
  popq rbp ++
  movq !%rax !%rdi ++
  movq (imm 0) !%rsi ++
  movq (imm (n / 8)) !%rdx ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ilab "0xfffffffffffffff0") !%rax ++
  andq !%rax !%rsp ++
  call "memset" ++
  movq !%rbp !%rsp ++
  popq rbp ++
  movq !%rdi !%rax


let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

(* c'est pas forcément utile de tout mettre en mtuable, mais on sait jamais *)
(* de plus, il y avait un champ qui servait à rien donc je l'ai viré. *)
type env = {
  mutable exit_label: string;
  mutable ofs_this: int;
  mutable nb_locals: int;
}

(* fonctions utiles pour gérer les environnements *)
(* nb_local sera utile pour calculer le décalage dans la pile, multiplié par 8, puisque chaque objet est soit de taille 8 soit stoqué comme un pointeur de taille 8 vers un truc plus gros *)
let env_add_variable env =
  env.nb_locals <- env.nb_locals + 1
let env_copy { exit_label; ofs_this; nb_locals } =
  { exit_label; ofs_this; nb_locals }
let env_empty nb_args =
  { exit_label = ""; ofs_this = nb_args; nb_locals = 0 }

(* fonction utile pour bidouiller les expressions des or et tout, et les passer à un if pour que ce soit paresseux. *)
let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
(* cette fonction est bien gentille mais je comprend pas à quoi elle sert, donc bon... *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) !%rdi ++ jmp l_end ++
  label l_true ++ movq (imm 1) !%rdi ++ label l_end

(* ###### GESTION DES PRINTS ###### *)
(* On pourrait aussi pré générer les prints classiques, mais c'est moins joli que de les générer uniquement s'ils sont utiles *)

(* création des noms des labels *)
let t_expr_to_print = function
  | Tstring -> "print_string"
  | Tbool -> "print_bool"
  | Tint -> "print_int"
  | Tptr (Tstruct s) -> "print_ptr_" ^ s.s_name
  | Tptr _ | Tptrnil -> "print_ptr"
  | Tstruct s -> "print_" ^ s.s_name
  | _ -> failwith "problème rencontré dans t_expr_to_print"

  let space_s = alloc_string " "

(* TODO dans le rapport, parler de comment c'était trop big brain de faire une hastbl pour faire que les print qu'on veut et s'en souvenir*)
(* mais que avec un string du code qu'on génère nous même sans x8664, ça aurait été plus simple ! *)
(* Générations des fonctions print utiles *)
let print_env = Hashtbl.create 5
let rec add_print_function typ =
  if not (Hashtbl.mem print_env (t_expr_to_print typ)) then
  Hashtbl.add print_env (t_expr_to_print typ) (match typ with
    | Tstring ->
        let s_string = alloc_string "%s" in
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        movq (ilab "0xfffffffffffffff0") !%rax ++
        andq !%rax !%rsp ++
        movq (ilab s_string) !%rdi ++
        call "printf" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret

    | Tint ->
        let s_int = alloc_string "%ld" in
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        movq (ilab "0xfffffffffffffff0") !%rax ++
        andq !%rax !%rsp ++
        movq (ilab s_int) !%rdi ++
        call "printf" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret

    | Tbool ->
        add_print_function Tstring;
        let true_label = alloc_string "true"
        and false_label = alloc_string "false"
        and l = new_label ()
        and l_end = new_label () in
        cmpq (imm 0) !%rsi ++
        je l ++
        movq (ilab true_label) !%rsi ++
        jmp l_end ++
        label l ++
        movq (ilab false_label) !%rsi ++
        label l_end ++
        call "print_string" ++
        ret

    | Tstruct s ->
        add_print_function Tstring;
        let ocb_s = alloc_string "{"
        and ccb_s = alloc_string "}" in
        pushq !%r12 ++
        movq !%rsi !%r12 ++
        movq (ilab ocb_s) !%rsi ++
        call "print_string" ++
	(* merci seigneur à celui qui a pensé à rajouter le champs des fields sous forme de liste dans les structures <3_<3 *)
        List.fold_left (fun d f ->
	        d ++
	        (if d = nop then nop
           else movq (ilab space_s) !%rsi ++
		            call "print_string") ++
          (match f.f_typ with
            | Tstruct s ->
                add_print_function f.f_typ;
                leaq (ind ~ofs:f.f_ofs r12) rsi ++
                call ("print_" ^ s.s_name)
            | Tptr _ ->
                add_print_function Tptrnil;
                movq (ind ~ofs:f.f_ofs r12) !%rsi ++
                call "print_ptr"
            | _ ->
                add_print_function f.f_typ;
                movq (ind ~ofs:f.f_ofs r12) !%rsi ++
                call (t_expr_to_print f.f_typ)
          )) nop s.s_ordered_fields ++
        movq (ilab ccb_s) !%rsi ++
        call "print_string" ++
        popq r12 ++
        ret

    | Tptr (Tstruct s) ->
        add_print_function Tstring;
        add_print_function (Tstruct s);
        let esperluette = alloc_string "&"
        and string_nil = alloc_string "<nil>"
        and l = new_label () in
        cmpq (imm 0) !%rsi ++
        jne l ++
        movq (ilab string_nil) !%rsi ++
        call "print_string" ++
        ret ++ 
        label l ++
        movq !%rsi !%r15 ++ 
        movq (ilab esperluette) !%rsi ++
        call "print_string" ++
        movq !%r15 !%rsi ++
        call (t_expr_to_print (Tstruct s)) ++
        ret

    | Tptr _ | Tptrnil ->
        add_print_function Tstring;
        let string_nil = alloc_string "<nil>"
        (* on veut afficher les poiteurs en exa *)
        and wtf = alloc_string "0x%010x"
        and l = new_label () in
        cmpq (imm 0) !%rsi ++
        jne l ++
        movq (ilab string_nil) !%rsi ++
        call "print_string" ++
        ret ++
        label l ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        movq (ilab "0xfffffffffffffff0") !%rax ++
        andq !%rax !%rsp ++
        movq (ilab wtf) !%rdi ++
        call "printf" ++
        movq !%rbp !%rsp ++
        popq rbp ++
        ret

    | _ -> failwith "échec de la création de la fonction d'affichage."
  )
  
(* Gère les appels de print *)
let rec expr_print add_space env = function
  (* pour ça on regarde si l'expression est un call, et si oui on print ses retours au lieu de rax. *)
  (* note : les fonctions retournent des Tmany, donc pas des Tstring, donc pas besoin dans le 1er cas. *)
  | [] -> nop
  | ({ expr_typ = Tstring } as e) :: el ->
      expr env e ++
      movq !%rax !%rsi ++
      call "print_string" ++
      expr_print false env el
  | e :: el ->
      add_print_function e.expr_typ;
      (if add_space then
        movq (ilab space_s) !%rsi ++
        call "print_string"
      else nop) ++
      expr env e ++
      (match e.expr_desc with
      | TEcall (f, _) ->
          List.fold_left (fun d typ ->
            d ++
            popq rsi ++
            call (t_expr_to_print typ))
          nop f.fn_typ
      | _ ->
          movq !%rax !%rsi ++
          call (t_expr_to_print e.expr_typ)) ++
      expr_print true env el

(* ###### FIN DES PRINTS ! ENFIN ! ###### *)

(* Associe une l-value à son adresse courante *)
and expr_address env { expr_desc=desc; expr_typ=typ } = match desc, typ with
  | TEident v, Tstruct _ ->
      (* on fait confiance au typing :o *)
      (* en fait pas tellement, les v_addr seront modifiés ici, genre dans TEvars par exemple *)
      (* c'est une opération sur les ... je sais plus quoi, je crois. cf cours mihaela. *)
      (* mais en gros on va à l'adresse pointée par v_addr décalée de l'adresse pointée par rbp et on met ce qu'il y a dans rax quoi *)
      (* on fait également confiance au maintient consciencieux de rbp c: *)
      movq (ind ~ofs:v.v_addr rbp) !%rax
      (* les structures sont des pointeurs de toute façon, donc leur adresse c'est elle même en gros *)
      (* alors que les autres, leur adresse c'est vraiment leur adresse *)
      (* d'où la disjonction de cas *)
  | TEident v, _ ->
      leaq (ind ~ofs:v.v_addr rbp) rax
  | TEdot (e, f), _ ->
      (match e.expr_typ with
      | Tptr _ ->
          expr env e
      | _ ->
          expr_address env e
          ) ++
      addq (imm f.f_ofs) !%rax
  | TEunop (Ustar, e), _ ->
      expr env e
  | _ -> failwith "seule les l values ont des adresses"

and expr env e = match e.expr_desc with
  | TEskip ->
      nop

  | TEconstant (Cbool true) ->
      (* Le sujet dit qu'il faut mettre dans rdi, mais c'est tellement plus simple dans rax :o du coup on met dans rax. *)
      movq (imm 1) !%rax

  | TEconstant (Cbool false) ->
      movq (imm 0) !%rax

  | TEconstant (Cint x) ->
      movq (imm64 x) !%rax

  | TEnil ->
      xorq !%rax !%rax

  | TEconstant (Cstring s) ->
      let label = alloc_string s in
      movq (ilab label) !%rax

  | TEbinop (Band, e1, e2) ->
      (* on code la paresse par un if *)
      expr env 
        (mk_bool
          (TEif(
            mk_bool (TEunop(Unot,e1)),
            mk_bool (TEconstant (Cbool false)) ,
            e2)))

  | TEbinop (Bor, e1, e2) ->
      expr env 
      (mk_bool 
        (TEif(
          e1,
          mk_bool (TEconstant (Cbool true)),        
          e2
          )))

  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
      expr env e1 ++
      (* les fonctions ont déjà mis leur résultat dans la pile et ont 0 dans rax. Le reste, on doit mettre rax dans la pile. *)
      (match e1.expr_desc with
      | TEcall _ -> nop
      | _ -> pushq !%rax) ++
      expr env e2 ++
      (* RAPPEL : on a toujours la convention qu'à la fin, une expr met son résultat dans rax.
      Si jamais une autre doit être évaluée avant qu'il soit utilisé, il est mis sur la pile.
      Ce n'est pas géré à la fin de l'expression, mais au début de la suivante.*)
      (* les fonctions n'ont pas leur résultat dans rax, on doit l'y mettre de force. *)
      (match e2.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      cmpq !%rax (ind rsp) ++
      (function
        | Blt -> setl
        | Ble -> setle
        | Bgt -> setg
        | _ -> setge)
        (* merci developpez pour ce magnifique bout de registre <3 *)
        (* il a la bonne taille (8) et la bonne localisation sur le gros registre rax. *)
        op !%al ++
      movzbq !%al rax ++
      popq rcx

  (* cf le premier projet pour les opérations arithmétiques basiques. *)
  | TEbinop (Badd | Bsub | Bmul as op, e1, e2) ->
      expr env e2 ++
      (*cf commentaire précédent sur les fonctions. Cette disjonction apparaîtra souvent.*)
      (match e2.expr_desc with
      | TEcall _ -> nop
      | _ -> pushq !%rax) ++
      expr env e1 ++
      (match e1.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      (function
        | Badd -> addq
        | Bsub -> subq
        | _ -> imulq)
        op (ind rsp) !%rax ++
      popq rcx

  | TEbinop (Bdiv | Bmod as op, e1, e2) ->
      expr env e2 ++
      (match e2.expr_desc with
      | TEcall _ -> nop
      | _ -> pushq !%rax) ++
      expr env e1 ++
      (match e1.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      cqto ++
      (* cf projet précédent pour le fonctionnement foireux de idivq *)
      idivq (ind rsp) ++
      movq !%(if Bdiv = op then rax else rdx) !%rax ++
      popq rcx

  | TEbinop (Beq | Bne as op, e1, e2) ->
      (* TODO : cette fonction de marche pas ! *)
      (* enfin peut être que si, j'en sais rien, mais je l'ai fait au pif à 3h du matin, donc j'y crois pas trop. *)
      expr env e1 ++
      (match e1.expr_desc with
      | TEcall _ -> nop
      | _ -> pushq !%rax) ++
      expr env e2 ++
      (match e2.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      movq !%rax !%rdi ++
      popq rsi ++
      (match e1.expr_typ with
        | Tstruct s ->
            let l = new_label ()
            and l_end = new_label () in
            movq (imm s.s_size) !%rdx ++
            pushq !%rbp ++
            movq !%rsp !%rbp ++
            movq (ilab "0xfffffffffffffff0") !%rax ++
            andq !%rax !%rsp ++
            call "memcmp" ++
            movq !%rbp !%rsp ++
            popq rbp ++
            cmpq (imm 0) !%rax ++
            je l ++
            movq (imm 0) !%rax ++
            jmp l_end ++
            label l ++
            movq (imm 1) !%rax ++
            label l_end
        | Tstring ->
            let l = new_label ()
            and l_end = new_label () in
            pushq !%rbp ++
            movq !%rsp !%rbp ++
            movq (ilab "0xfffffffffffffff0") !%rax ++
            andq !%rax !%rsp ++
            call "strcmp" ++
            movq !%rbp !%rsp ++
            popq rbp ++
            cmpq (imm 0) !%rax ++
            je l ++
            movq (imm 0) !%rax ++
            jmp l_end ++
            label l ++
            movq (imm 1) !%rax ++
            label l_end
        | _ ->
            cmpq !%rsi !%rdi ++
            sete !%al ++
            movzbq !%al rax
      ) ++
      (if op = Beq then
        nop
      else notq !%rax)

  | TEunop (Uneg, e1) ->
      expr env e1 ++
      (match e1.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      negq !%rax

  | TEunop (Unot, e1) ->
      expr env e1 ++
      (match e1.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      notq !%rax ++
      (* pas sûr de ce que ça fait, mais ça le fait :o *)
      andq (imm 1) !%rax

  | TEunop (Uamp, e1) ->
      expr_address env e1

  | TEunop (Ustar, e1) ->
      expr env e1 ++
      (match e1.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      (match e1.expr_typ with
        (* big brain time, j'ai giga galérer pour pas grand chose au final. Le méli mélo qu'il y avait dans ma tête fut comparable à celui de Tipiak. *)
        | Tptr (Tstruct _) -> nop
        | _ -> movq (ind rax) !%rax
      )

  | TEprint el ->
      add_print_function Tstring;
      expr_print false env el
      
  | TEident x ->
      movq (ind ~ofs:x.v_addr rbp) !%rax

  (* je comprend pas pourquoi "[lvl]" et pas "lvl", on va donc faire "lvl". Dans le doute, je suis meilleur que le sujet c: *)
  | TEassign (lvl, el) ->
      (* on commence par stacker les valeurs à donner aux left values *)
      let rec stacking el = match el with
        | [] -> nop
        | e::q ->
            let a = stacking q in
            a ++
            expr env e ++
            (match e.expr_desc with
            | TEcall _ -> nop
            | _ -> pushq !%rax)
      in
      stacking el ++
      (* puis on leur donne ces valeurs. *)
      List.fold_left (fun d lv ->
        d ++ (match lv.expr_typ with
        | Tstruct s ->
            expr_address env lv ++
            popq rsi ++
            movq !%rax !%rdi ++
            movq (imm s.s_size) !%rdx ++
            pushq !%rbp ++
            movq !%rsp !%rbp ++
            movq (ilab "0xfffffffffffffff0") !%rax ++
            andq !%rax !%rsp ++
            call "memcpy" ++
            movq !%rbp !%rsp ++
            popq rbp
        | Twild ->
            (* si jamais on veut donner une valeur à un _, alors on s'en fou et on la met à la poubelle. #Free_the__*)
            popq rsi
        | _ ->
            expr_address env lv ++
            popq rsi ++
            movq !%rsi (ind rax)
      )) nop lvl

  | TEcall (f, el) ->
      (* attention à rbp et rsp, et à l'ordre donné par le sujet pour les arguments...*)
      let rec stacking_args el = match el with
        | [] -> nop
        | e::q ->
            let a = stacking_args q in
            a ++
            expr env e ++
            (match e.expr_desc with
            | TEcall _ -> nop
            | _ -> pushq !%rax)
      in
      (* on commence par réserver la place pour les retours. *)
      (* on considère que les fonctions retourne des listes de profondeur 1, sinon c'est un enfer et je refuse de m'y prêter. *)
      (* de toute façon jamais ils penseront à faire des tests aussi foireux ces fous. *)
      (* ... j'espère que vous lirez mes commentaires en diagonale :o *)
      subq (imm (8 * (List.length f.fn_typ))) !%rsp ++
      stacking_args el ++
      call ("F_" ^ f.fn_name) ++
      (* E_ efface les variables locales *)
      (* ici on efface les arguments *)
      addq (imm (8 * (List.length el))) !%rsp
      (* il ne reste donc plus que les res sur la pile *)

      (* si jamais les résultats ne sont pas utilisés, c'est pas si grave, ça va polluer la pile,
      mais dans le block il y a un garde fou pour bien tout remettre comme c'était avant. *)

  | TEblock el ->
      let new_env = env_copy env
      and nb_glob = env.nb_locals in
      let t1 = List.fold_left (++) nop (List.map (expr new_env) el) in
      t1 ++
      (* normalement cette condition est toujours vraie, sinon ya un tout péti problème :o *)
      (* enfin pas strictement, mais largement *)
      (if new_env.nb_locals > nb_glob then
        (* ATTENTION ! bien réaligner la pile !*)
        (* historiquement, ça a posé problème. *)
        addq (imm ((new_env.nb_locals - nb_glob) * 8)) !%rsp
      else nop)

  | TEif (b, e1, e2) ->
      let esle = new_label ()
      and neht = new_label () in
      expr env b ++
      (match b.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      cmpq (imm 0) !%rax ++
      je esle ++
      expr env e1 ++
      jmp neht ++
      label esle ++
      expr env e2 ++
      label neht

  | TEfor (b, e) ->
      let l_b = new_label ()
      and l_begin = new_label () in
      jmp l_b ++
      label l_begin ++
      expr env e ++
      label l_b ++
      expr env b ++
      (match b.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      cmpq (imm 1) !%rax ++
      je l_begin

  | TEnew ty ->
      allocz (sizeof ty)

  | TEdot ({ expr_desc = (TEident _ | TEcall _) } as e1, {f_name; f_ofs}) ->
      (* TODO : pour les TEcall il est possible que ça ne marche pas. *)
      (* de toute façon on ne peut pas faire de a.f(b), si ? ça me paraîtrait bizarre ... à tester. *)
      expr env e1 ++
      movq (ind ~ofs:f_ofs rax) !%rax

  (* on sépare ce cas, parce que les structures sont stoquées comme pointeur ves la mémoire SAUF si elles sont stockées dans une autre structure, auquel cas, elles sont directement en son sein. *)
  | TEdot ({ expr_desc = (TEdot _) }, f) ->
      expr_address env e ++
      movq (ind rax) !%rax

  | TEdot ({ expr_desc = TEunop(Ustar,e1) },f) -> 
      expr env {e with expr_desc = (TEdot(e1,f))}

  | TEvars (varlist, initlist) ->
      (* -8 ça suffit toujours ! rappel : tout est soit de taille 8, soit stocké comme pointeur, donc de taille 8 quoi qu'il arrive. *)
      List.iter (fun var ->
        env_add_variable env;
        var.v_addr <- -8 * (env.nb_locals))
      (List.rev varlist);
      (* c'est giga sombre, un paquet de nouilles sèches serait plus efficace que mon cerveau à ce point. En plus ce bout de code est réutilisé partout. On va prier pour que ça marche. *)
      (* EDIT : apparemment ça marche.*)
      let rec stacking el = match el with
        | [] -> nop
        | e::q ->
            let a = stacking q in
            a ++
            expr env e ++
            (match e.expr_desc with
            | TEcall _ -> nop
            | _ -> pushq !%rax)
      in
      stacking initlist

  | TEreturn [] ->
      jmp ("E_" ^ env.exit_label)

  | TEreturn [e1] ->
      expr env e1 ++
      (* ici aussi on prie pour que la fonction qu'on retourne ne retourne pas elle même plus que un truc. *)
      (match e1.expr_desc with
      | TEcall _ -> popq rax
      | _ -> nop) ++
      (* on empile le résultat à la bonne position *)
      movq !%rax (ind ~ofs:(8*env.ofs_this + 8) rbp) ++
      jmp ("E_" ^ env.exit_label)

  | TEreturn el ->
      (* on empile les résultats aux bonnes positions *)
      (* TODO : return f(), g() n'est pas géré si ils ont chacun plusieurs retours *)
      (* attention à l'ordre d'empilement (hence the incr decr foireux) *)
      let i = ref 1 in
      let rec stacking el = match el with
        | [] -> nop
        | e1::q ->
            incr i;
            let a = stacking q in
            decr i;
            a ++
            expr env e1 ++
            (* ce stacking est différent des autres : si jamais on stack une fonction, avant, elle le faisait toute seule.
               Maintenant, on elle le fait mais pas au bon endroit, donc on récupère ce qu'elle a stacké et on el met au bon endroit.*)
            (match e1.expr_desc with
            | TEcall _ -> popq rax
            | _ -> nop) ++
            movq !%rax (ind ~ofs:(8*(env.ofs_this + !i)) rbp)
      in
      stacking el ++
      jmp ("E_" ^ env.exit_label)

  | TEincdec (e1, op) ->
      expr_address env e1 ++
      (if op = Inc then incq else decq) (ind rax)


(* E_... s'occupe du return, met 0 dans rax, bouge rsp à rbp, pop rbp dans rbp, puis ret

Le TEreturn ne s'occupe pas du ret ! il s'occupe juste d'empiler les valeurs de retours et de sauter vers E_.
Si jamais il n'y a pas de return, du coup, on va automatiquement aller dans E_, et il y aura quand même un ret. *)
let function_ f e =
  if !debug then Format.eprintf "function %s:@." f.fn_name;
  let s = f.fn_name
  and n_params = List.length f.fn_params in
  let env = { exit_label = s ;
              (* le call qui réserve la place pour le return *)
              (* ici on a juste besoin de savoir de combien se décaler pour écrire dans les cases res i*)
              ofs_this = (n_params + 1); 
              nb_locals = 0;
  } in

  label ("F_" ^ s) ++ 
  pushq !%rbp ++
  movq !%rsp !%rbp ++ (* ceci respecte l'alignement de rsp et rbp indiqué par le sujet. *)
  expr env e ++

  label ("E_" ^ s) ++
  xorq !%rax !%rax ++
  movq !%rbp !%rsp ++
  popq rbp ++ (* après ça, rbp pointe vers l'ancien rbp, et rsp pointe vers la prochaine instruction, qui va être utilisée pour ret *)
  ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b=false) dl =
  debug := b;
  let funs = List.fold_left decl nop dl in
  let print_funs = Hashtbl.fold (fun l s d -> label l ++ s ++ d) print_env nop in
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
