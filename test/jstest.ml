(* open Ast_mapper
 * open Ast_helper
 * open Asttypes
 * open Parsetree
 * open Longident *)

(* let test_mapper argv =
 *   { default_mapper with
 *     expr = fun mapper expr ->
 *       match expr with
 *       | { pexp_desc =
 *           Pexp_extension ({ txt = "test"; loc }, pstr)} ->
 *         begin match pstr with
 *           | PStr [{ pstr_desc = Pstr_eval (e, [])}] ->
 *             (\* replace with f __LOC__*\)
 *             Exp.apply ~loc e [Nolabel, Exp.ident ~loc
 *                                 { loc; txt = Longident.parse "__LOC__"}]
 *           | _ ->
 *             raise (Location.Error (
 *                 Location.error ~loc
 *                   "[%test] accepts a function, e.g. [%test assert_equal]"))
 *         end
 *       (\* Delegate to the default mapper. *\)
 *       | x -> default_mapper.expr mapper x;
 *   }
 *
 * let () = register "test" test_mapper *)
open Js_of_ocaml

module Console = struct
  let log_ok loc msg =
    Firebug.console##log_2 (Js.string ("%c[OK] "^loc^" : "^msg)) (Js.string "color: green")

  let log_ko loc msg =
    Firebug.console##log_2 (Js.string ("%c[KO] "^loc^" : "^msg)) (Js.string "color: red")

  let assert_equal loc x y =
    let eq = x = y in
    if eq then
      log_ok loc "Equal"
    else
      log_ko loc "Sould be equal"

  let assert_bool loc msg b =
    if b then
      log_ok loc msg
    else
      log_ko loc msg

  let assert_raises loc exp f =
    try
      f ();
      log_ko loc "No execption raised"
    with e ->
      if e = exp then
        log_ok loc ("Raised "^Printexc.to_string exp)
      else
        log_ko loc ("Should not raise "^Printexc.to_string e)

  let (>::) name f =
    fun () ->
      Firebug.console##log_2 (Js.string ("%c"^name^":")) (Js.string "color: blue");
      f ()

  let (>:::) name l =
    Firebug.console##log_2 (Js.string ("%c"^name^"\n--------------------------------------------------"))
      (Js.string "background: black, color: white");
    List.iter (fun f -> f ()) l;
    Firebug.console##log(Js.string "");
    ()

end
