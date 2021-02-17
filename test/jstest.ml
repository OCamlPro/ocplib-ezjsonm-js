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
