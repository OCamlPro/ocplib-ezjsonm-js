open Jstest.Console
open Ezjsonm

let bouffe = {|
{
  "fruits": [
    { "kiwis": 3,
      "mangues": 4,
      "pommes": null
    },
    { "panier": true }
  ],
  "legumes": {
      "patates": "amandine",
      "poireaux": false
    },
    "viandes": ["poisson","poulet","boeuf"]
}
|}

let superheroes = {|
{
  "squadName": "Super hero squad",
  "homeTown": "Metro City",
  "formed": 2016,
  "secretBase": "Super tower",
  "active": true,
  "members": [
    {
      "name": "Molecule Man",
      "age": 29,
      "secretIdentity": "Dan Jukes",
      "powers": [
        "Radiation resistance",
        "Turning tiny",
        "Radiation blast"
      ]
    },
    {
      "name": "Madame Uppercut",
      "age": 39,
      "secretIdentity": "Jane Wilson",
      "powers": [
        "Million tonne punch",
        "Damage resistance",
        "Superhuman reflexes"
      ]
    },
    {
      "name": "Eternal Flame",
      "age": 1000000,
      "secretIdentity": "Unknown",
      "powers": [
        "Immortality",
        "Heat Immunity",
        "Inferno",
        "Teleportation",
        "Interdimensional travel"
      ]
    }
  ]
}
|}


let test_features ctxt =
  let super = from_string superheroes in
  let superheroes2 = to_string super in
  let super2 = from_string superheroes2 in
  assert_equal __LOC__ super super2;

  let bouf = from_string bouffe in
  let bouffe2 = to_string bouf in
  let bouf2 = from_string bouffe2 in
  assert_equal __LOC__ bouf bouf2;

  assert_bool __LOC__ "mem 1" (mem bouf ["legumes";"patates"]);
  assert_bool __LOC__ "mem 2" (mem super ["active"]);
  assert_bool __LOC__ "mem 3" (mem super ["trucfarfelu"] = false);

  assert_bool __LOC__ "find 1"
    (find bouf ["legumes";"poireaux"] |> get_bool = false);
  assert_bool __LOC__ "find 2" (find super ["members"] |> get_list (fun m ->
      find m ["secretIdentity"] |> get_string) |> List.hd = "Dan Jukes"
    );
  ()

let mk_json_arr name size s =
  String.concat "," (Array.make size s |> Array.to_list) |>
  Printf.sprintf "{\"a\":[%s]}"

let rec mk_json_size name acc n =
  match n with
  | 0 -> acc
  | _ -> mk_json_size name (mk_json_arr name n acc) (n - 1)

let mk_json_size name n = mk_json_size name "[]" n

let mk_arr name size (s : t) : t =
  let l = Array.make size s |> Array.to_list |> List.map value in
  `O [name, `A l]

let rec mk_size name acc n =
  match n with
  | 0 -> acc
  | _ -> mk_size name (mk_arr name n acc) (n - 1)

let mk_size name n = mk_size name (`A []) n

let test_perf _ =
  let s3 = mk_json_size "a" 3 in
  assert_bool __LOC__ "read 3"
    (from_string s3 |> ignore; true);

  let j3 = mk_size "a" 3 in
  assert_bool __LOC__ "write 3"
    (to_string j3 |> ignore; true);

  let s4 = mk_json_size "a" 4 in
  assert_bool __LOC__ "read 4"
    (from_string s4 |> ignore; true);
  let j4 = mk_size "a" 4 in
  assert_bool __LOC__ "write 4"
    (to_string j4 |> ignore; true);

  let s5 = mk_json_size "a" 5 in
  assert_bool __LOC__ "read 5"
    (from_string s5 |> ignore; true);
  let j5 = mk_size "a" 5 in
  assert_bool __LOC__ "write 5"
    (to_string j5 |> ignore; true);

  let s6 = mk_json_size "a" 6 in
  assert_bool __LOC__ "read 6"
    (from_string s6 |> ignore; true);
  let j6 = mk_size "a" 6 in
  assert_bool __LOC__ "write 6"
    (to_string j6 |> ignore; true);

  let s7 = mk_json_size "a" 7 in
  assert_bool __LOC__ "read 7"
    (from_string s7 |> ignore; true);
  let j7 = mk_size "a" 7 in
  assert_bool __LOC__ "write 7"
    (to_string j7 |> ignore; true);

  let s8 = mk_json_size "a" 8 in
  assert_bool __LOC__ "read 8"
    (from_string s8 |> ignore; true);
  let j8 = mk_size "a" 8 in
  assert_bool __LOC__ "write 8"
    (to_string j8 |> ignore; true);

  let s10 = mk_json_size "a" 10 in
  assert_bool __LOC__ "read 10"
    (from_string s10 |> ignore; true);
  let j10 = mk_size "a" 10 in
  assert_bool __LOC__ "write 10"
    (to_string j10 |> ignore; true);

  ()


let _ = "Ezjsonm" >::: [
    "Testing features"   >:: test_features;
    "Testing performance"   >:: test_perf;
  ]
