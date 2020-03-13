(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2018 - OCamlPro SAS                                   *)
(*    Alain Mebsout <alain.mebsout@ocamlpro.com>                          *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Js_of_ocaml

type value =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list ]

type t =
  [ `A of value list
  | `O of (string * value) list ]

let value: t -> value = fun t -> (t :> value)

exception Parse_error of value * string

let parse_error t fmt =
  Printf.kprintf (fun msg ->
      raise (Parse_error (t, msg))
    ) fmt

let wrap t = `A [t]

let unwrap = function
  | `A [t] -> t
  | v -> parse_error (v :> value) "Not unwrappable"

type ('a, 'b) kind =
  | Leaf of 'b
  | Arr of 'a list
  | Obj of (string * 'a) list

type ('a, 'b) zip =
  | LeafVal of 'b
  | KeyVal of string
  | LeafZip of 'a
  | ArrZip of 'b list * 'a list
  | ObjZip of (string * 'b) list * (string * 'a) list

module type Converter = sig
  type 'a _from
  type _to
  val kind_of : 'a _from -> ('a _from, _to) kind
  val arr : _to list -> _to
  val obj : (string * _to) list -> _to
end

module Js_to_JSON_Converter : Converter with type 'a _from = 'a Js.t Js.opt
                                         and type _to = value
= struct

  type 'a _from = 'a Js.t Js.opt
  type _to = value

  let arr l = `A l

  let obj l = `O l

  let kind_of j =
    Js.Opt.case j
      (fun () -> (* null *)
         Leaf (`Null))
      (fun j -> (* not null *)
         match Js.to_string (Js.typeof j) with
         | "object" ->
           begin
             try Arr
                   (Array.to_list
                      (Js.to_array (Js.Unsafe.coerce j : 'a Js.js_array Js.t)))
             with _ ->
               let keys = Array.to_list (Js.to_array (Js.object_keys j)) in
               let l =
                 List.map (fun k -> Js.to_string k, Js.Unsafe.get j k) keys in
               Obj l
           end
         | "string" ->
           Leaf (`String
                   (Js.to_string (Js.Unsafe.coerce j : Js.js_string Js.t)))
         | "number" ->
           Leaf (`Float
                   (Js.float_of_number (Js.Unsafe.coerce j : Js.number Js.t)))
         | "boolean" ->
           Leaf (`Bool (Js.to_bool (Js.Unsafe.coerce j : bool Js.t)))
         (* | "undefined" -> Leaf (`Null) *)
         | tof -> raise (Invalid_argument ("json_of_js: "^tof))
      )
end

module JSON_to_Js_Converter : Converter with type 'a _from = value
                                         and type _to = Js.Unsafe.any
= struct

  type 'a _from = value
  type _to = Js.Unsafe.any

  let arr l = Js.array (Array.of_list l) |> Js.Unsafe.inject

  let obj l = Js.Unsafe.obj (Array.of_list l) |> Js.Unsafe.inject

  let kind_of = function
    | `Null -> Leaf (Js.Unsafe.inject Js.null)
    | `Bool b -> Leaf (Js.Unsafe.inject (Js.bool b))
    | `Float f -> Leaf (Js.Unsafe.inject (Js.number_of_float f))
    | `String s -> Leaf (Js.Unsafe.inject (Js.string s))
    | `A l -> Arr l
    | `O kl -> Obj kl
end

module Make_Conv (C : Converter) : sig

  val convert : ('a C._from, C._to) zip list -> C._to

end = struct

  (* This is tail-recursive with a manual (non-constant) stack. Js_of_ocaml
     compiles simple tail-recursive functions like this one to loops, so the
     manual stack will live in the heap. *)
  let rec convert = function
    | [LeafVal v] -> v
    | LeafZip j :: stack ->
      begin match C.kind_of j with
        | Leaf leaf -> convert (LeafVal leaf :: stack)
        | Arr [] -> convert (LeafVal (C.arr []) :: stack)
        | Obj [] -> convert (LeafVal (C.obj []) :: stack)
        | Arr (j :: l) -> convert (LeafZip j :: ArrZip ([], l) :: stack)
        | Obj ((k, j) :: kl) ->
          convert (LeafZip j :: KeyVal k :: ObjZip ([], kl) :: stack)
      end

    | LeafVal v :: ArrZip (ez_revlist, []) :: stack ->
      convert (LeafVal (C.arr (List.rev (v :: ez_revlist))) :: stack)
    | LeafVal v :: KeyVal k :: ObjZip (ez_revlist, []) :: stack ->
      convert (LeafVal (C.obj (List.rev ((k, v) :: ez_revlist))) :: stack)

    | LeafVal v :: ArrZip (ez_revlist, j :: js_list) :: stack ->
      convert (LeafZip j :: ArrZip (v :: ez_revlist, js_list) :: stack)

    | LeafVal v :: KeyVal k1 ::
      ObjZip (ez_revlist, (k2, j) :: js_list) :: stack ->
      convert (LeafZip j :: KeyVal k2 ::
               ObjZip ((k1, v) :: ez_revlist, js_list) :: stack)
    | _ -> assert false
end

module Js_to_JSON = Make_Conv (Js_to_JSON_Converter)

module JSON_to_Js = Make_Conv (JSON_to_Js_Converter)

let value_of_js j = Js_to_JSON.convert [LeafZip j]

let json_of_value res =
  match res with
  | `A l -> `A l
  | `O l -> `O l
  | _ -> assert false (* wrap res *)

let value_from_string (s : string) : value =
  try
    Js._JSON##parse (Js.string s) |> value_of_js
  with (Js.Error e) ->
    if Js.to_string e##name = "SyntaxError" then
      parse_error `Null "Ezjsonm.from_string %s" (Js.to_string e##message)
    else Js.raise_js_error e

let from_string (s : string) : [> t] =
  value_from_string s |> json_of_value

let js_of_json j  = JSON_to_Js.convert [LeafZip (value j)]
let js_of_value j  = JSON_to_Js.convert [LeafZip j]

let value_to_string ?(minify=true) (j : value) : string =
  if minify then
    Js._JSON##stringify (js_of_value j) |> Js.to_string
  else
    Js.Unsafe.fun_call (Js.Unsafe.variable "JSON.stringify")
      [| js_of_value j; Js.Unsafe.inject (Js.null); Js.Unsafe.inject 2 |]
    |> Js.to_string

let to_string ?(minify=true) (j : t) : string =
  value_to_string ~minify (value j)


(* Javascript objects additions *)

let to_js json = js_of_json json
let value_to_js json = js_of_value json
let from_js js = value_of_js js |> json_of_value
let value_from_js js = value_of_js js

(* Same as real Ezjsonm *)

let to_buffer ?minify buf json =
  let s = to_string ?minify json in
  Buffer.add_string buf s

let to_channel ?minify oc json =
  let s = to_string ?minify json in
  output_string oc s


let from_channel chan =
  let s = "" in
  let rec read_more =
    let b = Bytes.create 1024 in
    fun () ->
      try
        let l = input chan b 0 1024 in
        from_string (s ^ (Bytes.sub_string b 0 l))
      with
      | Parse_error _ -> read_more ()
      | e -> parse_error `Null "Ezjsonm.from_channel %s" (Printexc.to_string e)
  in
  read_more ()


(* unit *)
let unit () = `Null

let get_unit = function
  | `Null  -> ()
  | j      -> parse_error j "Ezjsonm.get_unit"

(* bool *)
let bool b = `Bool b

let get_bool = function
  | `Bool b -> b
  | j       -> parse_error j "Ezjsonm.get_bool"

(* string *)
let string s = `String s

let get_string = function
  | `String s -> s
  | j         -> parse_error j "Ezjsonm.get_string"

(* int *)
let int i = `Float (float_of_int i)
let int32 i = `Float (Int32.to_float i)
let int64 i = `Float (Int64.to_float i)

let get_int = function
  | `Float f -> int_of_float f
  | j        -> parse_error j "Ezjsonm.get_int"

let get_int32 = function
  | `Float f -> Int32.of_float f
  | j        -> parse_error j "Ezjsonm.get_int32"

let get_int64 = function
  | `Float f -> Int64.of_float f
  | j        -> parse_error j "Ezjsonm.get_int64"

(* flooat *)
let float f = `Float f

let get_float = function
  | `Float f -> f
  | j        -> parse_error j "Ezjsonm.get_float"

(* list *)
let list fn l =
  `A (List.map fn l)

let get_list fn = function
  | `A ks -> List.map fn ks
  | j     -> parse_error j "Ezjsonm.get_list"


(* string lists *)
let strings = list string

let get_strings = get_list get_string


(* options *)
let option fn = function
  | None   -> `Null
  | Some x -> `A [fn x]

let get_option fn = function
  | `Null  -> None
  | `A [j] -> Some (fn j)
  | j -> parse_error j "Ezjsonm.get_option"

(* dict *)
let dict d = `O d

let get_dict = function
  | `O d -> d
  | j    -> parse_error j "Ezjsonm.get_dict"

(* pairs *)
let pair fk fv (k, v) =
  `A [fk k; fv v]

let get_pair fk fv = function
  | `A [k; v] -> (fk k, fv v)
  | j         -> parse_error j "Ezjsonm.get_pair"

(* triple *)

let triple fa fb fc (a, b, c) =
  `A [fa a; fb b; fc c]

let get_triple fa fb fc = function
  | `A [a; b; c] -> (fa a, fb b, fc c)
  | j -> parse_error j "Ezjsonm.get_triple"

let mem t path =
  let rec aux j p = match p, j with
    | []   , _    -> true
    | h::tl, `O o -> List.mem_assoc h o && aux (List.assoc h o) tl
    | _           -> false in
  aux t path

let find t path =
  let rec aux j p = match p, j with
    | []   , j    -> j
    | h::tl, `O o -> aux (List.assoc h o) tl
    | _           -> raise Not_found in
  aux t path

let map_dict f dict label =
  let rec aux acc = function
    | []    ->
      begin match f `Null with
        | None   -> List.rev acc
        | Some j -> List.rev_append acc [label, j]
      end
    | (l,j) as e :: dict ->
      if l = label then
        match f j with
        | None   -> List.rev_append acc dict
        | Some j -> List.rev_append acc ((l,j)::dict)
      else
        aux (e::acc) dict in
  aux [] dict

let map f t path =
  let rec aux t = function
    | []    -> f t
    | h::tl ->
      match t with
      | `O d -> Some (`O (map_dict (fun t -> aux t tl) d h))
      | j    -> None in
  match aux t path with
  | None   -> raise Not_found
  | Some j -> j

let update t path v =
  map (fun _ -> v) t path
