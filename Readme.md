# ocplib-ezjsonm-js

[Js\_of\_ocaml](https://ocsigen.org/js_of_ocaml) ready version of
[ezjsonm](https://github.com/mirage/ezjsonm).

The code of Ezjsonm and Jsonm is not tail call optimized by js\_of\_ocaml which
makes them unusable in Javascript applications. This version uses the builtin
JSON parsing/printing facilities offered by the javascript runtimes (this
prevents stack overflows).

## Compilation

Simply issue `make` and `make install` to install it in your current opam
switch.

## Usage

Use the package called `ocplib-ezjsonm-js` with your favorite build tool.

Not everything is implemented (missing Sexplib, etc.) but `to_string` and
`from_string` are operational.
