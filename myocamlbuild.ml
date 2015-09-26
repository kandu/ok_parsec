open Ocamlbuild_plugin
open Command

let ()= Options.use_ocamlfind:= true

let ()= dispatch (function
  | After_rules ->
    begin
      flag ["ocaml"; "compile"] &
        S [A"-ppxopt"; A"lwt.ppx,-no-sequence"];
    end
  | _ -> ())

