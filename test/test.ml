open Ok_parsec
open Parsec
open Re

let reg= str2reg "(a|b)*abb"
let nfa= reg2nfa reg
let dfa= nfa2dfa nfa
let sm= dfa2sm dfa

let main ()=
  let p= many (regexp sm) << notFollowedBy (string "m") "m" in
  let%lwt result= parse_string p "aababbm" in
  (Lwt.return
    (match result with
    | Ok (r, _s)-> List.iter print_endline r
    | Error (p, s)-> Printf.printf "at %d, %s\n" p s));%lwt
  let%lwt result= parse_string p "aababbn" in
  Lwt.return
    (match result with
    | Ok (r, _s)-> List.iter print_endline r
    | Error (p, s)-> Printf.printf "at %d, %s\n" p s)

let test_nfa ()= Fa.display_svg_n nfa
let test_dfa ()= Fa.display_svg_d dfa
let test_state_machine ()= Fa.display_svg_sm sm

let test_concurrent_or_sequential ()= Lwt_main.run @@ main ()


let ()=
    test_nfa ();
    test_dfa ()

