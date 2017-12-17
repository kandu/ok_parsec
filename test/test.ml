open Ok_parsec
open Parsec
open Re

let reg= str2reg "(a|b)*abb"
let nfa= reg2nfa reg
let dfa= nfa2dfa nfa
let sm= dfa2sm dfa

let ()= Fa.display_svg_n nfa
let ()= Fa.display_svg_d dfa
let ()= Fa.display_svg_sm sm

let main ()=
  let p= many (regexp sm) << notFollowedBy (string "m") "m" in
  begin%m[@Lwt]
    (let%m[@Lwt] result= parse_string p "aababbm" in
    Lwt.return (match result with
    | Ok (r, s)-> List.iter print_endline r
    | Failed (p, s)-> Printf.printf "at %d, %s\n" p s));
    (let%m[@Lwt] result= parse_string p "aababbn" in
    Lwt.return (match result with
    | Ok (r, s)-> List.iter print_endline r
    | Failed (p, s)-> Printf.printf "at %d, %s\n" p s));
  end

let ()= Lwt_main.run @@ main ()
