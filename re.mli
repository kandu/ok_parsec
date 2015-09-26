type buffer = { str : string; len : int; pos : int; }
val initBuffer : string -> buffer
type token = OpC | OpO | TkE | TkC of char | TkS of string
type regexp =
    Ce
  | C of char
  | A of regexp * regexp
  | O of regexp * regexp
  | Cl of regexp
val getToken : buffer -> token * buffer
val str2reg : string -> regexp
val reg2nfa : regexp -> Fa.nfa
val nfa2dfa : Fa.nfa -> Fa.dfa
val dfa2sm : Fa.dfa -> Fa.sm
val make : string -> Fa.sm
val match_re : Fa.sm -> Common.state -> (bool * string) Lwt.t
