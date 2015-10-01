open Common
open Core_kernel.Std
open Result

type pos= int
type error= pos * string

type 'a reply= (('a * state), error) Result.t

type 'a parser= state -> 'a reply Lwt.t
type 'a t= 'a parser


(* parser generator *)

let any= fun state->
  let check state=
    let found= Buffer.nth state.buf state.pos in
    Ok (found, {state with pos= state.pos+1})
  in
  let need= state.pos + 1 - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + 1 - (Buffer.length state.buf) > 0 then
      Lwt.return (Error (state.pos, "out of bounds"))
    else
      Lwt.return (check state)
  else
    Lwt.return (check state)


let char c= fun state->
  let check state=
    let found= Buffer.nth state.buf state.pos in
    let open Printf in
    if found = c then
      Ok (found, {state with pos= state.pos+1})
    else
      Error (
        state.pos,
        sprintf "\"%c\" expected but \"%c\" found" c found)
  in
  let need= state.pos + 1 - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + 1 - (Buffer.length state.buf) > 0 then
      Lwt.return (Error (state.pos, "out of bounds"))
    else
      Lwt.return (check state)
  else
    Lwt.return (check state)


let string str= fun state->
  let len= String.length str in
  let check state=
    let found= Buffer.sub state.buf state.pos len in
    let open Printf in
    if found = str then
      Ok (found, {state with pos= state.pos+len})
    else
      Error (
        state.pos,
        sprintf "\"%s\" expected but \"%s\" found" str found)
  in
  let need= state.pos + len - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + len - (Buffer.length state.buf) > 0 then
      Lwt.return (Error (state.pos, "out of bounds"))
    else
      Lwt.return (check state)
  else
    Lwt.return (check state)


let satisfy test= fun state->
  let check state=
    let found= Buffer.nth state.buf state.pos in
    let open Printf in
    if test found then
      Ok (found, {state with pos= state.pos+1})
    else
      Error (
        state.pos,
        sprintf "\"%c\" isn't expected" found)
  in
  let need= state.pos + 1 - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + 1 - (Buffer.length state.buf) > 0 then
      Lwt.return (Error (state.pos, "out of bounds"))
    else
      Lwt.return (check state)
  else
    Lwt.return (check state)


let regexp re= fun state->
  let%m[@Lwt] (ok, result)= Re.match_re re state in
  if ok then
    let len= String.length result in
    Lwt.return (Ok (result, {state with pos= state.pos+len}))
  else
    Lwt.return (Error (state.pos, "doesn't match the regexp"))


(* combinator *)
let fail msg= fun state-> Lwt.return (Error (state.pos, msg))

let return v= fun state-> Lwt.return (Ok (v, state))

let bind (p: 'a parser) (f: 'a -> 'b parser)= fun state->
  let%m[@Lwt] result= p state in
  match result with
  | Error e-> Lwt.return (Error e)
  | Ok (v,state)-> f v state

let (>>=)= bind
let (>>) p1 p2= p1 >>= fun _ -> p2
let (<<) p1 p2= p1 >>= fun x-> p2 >> return x
let (|>>) p f= p >>= fun v-> return (f v)
let (>>$) p v= p >> return v

let (<|>) (p1:'a parser) (p2:'a parser)= fun state->
  let%m[@Lwt] result= p1 state in
  match result with
  | Error _ -> p2 state
  | Ok _-> Lwt.return result

let between left right p= left >> p << right

let many p=
  let rec parser s=
    (((p |>> fun v-> Some v) <|> return None) >>= (function
      | Some v-> parser |>> (fun r-> v :: r)
      | None-> return []))
      s
  in parser

let many1 p=
  p >>= fun v-> many p |>> fun l-> v :: l

let rec times num p s=
  if num > 0 then
    (p >>= (fun v-> times (num-1) p |>> (fun r-> v::r))) s
  else
    (return []) s

let sepBy1 sep p=
  p >>= fun head->
  many (sep >> p) >>= fun body->
  return (head :: body)

let sepBy sep p= sepBy1 sep p <|> return []

let sepEndBy sep p= many (p << sep)

let sepEndBy1 sep p= many1 (p << sep)

let opt default p=
  p <|> return default

let option p= p |>> (fun v-> Some v) <|> return None

let lookAhead p= fun state->
  let%m[@Lwt] reply= p state in
  Lwt.return (match reply with
  | Ok (r, newState)-> Ok (r, state)
  | Error _-> reply)

let followedBy p msg= fun state->
  let%m[@Lwt] reply= p state in
  Lwt.return (match reply with
  | Ok _-> Ok ((), state)
  | Error _-> Error (state.pos, msg))

let notFollowedBy p msg= fun state->
  let%m[@Lwt] reply= p state in
  Lwt.return (match reply with
  | Ok _-> Error (state.pos, msg)
  | Error _-> Ok ((), state))

(* parser *)
let eof state= Lwt.return 
  (if (state.eof && (state.pos >= Buffer.length state.buf))
  then Ok ((), state)
  else Error (state.pos, "not eof"))

let int8= any |>> int_of_char

let int16= any >>= fun l-> any |>> fun h-> int_of_char h lsl 8 + int_of_char l
let int16_net= any >>= fun h-> any |>> fun l-> int_of_char h lsl 8 + int_of_char l

let int32= int16 >>= fun l-> int16 |>> fun h-> Int32.((+) (shift_left (of_int_exn h) 16) (of_int_exn l))
let int32_net= int16_net >>= fun h-> int16_net |>> fun l-> Int32.((+) (shift_left (of_int_exn h) 16) (of_int_exn l))

let int64= int32 >>= fun l-> int32 |>> fun h-> Int64.((+) (shift_left (of_int32 h) 32) (of_int32 l))
let int64_net= int32_net >>= fun h-> int32_net |>> fun l-> Int64.((+) (shift_left (of_int32 h) 32) (of_int32 l))

let num_dec= satisfy (fun c->
  '0' <= c && c <= '9')

let num_bin= satisfy (fun c->
  c = '0' || c = '1')

let num_oct= satisfy (fun c->
  '0' <= c && c <= '7')

let num_hex= satisfy (fun c->
  '0' <= c && c <= '9'
  || 'a' <= c && c <= 'f'
  || 'A' <= c && c <= 'F')

let lowercase= satisfy (fun c->
  'a' <= c && c <= 'z')

let uppercase= satisfy (fun c->
  'A' <= c && c <= 'Z')

(* start parsing *)
let parse_string parser str=
  parser
    (Common.initState
      (Lwt_io.of_bytes ~mode:Lwt_io.input (Lwt_bytes.of_string str)))

let parse_channel parser chan=
  parser (Common.initState chan)
