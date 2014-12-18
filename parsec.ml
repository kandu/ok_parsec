open Common

type pos= int
type error= pos * string

type 'a reply=
  | Failed of error
  | Ok of 'a * state

type 'a parser= state -> 'a reply Lwt.t
type 'a t= 'a parser


(* parser generator *)
let char c= fun state->
  let check state=
    let found= Buffer.nth state.buf state.pos in
    let open Printf in
    if found = c then
      Ok (found, {state with pos= state.pos+1})
    else
      Failed (
        state.pos,
        sprintf "\"%c\" expected but \"%c\" found" c found)
  in
  let need= state.pos + 1 - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + 1 - (Buffer.length state.buf) > 0 then
      Lwt.return (Failed (state.pos, "out of bounds"))
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
      Failed (
        state.pos,
        sprintf "\"%s\" expected but \"%s\" found" str found)
  in
  let need= state.pos + len - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + len - (Buffer.length state.buf) > 0 then
      Lwt.return (Failed (state.pos, "out of bounds"))
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
      Failed (
        state.pos,
        sprintf "\"%c\" isn't expected" found)
  in
  let need= state.pos + 1 - (Buffer.length state.buf) in
  if need > 0 then
    let%m[@Lwt] state= input ~len:need state in
    if state.pos + 1 - (Buffer.length state.buf) > 0 then
      Lwt.return (Failed (state.pos, "out of bounds"))
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
    Lwt.return (Failed (state.pos, "doesn't match the regexp"))


(* combinator *)
let fail msg= fun state-> Lwt.return (Failed (state.pos, msg))

let return v= fun state-> Lwt.return (Ok (v, state))

let bind (p: 'a parser) (f: 'a -> 'b parser)= fun state->
  let%m[@Lwt] result= p state in
  match result with
  | Failed e-> Lwt.return (Failed e)
  | Ok (v,state)-> f v state

let (>>=)= bind
let (>>) p1 p2= p1 >>= fun _ -> p2
let (<<) p1 p2= p1 >>= fun x-> p2 >> return x
let (|>>) p f= p >>= fun v-> return (f v)

let (<|>) (p1:'a parser) (p2:'a parser)= fun state->
  let%m[@Lwt] result= p1 state in
  match result with
  | Failed _ -> p2 state
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

let sepBy1 sep p=
  p >>= fun head->
  many1 (sep >> p) >>= fun body->
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
  | Failed _-> reply)

let followedBy p msg= fun state->
  let%m[@Lwt] reply= p state in
  Lwt.return (match reply with
  | Ok _-> Ok ((), state)
  | Failed _-> Failed (state.pos, msg))

let notFollowedBy p msg= fun state->
  let%m[@Lwt] reply= p state in
  Lwt.return (match reply with
  | Ok _-> Failed (state.pos, msg)
  | Failed _-> Ok ((), state))

(* parser *)
let eof state= Lwt.return 
  (if (state.eof && (state.pos >= Buffer.length state.buf))
  then Ok ((), state)
  else Failed (state.pos, "not eof"))

let num_dec= satisfy (fun c->
  let code= int_of_char c
  and zero= int_of_char '0'
  and nine= int_of_char '9'
  in
  zero <= code &&  code <= nine)

let num_bin= satisfy (fun c->
  c = '0' || c = '1')

let num_oct= satisfy (fun c->
  let code= int_of_char c
  and zero= int_of_char '0'
  and seven= int_of_char '7'
  in
  zero <= code &&  code <= seven)

let num_hex= satisfy (fun c->
  let code= int_of_char c
  and zero= int_of_char '0'
  and nine= int_of_char '9'
  and a= int_of_char 'a'
  and f= int_of_char 'f'
  and ca= int_of_char 'A'
  and cf= int_of_char 'F'
  in
  zero <= code && code <= nine
  || a <= code && code <= f
  || ca <= code && code <= cf)

