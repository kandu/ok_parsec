open Lwt

type state= {
  chan: Lwt_io.input_channel;
  buf: Buffer.t;
  eof: bool;
  pos: int;
}

let initState chan= {
  chan;
  buf= Buffer.create 0;
  eof= false;
  pos= 0;
}

let input ?len state=
  if state.eof then
    return state
  else
    try%lwt
      match len with
      | Some len->
        let%lwt s= Lwt_io.read ~count:len state.chan in
        Buffer.add_string state.buf s;
        if String.length s < len then
          return {state with eof= true}
        else
          return state
      | None-> let%lwt s= Lwt_io.read state.chan in
        Buffer.add_string state.buf s;
        return {state with eof= true}
    with
    | End_of_file-> return {state with eof= true}

