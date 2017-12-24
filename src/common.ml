open Lwt

type dataStream=
  | Chan of Lwt_io.input_channel
  | Fd of Lwt_unix.file_descr

let read_fd_all fd=
  let tb_len= 1024 in
  let tmpbuf= Bytes.create tb_len in
  let buffer= Buffer.create 0 in
  let rec aux ()=
    let%lwt count= Lwt_unix.read fd tmpbuf 0 tb_len in
    if count = 0 then
      return (Buffer.to_bytes buffer)
    else if count = tb_len then
      (Buffer.add_bytes buffer tmpbuf;
      aux ())
    else
      (Buffer.add_bytes buffer (Bytes.sub tmpbuf 0 count);
      return (Buffer.to_bytes buffer))
  in
  if Lwt_unix.state fd = Lwt_unix.Opened then
    aux ()
  else
    return (Bytes.create 0)

let read_dataStream ?count ds=
  match ds with
  | Chan chan-> Lwt_io.read ?count chan
  | Fd fd->
    match count with
    | Some count->
      let buf= Bytes.create count in
      let%lwt c= Lwt_unix.read fd buf 0 count in
      let data=
        if c = count then
          buf
        else
          Bytes.sub buf 0 c
      in
      return (Bytes.to_string data)
    | None->
      let%lwt data= read_fd_all fd in
      return (Bytes.to_string data)

type state= {
  dataStream: dataStream;
  buf: Buffer.t;
  eof: bool;
  pos: int;
}

let initState dataStream= {
  dataStream;
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
        let%lwt s= read_dataStream ~count:len state.dataStream in
        Buffer.add_string state.buf s;
        if String.length s < len then
          return {state with eof= true}
        else
          return state
      | None-> let%lwt s= read_dataStream state.dataStream in
        Buffer.add_string state.buf s;
        return {state with eof= true}
    with
    | End_of_file-> return {state with eof= true}

