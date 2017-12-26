type dataStream=
  | Chan of Lwt_io.input_channel
  | Fd of Lwt_unix.file_descr

type state = {
  dataStream: dataStream;
  buf : Buffer.t;
  eof : bool;
  pos : int;
}
val initState : dataStream -> state
val getBuffered : state -> string Lwt.t
val input : ?len:int -> state -> state Lwt.t
