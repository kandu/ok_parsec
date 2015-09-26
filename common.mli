type state = {
  chan : Lwt_io.input_channel;
  buf : Buffer.t;
  eof : bool;
  pos : int;
}
val initState : Lwt_io.input_channel -> state
val input : ?len:int -> state -> state Lwt.t
