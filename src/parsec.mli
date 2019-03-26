type pos = int
type error = pos * string
type 'a reply = ('a * Common.state, error) Core_kernel.Result.t
type 'a parser = Common.state -> 'a reply Lwt.t
type 'a t = 'a parser
val any : char parser
val char : char -> char parser
val string : string -> string parser
val satisfy : (char -> bool) -> char parser
val regexp : Fa.sm -> string parser
val fail : string -> 'a parser
val return : 'a -> 'a parser
val bind :
  'a parser ->
  ('a -> 'b parser) ->
  'b parser
val ( >>= ) :
  'a parser ->
  ('a -> 'b parser) ->
  'b parser
val ( >> ) :
  'a parser ->
  'b parser ->
  'b parser
val ( << ) :
  'a parser ->
  'b parser ->
  'a parser
val ( |>> ) :
  'a parser ->
  ('a -> 'b) ->
  'b parser
val ( >>$ ) :
  'a parser ->
  'b ->
  'b parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val between :
  'a parser ->
  'b parser ->
  'c parser ->
  'c parser
val many : 'a parser -> 'a list parser
val many1 :
  'a parser ->
  'a list parser
val times : int -> 'a parser -> 'a list parser
val sepBy1 :
  'a parser ->
  'b parser ->
  'b list parser
val sepBy : 'a parser -> 'b parser -> 'b list parser
val sepEndBy : 'a parser -> 'b parser -> 'b list parser
val sepEndBy1 :
  'a parser ->
  'b parser ->
  'b list parser
val opt : 'a -> 'a parser -> 'a parser
val option : 'a parser -> 'a option parser
val lookAhead :
  'a parser ->
  'a parser
val followedBy :
  'a parser ->
  string ->
  unit parser
val notFollowedBy :
  'a parser ->
  string ->
  unit parser
val eof : unit parser
val int8 : int parser
val int16 : int parser
val int16_net : int parser
val int32 : Int32.t parser
val int32_net : Int32.t parser
val int64 : Int64.t parser
val int64_net : Int64.t parser
val num_dec : char parser
val num_bin : char parser
val num_oct : char parser
val num_hex : char parser
val lowercase : char parser
val uppercase : char parser
val parse_string : 'a parser -> string -> 'a reply Lwt.t
val parse_channel : 'a parser -> Lwt_io.input_channel -> 'a reply Lwt.t
val parse_fd : 'a parser -> Lwt_unix.file_descr -> 'a reply Lwt.t
