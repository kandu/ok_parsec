type pos = int
type error = pos * string
type 'a reply = Failed of error | Ok of 'a * Common.state
type 'a parser = Common.state -> 'a reply Lwt.t
type 'a t = 'a parser
val char : char -> Common.state -> char reply Lwt.t
val string : string -> Common.state -> string reply Lwt.t
val satisfy : (char -> bool) -> Common.state -> char reply Lwt.t
val regexp : Fa.sm -> Common.state -> string reply Lwt.t
val fail : string -> Common.state -> 'a reply Lwt.t
val return : 'a -> Common.state -> 'a reply Lwt.t
val bind : 'a parser -> ('a -> 'b parser) -> Common.state -> 'b reply Lwt.t
val ( >>= ) :
  'a parser -> ('a -> 'b parser) -> Common.state -> 'b reply Lwt.t
val ( >> ) : 'a parser -> 'b parser -> Common.state -> 'b reply Lwt.t
val ( << ) : 'a parser -> 'b parser -> Common.state -> 'a reply Lwt.t
val ( |>> ) : 'a parser -> ('a -> 'b) -> Common.state -> 'b reply Lwt.t
val ( >>$ ) : 'a parser -> 'b -> Common.state -> 'b reply Lwt.t
val ( <|> ) : 'a parser -> 'a parser -> Common.state -> 'a reply Lwt.t
val between :
  'a parser -> 'b parser -> 'c parser -> Common.state -> 'c reply Lwt.t
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> Common.state -> 'a list reply Lwt.t
val sepBy1 : 'a parser -> 'b parser -> Common.state -> 'b list reply Lwt.t
val sepBy : 'a parser -> 'b parser -> Common.state -> 'b list reply Lwt.t
val sepEndBy : 'a parser -> 'b parser -> 'b list parser
val sepEndBy1 : 'a parser -> 'b parser -> Common.state -> 'b list reply Lwt.t
val opt : 'a -> 'a parser -> Common.state -> 'a reply Lwt.t
val option : 'a parser -> Common.state -> 'a option reply Lwt.t
val lookAhead :
  (Common.state -> 'a reply Lwt.t) -> Common.state -> 'a reply Lwt.t
val followedBy :
  (Common.state -> 'a reply Lwt.t) ->
  string -> Common.state -> unit reply Lwt.t
val notFollowedBy :
  (Common.state -> 'a reply Lwt.t) ->
  string -> Common.state -> unit reply Lwt.t
val eof : Common.state -> unit reply Lwt.t
val num_dec : Common.state -> char reply Lwt.t
val num_bin : Common.state -> char reply Lwt.t
val num_oct : Common.state -> char reply Lwt.t
val num_hex : Common.state -> char reply Lwt.t
val lowercase : Common.state -> char reply Lwt.t
val uppercase : Common.state -> char reply Lwt.t
val parse_string : (Common.state -> 'a) -> string -> 'a
