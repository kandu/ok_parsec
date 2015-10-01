type pos = int
type error = pos * string
type 'a reply = ('a * Common.state, error) Core_kernel.Std.Result.t
type 'a parser = Common.state -> 'a reply Lwt.t
type 'a t = 'a parser
val any :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val char :
  char ->
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val string :
  Core_kernel.Std.String.t ->
  Common.state ->
  (string * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val satisfy :
  (char -> bool) ->
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val regexp :
  Fa.sm ->
  Common.state ->
  (Core_kernel.Std.String.t * Common.state, int * string)
  Core_kernel.Std.Result.t Lwt.t
val fail :
  'a -> Common.state -> ('b, int * 'a) Core_kernel.Std.Result.t Lwt.t
val return : 'a -> 'b -> ('a * 'b, 'c) Core_kernel.Std.Result.t Lwt.t
val bind :
  'a parser ->
  ('a -> 'b parser) ->
  Common.state -> ('b * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val ( >>= ) :
  'a parser ->
  ('a -> 'b parser) ->
  Common.state -> ('b * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val ( >> ) :
  'a parser ->
  'b parser ->
  Common.state -> ('b * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val ( << ) :
  'a parser ->
  'b parser ->
  Common.state -> ('a * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val ( |>> ) :
  'a parser ->
  ('a -> 'b) ->
  Common.state -> ('b * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val ( >>$ ) :
  'a parser ->
  'b ->
  Common.state -> ('b * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val between :
  'a parser ->
  'b parser ->
  'c parser ->
  Common.state -> ('c * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val many : 'a parser -> 'a list parser
val many1 :
  'a parser ->
  Common.state ->
  ('a list * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val times : int -> 'a parser -> 'a list parser
val sepBy1 :
  'a parser ->
  'b parser ->
  Common.state ->
  ('b list * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val sepBy : 'a parser -> 'b parser -> Common.state -> 'b list reply Lwt.t
val sepEndBy : 'a parser -> 'b parser -> 'b list parser
val sepEndBy1 :
  'a parser ->
  'b parser ->
  Common.state ->
  ('b list * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val opt : 'a -> 'a parser -> 'a parser
val option : 'a parser -> 'a option parser
val lookAhead :
  ('a -> ('b * 'a, 'c) Core_kernel.Std.Result.t Lwt.t) ->
  'a -> ('b * 'a, 'c) Core_kernel.Std.Result.t Lwt.t
val followedBy :
  (Common.state -> ('a, 'b) Core_kernel.Std.Result.t Lwt.t) ->
  'c ->
  Common.state ->
  (unit * Common.state, int * 'c) Core_kernel.Std.Result.t Lwt.t
val notFollowedBy :
  (Common.state -> ('a, 'b) Core_kernel.Std.Result.t Lwt.t) ->
  'c ->
  Common.state ->
  (unit * Common.state, int * 'c) Core_kernel.Std.Result.t Lwt.t
val eof :
  Common.state ->
  (unit * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val int8 :
  Common.state -> (int * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val int16 :
  Common.state -> (int * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val int16_net :
  Common.state -> (int * Common.state, error) Core_kernel.Std.Result.t Lwt.t
val int32 :
  Common.state ->
  (Core_kernel.Std.Int32.t * Common.state, error) Core_kernel.Std.Result.t
  Lwt.t
val int32_net :
  Common.state ->
  (Core_kernel.Std.Int32.t * Common.state, error) Core_kernel.Std.Result.t
  Lwt.t
val int64 :
  Common.state ->
  (Core_kernel.Std.Int64.t * Common.state, error) Core_kernel.Std.Result.t
  Lwt.t
val int64_net :
  Common.state ->
  (Core_kernel.Std.Int64.t * Common.state, error) Core_kernel.Std.Result.t
  Lwt.t
val num_dec :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val num_bin :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val num_oct :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val num_hex :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val lowercase :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val uppercase :
  Common.state ->
  (char * Common.state, int * string) Core_kernel.Std.Result.t Lwt.t
val parse_string : 'a parser -> string -> 'a reply Lwt.t
val parse_channel : 'a parser -> Lwt_io.input_channel -> 'a reply Lwt.t
