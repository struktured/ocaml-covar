module Level =
struct
  type t = [`Trace | `Debug | `Info | `Warn | `Error] [@@deriving sexp]
  let rank = function
    | `Trace -> 0 | `Debug -> 1 | `Info -> 2 | `Warn -> 3 | `Error -> 4
end

(* Logging facade with an optional level parameter.

   Logging is gated by a configurable threshold (default [`Warn]) so that
   diagnostic [info]/[log] calls on numerical hot paths (e.g. evaluating a
   Matern kernel in a GP loop) are cheap no-ops unless explicitly enabled
   via [set_level]. *)
module type S =
sig
 val set_level : Level.t -> unit
 val log  : ?level:Level.t -> ('a, Stdlib.out_channel, unit) format -> 'a
 val info : ('a, Stdlib.Format.formatter, unit) format -> 'a
end

module Printf : S =
struct
  let threshold = ref `Warn
  let set_level l = threshold := l
  let enabled level = Level.rank level >= Level.rank !threshold

  let log ?(level=`Info) s =
    if enabled level then Stdlib.Printf.printf s
    else Stdlib.Printf.ifprintf Stdlib.stdout s

  let info fmt =
    if enabled `Info then Stdlib.Format.printf fmt
    else Stdlib.Format.ifprintf Stdlib.Format.std_formatter fmt
end

include Printf
