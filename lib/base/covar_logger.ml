module Level =
struct
type t = [`Trace | `Debug | `Info | `Warn | `Error]
end

(* Logging facade with optional level parameter *)
module type S =
sig
 val log  : ?level:Level.t -> ('a, Caml.out_channel, unit) format -> 'a
 val info : ('a, Caml.Format.formatter, unit) format -> 'a
end

module Printf : S =
struct
  let log ?level:_ s = Caml.Printf.printf s
  let info = Caml.Format.printf
end

include Printf
