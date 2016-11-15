module Level =
struct
type t = [`Trace | `Debug | `Info | `Warn | `Error]
end

(* Logging facade with optional level paremeter *) 
module type S =
sig
 val log  : ?level:Level.t -> ('a, out_channel, unit) format -> 'a
 val info : ('a, Format.formatter, unit) format -> 'a
end

module Printf : S =
struct
  let log ?(level:Level.t option) s = Printf.printf s
  let info = Format.printf
end

include Printf
