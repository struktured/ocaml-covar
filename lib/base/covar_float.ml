module type S =
sig
  include module type of Float
  val two : t

  val dummy : t
  val default_epsilon : t
end

module Float0 (* : S *) =
struct
 include Float
 let two = 2.
 let one = 1.
end

include Float0
let default_epsilon = 1e-5

(*
let compare ?(epsilon=default_epsilon) =
  Gsl.Math.fcmp ~epsilon

let equals ?epsilon x x' =
  match compare ?epsilon x x' with 0 -> true | _ -> false

*)

let dummy = 0.0
