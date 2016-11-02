module type S =
sig
  include module type of Core.Std.Float
  val two : t
end

module Core_float (* : S *) =
struct
 include Core.Std.Float
 let two = 2.
end

include Core_float

let default_epsilon = 1e-5

let compare ?(epsilon=default_epsilon) =
  Gsl_math.fcmp ~epsilon

let equals ?epsilon x x' =
  match compare ?epsilon x x' with 0 -> true | _ -> false
