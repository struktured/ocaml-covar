module type S =
sig
  include module type of CCFloat
  val two : t
end

module CCFloat (* : S *) =
struct
 include CCFloat
 let two = 2.
 open CCFloat.Infix
 let (+) = (+.)
 let (-) = (-.)
 let ( * ) = ( *.)
 let (/) = (/.)
 let one = 1.
end

include CCFloat

let default_epsilon = 1e-5

let compare ?(epsilon=default_epsilon) =
  Gsl_math.fcmp ~epsilon

let equals ?epsilon x x' =
  match compare ?epsilon x x' with 0 -> true | _ -> false





