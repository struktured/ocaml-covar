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

(* Epsilon-tolerant comparison (relative, via gsl_fcmp). Kept distinct from
   [compare] so the module still satisfies Covar_float.S (Base.Float's compare). *)
let compare_eps ?(epsilon=default_epsilon) x x' =
  Gsl.Math.fcmp ~epsilon x x'

let equals ?epsilon x x' =
  match compare_eps ?epsilon x x' with 0 -> true | _ -> false

let dummy = 0.0
