module Float = Covar_float

let maybe_power ?pow ~f =match pow with
  | None -> f
  | Some p -> fun x -> f x **. p

let sin = maybe_power ~f:Float.sin
let cos = maybe_power ~f:Float.cos

let sin_sq = sin ~pow:2.0
let cos_sq = cos ~pow:2.0

let pi = Gsl.Math.pi
