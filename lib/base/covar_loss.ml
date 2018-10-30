(** Loss calculators over predictive functions *)


module Float = Covar_float

(** Interface for any loss function for a predictor.
    Requires a predictive function module and an instance of a predictor *)
module type S =
sig
type t
  module Instance : Covar_instance.S
  val loss : t -> Instance.t -> predicted:float -> actual:float -> float
end

(** Mean squared error loss function. For regression. *)
module MSE(Instance:Covar_instance.S) :
  S with module Instance = Instance =
struct
 type t
 module Instance = Instance
 let loss _t _x ~predicted ~actual =
   let open Float in
   (predicted - actual) **. 2.
end

(** Hinge loss error function. For classification. *)
module Hing_loss(Instance:Covar_instance.S) :
  S with module Instance = Instance =
struct
 type t
 module Instance = Instance
 let loss _t _x ~predicted ~actual =
   let open Float in
   max 0. (1. - actual*predicted)
end
