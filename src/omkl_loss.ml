(** Loss functions *)
open Core.Std
module type S =
sig
type t
module Predictive : Omkl_predictive.S
module Instance = Predictive.Instance
val loss : t -> Predictive.t -> Instance.t -> float -> float
end

module MSE(Predictive:Omkl_predictive.S) :
  S with module Predictive = Predictive =
struct
 type t
 module Predictive = Predictive
 module Instance = Predictive.Instance
 let loss t f x y =
   let open Float in
   let y_pred = Predictive.predict f x in
   (y_pred - y) ** 2.
end
