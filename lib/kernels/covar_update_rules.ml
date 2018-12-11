open !Import

module Loss = Covar_loss

(** An update rule produces a new weight
 from the previous weight and the prediction accuracy
 of a particular observation instance. *)
module type S =
sig
  module Instance : Instance.S
  module Loss : Loss.S with module Instance = Instance
  module Optional_args : Optional_args.S
  val update :
    ?opt:Optional_args.t ->
    loss:Loss.t ->
    predicted:Instance.t ->
    actual:Instance.t ->
    prev_weight:float ->
    Instance.t ->
    float
end

type hedge_loss_args =
  {hedge_loss_exponent:float} [@@deriving sexp]

module Hedge_loss_args :
  Optional_args.S with type t = hedge_loss_args =
struct
  type t = hedge_loss_args [@@deriving sexp]
  let default = {hedge_loss_exponent=0.8};
end

module Hedge_loss
    (Loss:Loss.S with module Instance = Instance.Float) :
  S with
  module Instance = Instance.Float and
  type Optional_args.t = hedge_loss_args =
  struct
    module Instance = Instance.Float
    module Loss = Loss
    module Optional_args = Hedge_loss_args
    let update ?(opt:Optional_args.t option) ~loss
        ~predicted ~actual ~prev_weight
        instance =
      let {hedge_loss_exponent} =
        Option.value opt ~default:Optional_args.default in
      let loss = Loss.loss loss instance ~predicted ~actual in
      (prev_weight *. hedge_loss_exponent) **. loss
  end
