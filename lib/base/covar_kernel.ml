module Float = Covar_float
type vec = Lacaml.D.Vec.t
module Mat = Lacaml.D.Mat
type mat = Lacaml.D.mat
module Instance = Covar_instance
module Optional_args = Covar_optional_args
module Create = Optional_args.Create

module Gamma = Covar_gamma
module Array = struct
include CCArray
let map2i t1 t2 ~f = let cnt = ref 0 in map2
  (fun e1 e2 -> let i = !cnt in
    let res = f i e1 e2 in cnt := i + 1; res) t1 t2

end

(** Nonstationary kernels. It accordingly requires
    two points to compute the covariance.
*)
module Nonstationary =
struct
  module type S =
  sig
    type t
    module Optional_args : Optional_args.S
    val create : ?opt:Optional_args.t -> unit -> t
    module Instance : Instance.S

    (** [covar k x x'] applies the covariance function k(x,x')
        to produce the covarience of instances [x] and [x'].
    *)
    val covar : t -> Instance.t -> Instance.t -> float
  end
end

(* Stationary kernels compute the covariance using
   only the distance between two instances.
*)
module Stationary =
struct
  module type S =
  sig
    type t
    module Optional_args : Optional_args.S
    val create : ?opt:Optional_args.t -> unit -> t
    module Instance : Instance.S
    val covar : t -> Instance.t -> float
  end

  module Distance(I:Instance.S) =
  struct
    module type S = sig val convert : I.t -> I.t -> I.t end
  end

  module Anisotropic : Distance(Instance.Float).S =
  struct
    let convert x x' = x -. x'
  end

  (** Kernels such that [covar(x,y) = covar(y,x)] and [covar(x,y) =
      f(|x - y|)] for some covariance function [f] over the distance
      between [x] and [y].
  *)
  module Isotropic : Distance(Instance.Float).S =
  struct
    let convert x x' = Anisotropic.convert x x' |> Float.abs
  end

  module Wrap
    (I:Instance.S)
    (D:Distance(I).S)
    (K:S with module Instance = I) :
  Nonstationary.S with type t = K.t and
   module Instance = I and
   module Optional_args = K.Optional_args =
  struct
    module Optional_args = K.Optional_args
    type t = K.t
    let create = K.create
    module Instance = K.Instance
    let covar t x x' = D.convert x x' |> K.covar t
  end

end

module type S = Nonstationary.S

module type S_FLOAT = Nonstationary.S with module Instance = Instance.Float
