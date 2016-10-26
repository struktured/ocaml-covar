open Core.Std
type vec = Lacaml_float64.vec
module Mat = Lacaml_D.Mat
type mat = Lacaml_D.mat
module Instance = Covar_instance
module Optional_args = Covar_optional_args
module Create = Optional_args.Create

module Gamma = Covar_gamma
module Array = struct
include Core.Std.Array
let map2i t1 t2 ~f = let cnt = ref 0 in Array.map2_exn t1 t2
  ~f:(fun e1 e2 -> let i = !cnt in
    let res = f i e1 e2 in cnt := i + 1; res)
end

module Nonstationary =
struct
  module type S =
  sig
    type t
    module Optional_args : Optional_args.S
    val create : ?opt:Optional_args.t -> unit -> t
    module Instance : Instance.S
    val covar : t -> Instance.t -> Instance.t -> float
  end
end

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

