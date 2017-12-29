open !Import

module Homogeneous_optional_args(K : Kernel.S) =
struct
  type t = {weights:Kernel.vec; kernels: K.t array} [@@deriving make]
  let default = {weights=Lacaml.D.Vec.empty; kernels=[||]}
end

module Make(K : Kernel.S) : Kernel.S with
  module Optional_args = Homogeneous_optional_args(K) and
  module Instance = Instance.Array(K.Instance) =
struct
  module Optional_args = Homogeneous_optional_args(K)
  open Optional_args
  type t = Optional_args.t
  include Kernel.Create(Optional_args)
  module Instance = Instance.Array(K.Instance)
  let covar t x x' =
    let dists = Array.mapi
        (fun i k -> K.covar k x.(i) x'.(i)) t.kernels
          |> Lacaml.D.Vec.of_array in
    Lacaml.D.dot t.weights dists
end
