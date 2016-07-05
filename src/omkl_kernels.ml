open Core.Std
type vec = Lacaml_float64.vec
module Mat = Lacaml_D.Mat

module Instance =
struct
  module type S = sig type t end

  module Float : S with type t = float =
  struct
    type t = float
  end

  module Float_array : S with type t = Float.t array =
  struct
    type t = Float.t array
  end

  module Temporal_feature :
  sig
      type t ={temporal:Float_array.t;feature:Float_array.t}
      include S with type t := t
  end =
  struct
    type t = {temporal:Float_array.t;feature:Float_array.t}
  end
end

module type S = sig
  type t
  module Instance : Instance.S
  val apply : t -> Instance.t -> Instance.t -> float
end

module Temporal : S with module Instance = Instance.Float =
struct
  module Instance = Instance.Float
  type t = {cycle_len:float;bandwidth:float} [@@deriving show, make]

  let apply t x x' =
    let open Float in
    let dist = abs (x - x') in
    (min dist (t.cycle_len - dist)) ** 2.0
    /
    (2.0 * t.bandwidth ** 2.0) |> neg |> exp
end

module Squared_exponential : S with module Instance = Instance.Float =
struct
  module Instance = Instance.Float
  type t = {amplitude:float [@default 1.];bandwidth:float} [@@deriving show, make]
  let apply t x x' =
    let open Float in
    (x - x') ** 2.0
    /
    (2.0 * t.bandwidth ** 2.0) |> neg |> exp |> scale (t.amplitude ** 2.0)
end

module Multi(K:S with module Instance = Instance.Float) : S with module Instance = Instance.Float_array =
struct
  type t = {weights:vec; kernels: K.t array} [@@deriving make]
  module Instance = Instance.Float_array
  let apply t x x' =
    let dists = Array.mapi
        ~f:(fun i k -> K.apply k x.(i) x'.(i)) t.kernels |> Lacaml_D.Vec.of_array in
    Lacaml_D.dot t.weights dists
end

module Temporal_descriptor
    (Temporal_kernel:S with module Instance = Instance.Float)
    (Feature_kernel:S with module Instance = Instance.Float) :
  S with module Instance = Instance.Temporal_feature =
struct
   module Multi_temporal_kernel = Multi(Temporal_kernel)
   module Multi_feature_kernel = Multi(Feature_kernel)
   module Instance = Instance.Temporal_feature
   type t = {temporal_kernel:Multi_temporal_kernel.t;
             feature_kernel:Multi_feature_kernel.t} [@@deriving make]

   let apply t (x:Instance.t) (x':Instance.t) =
     let open Float in 
     (Multi_temporal_kernel.apply
       t.temporal_kernel x.Instance.temporal x'.Instance.temporal)
     *
     (Multi_feature_kernel.apply
        t.feature_kernel x.Instance.feature x'.Instance.feature)
end


