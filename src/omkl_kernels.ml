open Core.Std
type vec = Lacaml_float64.vec
module Mat = Lacaml_D.Mat
type mat = Lacaml_D.mat
module Instance = Omkl_instance

module Array = struct
include Core.Std.Array
let map2i t1 t2 ~f = let cnt = ref 0 in Array.map2_exn t1 t2 
  ~f:(fun e1 e2 -> let i = !cnt in
    let res = f i e1 e2 in cnt := i + 1; res)
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
 (Temporal_kernel :S with module Instance = Instance.Float)
 (Feature_kernel: S with module Instance = Instance.Float) : S with module Instance = Instance.Temporal_feature =
struct
  type t = {weights:mat;
    temporal_kernels: Temporal_kernel.t array;
    feature_kernels:Feature_kernel.t array} [@@deriving make]
  module Instance = Instance.Temporal_feature
  let apply t x x' =
    let open Float in
    let sum = ref 0. in
    for i = 0 to Mat.dim1 t.weights do
      for j = 0 to Mat.dim2 t.weights do
        let w = t.weights.{i, j} in
        let k_t_dist = Temporal_kernel.apply t.temporal_kernels.(i)
            x.Instance.temporal.(i) x'.Instance.temporal.(i) in
        let k_f_dist = Feature_kernel.apply t.feature_kernels.(j)
            x.Instance.feature.(j) x'.Instance.feature.(j) in
        sum := !sum + w * k_t_dist * k_f_dist
      done
    done;
    !sum
end

module Normalized_temporal_descriptor
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
