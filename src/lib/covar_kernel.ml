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

module type S =
sig
 type t
  module Optional_args : Optional_args.S
  val create : ?opt:Optional_args.t -> unit -> t
  module Instance : Instance.S
  val covar : t -> Instance.t -> Instance.t -> float
end


module Temporal_optional_args =
struct
  type t = {cycle_len:float [@default 1.0];
            bandwidth:float [@default 1.0]} [@@deriving make, show]
  let default = make ()
end

module Temporal : S with
 module Optional_args = Temporal_optional_args and
 module Instance = Instance.Float =
struct
  module Optional_args = Temporal_optional_args
  open Optional_args
  module Instance = Instance.Float
  type t = Optional_args.t
  include Create(Optional_args)
  let covar t x x' =
    let open Float in
    let dist = abs (x - x') in
    (min dist (t.cycle_len - dist)) ** 2.0
    /
   (2.0 * t.bandwidth ** 2.0) |> neg |> exp
end

module Squared_exponential_optional_args =
struct
  type t = {amplitude:float [@default 1.0];
            bandwidth:float [@default 1.0]} [@@deriving make, show]
  let default = make ()
end

module Squared_exponential : S with
  module Optional_args = Squared_exponential_optional_args and
  module Instance = Instance.Float =
struct

  module Optional_args = Squared_exponential_optional_args 
  open Optional_args
  module Instance = Instance.Float

  type t = Optional_args.t
  include Create(Optional_args)
  let covar t x x' =
    let open Instance in
    (x - x') ** two
    /
    (two * t.bandwidth ** two) |> neg |> exp |> scale (t.amplitude ** two)
end

module Matern_optional_args =
struct
    type t =
    {amplitude:float [@default 1.];bandwidth:float; v:float} [@@deriving make, show]
    let default = {amplitude=1.0;bandwidth=1.0;v=1.5}
end

module Matern : S with
 module Optional_args = Matern_optional_args and
 module Instance = Instance.Float =
struct
  module K_v = Covar_bessel
  module Instance = Instance.Float
  module Optional_args = Matern_optional_args

  type t = {base:float; coeff:float; v:float} [@@deriving show]

let create ?(opt=Optional_args.default) () =
   let open Instance in
   let open Optional_args in
   let base = sqrt (two * opt.v) / opt.bandwidth in
   let pow_over_fact = (two ** (one - opt.v) / Gamma.gamma opt.v) in
   let coeff = pow_over_fact * opt.amplitude * opt.amplitude in
   let v = opt.v in
   {base;coeff;v}

let covar t x x' =
  let open Instance in
  let scaled_d = (x - x') in
    t.coeff *
    scaled_d ** t.v *
    K_v.bessel_k ~v:(to_int t.v) scaled_d
end


module Multi_optional_args(K : S (*with module Instance = Instance.Float*)) =
struct
  type t = {weights:vec; kernels: K.t array} [@@deriving make]
  let default = {weights=Lacaml_D.Vec.empty; kernels=[||]}
end

module Multi(K : S (*with module Instance = Instance.Float*)) : S with
  module Optional_args = Multi_optional_args(K) and
  module Instance = Instance.Array(K.Instance) =
struct
  module Optional_args = Multi_optional_args(K)
  open Optional_args
  type t = Optional_args.t
  include Create(Optional_args)
  module Instance = Instance.Array(K.Instance)
  let covar t x x' =
    let dists = Array.mapi
        ~f:(fun i k -> K.covar k x.(i) x'.(i)) t.kernels
          |> Lacaml_D.Vec.of_array in
    Lacaml_D.dot t.weights dists
end

module Heterogeneous_optional_args
  (K1 : S )
  (K2 : S ) =
struct
type t = {weights:mat; kernel1: K1.t array; kernel2 : K2.t array}
           [@@deriving make]
let default = {weights=Lacaml_D.Mat.empty; kernel1=[||];kernel2=[||]}
end

module Heterogeneous_optional_array_args
  (I1:Instance.S)
  (I2:Instance.S)
  (K1:S with module Instance = Instance.Array(I1))
  (K2:S with module Instance = Instance.Array(I2)) =
struct
type t = {weights:mat; kernel1: K1.t array; kernel2 : K2.t array}
           [@@deriving make]
let default = {weights=Lacaml_D.Mat.empty; kernel1=[||];kernel2=[||]}
end


module Heterogeneous
 (I1: Instance.S)
 (I2: Instance.S)
 (K1 : S with module Instance = I1)
 (K2 : S with module Instance = I2) :
S with
  module Optional_args = Heterogeneous_optional_args(K1)(K2) and
  module Instance = Instance.Heterogeneous_array_feature(I1)(I2) =
struct
  module Optional_args = Heterogeneous_optional_args(K1)(K2)
  open Optional_args
  type t = Optional_args.t
  module Instance = Instance.Heterogeneous_array_feature(I1)(I2)
  include Create(Optional_args)
  let covar t x x' =
    let open Float in
    let sum = ref 0. in
    for i = 0 to Mat.dim1 t.weights do
      for j = 0 to Mat.dim2 t.weights do
        let w = t.weights.{i, j} in
        let k_1_dist = K1.covar t.kernel1.(i)
            x.Instance.feature1.(i) x'.Instance.feature1.(i) in
        let k_2_dist = K2.covar t.kernel2.(j)
            x.Instance.feature2.(j) x'.Instance.feature2.(j) in
        sum := !sum + w * k_1_dist * k_2_dist
      done
    done;
    !sum
end

(*
module Heterogeneous2
 (K1 : S)
 (K2 : S) :
S with
  module Optional_args = Heterogeneous_optional_args(K1)(K2) and
  module Instance = Instance.Heterogeneous_array_feature(K1.Instance)(K2.Instance) =
struct
  module Optional_args = Heterogeneous_optional_args(K1.Instance)(K2.Instance)
  open Optional_args
  type t = Optional_args.t
  module Instance = Instance.Heterogeneous_array_feature(K1.Instance)(K2.Instance)
  include Create(Optional_args)
  let covar t x x' =
    let open Float in
    let sum = ref 0. in
    for i = 0 to Mat.dim1 t.weights do
      for j = 0 to Mat.dim2 t.weights do
        let w = t.weights.{i, j} in
        let k_1_dist = K1.covar t.kernel1.(i)
            x.Instance.feature1.(i) x'.Instance.feature1.(i) in
        let k_2_dist = K2.covar t.kernel2.(j)
            x.Instance.feature2.(j) x'.Instance.feature2.(j) in
        sum := !sum + w * k_1_dist * k_2_dist
      done
    done;
    !sum
end
*)

module Normalized_heterogeneous_optional_args
  (K1:S)
  (K2:S) =
struct
type t = {weights:mat; kernel1: K1.t array; kernel2 : K2.t array}
           [@@deriving make]
let default = {weights=Lacaml_D.Mat.empty; kernel1=[||];kernel2=[||]}
end


module Normalized_heterogeneuos
 (I1: Instance.S)
 (I2: Instance.S)
 (K1 : S with module Instance = I1)
 (K2 : S with module Instance = I2) :
  S with
   module Optional_args = Heterogeneous_optional_args(K1)(K2) and
   module Instance = Instance.Heterogeneous_array_feature(I1)(I2) =
struct
   module Multi_K1 = Multi(K1)
   module Multi_K2 = Multi(K2)
   module Optional_args = Heterogeneous_optional_args(K1)(K2)
   type t = {kernel1:Multi_K1.t;kernel2:Multi_K2.t} [@@deriving make]
   let create ?(opt=Optional_args.default) () = failwith("nyi")
 
   module Instance = Instance.Heterogeneous_array_feature(I1)(I2)
  (* type t = {temporal_kernel:Multi_K1.t;
             feature_kernel:Multi_K2.t} [@@deriving make]
*)
   let covar t (x:Instance.t) (x':Instance.t) =
     let open Float in
     (Multi_K1.covar
       t.kernel1 x.Instance.feature1 x'.Instance.feature1)
     *
     (Multi_K2.covar
        t.kernel2 x.Instance.feature2 x'.Instance.feature2)
end
