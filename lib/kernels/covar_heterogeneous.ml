open !Import

module Homogeneous = Covar_homogeneous
module Mat = Kernel.Mat

module Heterogeneous_optional_args
  (K1 : Kernel.S )
  (K2 : Kernel.S ) =
struct
type t = {weights:Kernel.mat; kernel1: K1.t array; kernel2 : K2.t array}
           [@@deriving make]
let default = {weights=Lacaml.D.Mat.empty; kernel1=[||];kernel2=[||]}
end

module Heterogeneous_optional_array_args
  (I1:Instance.S)
  (I2:Instance.S)
  (K1:Kernel.S with module Instance = Instance.Array(I1))
  (K2:Kernel.S with module Instance = Instance.Array(I2)) =
struct
type t = {weights:Kernel.mat; kernel1: K1.t array; kernel2 : K2.t array}
           [@@deriving make]
let default = {weights=Lacaml.D.Mat.empty; kernel1=[||];kernel2=[||]}
end


module Heterogeneous
 (I1: Instance.S)
 (I2: Instance.S)
 (K1 : Kernel.S with module Instance = I1)
 (K2 : Kernel.S with module Instance = I2) :
Kernel.S with
  module Optional_args = Heterogeneous_optional_args(K1)(K2) and
  module Instance = Instance.Heterogeneous_array_feature(I1)(I2) =
struct
  module Optional_args = Heterogeneous_optional_args(K1)(K2)
  open Optional_args
  type t = Optional_args.t
  module Instance = Instance.Heterogeneous_array_feature(I1)(I2)
  include Kernel.Create(Optional_args)
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
  (K1:Kernel.S)
  (K2:Kernel.S) =
struct
type t = {weights:Kernel.mat; kernel1: K1.t array; kernel2 : K2.t array}
           [@@deriving make]
let default = {weights=Lacaml.D.Mat.empty; kernel1=[||];kernel2=[||]}
end


module Normalized_heterogeneuos
 (I1: Instance.S)
 (I2: Instance.S)
 (K1 : Kernel.S with module Instance = I1)
 (K2 : Kernel.S with module Instance = I2) :
  Kernel.S with
   module Optional_args = Heterogeneous_optional_args(K1)(K2) and
   module Instance = Instance.Heterogeneous_array_feature(I1)(I2) =
struct
   module Homogeneous_K1 = Homogeneous.Make(K1)
   module Homogeneous_K2 = Homogeneous.Make(K2)
   module Optional_args = Heterogeneous_optional_args(K1)(K2)
   type t = {kernel1:Homogeneous_K1.t;kernel2:Homogeneous_K2.t} [@@deriving make]
   let create ?(opt=Optional_args.default) () = failwith("nyi")
 
   module Instance = Instance.Heterogeneous_array_feature(I1)(I2)
  (* type t = {temporal_kernel:Homogeneous_K1.t;
             feature_kernel:Homogeneous_K2.t} [@@deriving make]
*)
   let covar t (x:Instance.t) (x':Instance.t) =
     let open Float in
     (Homogeneous_K1.covar
       t.kernel1 x.Instance.feature1 x'.Instance.feature1)
     *
     (Homogeneous_K2.covar
        t.kernel2 x.Instance.feature2 x'.Instance.feature2)
end
