(** Kerned-based predictive functions *)
open Core.Std
module Vec = Lacaml_D.Vec
module type S =
sig
type t
module Kernel : Omkl_kernels.S
module Instance = Kernel.Instance
val predict : t -> Instance.t -> float
end


module Make(Kernel:Omkl_kernels.S) :
  S with module Kernel = Kernel =
struct
  module Kernel = Kernel
  module Instance = Kernel.Instance
  type t = {weights:Vec.t; instances:Instance.t array; kernel:Kernel.t} [@@deriving make]

  let predict t x = Array.map t.instances
      ~f:(fun x' -> Kernel.apply t.kernel x' x) |>
      Vec.of_array |>
      Lacaml_D.dot t.weights

  let add_support t ~weight x =
    if weight = 0. then t else
      {t with
       weights = Vec.append t.weights (Lacaml_D.Vec.of_array [|weight|]);
       instances = Array.append t.instances [|x|]}
end
