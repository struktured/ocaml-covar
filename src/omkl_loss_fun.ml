(** Loss functions *)
open Core.Std
module type S =
sig
type t
module Kernel : Omkl_kernels.S
val loss : t -> Kernel.t -> Kernel.Instance.t -> float -> float
end

module MSE(Kernel:Omkl_kernels.S) : S =
struct
 type t
 module Kernel = Kernel
 let loss t k i y =
   let open Float in
   (* add dot product to kernel function *)
   (Kernel.apply k i - y) ** 2.0
end
