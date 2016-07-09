(** Kerned-based predictive functions *)
open Core.Std
module Vec = Lacaml_D.Vec

(** A predictive kernel function. Requires a kernel type definition *)
module type S =
sig
type t
module Kernel : Omkl_kernel.S
module Instance = Kernel.Instance
val predict : t -> Instance.t -> float
(** Given a predictor and an instance, predict the output (target) value. *)
end

(** A buffered version of a predictive function, allowing
    adding of support instances to the model dynamically. *)
module Buffered = 
struct
module type S =
sig
  include S
  val empty : ?init_buffer_size:int -> ?bounded_buffer:bool -> Kernel.t -> t
  val add_support : t -> weight:float -> Instance.t -> t
end
module Make(Kernel:Omkl_kernel.S with module Instance = Omkl_instance.Float)(Predictor:S) :
  S with module Kernel = Kernel =
struct
  module Kernel = Kernel
  module Instance = Kernel.Instance
  module Instance_buffer = CCRingBuffer.Make(Instance)
  module Weights_buffer = CCRingBuffer.Make(Float)
  let default_buffer_size = 1000
  let default_bounded_buffer = false
  type t = {weights:Weights_buffer.t; instances:Instance_buffer.t; kernel:Kernel.t} [@@deriving make]

  let empty ?(init_buffer_size=default_buffer_size)
    ?(bounded_buffer=default_bounded_buffer) kernel = make
      ~weights:(Weights_buffer.create init_buffer_size ~bounded:bounded_buffer)
      ~instances:(Instance_buffer.create init_buffer_size ~bounded:bounded_buffer)
      ~kernel

  let predict (t:t) (x:Instance.t) =
    let instances : Float.t array = Instance_buffer.to_array t.instances in instances |>
      Array.map ~f:(fun x' -> Kernel.apply t.kernel x' x) |>
      Vec.of_array |> 
      Lacaml_D.dot (Weights_buffer.to_array t.weights |> Vec.of_array)

  let add_support (t:t) ~weight instance =
    if weight = 0. then t else
      begin
        Weights_buffer.push_back t.weights weight;
        Instance_buffer.push_back t.instances instance;
        t
      end
end
end
