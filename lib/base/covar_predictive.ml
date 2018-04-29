(** Kerned-based predictive functions *)
module Vec = Lacaml.D.Vec
module Float = Covar_float
(** A predictive kernel function. Requires a kernel type definition *)
module type S =
sig
type t
module Kernel : Covar_kernel.S
module Instance = Kernel.Instance

(** Given a predictor and an instance, predict the output (target) value. *)
val predict : t -> Instance.t -> float
end

(** A buffered version of a predictive function, allowing
    adding of support instances to the model dynamically.

    The buffer by default is a fixed size ring buffer, effectively
    causing a sliding window type of behavior.

    A future model might instead keep the N best samples,
    rather than N last (maybe with redis?)
*)
module Buffered =
struct
module type S =
sig
  include S
  val empty : ?init_buffer_size:int -> ?bounded_buffer:bool -> Kernel.t -> t
  val add_support : t -> weight:float -> Instance.t -> t
end

module Make(Kernel:Covar_kernel.S with
             module Instance = Covar_instance.Float) :
  S with module Kernel = Kernel =
struct
  module Kernel = Kernel
  module Instance = Kernel.Instance
  module Instance_buffer = CCRingBuffer.Make(Instance)
  module Weights_buffer = CCRingBuffer.Make(Covar_instance.Float)
  let default_buffer_size = 1000
  let default_bounded_buffer = false
  type t = {weights:Weights_buffer.t;
            instances:Instance_buffer.t;
            kernel:Kernel.t} [@@deriving make]

  let empty
      ?(init_buffer_size=default_buffer_size)
      ?(bounded_buffer=default_bounded_buffer) kernel = make
      ~weights:(Weights_buffer.create init_buffer_size)
      ~instances:(Instance_buffer.create init_buffer_size)
      ~kernel

  let predict (t:t) (x:Instance.t) =
    let instances : Float.t array =
      Instance_buffer.to_array t.instances in instances |>
      Array.map ~f:(fun x' -> Kernel.covar t.kernel x' x) |>
      Vec.of_array |>
      Lacaml.D.dot (Weights_buffer.to_array t.weights |> Vec.of_array)

  let add_support (t:t) ~(weight:float) instance =
    if Float.equal weight 0. then t else
      begin
        Weights_buffer.push_back t.weights weight;
        Instance_buffer.push_back t.instances instance;
        t
      end
end
end
