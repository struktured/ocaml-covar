open Core
open Async
open !Import

module Make(Kernel:Kernel.S) =
struct

  type instance = Kernel.Instance.t (*[@@deriving sexp]*)
  type prediction = {value:float} [@@deriving sexp]
  type reader = Kernel.Instance.t Pipe.Reader.t
  type writer = prediction Pipe.Writer.t


  let predict = failwith "nyi"
end
