open Core
open Async
open !Import

type prediction = {value:float} [@@deriving sexp]

type reader = float Pipe.Reader.t

(** A kernel over floats which has memory. The [predict]
    function retains information from previous states
    to aid future predictions *)
module type KERNEL = sig

  module Predictive :
    Covar_base.Covar_predictive.S

  include Covar_base.Covar_kernel.S with module Instance = Instance.Float
  val create :
    ?kernel_opt:Optional_args.t -> unit -> Predictive.t
  val predict :
    Predictive.t -> reader -> reader
end

module Make(Kernel:Kernel.S with module Instance = Instance.Float) : KERNEL =
struct

  include Kernel
  module Predictive =
    Covar_base.Covar_predictive.Buffered.Make(Kernel)

  type writer = prediction Pipe.Writer.t

  let create ?kernel_opt () =
    let kernel = Kernel.create ?opt:kernel_opt () in
    Predictive.empty
      ?init_buffer_size:None
      ?bounded_buffer:None
      kernel

  let predict t (reader:reader) =
    Pipe.folding_map reader ~init:t ~f:
      (fun t x ->
          let y =
            Predictive.predict t x in
          let t' = Predictive.add_support t ~weight:1.0 x in
          t', y
      )

end

type predictive =
  <
    predict : float -> prediction Deferred.t;
    add_support : weight:float -> float -> unit Deferred.t
  >

let make_predictive ~predict ~add_support : predictive =
  object method predict = predict method add_support = add_support end

type predictives =
  {
    predictives : predictive array;
    weights : Lacaml.D.Vec.t
  }

module Optional_args :
  Optional_args.S with type t = predictives =
struct
  module Instance = Instance.Float
  type t = predictives

  let default : t = {weights=Lacaml.D.Vec.empty;predictives=[||]}
  module type KERNEL = Kernel.S with module Instance = Instance

  let create weights kernels : t =
    let predictives =
      Array.map kernels ~f: (fun
          (type t)
          ((module K : KERNEL with type t = t),
           (t:t)
          ) ->
          let module P =
            Covar_base.Covar_predictive.Buffered.Make(K) in
          let k = K.create ?opt:None () in
          let p = P.empty
              ?init_buffer_size:None
              ?bounded_buffer:None
              k in
          make_predictive
            ~predict:
              (fun v ->
                 P.predict p v |> return
                 >>| fun value -> {value}
              )
           ~add_support:
            (fun ~weight v ->
              P.add_support p weight v |>
              return |> Deferred.ignore
            )
        ) in
    {weights;predictives}
end

(*
module T =
struct

  type instance = Instance.Float.t [@@derivign sexp]
  type prediction = {value:float} [@@deriving sexp]
  type reader = instance Pipe.Reader.t
  type writer = prediction Pipe.Writer.t

  let create (type opt) (module Kernel:Kernel.S with type Optional_args.t = opt) ?(kernel_opt:opt option) () =
    let kernel = Kernel.create ?opt:kernel_opt () in
    Predictive.empty
      ?init_buffer_size:None
      ?bounded_buffer:None
      kernel

  let predict t (reader:reader) =
    Pipe.folding_map reader ~init:t ~f:
      (fun t x ->
          let y =
            Predictive.predict t x in
          let t' = Predictive.add_support t ~weight:1.0 x in
          t', y
      )


end
*)
