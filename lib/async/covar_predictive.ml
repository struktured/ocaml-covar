open! Import
open! Covar_kernels.Std

module Predictive = Covar_base.Covar_predictive
module Base = Covar_base

module Make(Kernel:Kernel.S with module Instance = Instance.Float) =
struct

  module Predictive =
    Covar_base.Covar_predictive.Buffered.Make(Kernel)
  module Kernel = Kernel
  module Instance = Instance
  module Optional_args = Kernel.Optional_args

  type instance = Kernel.Instance.t [@@deriving sexp]
  type prediction = {value:float} [@@deriving sexp]
  type reader = Kernel.Instance.t Pipe.Reader.t
  type writer = prediction Pipe.Writer.t

  let create ?kernel_opt () =
    let kernel = Kernel.create ?opt:kernel_opt () in
    Predictive.empty
      ?init_buffer_size:None
      kernel

  let predict t x =
    let y =
      Predictive.predict t x in
    let t' = Predictive.add_support t ~weight:1.0 x in
    t', y

end

(*
module Multiple = struct

  type reader = Instance.Float_array.t Pipe.Reader.t

  let create vec kernels =
    Kernel.Optional_args.create
      vec kernels

  let predict t (reader:reader) =
    Pipe.folding_map reader ~init:t ~f:
      (fun t x ->
         Array.map2_exn x t
           ~f:
             (fun x (module P:S) ->
               let y =
                 P.predict t x in
             )
      )

end*)
