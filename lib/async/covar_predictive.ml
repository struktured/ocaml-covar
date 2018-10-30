open! Import

module Make(Kernel:Kernel.S with module Instance = Instance.Float) =
struct

  module Predictive =
    Covar_base.Covar_predictive.Buffered.Make(Kernel)

  type instance = Kernel.Instance.t (*[@@deriving sexp]*)
  type prediction = {value:float} [@@deriving sexp]
  type reader = Kernel.Instance.t Pipe.Reader.t
  type writer = prediction Pipe.Writer.t


  let create ?kernel_opt () =
    let kernel = Kernel.create ?opt:kernel_opt () in
    Predictive.empty
      ?init_buffer_size:None
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
