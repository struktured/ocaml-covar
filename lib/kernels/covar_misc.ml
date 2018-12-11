open !Import
module Kernel = Covar_kernel

module Price_time = struct
  type t = {price:(module Kernel.S_FLOAT) ;
            time:(module Kernel.S_FLOAT)
           } [@@deriving fields]
  let fields : ((module Kernel.S_FLOAT), t) 
      Covar_record.field list =
    let f = Fields.[price;time] in
    f

  let default =
    { price=(module Covar_matern);
      time=(module Covar_temporal)
    }

end

module type ENUM = sig
  type t [@@deriving sexp]
  val all : t list

  val kernel : t -> (module Kernel.S)
end


(** A kernel where each feature is defined by
    an enumeration value. Kernels are mapped to
    values as chosen by the module implementation.

    TODO speculatively useful. needs more thought
  *)
module Enum_kernel : ENUM = struct
  type t = [`Ethusd| `Btceth] [@@deriving enumerate, sexp]

  let kernel (_:[<t]) = (module Covar_matern:Kernel.S)

end


