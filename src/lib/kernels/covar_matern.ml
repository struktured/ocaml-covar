open Covar.Std
module Isotropic = Kernel.Stationary.Isotropic

module Matern_optional_args =
struct
    type t = {amplitude:float [@default 1.];bandwidth:float; v:float}
      [@@deriving make, show]
    let default = {amplitude=1.0;bandwidth=1.0;v=1.5}
end

module Stationary : Kernel.Stationary.S with
 module Optional_args = Matern_optional_args and
 module Instance = Instance.Float =
struct
  module K_v = Bessel
  module Instance = Instance.Float
  module Optional_args = Matern_optional_args

  type t = {base:float; coeff:float; v:float} [@@deriving show]

let create ?(opt=Optional_args.default) () =
   let open Instance in
   let open Optional_args in
   let base = sqrt (two * opt.v) / opt.bandwidth in
   let pow_over_fact = (two ** (one - opt.v) / Gamma.gamma opt.v) in
   let coeff = pow_over_fact * opt.amplitude * opt.amplitude in
   let v = opt.v in
   {base;coeff;v}

let covar t x =
  let open Instance in
    t.coeff *
    x ** t.v *
    K_v.bessel_k ~v:(to_int t.v) x
end

module Nonstationary : Kernel.Nonstationary.S with
 module Optional_args = Matern_optional_args and
 module Instance = Instance.Float =
struct
module Optional_args = Matern_optional_args
module Instance = Instance.Float
include (Kernel.Stationary.Wrap
  (Instance)(Isotropic)(Stationary) : Kernel.S with
  module Instance := Instance and
  module Optional_args := Optional_args)
end

include Nonstationary

