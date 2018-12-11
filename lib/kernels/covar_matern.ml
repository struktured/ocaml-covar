open !Import

module Isotropic = Kernel.Stationary.Isotropic

module Matern_optional_args =
struct
    type t = {amplitude:float [@default 1.];bandwidth:float; v:float}
    [@@deriving make, sexp]
    let default = {amplitude=1.0;bandwidth=1.0;v=1.5}
end

module Stationary : Kernel.Stationary.S with
 module Optional_args = Matern_optional_args and
 module Instance = Instance.Float =
struct
  module K_v = Bessel
  module Instance = Instance.Float
  module Optional_args = Matern_optional_args

  type t = {amp_sqr:float; base:float; coeff:float; v:float} [@@deriving sexp]

let create ?(opt=Optional_args.default) () =
   let open Instance in
   let open Optional_args in
   let base = sqrt (two * opt.v) / opt.bandwidth in
   let pow_over_fact = (two ** (one - opt.v) / Gamma.gamma opt.v) in
   let amp_sqr = opt.amplitude * opt.amplitude in
   let coeff = pow_over_fact * amp_sqr in
   let v = opt.v in {amp_sqr;base;coeff;v}

let covar t x =
  if Float.equal x 0.0 then t.amp_sqr else
  let open Instance in
    t.coeff *
    x ** t.v *
    begin
    Logger.info "[matern covar] bessel_k (v=%f) (x=%f)\n" t.v x;
    Caml.(flush stdout);
    K_v.bessel_k ~nu:t.v x
    end
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
module type S = module type of Nonstationary
