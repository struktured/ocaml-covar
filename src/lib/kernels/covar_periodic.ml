open Covar.Std
module Isotropic = Kernel.Stationary.Isotropic
module Periodic_optional_args =
struct
  type t = {amplitude:float [@default 1.0]; period:float [@default 0.5];
            bandwidth:float [@default 1.0]} [@@deriving make, show]
  let default = make ()
end

module Stationary : Kernel.Stationary.S with
  module Optional_args = Periodic_optional_args and
  module Instance = Instance.Float =
struct
  module Optional_args = Periodic_optional_args
  open Optional_args
  module Instance = Instance.Float

  type t = {amp_sqr:float [@default 1.0]; period:float [@default 0.5];
            bandwidth_sqr:float [@default 1.0]} [@@deriving make, show]

  let create ?(opt=Optional_args.default) () =
    let open Float in
    {amp_sqr = opt.amplitude**two;
     period=opt.period;
     bandwidth_sqr=opt.bandwidth**two}
  let covar t x =
    let open Instance in
    let x' = Trig.pi * x / t.period in
    two * Trig.sin ~pow:two x'
    /
    (t.bandwidth_sqr)
    |> neg |> exp |> scale t.amp_sqr
end

module Nonstationary : Kernel.Nonstationary.S with
 module Optional_args = Periodic_optional_args and

module Instance = Instance.Float =
struct
module Optional_args = Periodic_optional_args
module Instance = Instance.Float
include (Kernel.Stationary.Wrap
  (Instance)(Isotropic)(Stationary) : Kernel.S with
  module Instance := Instance and
  module Optional_args := Optional_args)
end

include Nonstationary

