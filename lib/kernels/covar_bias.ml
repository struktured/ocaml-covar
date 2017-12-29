open !Import

module Bias_optional_args =
struct
  type t = {amplitude:float [@default 1.0] } [@@deriving make]
  let default = make ()
end


module Isotropic = Kernel.Stationary.Isotropic
module Wrap = Kernel.Stationary.Wrap(Instance.Float)(Isotropic)

module Stationary  :
Kernel.Stationary.S with
 module Optional_args = Bias_optional_args and
 module Instance = Instance.Float =
struct
    module Optional_args = Bias_optional_args
  open Optional_args
  module Instance = Instance.Float
  type t = {amp_sqr:Float.t}
  let create ?(opt=Optional_args.default) () =
    {amp_sqr=opt.amplitude*.opt.amplitude}
  let covar t (x:Instance.t) = t.amp_sqr
end

module Nonstationary : Kernel.Nonstationary.S with
 module Optional_args = Bias_optional_args and
 module Instance = Instance.Float =
struct
module Optional_args = Bias_optional_args
module Instance = Instance.Float
include (Kernel.Stationary.Wrap
  (Instance)(Isotropic)(Stationary) : Kernel.S with
  module Instance := Instance and
  module Optional_args := Optional_args)
end

include Nonstationary
