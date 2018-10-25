open !Import

module Brownian_optional_args =
struct
  type t = unit
  let default = ()
end


module Nonstationary  :
Kernel.S with
 module Optional_args = Brownian_optional_args and
 module Instance = Instance.Float =
struct
    module Optional_args = Brownian_optional_args
  open! Optional_args
  module Instance = Instance.Float
  type t = Optional_args.t
  include Kernel.Create(Optional_args)
  let covar (_t:t) x y = Float.min (Float.abs x) (Float.abs y)
end

include Nonstationary
