open !Import
module Isotropic = Kernel.Stationary.Isotropic
module Squared_exponential_optional_args =
struct
  type t = {amplitude:float [@default 1.0];
            bandwidth:float [@default 1.0]} [@@deriving make, show]
  let default = make ()
end

module Stationary : Kernel.Stationary.S with
  module Optional_args = Squared_exponential_optional_args and
  module Instance = Instance.Float =
struct
  module Optional_args = Squared_exponential_optional_args
  open Optional_args
  module Instance = Instance.Float

  type t = Optional_args.t
  include Kernel.Create(Optional_args)
  let covar t x =
    let open Instance in
    x ** two
    /
    (two * t.bandwidth ** two) |> neg |> exp |> scale (t.amplitude ** two)
end

module Nonstationary : Kernel.Nonstationary.S with
 module Optional_args = Squared_exponential_optional_args and

module Instance = Instance.Float =
struct
module Optional_args = Squared_exponential_optional_args
module Instance = Instance.Float
include (Kernel.Stationary.Wrap
  (Instance)(Isotropic)(Stationary) : Kernel.S with
  module Instance := Instance and
  module Optional_args := Optional_args)
end

include Nonstationary

