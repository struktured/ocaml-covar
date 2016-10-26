module Kernel = Covar_kernel
module Instance = Covar_instance.Float
module Float = Covar_float
module Temporal_optional_args =
struct
  type t = {cycle_len:float [@default 1.0];
            bandwidth:float [@default 1.0]}
            [@@deriving make, show]
  let default = make ()
end


module Isotropic = Kernel.Stationary.Isotropic
module Wrap = Kernel.Stationary.Wrap(Instance)(Isotropic)

module Stationary  :
Kernel.Stationary.S with
 module Optional_args = Temporal_optional_args and
 module Instance = Instance =
struct
    module Optional_args = Temporal_optional_args
  open Optional_args
  module Instance = Instance
  type t = Optional_args.t
  include Kernel.Create(Optional_args)
  let covar t dist =
    let open Float in
    (min dist (t.cycle_len - dist)) ** 2.0
    /
   (2.0 * t.bandwidth ** 2.0) |> neg |> exp
end
module Nonstationary : Kernel.Nonstationary.S with
 module Optional_args = Temporal_optional_args and
 module Instance = Instance =
struct
module Optional_args = Temporal_optional_args
module Instance = Instance
include (Wrap(Stationary) : Kernel.Nonstationary.S with
  module Instance := Instance and
  module Optional_args := Optional_args)
end
include (Nonstationary : 
  Kernel.Nonstationary.S with module Instance := Instance)
