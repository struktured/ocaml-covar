open !Import

module Linear_optional_args =
struct
  type t = {bias:float [@default 0.0];
            amplitude:float [@default 1.0];
            offset:float [@default 0.0]} [@@deriving make]
  let default = make ()
end


module Nonstationary :
Kernel.S with
 module Optional_args = Linear_optional_args and
 module Instance = Instance.Float =
struct
  module Optional_args = Linear_optional_args
  open Optional_args
  module Instance = Instance.Float
  type t = {bias_sqr:float [@default 0.0];
            amp_sqr:float [@default 1.0];
            offset:float [@default 0.0]}

  let create ?(opt=Optional_args.default) () =
      let open Float in
      {bias_sqr=opt.bias*opt.bias;
       amp_sqr=opt.amplitude*opt.amplitude;
       offset=opt.offset}

  let covar (t:t) x y = let open Float in
    t.bias_sqr +
      t.amp_sqr * (x - t.offset) * (y - t.offset)
end

include Nonstationary
