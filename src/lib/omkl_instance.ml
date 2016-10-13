(** Interface for instances from training sets. *)
module type S = sig type t end

module Float (* : S with type t = float *) =
struct
  include Omkl_float
end

module Array(T:S) : S with type t = T.t array =
struct
  type t = T.t array
end

module Float_array : S with type t = Float.t array =
struct
  include Array(Float)
end

module Heterogeneous_feature(I1 : S) (I2 : S) :
sig
  module I1 : S
  module I2 : S
  type t ={feature1:I1.t;feature2:I2.t}
  include S with type t := t
end =
struct
  module I1 = I1
  module I2 = I2
  type t = {feature1:I1.t;feature2:I2.t}
end

type float_array2 = {feature1:Float_array.t;feature2:Float_array.t}

module Float_array2 :
sig
  type t = {feature1:Float_array.t;feature2:Float_array.t}
  include S with type t := t
end =
struct
  type t = {feature1:Float_array.t;feature2:Float_array.t}
end

