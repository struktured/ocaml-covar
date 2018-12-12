(** Interface for instances from training sets. *)
module type S = sig type t val dummy : t end

module Float (* : S with type t = float *) =
struct
  include (Covar_float : Covar_float.S)
  let dummy = 0.0
end

module Array(T:S) : S with type t = T.t array =
struct
  type t = T.t array
  let dummy = [||]
end

module Float_array : S with type t = Float.t array =
struct
  include Array(Float)
  let dummy = [||]
end

module Heterogeneous_feature(I1 : S) (I2 : S) :
sig(*
  module I1 : S
  module I2 : S*)
  type t = {feature1:I1.t;feature2:I2.t}
  include S with type t := t
end =
struct (*
  module I1 = I1
  module I2 = I2 *)
  type t = {feature1:I1.t;feature2:I2.t}
  let dummy = {feature1=I1.dummy;feature2=I2.dummy}
end

module Heterogeneous_array_feature(T1 : S) (T2 : S) :
sig (*
  module I1 : module type of Array(T1)
  module I2 : module type of Array(T2) *)
  type t = {feature1:T1.t array;feature2:T2.t array}

  include S with type t := t


end =
struct
   module I11 = Array(T1)
   module I22 = Array(T2)
   module H = Heterogeneous_feature(I11)(I22)
   include H
 (*  include (H : module type of H with module I1 := I1 and module I2 := I2) *)
end

type float_array2 =
  {feature1:Float_array.t;feature2:Float_array.t}

module Float_array2 :
sig
  type t = {feature1:Float_array.t;feature2:Float_array.t}
  include S with type t := t
end =
struct
  type t = {feature1:Float_array.t;feature2:Float_array.t}
  let dummy = {feature1=[||];feature2=[||]}
end

