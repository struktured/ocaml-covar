(** Interface for instances from training sets. *)
module type S = sig type t end

module Float : S with type t = float =
struct
  type t = float
end

module Float_array : S with type t = Float.t array =
struct
  type t = Float.t array
end

module Temporal_feature :
sig
  type t ={temporal:Float_array.t;feature:Float_array.t}
      include S with type t := t
end =
struct
  type t = {temporal:Float_array.t;feature:Float_array.t}
end
