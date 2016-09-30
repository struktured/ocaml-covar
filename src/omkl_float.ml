module type S =
sig
  include module type of Core.Std.Float
  val two : t
end

module Core_float (* : S *) =
struct
 include Core.Std.Float
 let two = 2.
end

include Core_float
