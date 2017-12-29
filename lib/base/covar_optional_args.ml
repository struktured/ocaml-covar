module type S =
sig
  type t
  val default : t
end

module Create(Default:S) =
struct
  let create ?(opt=Default.default) () = opt
end

