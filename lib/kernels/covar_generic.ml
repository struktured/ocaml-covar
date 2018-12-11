open !Import
open Base

(** A weighted array of covariance functions over
    instance type ['a].
*)
type 'a covar =
  {weights:Lacaml.D.Vec.t; covars:('a -> 'a -> float) array}

module Optional_args(Instance:Instance.S)
  (*Optional_args.S with type t = Instance.t covar*) =
struct
  type t = Instance.t covar

  let default : t = {weights=Lacaml.D.Vec.empty;covars=[||]}
  module type KERNEL = Kernel.S with module Instance = Instance

  let create weights (kernels:('a * 'b) array) : t =
    {weights;
     covars=
       Array.map kernels ~f:
         (fun
           (type t)
           ((module K : KERNEL with type t = t),
            (t:t)
           ) ->
           fun x y -> K.covar t x y
         )
    }
end


module Make(I:Instance.S) :
  Kernel.S with
  module Instance = Instance.Array(I) and
module Optional_args = Optional_args(I) =
struct

  module Optional_args = Optional_args(I)
  type t = Optional_args.t
  let create ?(opt=Optional_args.default) () = opt

  module Instance = Instance.Array(I)

  let covar (t:t) (x:'a array) (x':'a array) =
    let dists = Array.mapi t.covars
        ~f:(fun i k -> k x.(i) x'.(i))
                |> Lacaml.D.Vec.of_array in
    Lacaml.D.dot t.weights dists

  let _update t weights =
    {t with weights}

end
