(** Records of kernels. *)
open !Import
module Kernel = Covar_kernel

type ('a, 't) field =
  ([ `Read | `Set_and_create ], 't, 'a) Field.t_with_perm

module type FIELDS = sig
  type t
  val fields :
    ([`Read | `Set_and_create], t,
     (module Kernel.S_FLOAT)) Field.t_with_perm list
end


module Predict = struct
  module type S = sig
    module Kernel : Kernel.S_FLOAT
    val predict : Instance.Float.t -> float
  end
  module Make(K:Kernel.S_FLOAT) : S with module Kernel = K =
  struct
    module Kernel = K
    let kernel = K.create ()
    module P = Predictive.Buffered.Make(Kernel)
    let predictive = P.empty kernel
    let predict = P.predict predictive
  end
end

module type S =
sig
  type t = {weights : Lacaml.D.Vec.t;
            predictives :
              (module Predict.S) array;
           }

  module Optional_args : sig
    module Fields : FIELDS
    type t = (Fields.t, (module Predictive.S)) List.Assoc.t
    include Kernel.Optional_args.S with type t:=t
  end

  module Instance = Instance.Float_array
  val predict : t -> Instance.t -> float
end

module Make(F:FIELDS) (*: S *) =
struct
  module Instance = Instance.Float_array
  type t = {weights : Lacaml.D.Vec.t;
            predictives :
              (module Predict.S) array;
           }

  let kernel (field:((module Kernel.S_FLOAT), F.t) field) =
    Field.get field

  module Optional_args (*:Optional_args.S with type t =
    (F.t, (module Predictive.S)) List.Assoc.t *) =
  struct
    module Fields = F
    type t = (F.t, (module Predictive.S)) List.Assoc.t
    let default : t = []
  end

  let _weights = Lacaml.D.Vec.create (List.length F.fields)
    let _map ~f t =
      List.map F.fields
        ~f:
          (fun field ->
            let k = kernel field t in
            f (Field.name field) k
          )

    let _map2_exn ~f t t' =
      List.map2_exn F.fields t'
        ~f:
          (fun field x ->
            let k = kernel field t in
            f (Field.name field) k x
          )

    let _fold ~(f:'acc -> 'a -> 'b) ~(init:'acc) t : 'acc =
      List.fold F.fields
        ~init
        ~f:
          (fun (acc:'acc) field x ->
            let k = kernel field t in
            f (acc:'acc) (Field.name field) k x
          )

    let create ?(opt=[]) () : t =
      let predictives = List.map opt
        ~f:
          (fun ((_field:F.t), (module K: Kernel.S_FLOAT)) ->
             let module P = Predict.Make(K) in
             (module P:Predict.S)
          ) |> Array.of_list in
      {predictives;
       weights=Lacaml.D.Vec.create (Array.length predictives)
      }

    let predict ({predictives;weights}:t) (x:Instance.t) : float =
      let predictions = Array.mapi predictives
        ~f:
          (fun i (module P:Predict.S) ->
            P.predict x.(i)
          ) |> Lacaml.D.Vec.of_array in
      Lacaml.D.dot weights predictions

end
