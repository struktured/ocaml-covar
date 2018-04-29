open Core (* TODO put in jbuilder *)
open !Import
open Async_extended

let from_reader ~instance_of_string reader :
'instance array Pipe.Reader.t Deferred.t =
  Csv.fold_reader_to_pipe
    ?strip:None
    ~skip_lines:1
    ?quote:None
    ?header:None (* TODO set header *)
    ?on_invalid_row:None
    Csv.Row.builder reader |> fun pipe ->
    Pipe.map pipe ~f:(
      fun x ->
      Csv.Row.to_array x |>
      Array.map ~f:instance_of_string
    )
  |> return


module Predictive = Covar_predictive

module type OF_STRING_INSTANCE =
sig
  include Instance.S

  val of_string : string -> t
end

module Instance = Instance.Array(Instance.Float)

module Kernel = Generic.Make(Instance)

module P = Predictive.Make(Brownian)

let train ~file =
  let file_reader file ~f =
    Reader.with_file
      ?buf_len:None
      ?exclusive:None
      file
      ~f in
  file_reader file
    ~f:(from_reader
          ~instance_of_string:Float.of_string
       ) >>=
  fun reader -> return @@
  Pipe.map reader ~f:
    (fun row -> row
    
    )
(*
module Default =
  Make(struct include Instance.Float let of_string = Float.of_string end)
*)
