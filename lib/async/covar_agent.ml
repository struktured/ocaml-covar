open Core (* TODO put in jbuilder *)
open !Import
open Async_extended

let file_reader file ~f =
  Reader.with_file
    ?buf_len:None
    ?exclusive:None
    file
    ~f

let from_reader reader :
float array Pipe.Reader.t Deferred.t =
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
      Array.map ~f:Float.of_string
    )
  |> return


module Predictive = Covar_predictive
module Input = Instance.Float_array
let train ~file =
  file_reader file ~f:from_reader
  


