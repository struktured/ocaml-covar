open Kaputt.Abbreviations

open Printf

open Covar.Std
open Covar_kernels.Std
module type S_float =
sig
include Kernel.S with
  module Instance = Float
end

let some x = Some x

module Predicates =
struct

 let _compare ?epsilon ~target = fun ~actual x x' ->
  let cmp = Float.compare ?epsilon actual target in
  cmp

 let equal_to ?epsilon ~expected = fun ~actual x x' ->
  let cmp = _compare ~actual ?epsilon ~target:expected x x'
  in cmp = 0

 let positive = fun ~actual x x' ->
  let cmp = _compare ~actual ~target:0.0 x x'
  in cmp = 1

 let non_negative = fun ~actual x x' ->
  let cmp = _compare ~actual ~target:0.0 x x'
  in cmp >= 0

 let to_two_arg f = fun ~actual x x' ->
    f ~actual x
 let to_one_arg f = fun ~actual x ->
    f ~actual x x
end

module Test_params(K:Kernel.S) =
struct
module type S =
sig
  val title_tag : string
  val qualifier : string
  val opts : K.t option
end
end

module Make
(K:S_float)
(Test_params : Test_params(K).S) =
struct
 let default_trials = 100
 let min_float = -1e5
 let max_float = abs_float min_float
 let eval kernel x x' =
   printf "[pre] x=%f, x'=%f\n" x x';
   let actual = K.covar kernel x x' in
   printf "[post] x=%f, x'=%f, actual=%f\n" x x' actual;
   actual
let eval_x_x kernel x = eval kernel x x

let for_k_x_x ?kernel ?(trials=default_trials) ~qualifier ~title_tag ~pred =
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
   Test.make_random_test
            ~title:(sprintf "%s (%s): k(x,x)"  title_tag qualifier)
            ~nb_runs:trials
            (Gen.make_float min_float max_float)
            (eval_x_x kernel)
            [ Spec.always => fun (x,actual) -> pred ~actual x ]
let for_k_x_x' ?kernel ?(trials=default_trials) ~qualifier ~title_tag ~pred =
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
   Test.make_random_test
            ~title:(sprintf "%s (%s) : k(x,x') " title_tag qualifier)
            ~nb_runs:trials
            (Gen.zip2 (Gen.make_float min_float max_float)
                   (Gen.make_float min_float max_float))
            (fun (x,x') -> eval kernel x x')
            [ Spec.always => fun ((x,x'), actual) ->
              pred ~actual x x']

let symmetric ?kernel ?trials ~title_tag ~pred =
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
  for_k_x_x' ~kernel ?trials ~qualifier:"symmetric" ~title_tag
  ~pred:(fun ~actual x x' -> Float.equals (K.covar kernel x' x) actual)

let for_weighted_k_x_x' ?kernel ?(trials=default_trials) 
  ~qualifier ~title_tag ~pred =
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
   Test.make_random_test
            ~title:(sprintf "%s (%s) : k(x,x') " title_tag qualifier)
            ~nb_runs:trials
            (Gen.zip2 (Gen.make_float min_float max_float)
                   (Gen.make_float min_float max_float))
            (fun (x,x') -> eval kernel x x')
            [ Spec.always => fun ((x,x'), actual) ->
              pred ~actual x x']


(*      
let positive_semidefinite ?kernel ?trials ~title_tag =
  for_k_x_x' ?kernel ?trials ~qualifier:"symmetric" ~title_tag
  ~pred:(fun ~actual x x' -> pred ~actual x x' && pred ~actual x' x)
*)


end

(*
module Make(K: Kernel.S)(T:Test_params(K).S) =
struct
 module K = Squared_exponential
 module Comparison_test = Comparison_test(K)
 let kernel = K.create ()
 let one_when_same =
   Comparison_test.for_k_x_x' ~kernel
      ?trials:None ~title_tag:T.title_tag ~equal_to:T.equal_to
 let tests = [one_when_same]


end
*)
let equal_to_one _ _ = 1.0

module Squared_exponential_test = Make
  (Squared_exponential)
  (struct
    let equal_to = equal_to_one
    let title_tag = "Squared exponential"
    let opts = None
   end)

module Matern_test = Make
  (Matern)
  (struct
    let equal_to = equal_to_one
    let title_tag = "Matern"
    let opts = None
   end)


module Brownian_test = Make
  (Brownian)
  (struct
    let equal_to = Float.min
    let title_tag = "Brownian"
    let opts = None
   end)

module Periodic_test = Make
  (Periodic)
  (struct
    let equal_to = equal_to_one
    let title_tag = "Periodic"
    let opts = None
   end)


module Linear_test = Make
  (Linear)
  (struct
    let opts = some @@ Linear.Optional_args.make ~bias:0.0 ()
    let equal_to = 1.0
    let title_tag = "Periodic"
   end)



let () = Kaputt.Abbreviations.Test.run_tests @@
    List.concat [Squared_exponential_test.tests ; Matern_test.tests]
