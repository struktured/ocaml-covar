open Kaputt.Abbreviations
module Array = Core.Std.Array
open Covar.Std
open Covar_kernels.Std
module type S_float =
sig
include Kernel.S with
  module Instance = Float
end
let sprintf = Printf.sprintf
let some x = Some x

let debug () = false

let printf = Logger.info

let _ = begin Gsl_error.handler := function
| Gsl_error.EOVRFLW
| Gsl_error.EUNDRFLW ->
  (fun message -> printf "(over|under)flow condition: %s\n" message)
| e -> fun message -> failwith (sprintf "Gsl_error(%s): %s"
        (Gsl_error.strerror e) message)
end
module Predicates =
struct

 let _compare ?epsilon ~target = fun ~actual x x' ->
  if debug () then printf "[cmp (actual=%f) (target=%f)]\n" actual target;
  let cmp = Float.compare ?epsilon actual target in
  cmp

 let equal_to ?epsilon ~expected = fun ~actual x x' ->
  if debug () then printf "[equal_to (actual=%f) (expected=%f)]\n" actual expected;
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
  val kernel_name : string
  val opts : K.Optional_args.t option
end
end

module Make_test_builder
(K:S_float)
=
struct
 module G = Kaputt.Generator
 let default_trials = 100
 let min_float = -1e5
 let max_array_len = 5
 let min_array_len = 0
 let max_float = abs_float min_float
 let float_gen () = G.make_float min_float max_float
 let float_tuple_gen () = G.zip2 (float_gen()) (float_gen())

 let array_gen
   ?(min_len=min_array_len)
   ?(max_len=max_array_len) gen = G.array
   (G.make_int min_len (max_len+1))
   gen
 let float_array_gen ?min_len ?max_len () =
   array_gen ?min_len ?max_len (float_gen())
 let float_tuple_array_gen ?min_len ?max_len () =
   array_gen ?min_len ?max_len (float_tuple_gen())
 let eval ~test_name ~kernel_name kernel x x' =
  if debug () then printf "[pre] x=%f, x'=%f\n" x x';
  let actual = K.covar kernel x x' in
  if debug () then printf "[post (test=%s) (kernel=%s)] actual=%f\n" test_name kernel_name actual;
  if debug () then printf "[post (test=%s) (kernel=%s)] x=%f, x'=%f, actual=%f\n" test_name kernel_name x x' actual;
  actual
let eval_x_x ~test_name ~kernel_name kernel x = eval ~kernel_name ~test_name kernel x x

let for_k_x_x ?kernel ?(trials=default_trials) ~test_name ~kernel_name ~pred =
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
   Test.make_random_test
            ~title:(sprintf "%s (kernel=%s) (%s): k(x,x)" kernel_name kernel_name test_name)
            ~nb_runs:trials
            (Gen.make_float min_float max_float)
            (eval_x_x ~test_name ~kernel_name kernel)
            [ Spec.always => fun (x,actual) -> pred ~actual x ]
let positive_k_x_x ?kernel ?trials ~kernel_name =
   for_k_x_x ?kernel ?trials ~test_name:"positive for k(x,x)"
   ~kernel_name ~pred:(Predicates.(to_one_arg positive))
let for_k_x_x' ?kernel ?(trials=default_trials) 
   ~kernel_name ~test_name ~pred =
   if debug () then printf "[for_k_x_x'] kernel=%s\n" kernel_name;
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in begin
   Test.make_random_test
            ~title:(sprintf "%s (%s) : k(x,x') " kernel_name test_name)
            ~nb_runs:trials
            (Gen.zip2 (Gen.make_float min_float max_float)
                   (Gen.make_float min_float max_float))
            (fun (x,x') -> eval ~test_name ~kernel_name kernel x x')
            [ Spec.always => fun ((x,x'), actual) ->
              pred ~actual x x']
    end
let symmetric ?kernel ?trials ~kernel_name =
   if debug() then printf "[symmetric] kernel_name=%s\n" kernel_name;
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
  for_k_x_x' ~kernel ?trials ~test_name:"symmetric" ~kernel_name
  ~pred:(fun ~actual x x' -> Float.equals (K.covar kernel x' x) actual)

let for_weighted_k_x_x' ?kernel ?(trials=default_trials)
  ~kernel_name ~test_name ~pred =
   let kernel = match kernel with
       | None -> K.create() | Some k -> k in
   let module G = Kaputt.Generator in
   let gen = float_tuple_array_gen () in
   Test.make_random_test
            ~title:(sprintf "%s (%s) : k(x,x') " kernel_name test_name)
            ~nb_runs:trials
            gen
   (fun samples ->
     let prod = Array.cartesian_product samples samples in
     let weighted = Array.map ~f:(fun ((x, w), (x', w')) -> let open Float in
           w * w' * eval ~test_name ~kernel_name kernel x x') prod in
     Array.fold ~f:Float.add ~init:0.0 weighted)
    [ Spec.always => fun (input, sum) ->
            pred ~sum input]


let positive_semidefinitive ?kernel ?trials ~kernel_name =
  for_weighted_k_x_x' ?kernel ?trials ~kernel_name
  ~test_name:"positive semi-definite"
  ~pred:(fun ~sum input -> sum >= 0.0)

end

let equal_to_one _ _ = 1.0

module Make_test_suite
(K:S_float)
(Test_params : Test_params(K).S) =
struct
module Test_builder = Make_test_builder(K)
let kernel = K.create ?opt:Test_params.opts ()
let trials = None 
let kernel_name = Test_params.kernel_name
let positive_k_x_x_test = Test_builder.positive_k_x_x
  ~kernel ?trials ~kernel_name
let symmetric_test = 
   if debug () then printf "[symmetric_test] kerenel=%s" kernel_name;
    Test_builder.symmetric
  ~kernel ?trials ~kernel_name
let positive_semidef_test = Test_builder.positive_semidefinitive
  ~kernel ?trials ~kernel_name
let tests = [positive_k_x_x_test; positive_semidef_test; symmetric_test]
end

module Squared_exponential_test = Make_test_suite
  (Squared_exponential)
  (struct
    let equal_to = equal_to_one
    let kernel_name = "Squared exponential"
    let opts = None
   end)

module Matern_test = Make_test_suite
  (Matern)
  (struct
    let equal_to = equal_to_one
    let kernel_name = "Matern"
    let opts = None
   end)


module Brownian_test = Make_test_suite
  (Brownian)
  (struct
    let equal_to = Float.min
    let kernel_name = "Brownian"
    let opts = None
   end)

module Periodic_test = Make_test_suite
  (Periodic)
  (struct
    let equal_to = equal_to_one
    let kernel_name = "Periodic"
    let opts = None
   end)

module Temporal_test = Make_test_suite
  (Temporal)
  (struct
    let equal_to = equal_to_one
    let kernel_name = "Temporal"
    let opts = None
   end)


module Linear_test = Make_test_suite
  (Linear)
  (struct
    let opts = some @@ Linear.Optional_args.make ~bias:0.0 ()
    let equal_to = 1.0
    let kernel_name = "Linear"
   end)

let () = Kaputt.Abbreviations.Test.run_tests @@
List.concat [
Squared_exponential_test.tests;
Matern_test.tests;
Periodic_test.tests;
Brownian_test.tests;
Temporal_test.tests;
Linear_test.tests]

