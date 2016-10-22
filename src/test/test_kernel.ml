open Kaputt.Abbreviations

open Printf

include Covar_kernel
module type S_float =
sig
include S with
  module Instance = Covar_instance.Float
end

module Comparison_test (K:S_float) =
struct
 let default_trials = 1000
 let min_float = -1e5
 let max_float = abs_float min_float
 let test ?(kernel=(K.create())) ~equal_to x x' =
      let cov = K.covar kernel x x' in
      let cmp = Gsl_math.fcmp cov equal_to ~epsilon:(1.e-5) in
      printf "x=%f, x'=%f, equal_to=%f, cov=%f, cmp=%d\n" x x' 
        equal_to cov cmp;
      cmp
 let for_k_x_x ?kernel ?(trials=default_trials) ~title_tag ~equal_to =
   Test.make_random_test
            ~title:(sprintf "%s: k(x,x)=%f" title_tag equal_to)
            ~nb_runs:trials
            (Gen.make_float min_float max_float)
            (fun x -> test ?kernel ~equal_to x x)
            [ Spec.always ==> fun cmp -> cmp = 0 ]
 let for_k_x_x' ?kernel ?(trials=default_trials) ~title_tag ~equal_to =
   Test.make_random_test
            ~title:(sprintf "%s: k(x,x')=%f" title_tag equal_to)
            ~nb_runs:trials
            (Gen.zip2 (Gen.make_float min_float max_float)
                   (Gen.make_float min_float max_float))
            (fun (x, x') -> test ?kernel ~equal_to x x')
            [ Spec.always ==> fun cmp -> cmp = 0 ]

end


module Squared_exponential_test =
struct
 module K = Squared_exponential
 module Comparison_test = Comparison_test(K)
 let kernel = K.create ()
 let one_when_same =
   Comparison_test.for_k_x_x ~kernel
      ?trials:None ~title_tag:"squared_exponential" ~equal_to:1.0
 let tests = [one_when_same]
end

module Matern_test =
struct
 module K = Matern
 module Comparison_test = Comparison_test(K)
 let kernel = K.create ()
 let one_when_same =
   Comparison_test.for_k_x_x ~kernel
      ?trials:None ~title_tag:"matern" ~equal_to:1.0
 let tests = [one_when_same]
end



let () = Kaputt.Abbreviations.Test.run_tests @@
    List.concat [Squared_exponential_test.tests; Matern_test.tests]
