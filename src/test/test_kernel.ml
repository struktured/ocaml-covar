open Kaputt.Abbreviations

open Printf

include Covar_kernel
module type S_float =
sig 
include S with
  module Instance = Covar_instance.Float
end

module Equals_test (K:S_float) =
struct
 let default_trials = 1000
 let min_float = -1e5
 let max_float = abs_float min_float
 let test ?(kernel=(K.create())) ~equal_to x x' =
      let cov = K.covar kernel x x' in
      let cmp = Gsl_math.fcmp cov equal_to ~epsilon:(1.e-3) in
      printf "x=%f, x'=%f, equal_to=%f, cov=%f, cmp=%d\n" x x' 
        equal_to cov cmp;
      cmp = 0
 let create ?kernel ?(trials=default_trials) ~title_tag ~equal_to =
   Test.make_random_test
            ~title:(sprintf "Equals_test (%s)" title_tag)
            ~nb_runs:trials
            (Kaputt.Abbreviations.Gen.make_float min_float max_float)
            (test ?kernel ~equal_to)
            [ Spec.always ==> Spec.always ]
end


module Squared_exponential_test =
struct
 module K = Squared_exponential
 module Eq_test = Equals_test(K)
 let kernel = K.create ()
 let one_when_same =
   Eq_test.create ~kernel 
      ?trials:None ~title_tag:"squared_exponential" ~equal_to:0.0
end

let () = Kaputt.Abbreviations.Test.run_tests
  [Squared_exponential_test.one_when_same]
