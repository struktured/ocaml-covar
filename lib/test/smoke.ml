(* Modern smoke test for ocaml-covar on OCaml 5.4.
   Exercises the squared-exponential, Matern, and periodic kernels for the
   three properties any covariance function must satisfy:
     - variance at zero distance equals amplitude^2,
     - the kernel decays as points move apart,
     - the kernel is symmetric in its two arguments.
   The legacy Kaputt property suite (test_kernel.ml.legacy) was left
   half-migrated upstream; this is a clean, dependency-light replacement. *)

module SE = Covar_kernels.Squared_exponential
module Matern = Covar_kernels.Matern
module Periodic = Covar_kernels.Periodic

let failures = ref 0

let check name cond =
  Printf.printf "  [%s] %s\n" (if cond then "ok " else "FAIL") name;
  if not cond then incr failures

let approx ?(eps = 1e-6) a b = Stdlib.Float.abs (a -. b) <= eps

let () =
  Printf.printf "== ocaml-covar smoke test (OCaml 5.4) ==\n";

  (* Squared exponential: amplitude = 1, bandwidth = 1. *)
  let se = SE.create () in
  check "SE k(x,x) = amplitude^2 = 1" (approx (SE.covar se 0.0 0.0) 1.0);
  check "SE k(0,1) = exp(-1/2)" (approx (SE.covar se 0.0 1.0) (Stdlib.exp (-0.5)));
  check "SE decays with distance"
    (SE.covar se 0.0 1.0 > SE.covar se 0.0 2.0);
  check "SE symmetric"
    (approx (SE.covar se 0.0 1.0) (SE.covar se 1.0 0.0));

  (* Matern: amplitude = 1, bandwidth = 1, v = 1.5. *)
  let m = Matern.create () in
  check "Matern k(x,x) = amplitude^2 = 1" (approx (Matern.covar m 2.0 2.0) 1.0);
  check "Matern decays with distance"
    (Matern.covar m 0.0 0.5 > Matern.covar m 0.0 2.0);
  check "Matern symmetric"
    (approx (Matern.covar m 0.0 1.0) (Matern.covar m 1.0 0.0));

  (* Periodic: period = 0.5, so k(x, x + period) returns to the peak. *)
  let p = Periodic.create () in
  check "Periodic k(x,x) = amplitude^2 = 1" (approx (Periodic.covar p 1.0 1.0) 1.0);
  check "Periodic repeats after one period"
    (approx ~eps:1e-4 (Periodic.covar p 0.0 0.5) (Periodic.covar p 0.0 0.0));
  check "Periodic symmetric"
    (approx (Periodic.covar p 0.0 0.3) (Periodic.covar p 0.3 0.0));

  if !failures = 0 then Printf.printf "ALL PASS\n"
  else (
    Printf.printf "%d FAILURE(S)\n" !failures;
    Stdlib.exit 1)
