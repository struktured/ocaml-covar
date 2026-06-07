(* Exact Gaussian-process regression over an arbitrary covariance kernel.

   Given training inputs X, targets y, and observation noise sigma^2, this
   computes the posterior predictive distribution at a new point xn:

     mean(xn)     = ks^T (K + sigma^2 I)^-1 y
     variance(xn) = k(xn,xn) - ks^T (K + sigma^2 I)^-1 ks

   where K_ij = k(x_i, x_j) and ks_i = k(xn, x_i) is the test-train covariance.

   A Cholesky factorization of (K + sigma^2 I) is computed once (cost O(n^3));
   thereafter each [mean] is an O(n) dot product and each [variance] an O(n^2)
   triangular solve. The log marginal likelihood is exposed for hyperparameter
   selection (maximize it over the kernel's parameters and the noise).

   This is the uncertainty layer the kernels were missing: [predict] returns
   both the mean and the variance, so callers can size decisions by confidence
   (e.g. spread proportional to posterior standard deviation). *)

module D = Lacaml.D

module Make (K : Covar_kernel.S) = struct
  module Kernel = K

  type t =
    { kernel : K.t
    ; inputs : K.Instance.t array
    ; noise : float
    ; chol : D.mat (* Cholesky factor of (K + noise I), upper triangle *)
    ; alpha : D.vec (* (K + noise I)^-1 y *)
    ; log_marginal_likelihood : float
    }

  let two_pi = 2.0 *. Covar_trig.pi

  let create ?(noise = 1e-6) ~kernel ~inputs ~targets () =
    let n = Array.length inputs in
    if n = 0 then failwith "Covar_gp.create: empty training set";
    if Array.length targets <> n then
      failwith "Covar_gp.create: inputs/targets length mismatch";
    (* Assemble K + noise*I. *)
    let kmat = D.Mat.create n n in
    for i = 1 to n do
      for j = 1 to n do
        kmat.{i, j} <- K.covar kernel inputs.(i - 1) inputs.(j - 1)
      done
    done;
    for i = 1 to n do
      kmat.{i, i} <- kmat.{i, i} +. noise
    done;
    (* Cholesky factorize in place (upper). *)
    D.potrf ~up:true kmat;
    (* alpha = (K + noise I)^-1 y via the two triangular solves. *)
    let yb = D.Mat.create n 1 in
    for i = 1 to n do
      yb.{i, 1} <- targets.(i - 1)
    done;
    D.potrs ~up:true kmat yb;
    let alpha = D.Vec.create n in
    for i = 1 to n do
      alpha.{i} <- yb.{i, 1}
    done;
    (* log marginal likelihood:
       -1/2 y^T alpha - sum_i log L_ii - n/2 log(2 pi).
       (det(K) = prod L_ii^2, so 1/2 log det = sum log L_ii.) *)
    let quad = ref 0.0 in
    for i = 1 to n do
      quad := !quad +. (targets.(i - 1) *. alpha.{i})
    done;
    let logdet = ref 0.0 in
    for i = 1 to n do
      logdet := !logdet +. Stdlib.log kmat.{i, i}
    done;
    let lml =
      (-0.5 *. !quad) -. !logdet -. (Float.of_int n /. 2.0 *. Stdlib.log two_pi)
    in
    { kernel; inputs; noise; chol = kmat; alpha; log_marginal_likelihood = lml }

  let k_star t x =
    let n = Array.length t.inputs in
    let ks = D.Vec.create n in
    for i = 1 to n do
      ks.{i} <- K.covar t.kernel x t.inputs.(i - 1)
    done;
    ks

  (* Posterior predictive mean at [x]. *)
  let mean t x =
    let ks = k_star t x in
    D.dot ks t.alpha

  (* Posterior predictive variance at [x] (>= 0; clamped against roundoff). *)
  let variance t x =
    let n = Array.length t.inputs in
    let ks = k_star t x in
    let kb = D.Mat.create n 1 in
    for i = 1 to n do
      kb.{i, 1} <- ks.{i}
    done;
    D.potrs ~up:true t.chol kb;
    (* kb = (K + noise I)^-1 k_star *)
    let v = ref 0.0 in
    for i = 1 to n do
      v := !v +. (ks.{i} *. kb.{i, 1})
    done;
    let kxx = K.covar t.kernel x x in
    Float.max 0.0 (kxx -. !v)

  (* Posterior predictive mean and variance at [x]. *)
  let predict t x = mean t x, variance t x

  (* Posterior predictive standard deviation at [x]. *)
  let std t x = Stdlib.sqrt (variance t x)

  let log_marginal_likelihood t = t.log_marginal_likelihood
end
