module type S =
sig
  (** Modified bessel function of the second kind *)
  val bessel_k : nu:float -> float -> float
end

module Gsl_bessel : S =
struct
 (** Uses ocaml-gsl as backend for now *)
 let bessel_k ~nu = Gsl_sf.bessel_Knu nu
end

include Gsl_bessel
