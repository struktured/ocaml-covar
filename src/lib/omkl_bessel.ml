module type S =
sig
  (** Modified bessel function of the second kind *)
  val bessel_k : v:int -> float -> float
end

module Gsl_bessel : S =
struct
 (** Uses ocaml-gsl as backend for now *)
 let bessel_k ~v = Gsl_sf.bessel_kl_scaled v
end

include Gsl_bessel
