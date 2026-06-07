# ocaml-covar

Covariance functions implemented as composable kernels, in OCaml.

[![OCaml](https://img.shields.io/badge/OCaml-5.4-orange)](https://ocaml.org/)

## Introduction

This library started as an OCaml implementation of
[Temporal Kernel Descriptors for Learning with Time-sensitive Patterns](http://www.doyensahoo.com/uploads/5/3/7/3/53734297/tkd__sdm_2016_.pdf)
by Sahoo, Sharma, Hoi, and Zhao — an online kernel-learning methodology with
kernels dedicated to detecting time-sensitive patterns and associating them
with traditional machine-learning features.

More standard kernels (squared exponential, Matérn, periodic, linear, …) were
added over time, along with arbitrary kernel composition.

On its own this library provides covariance functions and an online,
kernel-weighted predictor. It is meant to be used as the kernel layer for
Gaussian-process / kernel-method modelling — feed it features, get
covariances, and plug those into an inference or online-learning loop.

## Status

Modernized to build on **OCaml 5.4 / dune 3.x** with current `core`/`async`,
`lacaml`, and `gsl`. The `covar-base` and `covar-kernels` libraries build clean
and ship a passing smoke test. The optional `covar-async` layer is currently
disabled (it depended on the unmaintained `async_extended`); the kernel and
predictive math live entirely in `covar-base` + `covar-kernels`.

## Features

- Multiple covariance functions, in OCaml
- Support for temporal kernel descriptors
- Clean, extensible API for both kernels and data types
- Arbitrary kernel composition (weighted sums via `Generic`)
- Online kernel-weighted predictor with a sliding-window buffer
- REPL (`utop`) friendly

## Supported kernels

- **Squared exponential** (RBF)
- **Matérn** (smoothness parameter `v`; Bessel via GSL)
- **Periodic**
- **Linear**
- **Brownian**
- **Temporal** (temporal kernel descriptors)
- **Bias**
- **Composition** — homogeneous / heterogeneous / generic weighted combinations

## Building

Requires the GSL and BLAS/LAPACK development libraries on the system:

```sh
# Debian/Ubuntu
sudo apt-get install -y libgsl-dev liblapacke-dev libopenblas-dev
```

Then the OCaml dependencies and a build:

```sh
opam install -y lacaml gsl containers containers-data \
                ppx_deriving expect_test_helpers_core

dune build
dune runtest
```

## Example

```ocaml
module SE = Covar_kernels.Squared_exponential

(* Default hyperparameters: amplitude = 1, bandwidth = 1 *)
let se = SE.create ()

let () =
  Printf.printf "k(x,x)  = %f\n" (SE.covar se 0.0 0.0);  (* 1.0       *)
  Printf.printf "k(0,1)  = %f\n" (SE.covar se 0.0 1.0)   (* exp(-1/2) *)
```

Hyperparameters are set through each kernel's `Optional_args` (e.g.
`SE.create ~opt:(SE.Optional_args.make ~bandwidth:0.5 ()) ()`). The same
`create` / `covar` interface is shared by every kernel, so they compose and
swap freely.

## Testing

`dune runtest` runs `lib/test/smoke.ml`, which checks the core covariance
properties (variance at zero distance, decay with distance, symmetry) for the
squared-exponential, Matérn, and periodic kernels.

The original Kaputt-based property suite is preserved as
`lib/test/test_kernel.ml.legacy` pending a port to the current toolchain.

## Roadmap

- **Gaussian-process inference layer** — exact GP posterior **mean and
  variance** over any kernel (`K = [k(xᵢ,xⱼ)] + σ²I`, Cholesky solve via
  `lacaml`), plus marginal-likelihood hyperparameter fitting. The kernels here
  are the foundation; the posterior-variance / uncertainty layer is the next
  addition.
- Re-enable the `covar-async` streaming layer without `async_extended`.
- Port the legacy property tests.

## License

MIT
