# README #

Covariance functions implemented as Kernels, in Ocaml. Work in progress!

## Introduction ##

This library initially started as an OCaml implementation of [Temporal Kernel Descriptors for Learning with Time-sensitive Patterns](http://www.doyensahoo.com/uploads/5/3/7/3/53734297/tkd__sdm_2016_.pdf) by Sahoo, Sharma, Hoi, and Zhao. It is a online kernel learning methodology with kernels dedicated to detecting time-sensitive patterns and associating them with traditional machine learning features.

Eventually more standard kernels such squared exponential and matern were added. Composition of these kernels is already possible and will continue to be extended.

On it's own this module only provides covariance functions. It is used in conjunction with other maching learning libraries to
train data with these functions. Having said that, it is informative to load this library in ```utop``` and plot the function
using ```owl```.

## Features ###

 - Multiple covariance functions, in OCaml
 - Support for temporal kernel descriptors
 - Clean, extensible api for both kernels and data types.
 - REPL (utop) friendly
 - Can compose kernels arbitrarily

## Supported Kernels ##

### Squared exponential

### Matern

### Linear

### Periodic

### Temporal

### Kernel Composition

## Examples ##

TODO
## Testing

ocaml-covar uses kaputt for specification style tests. See 'src/tests' for more information.
