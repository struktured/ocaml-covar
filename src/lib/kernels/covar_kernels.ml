module Std =
struct
  module Squared_exponential =
    Covar_squared_exponential
  module Matern = Covar_matern
  module Temporal = Covar_temporal
  module Homogeneous =  Covar_homogeneous
  module Heterogeneous = Covar_heterogeneous
  module Normalized_heterogeneous =
    Heterogeneous.Normalized_heterogeneuos
  module Linear = Covar_linear
  module Periodic = Covar_periodic
  module Brownian = Covar_brownian
  module Bias = Covar_bias
end
