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
end
