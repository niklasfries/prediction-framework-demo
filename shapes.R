
RAW_SHAPES = c(function(x) x, # Linear
               function(x) x^2, # Quadratic
               function(x) ifelse(x < 0, 0, x)) # Ramp

# Means of transformed variables when the distribution of X is standard normal.
SHAPE_MEANS = c(0,
                1,
                integrate(function(x) dnorm(x)*RAW_SHAPES[[3]](x), -5, 5)$value)

# Variances of transformed variables when the distribution of X is standard normal.
SHAPE_VARS = c(1,
               2,
               integrate(function(x) dnorm(x)*(RAW_SHAPES[[3]](x) - SHAPE_MEANS[3])^2, -5, 5)$value)

# Standardized shape functions so that the transformed variables have mean 0 and variance 1.
SHAPES = c(function(x) (RAW_SHAPES[[1]](x) - SHAPE_MEANS[1])/sqrt(SHAPE_VARS[1]),
           function(x) (RAW_SHAPES[[2]](x) - SHAPE_MEANS[2])/sqrt(SHAPE_VARS[2]),
           function(x) (RAW_SHAPES[[3]](x) - SHAPE_MEANS[3])/sqrt(SHAPE_VARS[3]))

