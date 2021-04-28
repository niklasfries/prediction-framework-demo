require(boot)

source('shapes.R')

# Yields a vector of m importances where low values are more common.
# L2 norm sums to 1. m should be at least 5.
get_gammas = function(m) {
    stopifnot(m >= 5) # Gives weird numbers for small values of m
    gammas = qgeom((0:(m - 1))/m, .6)
    gammas = gammas/sqrt(sum(gammas^2))
    return(gammas)
}

# Simulates a dataset.
#
# Parameters:
#   n:      number of observations
#   m:      number of explanatory variables
#   gammas: relative importance of each variable, L2 norm should sum to 1
#   C:      overall importance of model, i.e., standard deviation of the log-odds
#   p_base: base probability of y = 1
#   shapes: the shape functions for transforming the explanatory variables.
#           Should be either 'linear', 'quadratic', 'ramp' or 'mixed'.
#           See shapes.R for further info.
#
# Return values: A list with the following elements:
#   X:      matrix of explanatory variables
#   gammas: vector of relative importances
#   C:      overall importance, i.e., standard deviation of the log-odds
#   p_base: base probability of y = 1
#   shapes: vector of indices of the shape functions used 
#           to transform each explanatory variable
#   contrs: actual contributions to the log-odds for each observation and variable
#   p:      probability of y = 1 for each observation
#   y:      observable binary outcomes for each observation 
build_dataset = function(
    n,
    m,
    gammas=get_gammas(m),
    C=1,
    p_base=0.5,
    shapes='mixed') {

    # Matrix of explanatory variables
    X = matrix(rnorm(n*m), n, m)

    # Create vector of incides for each shape function
    stopifnot(shapes %in% c('linear', 'quadratic', 'ramp', 'mixed'))
    if (shapes == 'linear') {
        shapes = rep(1, m)
    } else if (shapes == 'quadratic') {
        shapes = rep(2, m)
    } else if (shapes == 'ramp') {
        shapes = rep(3, m)
    } else {
        shapes = sample(1:3, m, replace=T)
    }

    # Compute contribution of each observation and variable by tranforming 
    # by the shape function of the variable and multiplying by the 
    # relative and overall importances
    contrs = matrix(NA, n, m)
    for (c in 1:m) {
        contrs[,c] = C*gammas[c]*SHAPES[[shapes[c]]](X[,c])
    }

    # Compute probabilities of y = 1, and simulate observable y-values
    alpha = logit(p_base)
    p = inv.logit(alpha + rowSums(contrs))
    y = rbinom(n, 1, p)

    return(list(X=X, gammas=gammas, C=C, p_base=p_base, 
                shapes=shapes, contrs=contrs, p=p, y=y))
}

