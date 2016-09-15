#' A Reference Class to a linear regression model.
#'
#' @field formlua an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @data data a data frame or (or object coercible by
#'   \code{\link{as.data.frame)}}.


# Document structure:
# 1. Define all methods used in class
# 2. Define the class

# Methods used in class --------------------------------------------------------
coefficientsMethod <- function() {
  # Extracr X matrix and Y matrix (vector) from data and formula
  X <- model.matrix(.self$formula, .self$data)
  yName <- all.vars(.self$formula)[1]
  Y <- .self$data[, yName]

  # Estimates
  betaHat <- solve(t(X) %*% X) %*% t(X) %*% Y

  # Format in the same way as lm()
  betaHat <- betaHat[, 1]

  return(betaHat)
}
residualsMethod <- function() {
  stop("method not implemented")
}
predMethod <- function() {
  stop("method not implemented")
}

# Class ------------------------------------------------------------------------
LinregClass <- setRefClass("Linreg",
                       fields = list(formula = "formula",
                                     data = "data.frame"),
                       methods = list(
                         coefficients = coefficientsMethod,
                         residuals = residualsMethod,
                         pred = predMethod
                       )
)
