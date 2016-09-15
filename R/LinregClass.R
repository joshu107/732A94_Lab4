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
  # Calculates and returns coefficients of the model
  #
  # Args:
  #
  # Returns:
  #   Named vector of estimated coefficients.

  # Extract X matrix and Y matrix (vector) from data and formula
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
  # Extract X matrix and Y matrix (vector) from data and formula
  X <- model.matrix(.self$formula, .self$data)
  yName <- all.vars(.self$formula)[1]
  Y <- .self$data[yName]

  # Get predicted values
  yHat <- .self$pred()

  epsilon <- Y - yHat

  # Format in the same way as lm()
  epsilonVector <- epsilon[, 1]
  names(epsilonVector) <- rownames(epsilon)
  epsilon <- epsilonVector

  return(epsilon)
}
predMethod <- function() {
  # Extract X matrix and Y matrix (vector) from data and formula
  X <- model.matrix(.self$formula, .self$data)

  # Get estimated coefficients
  betaHat <- .self$coefficients()

  yHat <- betaHat %*% t(X)
  # # Format in the same way as lm()
  yHatVector <- as.vector(yHat)
  names(yHatVector) <- colnames(yHat)
  yHat <- yHatVector

  return(yHat)
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
