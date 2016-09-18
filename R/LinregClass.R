#' A Reference Class to a linear regression model.
#'
#' @field formula an object of \code{\link{class}} \code{\link{formula}}: a symbolic description of the model to
#'   be fitted.
#' @field data a data frame
"LinregClass"

# Document structure:
# 1. Define all methods used in class
# 2. Define the class

# Methods used in class --------------------------------------------------------
isCachedMethod <- function(method) {
  # Checks whether the result of a method coef, pred, resid are stored in cache
  #
  # Args:
  #   method: A character string with the name of the method to check for
  #           whether its result is alread cached
  #
  # Returns:
  #   TRUE if the result is cached. FALSE if it is not.

  require(digest)

  # Check if it is NULL (never initialized)
  if (is.null(.self$cache[[method]]$hash)) {
    return(FALSE)
  }

  # Check if hash of current data, formula is the same as it was when cache was
  # computed
  currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
  if (currentHash == .self$cache[[method]]$hash) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

storeCacheMethod <- function(method, value) {
  # Stores supplied value for the corresponding cache, along with hash of data
  # used to compute the value
  #
  # Args:
  #   method: character name of the method
  #   value:  value that the method returns
  #
  # Returns:
  #   Nothing, but modifies fields of a Linreg object

  require(digest)

  currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
  .self$cache[[method]] <- list(
    hash = currentHash,
    value = value
  )

  return(NULL)
}

coefficientsMethod <- function() {
  # Calculates and returns coefficients of the model.
  #
  # Args:
  #
  # Returns:
  #   Named vector of estimated coefficients.

  # Check if the result is already cached
  if (isCached("coef")) {
    return(.self$cache$coef$value)
  }

  # Extract X matrix and Y matrix (vector) from data and formula
  X <- model.matrix(.self$formula, .self$data)
  yName <- all.vars(.self$formula)[1]
  Y <- .self$data[, yName]

  # Estimates
  betaHat <- solve(t(X) %*% X) %*% t(X) %*% Y

  # Format in the same way as lm()
  betaHat <- betaHat[, 1]

  # Store the result in cache
  storeCache("coef", betaHat)
  return(betaHat)
}
residualsMethod <- function() {
  # Calculates and returns residuals of the model.
  #
  # Args:
  #
  # Returns:
  #   Named vector of residuals.

  # Check if the result is already cached
  if (isCached("resid")) {
    retur(.self$cache$resid$value)
  }

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

  # Store result in cache
  storeCache("resid", epsilon)

  return(epsilon)
}
predMethod <- function() {
  # Calculates and returns predicted values of the model
  #
  # Args:
  #
  # Returns:
  #   Named vector of predicted values.

  # Check if the result is cached
  if (isCached("pred")) {
    return(.self$cache$pred$value)
  }

  # Extract X matrix and Y matrix (vector) from data and formula
  X <- model.matrix(.self$formula, .self$data)

  # Get estimated coefficients
  betaHat <- .self$coef()

  yHat <- betaHat %*% t(X)
  # # Format in the same way as lm()
  yHatVector <- as.vector(yHat)
  names(yHatVector) <- colnames(yHat)
  yHat <- yHatVector

  # Store result in cache
  storeCache("pred", yHat)

  return(yHat)
}
printMethod<-function(x,...){
  stop("Method not implemented")
}
plotMethod<-function(x,...){
  stop("Method not implemented")
}
summaryMethod<- function(x,...){
  stop("Method not implemented")
}

# Class ------------------------------------------------------------------------
LinregClass <- setRefClass("Linreg",
                       fields = list(formula = "formula",
                                     data = "data.frame",
                                     cache = "list"),
                       methods = list(
                         coef = coefficientsMethod,
                         resid = residualsMethod,
                         pred = predMethod,
                         print = printMethod,
                         plot = plotMethod,
                         summary = summaryMethod,
                         isCached = isCachedMethod,
                         storeCache = storeCacheMethod
                       )
)
