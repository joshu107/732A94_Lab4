#' A Reference Class to a linear regression model.
#'
#' @field formula an object of \code{\link{class}} \code{\link{formula}}: a
#'   symbolic description of the model to be fitted.
#' @field data a data frame
#' @field cache a list that contains cached values for results of methods
#'   \code{coef()}, \code{resid()}, \code{pred()} and the hash values of fields
#'   \code{formula} and \code{data} used to compute the output
#'

# How caching works
#
# 1. Before copmuting anything, check if the result is already stored in cache.
# 2a. If the result is stored, then return it.
# 2b. If the result is not stored, then compute it, save it in cache, return it
#
# Structure of the cache list:
# cache$
#       coef[["hash", "value"]]
#       resid[["hash", "value"]]
#       pree[["hash", "value"]]


# Class ------------------------------------------------------------------------
Linreg <- setRefClass(
  "Linreg",
  # Fields ---------------------------------------------------------------------
  fields = list(formula = "formula",
           data = "data.frame",
           cache = "list"),
  methods = list(
  # Methods --------------------------------------------------------------------
    coef = function() {
      "Computes and returns coefficients of the model"
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
    },
    resid = function() {
      "Computes and returns residuals of the model"
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
    },
    pred = function() {
      "Computes and returns predicted values of the model"
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
    },
    #needs to be in the format of
    #dd <- lm(formula = Petal.Length ~ Species, data = iris)
    #print(dd)
    print = function(x,...){
      #This is to emulate the lm() printout
      print(list(Call=paste("linreg(formula = ",.self$formula,")"),(Coefficients=.self$coef())))
    },
    #needs to be in the format of
    #dd <- lm(formula = Petal.Length ~ Species, data = iris)
    #plot(dd)
    plot = function(model){
      p1<-ggplot(model, aes(.fitted, .resid))+
        geom_point()+
        stat_smooth(method="lm")+
        geom_hline(yintercept=0, col="red", linetype="dashed")+
        xlab("Fitted values")+
        ylab("Residuals")+
        ggtitle("Residual vs Fitted Plot")+
        theme_bw()

      p2<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+
        geom_point(na.rm=TRUE)+
        stat_smooth(method="lm", na.rm = TRUE)+
        xlab("Fitted Value")+
        ylab(expression(sqrt("|Standardized residuals|")))+
        ggtitle("Scale-Location")+
        theme_bw()

      return(list(rvfPlot=p1, sclLocPlot=p2))
    },
    ##not sure where to pull these values from
    summary = function(x,...){
      Call=paste("linreg(formula = ",.self$formula,")")
      Coefficients="This needs to be a matrix of coefficients on rows and Estimate,std error, t value, p value on columns.  It should look like summary(lm(formula = Petal.Length ~ Species, data = iris))$coefficients"
      #data.frame(rownames=c("(Intercept)",**Formula**),colnames=c("Estimate","Std. Error","t value","Pr(>|t|)"))
      #[,1]<-
      #[,2]<-
      #etc.
      Variance="The variance is the square of the standard deviation, the second central moment of a distribution, and the covariance of the random variable with itself, sigma ^2"
      DofF="The number of parameters - 1"
      return(list(Call,Coefficients,Variance,DofF))
    },
    isCached = function(methodName) {
      "Checks whether the result of a method is stored in cache"
      #
      # Args:
      #   method: A character string with the name of the method to check for
      #           whether its result is alread cached
      #
      # Returns:
      #   TRUE if the result is cached. FALSE if it is not.

      require(digest)

      # Check if it is NULL (never initialized)
      if (is.null(.self$cache[[methodName]]$hash)) {
        return(FALSE)
      }

      # Check if hash of current data, formula is the same as it was when cache
      # was computed
      currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
      if (currentHash == .self$cache[[methodName]]$hash) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    storeCache = function(methodName, value) {
      "Stores result for the method, computes hash of data and stores it"
      # used to compute the value
      #
      # Args:
      #   method: character name of the method
      #   value:  value that the method returns
      #
      # Returns:
      #   Nothing, but modifies fields of a Linreg object

      require(digest)

      # Calculate hash of the list with two objects
      # - formula
      # - data
      currentHash <- digest::digest(list(.self$formula, .self$data), algo = "md5")
      # Store hash in the cache list under the appropriate method
      .self$cache[[methodName]] <- list(
       hash = currentHash,
       value = value
      )

     return(currentHash)
    }
  )
)
