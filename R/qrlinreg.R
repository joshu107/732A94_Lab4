#' Perform linear regression
#'
#' Use \code{qrlinreg} to perform linear regressions.
#'
#' @param formlua an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @param data a data frame or (or object coercible by
#'   \code{\link{as.data.frame}}).
#'
#' @return \code{qrlinreg} returns an
#'  object of class "\code{Linreg}".
#'
#'   An object of class "\code{Linreg}" have the following methods:
#'   \item{coefficients()}{a named vector of coefficients}
#'   \item{residuals()}{the residuals, that is response minus fitted values}
#'   \item{pred()}{predicted values of of a model}
#'
#' @seealso \code{\link{qr}}, \code{\link{class}}, \code{\link{formula}}
#'
#' @examples
#' qrlinreg(formula = Sepal.Length ~ Sepal.Width, data = iris)
#'
#' \dontrun{
#' qrlinreg(TRUE, TRUE)
#' }
#' @export


qrlinreg<-function(x,y,...){
  x<-unclass(x)
  y<-unclass(y)
  X<-cbind(1,x)
  n<-nrow(X)
  p<-ncol(X)
  qr.X<-qr(X)
  b<-(t(qr.Q(qr.X)) %*% y)[1:p]
  R<-qr.R(qr.X)
  beta<-as.vector(backsolve(R,b))
  res<-as.vector(y-X %*% beta)
  beta.names <- paste("X", 1:(p-1), sep = "")
  beta.names <- c("Intercept", beta.names)
  names(beta)<-beta.names
  ans <- list(coef = beta, residuals = res, intercept = TRUE)
  return(ans)
}

