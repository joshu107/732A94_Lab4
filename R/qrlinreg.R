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
#' @return THIS NEEDS TO BE CHANGED \code{qrlinreg} returns an
#'  object of class "\code{Linreg}".
#'
#'   An object of class "\code{Linreg}" have the following methods:
#'   \item{coefficients()}{a named vector of coefficients}
#'   \item{residuals()}{the residuals, that is response minus fitted values}
#'   \item{pred()}{predicted values of of a model}
#'
#' @seealso \code{\link{lm}}, \code{\link{class}}, \code{\link{formula}}
#'
#' @examples
#' qrlinreg(formula = Sepal.Length ~ Sepal.Width, data = iris)
#'
#' \dontrun{
#' qrlinreg(TRUE, TRUE)
#' }
#' @export

qrlinreg <- function(){
  
}

#args(lsfit)

#set.seed(1)

#x<-rnorm(5)
#y<-rnorm(5)

#fit<-lsfit(x,y)

#if(fit$intercept){
#  X<-cbind(1,x)
#} else{
#  X<-x
#}

#n<-nrow(X)
#p<-ncol(X)

#qr.X<-qr(X)

#b<-(t(qr.Q(qr.X)) %*% y)[1:p]

#R<-qr.R(qr.X)
#beta<-as.vector(backsolve(R,b))

#res<-as.vector(y-X %*% beta)

#if(fit$intercept) {
#  beta.names <- paste("X", 1:(p-1), sep = "")
#  beta.names <- c("Intercept", beta.names)
#} else {
#  beta.names <- paste("X", 1:p, sep = "")
#}
#names(beta)<-beta.names
#ans <- list(coef = beta, residuals = res, intercept = fit$intercept)
#var.ans<-var(res)


