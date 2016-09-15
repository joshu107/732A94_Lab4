#' Perform linear regression
#'
#' Use \code{linreg} to perform linear regressions.
#'
#' @param formlua an object of \code{\link{class}} \code{\link{formula}} (or one
#'   that can be coerced to that class): a symbolic description of the model to
#'   be fitted.
#' @param data a data frame or (or object coercible by
#'   \code{\link{as.data.frame)}}.
#'
#' @return \code{linreg} returns an object of class "\code{Linreg}".
#'
#'   An object of class "\code{Linreg}" have the following methods:
#'   \item{coefficients()}{a named vector of coefficients}
#'   \item{residuals()}{the residuals, that is response minus fitted values}
#'   \item{pred()}{predicted values of of a model}
#'
#' @seealso \code{\link{lm}}, \code{\link{class}}, \code{\link{formula}}
#'
#' @examples
#' linreg(formula = Sepal.Length ~ Sepal.Width, data = iris)
#'
#' \dontrun{
#' linreg(TRUE, TRUE)
#' }
#' @export

linreg <- function(formula, data) {
  linregObject <- LinregClass$new(formula = as.formula(formula),
                             data = as.data.frame(data))
  return(linregObject)
}
