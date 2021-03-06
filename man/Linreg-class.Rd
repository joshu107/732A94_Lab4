% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinregClass.R
\docType{class}
\name{Linreg-class}
\alias{Linreg}
\alias{Linreg-class}
\title{A Reference Class to a linear regression model.}
\description{
A Reference Class to a linear regression model.
}
\section{Fields}{

\describe{
\item{\code{formula}}{an object of \code{\link{class}} \code{\link{formula}}: a
symbolic description of the model to be fitted.}

\item{\code{data}}{a data frame}

\item{\code{cache}}{a list that contains cached values for results of methods
\code{coef()}, \code{resid()}, \code{pred()} and the hash values of fields
\code{formula} and \code{data} used to compute the output}
}}
\section{Methods}{

\describe{
\item{\code{coef()}}{Computes and returns coefficients of the model}

\item{\code{isCached(methodName)}}{Checks whether the result of a method is stored in cache}

\item{\code{plot()}}{Plots Residuals vs. Fits and Scale-Location graphs.}

\item{\code{pred()}}{Computes and returns predicted values of the model}

\item{\code{print(digits = 3, ...)}}{Prints a very brief summary of a Linreg object.}

\item{\code{resid()}}{Computes and returns residuals of the model}

\item{\code{storeCache(methodName, value)}}{Stores result for the method, computes hash of data and stores it}

\item{\code{summary(digits = 3, ...)}}{Prints a summary of a Linreg object.}
}}

