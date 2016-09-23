## ---- echo = FALSE, message = FALSE--------------------------------------
devtools::use_package("pryr")

## ---- results = 'hide'---------------------------------------------------
library(LiuLinReg)
data(iris)

tmp <- linreg(formula = Sepal.Length ~ Sepal.Width, data = iris)
pryr::otype(tmp)

## ------------------------------------------------------------------------
pryr::otype(tmp)

## ------------------------------------------------------------------------
tmp$print()

## ------------------------------------------------------------------------
residuals <- tmp$resid()
head(residuals, 10)

## ------------------------------------------------------------------------
predicted <- tmp$pred()
head(predicted, 10)

## ------------------------------------------------------------------------
tmp$coef()

## ------------------------------------------------------------------------
tmp$summary()

## ---- echo = FALSE, message = FALSE--------------------------------------
devtools::use_package("ggplot2")

## ---- results = 'hide', message = FALSE, fig.width = 5-------------------
tmp$plot()

