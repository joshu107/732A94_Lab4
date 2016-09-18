context("Testing function linreg")

# Wrong paramaters
test_that("Wrong parameters", {
  expect_error(linreg(function(x) {}, "data"),    "wrong parameters")
  expect_error(linreg("formula", function(x) {}), "wrong parameters")
  expect_error(linreg(X ~ Sepal.Width, data = iris), "variable(s) not in data",
               fixed = TRUE)
  expect_error(linreg(Sepal.Width ~ ., data = iris), "variable(s) not in data")
  expect_error(linreg(x ~ y, data.frame(x = numeric(0), y = numeric(0))),
               "data must have >=1 cols/rows")
})

context("Testing methods of LinregClass")

# Compare results to lm function
lmObject <-         lm(formula = mpg ~ hp, data = mtcars)
linregObject <- linreg(formula = mpg ~ hp, data = mtcars)
test_that("Same as in lm", {
  expect_that(linregObject$coef(), equals(lmObject$coefficients))
  expect_that(linregObject$resid(),    equals(lmObject$residuals))
  expect_that(linregObject$pred(),         equals(predict(lmObject)))
})
