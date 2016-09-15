context("Testing object linreg")

# Wrong paramaters
test_that("Wrong parameters", {
  expect_error(linreg("formula", "data"), "wrong parameters")
  expect_error(linreg(X ~ Y, "data"),     "wrong parameters")
  expect_error(linreg(X ~ Y, TRUE),       "wrong parameters")
  expect_error(linreg(X ~ Sepal.Width, data = iris), "variable(s) not in data")
})

# Compare results to lm function
lmObject <-         lm(formula = Sepal.Length ~ Sepal.Width, data = iris)
linregObject <- linreg(formula = Sepal.Length ~ Sepal.Width, data = iris)

test_that("Same as in lm", {
  expect_that(linregObject$coefficients, equals(lmObject$coefficients))
  expect_that(linregObject$residuals,    equals(lmObject$residuals))
  expect_that(linregObject$pred,         equals(predict(lmObject)))
})


