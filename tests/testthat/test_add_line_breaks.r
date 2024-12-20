
test_that("input < char_max", {

  input <- "A ~ Bernoulli(0.5)"
  out <- add_line_breaks(input)

  expect_equal(input, out)
})

test_that("one break needed", {
  input <- paste0("T[T] ~ (-(log(Unif(0, 1))/(2*exp(X1*log(1.8) + X2*log(1.8)",
                  " + X4*log(1.8) + I(X5^2)*log(2.3))))")
  expected <- paste0("T[T] ~ (-(log(Unif(0, 1))/(2*exp(X1*log(1.8) + ",
                     "X2*log(1.8) +\n         X4*log(1.8) + ",
                     "I(X5^2)*log(2.3))))")

  out <- add_line_breaks(input, char_max=60)

  expect_equal(out, expected)
})

test_that("multiple breaks needed", {
  input <- paste0("T[T] ~ (-(log(Unif(0, 1))/(2*exp(X1*log(1.8) + X2*log(1.8)",
                  " + X4*log(1.8) + I(X5^2)*log(2.3)",
                  " + X4*log(1.8) + I(X5^2)*log(2.3)",
                  " + X4*log(1.8) + I(X5^2)*log(2.3)))))")
  expected <- paste0("T[T] ~ (-(log(Unif(0, 1))/(2*exp(X1*log(1.8) + ",
                     "X2*log(1.8) +\n         X4*log(1.8) + I(X5^2)*log(2.3)",
                     " + X4*log(1.8) +\n         I(X5^2)*log(2.3) + ",
                     "X4*log(1.8) +\n         I(X5^2)*log(2.3)))))")

  out <- add_line_breaks(input, char_max=60)

  expect_equal(out, expected)
})

test_that("no break possible due to long name", {

  input <- paste0("A ~ Bernoulli(logit(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                  "aaaaaaabbbbbbbbcccccccccccc*0.3))")
  out <- add_line_breaks(input)

  expect_equal(out, input)
})
