
test_that("with weibull distribution", {
  test_node <- node("A", type="cox", parents=c("D", "V"),
                    betas=c(1.2, 0.3), surv_dist="weibull",
                    lambda=0.2, gamma=0.5)
  expect_equal(str_eq_cox(test_node),
               c("A[T] ~ (-(log(Unif(0, 1))/(0.2*exp(1.2*D + 0.3*V))))^(1/0.5)",
                 "A[C] ~ Inf"))
})

test_that("with weibull distribution and censoring", {
  test_node <- node("A", type="cox", parents=c("D", "V"),
                    betas=c(1.2, 0.3), surv_dist="weibull",
                    lambda=0.2, gamma=0.5, cens_dist="runif",
                    cens_args=list(min=0, max=20))
  expect_equal(str_eq_cox(test_node),
               c("A[T] ~ (-(log(Unif(0, 1))/(0.2*exp(1.2*D + 0.3*V))))^(1/0.5)",
                 "A[C] ~ runif(min=0, max=20)"))
})

test_that("with exponential distribution", {
  test_node <- node("A", type="cox", parents=c("D", "V"),
                    betas=c(1.2, 0.3), surv_dist="exponential",
                    lambda=0.2, gamma=0.5)
  expect_equal(str_eq_cox(test_node),
               c("A[T] ~ -(log(Unif(0, 1))/(0.2*exp(1.2*D + 0.3*V)))",
                 "A[C] ~ Inf"))
})
