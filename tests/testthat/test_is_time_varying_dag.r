
test_that("FALSE when no time-varying nodes", {
  dag <- empty_dag() +
    node("ay", "binomial", p=0.4)

  expect_true(!is_time_varying_dag(dag))
})

test_that("TRUE when with time-varying nodes", {
  dag <- empty_dag() +
    node_td("ay", "time_to_event", p=0.4)

  expect_true(is_time_varying_dag(dag))
})
