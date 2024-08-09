
test_that("general test case", {
  set.seed(234)

  dag <- empty_dag() +
    node("A", "rbernoulli") +
    node("B", "rnorm") +
    node("C", "rcategorical", probs=c(0.2, 0.2, 0.8), output="factor")
  data <- sim_from_dag(dag, n_sim=200)
  data$D <- c(rep("Test1", 100), rep("Test2", 100))

  d_combs <- get_cat_col_levs(data)

  expected <- data.table(name=c("A", "A", "C", "C", "C", "D", "D"),
                         categories=c("TRUE", "FALSE", "0", "1", "2",
                                      "Test1", "Test2"))
  expected[, levs := paste0(name, categories)]

  expect_equal(d_combs, expected)
})

