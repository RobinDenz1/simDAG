
test_that("general test cases", {

  set.seed(234)

  dag <- empty_dag() +
    node("A", "rbernoulli") +
    node("B", "rnorm") +
    node("C", "rcategorical", probs=c(0.2, 0.2, 0.8), coerce2factor=TRUE)
  data <- sim_from_dag(dag, n_sim=200)

  d_combs <- get_cat_col_levs(data)

  out1 <- get_interaction_term_for_formula(c("ATRUE", "B"),
                                           data=data, d_combs=d_combs)
  out2 <- get_interaction_term_for_formula(c("B", "ATRUE"),
                                           data=data, d_combs=d_combs)
  out3 <- get_interaction_term_for_formula(c("C2", "ATRUE"),
                                           data=data, d_combs=d_combs)

  expect_equal(out1, "A * B")
  expect_equal(out2, "B * A")
  expect_equal(out3, "C * A")
  expect_error(get_interaction_term_for_formula(c("C5", "B"),
                                                data=data, d_combs=d_combs),
               paste0("The variable 'C5' named in the interaction 'C5:B' is ",
                      "neither an existing column nor a columname plus a ",
                      "valid level of a categorical variable."))
})
