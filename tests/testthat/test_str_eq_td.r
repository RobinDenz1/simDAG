
test_that("without parents", {
  node_testin <- function(data, sim_time, lmao=2){return(23)}
  assign("node_testin", value=node_testin, envir=.GlobalEnv)
  test_node <- node_td("A", type="testin", lmao=3)
  expect_equal(str_eq_td(test_node), "A(t) ~ node_testin(t, lmao)")
})

test_that("with parents", {
  node_testin <- function(data, sim_time, lmao=2){return(23)}
  assign("node_testin", value=node_testin, envir=.GlobalEnv)
  test_node <- node_td("A", type="testin", parents=c("C", "D"), lmao=3)
  expect_equal(str_eq_td(test_node), "A(t) ~ node_testin(C(t), D(t), t, lmao)")
})

test_that("with parents, no sim_time", {
  node_testin <- function(data, lmao=2){return(23)}
  assign("node_testin", value=node_testin, envir=.GlobalEnv)
  test_node <- node_td("A", type="testin", parents=c("C", "D"), lmao=3)
  expect_equal(str_eq_td(test_node), "A(t) ~ node_testin(C(t), D(t), lmao)")
})
