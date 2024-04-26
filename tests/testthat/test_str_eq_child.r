
test_that("general test case", {
  node_testin <- function(data, a=1) {return(2)}
  assign("node_testin", value=node_testin, envir=.GlobalEnv)
  test_node <- node("A", type="testin", parents=c("C", "D"), a=2)
  expect_equal(str_eq_child(test_node), "A ~ node_testin(C, D, a)")
})
