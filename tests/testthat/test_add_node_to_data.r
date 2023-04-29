
set.seed(2341243)

test_that("adding column", {
  data <- data.table(.id=seq(1, 10),
                     A=rnorm(10))
  data2 <- add_node_to_data(data=data, new=rnorm(10), name="B")

  expect_true(ncol(data2)==3)
  expect_equal(colnames(data2), c(".id", "A", "B"))
  expect_true(is.numeric(data2$B))
})

test_that("adding data.frame", {
  data <- data.table(.id=seq(1, 10),
                     A=rnorm(10))
  data2 <- add_node_to_data(data=data, new=data.frame(B=rnorm(10),
                                                      C=rnorm(10)),
                            name=NULL)

  expect_true(ncol(data2)==4)
  expect_equal(colnames(data2), c(".id", "A", "B", "C"))
  expect_true(is.numeric(data2$B) & is.numeric(data2$C))
})

test_that("adding data.table", {
  data <- data.table(.id=seq(1, 10),
                     A=rnorm(10))
  data2 <- add_node_to_data(data=data, new=data.table(B=rnorm(10),
                                                      C=rnorm(10)),
                            name=NULL)

  expect_true(ncol(data2)==4)
  expect_equal(colnames(data2), c(".id", "A", "B", "C"))
  expect_true(is.numeric(data2$B) & is.numeric(data2$C))
})
