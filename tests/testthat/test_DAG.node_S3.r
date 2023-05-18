
test_that("S3 print root node, no params", {
  test_node <- node("A", type="rbernoulli")
  expect_snapshot_output(print(test_node))
})

test_that("S3 print root node, with params", {
  test_node <- node("A", type="rbernoulli", p=0.45)
  expect_snapshot_output(print(test_node))
})

test_that("S3 print child node, no params", {
  test_node <- node("A", type="gaussian", parents=c("C", "D"))
  expect_snapshot_output(print(test_node))
})

test_that("S3 print child node, with params", {
  test_node <- node("A", type="gaussian", parents=c("C", "D"),
                    betas=c(1, 2), intercept=15, error=4)
  expect_snapshot_output(print(test_node))
})

test_that("S3 summary", {
  test_node <- node("A", type="rbernoulli")
  expect_snapshot_output(summary(test_node))
})
