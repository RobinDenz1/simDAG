
test_that("one node", {

  expected <- c("Generate 'A' status")
  out <- get_left_box_labels("A")

  expect_equal(out, expected)
})

test_that("two nodes", {

  expected <- c("Generate 'A' status", "Generate 'B' status")
  out <- get_left_box_labels(c("A", "B"))

  expect_equal(out, expected)
})
