
sim <- list()
class(sim) <- "simDT"

test_that("correct right_boxes", {
  expect_error(plot.simDT(sim, right_boxes=""))
})

test_that("numeric values", {
  expect_error(plot.simDT(sim, box_hdist=TRUE))
})

test_that("single character strings", {
  expect_error(plot.simDT(sim, box_1_text_left=1))
})

test_that("box_1_text_right", {
  expect_error(plot.simDT(sim, box_1_text_right=1))
})

test_that("box_l_node_labels", {
  expect_error(plot.simDT(sim, box_l_node_labels=1))
})

test_that("box_r_node_labels", {
  expect_error(plot.simDT(sim, box_r_node_labels=1))
})
