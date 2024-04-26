
test_that("general test case", {
  equations <- c("A ~ some stuff",
                 "another_name ~ more stuff")
  expect_equal(align_str_equations(equations),
               c("           A ~ some stuff", "another_name ~ more stuff"))
})
