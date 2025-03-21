
test_that("general test case", {
  formstr <- "A*0.2+C*0.13+D*1000 + (1|D) + (D|1+C)"
  replace <- c("(1|D)", "(D|1+C)")
  out <- str_replace_all(formstr, replace)
  expect_equal(out, "A*0.2+C*0.13+D*1000 +  + ")
})
