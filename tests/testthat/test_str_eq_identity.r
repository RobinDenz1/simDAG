
test_that("general test", {

  test_node <- node("bmi", type="identity",
                    formula= ~ age / 2 + age^2 - ifelse(sex, 2, 3) + 2)

  out <- str_eq_identity(test_node)
  expect_equal(out, "bmi ~ age/2 + age^2 - ifelse(sex, 2, 3) + 2")
})
