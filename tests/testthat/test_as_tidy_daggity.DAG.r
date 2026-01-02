
test_that("general test case", {

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm") +
    node("D", type="binomial", formula= ~ -2 + A*2 + B*3) +
    node("E", type="binomial", formula= ~ -3 + A*2 + C*4 + D*2) +
    node("F", type="binomial", formula= ~ -3 + E*1 + D*2)

  obj <- as_tidy_dagitty(dag)
  expect_true(inherits(obj, "tidy_dagitty"))
})

test_that("nodes without any edges are included", {

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm") +
    node("D", type="binomial", formula= ~ -2 + A*2 + B*3)

  # NOTE: this currently doesn't work because the nodes get ignored by
  #       the as_tidy_dagitty.data.frame() method
  obj <- as_tidy_dagitty(dag)
  expect_true(inherits(obj, "tidy_dagitty"))
})
