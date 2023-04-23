
f2 <- function(envir, i, j) {
  assign2list(name="nested_list", i=i, j=j, value=999, envir=envir)
}

f1 <- function() {

  nested_list <- list(A=list(1, 1, 1, 1, 1),
                      B=list(1, 1, 1, 1, 1))

  envir <- environment()

  f2(envir=envir, i="A", j=3)
  f2(envir=envir, i="B", j=2)

  return(nested_list)
}

expected <- list(A=list(1, 1, 999, 1, 1),
                 B=list(1, 999, 1, 1, 1))

test_that("example case working", {
  created <- f1()
  expect_equal(created, expected)
})
