
test_that("varying numbers of plusses", {
  formstr <- "-2++10*A+9*B++++8*C"
  out <- remove_mistaken_plus(formstr)
  expect_equal(out, "-2+10*A+9*B+8*C")
})

test_that("with plusses at the end", {
  formstr <- "-2++10*A+9*B++++8*C++++++"
  out <- remove_mistaken_plus(formstr)
  expect_equal(out, "-2+10*A+9*B+8*C")
})

test_that("with plusses at the beginning", {
  formstr <- "+++-2++10*A+9*B++++8*C+++"
  out <- remove_mistaken_plus(formstr)
  expect_equal(out, "-2+10*A+9*B+8*C")
})

test_that("with plusses at the beginning and the end", {
  formstr <- "+-2++10*A+9*B++++8*C++"
  out <- remove_mistaken_plus(formstr)
  expect_equal(out, "-2+10*A+9*B+8*C")
})
