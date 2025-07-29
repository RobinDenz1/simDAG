
test_that("general test case", {
  out <- get_netname_from_net("net(mean(A), net='ayyyy')")
  expect_equal(out, "ayyyy")
})

test_that("as unnamed argument", {
  out <- get_netname_from_net("net(sum(A), 'something')")
  expect_equal(out, "something")
})

test_that("returning NA if not in there", {
  out <- get_netname_from_net("net(sum(A))")
  expect_equal(out, NA_character_)
})

test_that("returning NA if not in there, with comma in first argument", {
  out <- get_netname_from_net("net(sum(A, na.rm=TRUE))")
  expect_equal(out, NA_character_)
})

test_that("with complicated nested functions in first arg", {
  out <- get_netname_from_net("net((as.numeric(any(A/2))) > 2, net='net1')")
  expect_equal(out, "net1")

  out <- get_netname_from_net("net((as.numeric(any(A/2))) > 2, 'net1')")
  expect_equal(out, "net1")
})

test_that("with comma in first argument", {
  out <- get_netname_from_net("net(sum(A, na.rm=TRUE), net='mhhh')")
  expect_equal(out, "mhhh")

  out <- get_netname_from_net("net(sum(A, na.rm=TRUE), 'mhhh')")
  expect_equal(out, "mhhh")
})

test_that("with complicated variable name", {
  out <- get_netname_from_net("net(mean(head(`some-variable^3`)), net='ahhh')")
  expect_equal(out, "ahhh")

  out <- get_netname_from_net("net(mean(head(`some-variable^3`)), 'ahhh')")
  expect_equal(out, "ahhh")
})
