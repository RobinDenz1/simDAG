
test_that("with small cycle", {
  g <- igraph::make_directed_graph(c("A", "B", "B", "A"))

  # from A
  out <- find_cycle(g, start="A")
  expect_equal(out, c("A", "B", "A"))

  # from B
  out <- find_cycle(g, start="B")
  expect_equal(out, c("B", "A", "B"))
})

test_that("with larger cycle", {
  g <- igraph::make_directed_graph(c("A", "B",
                                     "B", "C",
                                     "B", "D",
                                     "B", "E",
                                     "B", "F",
                                     "E", "A",
                                     "F", "G",
                                     "G", "A",
                                     "G", "B"))

  # from A
  out <- find_cycle(g, start="A")
  expect_equal(out, c("A", "B", "E", "A"))

  # from B
  out <- find_cycle(g, start="B")
  expect_equal(out, c("B", "E", "A", "B"))
})

test_that("with no cycle", {
  g <- igraph::make_directed_graph(c("A", "C", "B", "D", "B", "C"))

  # from A
  out <- find_cycle(g, start="A")
  expect_equal(out, NULL)

  # from B
  out <- find_cycle(g, start="B")
  expect_equal(out, NULL)
})
