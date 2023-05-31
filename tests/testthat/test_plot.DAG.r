
dag <- empty_dag() +
  node("node 1", type="rnorm") +
  node("A", type="rbernoulli") +
  node("B", type="gaussian", parents=c("node 1", "A")) +
  node("C", type="binomial", parents=c("node 1", "A", "B")) +
  node("D", type="multinomial", parents=c("node 1", "C"))

set.seed(41234)

test_that("defaults", {
  expect_snapshot_output(plot(dag))
})

test_that("change layout", {
  expect_snapshot_output(plot(dag, layout="in_circle"))
})

test_that("change node_size", {
  expect_snapshot_output(plot(dag, node_size=0.1))
})

test_that("change node_names", {
  expect_snapshot_output(plot(dag, node_names=c("1", "2", "3", "4", "5")))
})

test_that("change node outline", {
  expect_snapshot_output(plot(dag, node_color="green", node_linewidth=1,
                              node_linetype="dashed"))
})

test_that("change node insides", {
  expect_snapshot_output(plot(dag, node_fill="green", node_alpha=0.3))
})

test_that("change node text", {
  expect_snapshot_output(plot(dag, node_text_color="white",
                              node_text_alpha=0.8,
                              node_text_family="serif",
                              node_text_fontface="italic",
                              node_text_size=10))
})

test_that("change arrow lines", {
  expect_snapshot_output(plot(dag, arrow_color="blue",
                              arrow_linetype="dashed",
                              arrow_linewidth=1.1,
                              arrow_alpha=0.7))
})

test_that("change arrow head", {
  expect_snapshot_output(plot(dag, arrow_head_size=10, arrow_head_unit="mm",
                              arrow_type="open"))
})

test_that("change other paramaters", {
  expect_snapshot_output(plot(dag, arrow_node_dist=0.01,
                              gg_theme=ggplot2::theme_bw()))
})

test_that("change everything", {
  expect_snapshot_output(plot(dag,
                              layout="in_circle",
                              node_size=0.1,
                              node_names=c("1", "2", "3", "4", "5"),
                              node_color="green",
                              node_linewidth=1,
                              node_linetype="dashed",
                              node_fill="green",
                              node_alpha=0.3,
                              node_text_color="white",
                              node_text_alpha=0.8,
                              node_text_family="serif",
                              node_text_fontface="italic",
                              node_text_size=10,
                              arrow_color="blue",
                              arrow_linetype="dashed",
                              arrow_linewidth=1.1,
                              arrow_alpha=0.7,
                              arrow_head_size=10,
                              arrow_head_unit="mm",
                              arrow_type="open",
                              arrow_node_dist=0.01,
                              gg_theme=ggplot2::theme_bw()
                              ))
})
