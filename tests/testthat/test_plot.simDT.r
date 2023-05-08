
# fake simDT object with one node
sim_1_node <- list(tx_nodes=list(
  list(name="sickness_1",
       type="time_to_event",
       parents=c("age", "sex", "bmi"))),
  max_t=100
)
class(sim_1_node) <- "simDT"

# fake simDT object with two nodes
sim_2_node <- list(tx_nodes=list(
  list(name="sickness_1",
       type="time_to_event",
       parents=c("age", "sex", "bmi")),
  list(name="sickness_2",
       type="time_to_event",
       parents=c("age", "sex", "bmi"))),
  max_t=100
)
class(sim_2_node) <- "simDT"


test_that("defaults 1 node", {
  expect_snapshot_output(plot.simDT(sim_1_node))
})

test_that("defaults 2 nodes", {
  expect_snapshot_output(plot.simDT(sim_2_node))
})

test_that("without right boxes 1 node", {
  expect_snapshot_output(plot.simDT(sim_1_node, right_boxes=FALSE))
})

test_that("without right boxes 2 nodes", {
  expect_snapshot_output(plot.simDT(sim_2_node, right_boxes=FALSE))
})

test_that("some space changes 1 node", {
  expect_snapshot_output(plot.simDT(sim_1_node,
                                    box_hdist=2.5,
                                    box_vdist=1,
                                    box_l_width=0.6,
                                    box_l_height=0.4))
})

test_that("some space changes 2 nodes", {
  expect_snapshot_output(plot.simDT(sim_2_node,
                                    box_hdist=2.5,
                                    box_vdist=1,
                                    box_l_width=0.6,
                                    box_l_height=0.4))
})

test_that("test everything", {
  expect_snapshot_output(
    plot.simDT(sim_2_node,
               box_hdist=1.1,
               box_vdist=0.9,
               box_l_width=0.32,
               box_l_height=0.25,
               box_r_width=0.4,
               box_r_height=0.15,
               box_alpha=0.6,
               box_linetype="dashed",
               box_linewidth=0.7,
               box_border_colors=c("black", "orange"),
               box_fill_colors=c("green", "purple"),
               box_text_color="white",
               box_text_alpha=0.8,
               box_text_angle=10,
               box_text_family="sans",
               box_text_fontface="bold",
               box_text_size=6,
               box_text_lineheight=0.8,
               box_1_text_left="Some text",
               box_1_text_right="More text on right",
               box_2_text="Increase t by something",
               box_l_node_labels=c("this the first", "this the second"),
               box_r_node_labels=c("which number again?", "maybe 42"),
               box_last_text="not sure why you would change this",
               arrow_line_type="dotdash",
               arrow_line_width=0.7,
               arrow_line_color="red",
               arrow_line_alpha=0.6,
               arrow_head_angle=50,
               arrow_head_size=8,
               arrow_head_unit="mm",
               arrow_head_type="closed",
               arrow_left_pad=0.4,
               hline_width=1,
               hline_type="solid",
               hline_color="grey",
               hline_alpha=0.6)
  )
})
