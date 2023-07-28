
data <- data.table(.id=seq_len(10))

test_that("one time_to_event node, no optionals", {

  dag <- empty_dag() +
    node_td("woah", type="time_to_event", parents=c("A", "C"))

  out <- initialize_columns(data=data,
                            tx_nodes=dag$tx_nodes,
                            tx_node_names="woah",
                            tx_node_types="time_to_event")

  expect_equal(colnames(out), c(".id", "woah_event", "woah_time"))
  expect_true(all(!out$woah_event))
  expect_true(all(is.na(out$woah_time)))
})

test_that("one time_to_event node, with time_since_last", {

  dag <- empty_dag() +
    node_td("woah", type="time_to_event", parents=c("A", "C"),
            time_since_last=TRUE)

  out <- initialize_columns(data=data,
                            tx_nodes=dag$tx_nodes,
                            tx_node_names="woah",
                            tx_node_types="time_to_event")

  expect_equal(colnames(out), c(".id", "woah_event", "woah_time",
                                "woah_time_since_last"))
  expect_true(all(!out$woah_event))
  expect_true(all(is.na(out$woah_time)))
  expect_true(all(is.na(out$woah_time_since_last)))
})

test_that("one time_to_event node, with event_count", {

  dag <- empty_dag() +
    node_td("woah", type="time_to_event", parents=c("A", "C"),
            event_count=TRUE)

  out <- initialize_columns(data=data,
                            tx_nodes=dag$tx_nodes,
                            tx_node_names="woah",
                            tx_node_types="time_to_event")

  expect_equal(colnames(out), c(".id", "woah_event", "woah_time",
                                "woah_event_count"))
  expect_true(all(!out$woah_event))
  expect_true(all(is.na(out$woah_time)))
  expect_true(all(out$woah_event_count==0))
})

test_that("one time_to_event node, with time_since_last & event_count", {

  dag <- empty_dag() +
    node_td("woah", type="time_to_event", parents=c("A", "C"),
            time_since_last=TRUE, event_count=TRUE)

  out <- initialize_columns(data=data,
                            tx_nodes=dag$tx_nodes,
                            tx_node_names="woah",
                            tx_node_types="time_to_event")

  expect_equal(colnames(out), c(".id", "woah_event", "woah_time",
                                "woah_event_count", "woah_time_since_last"))
  expect_true(all(!out$woah_event))
  expect_true(all(is.na(out$woah_time)))
  expect_true(all(is.na(out$woah_time_since_last)))
  expect_true(all(out$woah_event_count==0))
})

test_that("one competing_events node", {

  dag <- empty_dag() +
    node_td("woah", type="competing_events", parents=c("A", "C"))

  out <- initialize_columns(data=data,
                            tx_nodes=dag$tx_nodes,
                            tx_node_names="woah",
                            tx_node_types="competing_events")

  expect_equal(colnames(out), c(".id", "woah_event", "woah_time"))
  expect_true(all(!out$woah_event))
  expect_true(all(is.na(out$woah_time)))
})
