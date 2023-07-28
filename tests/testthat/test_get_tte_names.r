
test_that("no time_to_event nodes", {

  dag <- empty_dag() +
    node_td("test", type="competing_events", parents=c("A", "B"))

  out <- get_tte_names(tx_node_names="test", tx_node_types="competing_events",
                       tx_nodes=dag$tx_nodes)
  expect_true(is.null(out))
})

test_that("one time_to_event node, no optionals", {

  dag <- empty_dag() +
    node_td("test", type="time_to_event", parents=c("A", "B"))

  out <- get_tte_names(tx_node_names="test", tx_node_types="time_to_event",
                       tx_nodes=dag$tx_nodes)

  expect_equal(out, c("test_event", "test_time"))
})

test_that("two time_to_event nodes, no optionals", {

  dag <- empty_dag() +
    node_td("test", type="time_to_event", parents=c("A", "B")) +
    node_td("test2", type="time_to_event", parents=c("D", "E"))

  out <- get_tte_names(tx_node_names=c("test", "test2"),
                       tx_node_types=c("time_to_event", "time_to_event"),
                       tx_nodes=dag$tx_nodes)

  expect_equal(out, c("test_event", "test_time", "test2_event", "test2_time"))
})

test_that("time_to_event node, with time_since_last", {

  dag <- empty_dag() +
    node_td("test", type="time_to_event", parents=c("A", "B"),
            time_since_last=TRUE)

  out <- get_tte_names(tx_node_names="test", tx_node_types="time_to_event",
                       tx_nodes=dag$tx_nodes)

  expect_equal(out, c("test_event", "test_time", "test_time_since_last"))
})

test_that("time_to_event node, with event_count", {

  dag <- empty_dag() +
    node_td("test", type="time_to_event", parents=c("A", "B"),
            event_count=TRUE)

  out <- get_tte_names(tx_node_names="test", tx_node_types="time_to_event",
                       tx_nodes=dag$tx_nodes)

  expect_equal(out, c("test_event", "test_time", "test_event_count"))
})

test_that("time_to_event node, with time_since_last & event_count", {

  dag <- empty_dag() +
    node_td("test", type="time_to_event", parents=c("A", "B"),
            time_since_last=TRUE, event_count=TRUE)

  out <- get_tte_names(tx_node_names="test", tx_node_types="time_to_event",
                       tx_nodes=dag$tx_nodes)

  expect_equal(out, c("test_event", "test_time", "test_event_count",
                      "test_time_since_last"))
})
