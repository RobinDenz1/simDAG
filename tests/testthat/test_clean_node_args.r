
test_that("corrects parents of time_to_event node", {

  f <- function() {

  }

  node_tte <- list(parents=c("A", "B"), name="Ay", prob_fun=f,
                   event_duration=10, immunity_duration=100,
                   save_past_events=TRUE, type_str="time_to_event",
                   type_fun=node_time_to_event)

  new_node <- clean_node_args(node_tte)

  expect_equal(new_node$parents, c(".id", "A", "B", "Ay_event", "Ay_time"))
})

test_that("corrects parents of competing_events node", {

  f <- function() {

  }

  node_ce <- list(parents=c("A", "B"), name="By", prob_fun=f,
                  event_duration=c(100, 10), immunity_duration=100,
                  save_past_events=TRUE, type_str="competing_events",
                  type_fun=node_competing_events)

  new_node <- clean_node_args(node_ce)

  expect_equal(new_node$parents, c(".id", "A", "B", "By_event", "By_time"))
})

test_that("removes 'name' if not needed", {

  node_tx <- list(parents=c("A", "B"), name="By", type_str="gaussian",
                  type_fun=node_gaussian)

  new_node <- clean_node_args(node_tx)

  expect_equal(new_node$name, NULL)
})
