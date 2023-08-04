
set.seed(354154)
data <- data.table(A=rnorm(10),
                   B=rnorm(10),
                   C=runif(10))

dag <- empty_dag() +
  node("A", type="rnorm") +
  node("B", type="gaussian", parents="A")

test_that("not a DAG", {
  expect_error(dag_from_data(dag="", data=data))
})

test_that("wrong data", {
  expect_error(dag_from_data(dag=dag, data=""))
})

test_that("wrong return_models", {
  expect_error(dag_from_data(dag=dag, data=data, return_models=1))
})

test_that("wrong na.rm", {
  expect_error(dag_from_data(dag=dag, data=data, na.rm=1))
})

test_that("DAG with time-varying nodes", {
  dag <- dag + node_td("C", "time_to_event")
  expect_error(dag_from_data(dag=dag, data=data))
})

test_that("node not in data", {
  dag <- dag + node("F", "rnorm")
  expect_error(dag_from_data(dag=dag, data=data))
})
