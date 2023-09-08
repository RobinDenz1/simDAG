
dag <- empty_dag() +
  node("A", "rbernoulli")

test_that("not a DAG", {
  expect_error(sim_n_datasets(dag=1, n_repeats=2, n_cores=1, n_sim=100))
})

test_that("wrong n_repeats", {
  expect_error(sim_n_datasets(dag=dag, n_repeats=-2, n_cores=1, n_sim=100))
})

test_that("wrong n_cores", {
  expect_error(sim_n_datasets(dag=dag, n_repeats=2, n_cores=-1, n_sim=100))
})

test_that("wrong data_format", {
  expect_error(sim_n_datasets(dag=dag, n_repeats=2, n_cores=1, n_sim=100,
                              data_format=1))
})

test_that("wrong data_format_args", {
  expect_error(sim_n_datasets(dag=dag, n_repeats=2, n_cores=1, n_sim=100,
                              data_format_args=1))
})

test_that("wrong progressbar", {
  expect_error(sim_n_datasets(dag=dag, n_repeats=2, n_cores=1, n_sim=100,
                              progressbar=1))
})
