
test_that("general test case", {

  set.seed(457456)

  # get some example data from a known DAG
  dag <- empty_dag() +
    node("death", type="binomial", parents=c("age", "sex"), betas=c(1, 2),
         intercept=-10) +
    node("age", type="rnorm", mean=10, sd=2) +
    node("sex", parents="", type="rbernoulli", p=0.5) +
    node("smoking", parents=c("sex", "age"), type="binomial",
         betas=c(0.6, 0.2), intercept=-2)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  # suppose we only know the causal structure and the node type:
  dag <- empty_dag() +
    node("death", type="binomial", parents=c("age", "sex")) +
    node("age", type="rnorm") +
    node("sex", type="rbernoulli") +
    node("smoking", type="binomial", parents=c("sex", "age"))

  # get parameter estimates from data
  dag_full <- dag_from_data(dag=dag, data=data)


  expected_dag <- empty_dag() +
    node("death", type="binomial", parents=c("age", "sex"),
         betas=c(0.9699486, 1.8823713), intercept=-9.631358) +
    node("age", type="rnorm", mean=9.973241, sd=1.985875) +
    node("sex", type="rbernoulli", p=0.49) +
    node("smoking", type="binomial", parents=c("sex", "age"),
         betas=c(0.4799525, 0.2378301), intercept=-2.492591)

  expect_equal(dag_full$dag, expected_dag, tolerance=0.0001)

})
