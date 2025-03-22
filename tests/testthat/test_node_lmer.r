
test_that("simple random effect", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + (1|E),
         var_corr=0.5, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -1.99)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + 1.5*A + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.22)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + A*1.5 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.39)
})

test_that("simple random effect in disrete-time simulation", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node_td("Y", type="gaussian", formula= ~ -2 + A*1.5 + (1|E),
            var_corr=0.5, error=1)
  data <- sim_discrete_time(dag_gaus, n_sim=100, save_states="all", max_t=5)
  data <- sim2data(data, to="long")
  expect_equal(round(mean(data$Y), 3), -1.852)

  # binomial
  dag_bin <- dag +
    node_td("Y", type="binomial", formula= ~ -2 + A*1.5 + (1|E),
           var_corr=0.5)
  data <- sim_discrete_time(dag_bin, n_sim=100, save_states="all", max_t=5)
  data <- sim2data(data, to="long")
  expect_equal(round(mean(data$Y), 3), 0.202)

  # poisson
  dag_pois <- dag +
    node_td("Y", type="poisson", formula= ~ -2 + A*1.5 + (1|E),
            var_corr=0.5)
  data <- sim_discrete_time(dag_pois, n_sim=100, save_states="all", max_t=5)
  data <- sim2data(data, to="long")
  expect_equal(round(mean(data$Y), 3), 0.426)
})

test_that("simple random effect + categorical fixed effect", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rcategorical", probs=c(0.3333, 0.3333, 0.3333),
         labels=c("var1", "var2", "var3")) +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + Avar2*1.5 + Avar3*3 + (1|E),
         var_corr=0.5, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -0.513)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + Avar2*1.5 + Avar3*3 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.48)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + Avar2*1.5 + Avar3*3 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 2.02)
})

test_that("simple random effect + interaction effect", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm") +
    node("B", type="rnorm") +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + B*3 + A:B*0.2 + (1|E),
         var_corr=0.5, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -2.253)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + A*1.5 + B*3 + A:B*0.2 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.25)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + A*1.5 + B*3 + A:B*0.2 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 21.88)
})

test_that("simple random effect + interaction effect of categorical vars", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm") +
    node("B", type="rcategorical", probs=c(0.33, 0.33, 0.33),
         labels=c("var1", "var2", "var3")) +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + Bvar2*3 + A:Bvar2*0.2 +
           (1|E), var_corr=0.5, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -1.079)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + A*1.5 + Bvar2*3 + A:Bvar2*0.2 +
           (1|E), var_corr=0.5)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.31)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + A*1.5 + Bvar2*3 + A:Bvar2*0.2 +
           (1|E), var_corr=0.5)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 4.19)
})

test_that("simple random effect + cubic terms", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + I(A^2)*0.2 + (1|E),
         var_corr=0.5, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -1.797)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + A*1.5 + I(A^2)*0.2 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.22)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + A*1.5 + I(A^2)*0.2 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 1.34)
})

test_that("simple random effect + ONLY cubic terms", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + I(A^2)*0.2 + (1|E),
         var_corr=0.5, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -1.851)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + I(A^2)*0.2 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.19)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + I(A^2)*0.2 + (1|E),
         var_corr=0.5)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.27)
})

test_that("multiple random effects", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node(c("E", "F"), type="rcategorical", probs=rep(0.1, 10),
         labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + (1|E) + (1|F),
         var_corr=list("E"=0.5, "F"=0.8), error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -2.179)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + A*1.5 + (1|E) + (1|F),
         var_corr=list("E"=0.5, "F"=0.8))
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.15)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + A*1.5 + (1|E) + (1|F),
         var_corr=list("E"=0.5, "F"=0.8))
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.75)

})

test_that("with random effects and random slopes", {

  set.seed(324)

  var_corr <- matrix(c(0.5, 0.05, 0.05, 0.1), 2)

  dag <- empty_dag() +
    node("A", type="rnorm") +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10])

  # gaussian
  dag_gaus <- dag +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + (A|E),
         var_corr=var_corr, error=1)
  data <- sim_from_dag(dag_gaus, n_sim=100)
  expect_equal(round(mean(data$Y), 3), -1.974)

  # binomial
  dag_bin <- dag +
    node("Y", type="binomial", formula= ~ -2 + A*1.5 + (A|E),
         var_corr=var_corr)
  data <- sim_from_dag(dag_bin, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.21)

  # poisson
  dag_pois <- dag +
    node("Y", type="poisson", formula= ~ -2 + A*1.5 + (A|E),
         var_corr=var_corr)
  data <- sim_from_dag(dag_pois, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.57)
})

test_that("mixed model term with + in it", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node(c("E", "G"), type="rcategorical", probs=rep(0.1, 10),
         labels=LETTERS[1:10]) +
    node("Y", type="gaussian", formula= ~ -2 + A*1.5 + (1 + A|E),
         var_corr=matrix(c(0.5, 0.05, 0.05, 0.1), 2), error=1)
  data <- sim_from_dag(dag, n_sim=1000)
  expect_equal(round(mean(data$Y), 3), -2.365)
})

test_that("works with function input", {

  set.seed(324)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node(c("E", "G"), type="rcategorical", probs=rep(0.1, 10),
         labels=LETTERS[1:10]) +
    node("Y", type=node_gaussian, formula= ~ -2 + A*1.5 + (1 + A|E),
         var_corr=matrix(c(0.5, 0.05, 0.05, 0.1), 2), error=1)
  data <- sim_from_dag(dag, n_sim=1000)
  expect_equal(round(mean(data$Y), 3), -2.365)
})

test_that("at least one fixed effect needs to be there", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rnorm", mean=0, sd=1) +
      node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10]) +
      node("Y", type="binomial", formula= ~ -2 + (1|E), var_corr=0.4)
  }, paste0("A 'formula' cannot consist soley of random effects and/or",
            " random slopes. At least one fixed effect must also be supplied."))
})

test_that("random effects and random slopes only in supported types", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rnorm", mean=0, sd=1) +
      node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10]) +
      node("Y", type="negative_binomial", formula= ~ -2 + A*2 + (1|E),
           var_corr=0.4, theta=2)
  }, paste0("Random effects and random slopes are currently only supported ",
            "in 'formula' for nodes of type 'gaussian', 'binomial', or ",
            "'poisson', not negative_binomial."))
})

test_that("missing var_corr argument", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rnorm", mean=0, sd=1) +
      node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10]) +
      node("Y", type="binomial", formula= ~ -2 + A*0.2 + (1|E))
    data <- sim_from_dag(dag, n_sim=10)
  }, paste0("'var_corr' must be specified when random effects or random ",
            "slopes are included in 'formula'."))
})
