
test_that("general test cases", {

  dag <- empty_dag() +
    node(c("X1", "X2"), type="rnorm") +
    node("X3", type="rbernoulli") +
    node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2 + 1.1*X3,
         cutpoints=c(0.2, 1), link="logistic") +
    node("Y2", type="polr", formula= ~ -2*X1 + 0.3*X2 + 1.1*X3,
         cutpoints=c(0.2, 1, 1.5), link="probit") +
    node("Y3", type="polr", formula= ~ -2*X1 + 0.3*X2 + 1.1*X3,
         cutpoints=c(0.2), link="cauchit") +
    node("Y4", type="polr", formula= ~ -2*X1 + 0.3*X2 + 1.1*X3,
         cutpoints=c(0.2, 1, 1.5, 2.2))

  set.seed(1234)
  data <- sim_from_dag(dag, n_sim=500)

  expect_equal(length(unique(data$Y1)), 3)
  expect_equal(length(unique(data$Y2)), 4)
  expect_equal(length(unique(data$Y3)), 2)
  expect_equal(length(unique(data$Y4)), 5)
})

test_that("with labels", {

  dag <- empty_dag() +
    node(c("X1", "X2"), type="rnorm") +
    node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
         cutpoints=c(0.2, 1), link="logistic", labels=c("A", "B", "C"),
         output="character")

  set.seed(1234)
  data <- sim_from_dag(dag, n_sim=1000)

  expect_equal(unique(data$Y1), c("B", "A", "C"))
})

test_that("calling the function directly", {

  dag <- empty_dag() +
    node(c("X1", "X2"), type="rnorm")

  set.seed(1234)
  data <- as.data.frame(sim_from_dag(dag, n_sim=1000))

  Y <- node_polr(data=data, parents=c("X1", "X2"),
                 formula= ~ X1 + X2, betas=c(-2, 0.3), cutpoints=c(0.2, 1))

  expect_equal(levels(Y), c("1", "2", "3"))
})

test_that("input checks", {

  expect_error(node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
                    cutpoints=c(0.2, 1.4), link=1),
               "Argument 'link' must be a single character string.")

  expect_error(node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
                    cutpoints=c(0.2, 1.4), link="log"),
               paste0("Argument 'link' must be either 'logistic', 'probit',",
                      " 'loglog', 'cloglog' or 'cauchit' not 'log'."))

  expect_error(node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
                    cutpoints=c(0.2, 1.4), output="numeric"),
               paste0("Argument 'output' must be either 'character' or ",
                      "'factor' when using type='polr'."))

  expect_error(node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
                    link="logistic"),
               "Argument 'cutpoints' must be specified when using type='polr'.")

  expect_error(node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
                    link="logistic", cutpoints=c("A")),
               "Argument 'cutpoints' must be a numeric vector with length > 0.")

  expect_error(node("Y1", type="polr", formula= ~ -2*X1 + 0.3*X2,
                    link="logistic", cutpoints=c(1, 2), labels=c("a", "b")),
               "Argument 'labels' must be of length length(cutpoints) + 1.",
               fixed=TRUE)
})
