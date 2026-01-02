library(simDAG)
library(testthat)
library(data.table)
library(igraph)
library(ggdag)

data.table::setDTthreads(1)

test_check("simDAG")
