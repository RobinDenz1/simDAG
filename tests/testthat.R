library(simDAG)
library(testthat)
library(data.table)
library(igraph)

data.table::setDTthreads(1)

test_check("simDAG")
