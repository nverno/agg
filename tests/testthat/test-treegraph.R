context("df-graphs")
library(data.table)
library(igraph)

## Using this data and income from data folder as tests
df <- data.frame(a=factor(sample(2, 20, TRUE)),
                 b=factor(sample(3, 20, TRUE)),
                 c=rnorm(20))
