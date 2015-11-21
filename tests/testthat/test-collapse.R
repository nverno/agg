context('collapse dftree')
library(data.table)

## Collapse all aggregating columns into just head and tail columns
## compatible with various packages like googleVis and data.tree
load_graph()
tst1 <- collapse(dtree, sdcols=tree.order, copy=TRUE)   # collapse full level dtree
tst2 <- collapse(dtree1, sdcols=tree.order, copy=TRUE)  # dropped levels
fullLevs <- lapply(dtree[, tree.order, with=FALSE], levels)
full <- sum(cumprod(lengths(fullLevs))) + 1L

test_that('collapse results in the proper two columns', {
  expect_equal(nrow(tst1), full)
  expect_less_than(nrow(tst2), nrow(tst1))

  ## dropped levels should all have values
  ## and opposite for non-dropped
  expect_true(sum(is.na(tst2$out))==0L)
  expect_false(sum(is.na(tst1$out))==0L)  
  
})
