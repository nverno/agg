context('edgelist')
library(data.table)
library(igraph)

tree.order = c('education', 'status', 'gender', 'residence')
dtree <- df2dtree(income, tree.order = tree.order,
                funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                          meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                targets=list(c('income', 'expense'), c('income', 'expense')))


test_that('edge_list creates the proper edges from a list of levels.', {
    ## the example dtree has 59 nodes and should have
    ## 58 edges
    levs <- lapply(dtree[, c('total', tree.order), with=FALSE], levels)
    es <- edge_list(levs)
    g <- graph_from_edgelist(do.call(cbind, es)[-1,])
    expect_equivalent(lengths(es), rep.int(nrow(dtree), 2))
    expect_true(all(degree(g, mode='in')[-1L] == 1))
})
