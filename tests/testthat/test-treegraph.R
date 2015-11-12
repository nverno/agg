context("df-graphs")
library(data.table)
library(igraph)

## Using this data and income from data folder as tests
df <- data.frame(a=factor(sample(2, 20, TRUE)),
                 b=factor(sample(3, 20, TRUE)),
                 c=rnorm(20))

tree.order = c('education', 'status', 'gender', 'residence')
valCols <- c('mean', 'meanfrac')
dtree <- df2dtree(income, tree.order=tree.order,
                funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                          meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                targets=list(c('income', 'expense'), c('income', 'expense')))


allValues <- c("id", "label", "level", "count", valCols)
g <- dtree2graph(dtree)

test_that('dtree2graph produces a correct size graph', {
    expect_equal(length(V(g)), nrow(dtree))
    expect_equal(length(E(g)), nrow(dtree)-1L)
    expect_true(all(degree(g, mode='in')[-1L] == 1))  # all nodes except root have one in
    expect_equivalent(names(vertex_attr(g)), allValues)
})

test_that('dtree2graph produces different attributes', {
    vals <- c('count', 'mean')
    dtree <- df2dtree(income, tree.order=tree.order,
                      funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                                meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                      targets=list(c('income', 'expense'), c('income', 'expense')))

    ## Keep specific values
    g <- dtree2graph(dtree, values=vals)
    expect_equal(names(vertex_attr(g)), c('id', 'label', vals))

    ## No attributes
    g <- dtree2graph(dtree, values=FALSE)
    expect_equal(names(vertex_attr(g)), c('id', 'label'))
})

test_that('dtree2graph handles categorical column selection', {
    ## All categorical columns
    cols <- TRUE
    n <- match(FALSE, sapply(dtree, is.factor)) - 1  # number of categories
    levs <- sapply(dtree[, 1:n, with=FALSE], levels)
    g <- dtree2graph(dtree, cols=cols)
    expect_equivalent(unique(vertex_attr(g)$level), 1:n)
    expect_equivalent(cumprod(lengths(levs, FALSE)), rle(vertex_attr(g)$level)$lengths)

    ## Some categorical columns
    cols <- c('total')
    g <- dtree2graph(dtree, cols=cols)
})
