context("graph conversion")
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

test_that('dtree2graph handles depth selection', {
    dtree <- df2dtree(income, tree.order=tree.order,
                      funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                                meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                      targets=list(c('income', 'expense'), c('income', 'expense')))

    ## Complete graph
    tree.depth <- TRUE
    n <- match(FALSE, sapply(dtree, is.factor)) - 1  # number of categories
    levs <- sapply(dtree[, 1:n, with=FALSE], levels)
    levs <- c(total='Total', levs)
    g <- dtree2graph(dtree, tree.depth=tree.depth)
    expect_equivalent(unique(vertex_attr(g)$level), 1:(n+1))  # +1 for total
    expect_equivalent(cumprod(lengths(levs, FALSE)), rle(vertex_attr(g)$level)$lengths)

    ## Error with NULL depth, warning with length(tree.depth) > 1
    expect_error(dtree2graph(dtree, tree.depth=NULL))
    expect_warning(dtree2graph(dtree, tree.depth=2:3))

    ## Throw error on depth < 2
    expect_error(dtree2graph(dtree, tree.depth=1))
    expect_error(dtree2graph(dtree, tree.depth='total'))
    
    ## Partial depth
    g <- dtree2graph(dtree, tree.depth=3)
    leaf <- max(V(g))
    paths <- all_simple_paths(g, from=1, to=leaf, mode='out')
    expect_equal(length(paths), 1)
    expect_equal(length(paths[[1]]), 3)
    ## expect_equivalent(vertex_attr(g, 'mean', leaf), dtree[leaf, mean])
})
