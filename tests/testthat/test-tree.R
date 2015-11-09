context("df-trees")
library(data.table)

## Check proper combinations of inputs
test_that('df2dtree fails appropriately', {
    ## zero-length tree.order, no funs
    expect_more_than(nrow(df2dtree(income, tree.order=c(''))), 0)

    ## Test errors/warnings
    tree.order <- c('gender')
    expect_warning(df2dtree(income, tree.order, targets=list(c(income))))

    ## Currently only errors when 'funs' are present, but 'targets' are
    ## missing or NULL
    expect_error(df2dtree(income, tree.order, funs=list('sum'), targets=NULL))
    expect_error(df2dtree(income, tree.order, funs=list('sum'), targets=list('income', 'expense')))
    expect_error(df2dtree(income, tree.order, funs=list('sum', '*'),
                          targets = list('income')))
})

## Check output for grouping
test_that('df2dtree produces correct output at grouping levels', {
    
    expect_equal(df2dtree(income, funs=list(sum='sum'), targets = list('income'))$sum, 11540)
    expect_equal(df2dtree(income, tree.order = c('gender'), funs=list('sum'),
                          targets = list('income'))$output1, c(11540, 4651, 6889))

    res <- df2dtree(income, tree.order = c('gender', 'education'), funs=list(sum_inc='sum'),
                    targets=list('income'))
    expect_equal(res[education=='College' & gender=='M', sum_inc], 2769)

    ## Try custom function, multiple targets
    res2 <- df2dtree(income, tree.order = c('gender', 'education'), funs=list(agg.inc.exp='agg'),
                    targets=list(c('income', 'expense')))
})


## df2dtree(income, funs=list('sum'), targets=list('income'))

## grp <- c('')
## for (i in c('education', 'gender', 'residence')) {
##     tr <- grp

##     df2dtree(income, tr, funs=list(agg='agg4', agg1='agg3'),
##              targets=list(agg1=c('income', 'income'), c('expense', 'income')))
##     agg4(income$income, income$income)
