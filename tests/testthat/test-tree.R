context("df-trees")
library(data.table)
require(dplyr, quietly = TRUE)  # only to test against if wanted

## Using this data and income from data folder as tests
df <- data.frame(a=factor(sample(2, 20, TRUE)),
                 b=factor(sample(3, 20, TRUE)),
                 c=rnorm(20))

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
    expect_equal(df2dtree(df)$count, nrow(df))
    expect_equivalent(df2dtree(df, tree.order=c('a','b'),
                          funs=list(out=function(x,y) sum(x*y/20)),
                          targets=list(c('c', 'c')))$out[[1]], sum(df$c*df$c/20))
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

test_that('df2dtree creates empty rows for missing factor levels', {
    res <- df2dtree(income, tree.order = c('gender', 'education', 'status'),
                    funs=list(sum="sum"), targets=list("income"))
    expect <- sapply(c('gender', 'education', 'status'),
                     function(i) length(unique(income[[i]])))
    expect <- sum(cumprod(expect))+1  # for 'Total'
    expect_equal(nrow(res), expect)
})

test_that('df2dtree creates list columns', {
    f <- function(x, y) list(list(x, y))  # must be list(list(...)) since a list is expected return

    ## The 'out' column should be a list of list(income, expense) at each aggregation level
    res <- df2dtree(income, 'gender', funs=list(out=f), targets=list(c('income', 'expense')))

    if (!require(dplyr)) skip('dplyr is available')
    expect <- income %>% group_by('gender') %>% filter(gender == "F") %>% ungroup() %>%
        select(income) %>% unlist(., use.names=FALSE)
      
    expect_equal(res[gender =="F", out][[1]][[1]], expect)
})
