context("df-trees")
library(data.table)
require(dplyr, quietly = TRUE)    # only to test against if wanted
require(treemap, quietly = TRUE)  # test against tree w/o all levels

## Using this data and income from data folder as tests
set.seed(0)
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


if (!require(dplyr))
    skip('dplyr is unavailable')

test_that('df2dtree creates list columns', {
    f <- function(x, y) list(list(x, y))  # must be list(list(...)) since a list is expected return

    ## The 'out' column should be a list of list(income, expense) at each aggregation level
    res <- df2dtree(income, 'gender', funs=list(out=f), targets=list(c('income', 'expense')))

    expect <- income %>% group_by('gender') %>% filter(gender == "F") %>% ungroup() %>%
        dplyr::select(income) %>% unlist(., use.names=FALSE)
      
    expect_equal(res[gender =="F", out][[1]][[1]], expect)
})


if (!require(treemap)) skip('treemap is unavailable')
test_that('df2dtree removes levels correctly', {
    ## For testing without keeping all level combinations
    ## Test against treemap function
    data(business, package="treemap")  # data with huge numbers of levels
    tree.order <- c("NACE1", "NACE2")
    { pdf(NULL);
        tst <- treemap(business,
                       index = tree.order,
                       vSize="employees",
                       title.legend="number of NACE4 categories",
                       type="value")
        dev.off()
    }
    tst <- as.data.table(tst$tm[,c("level", tree.order, "vSize")])
    setkeyv(tst, c(tree.order))
    
    res <- df2dtree(business, tree.order=tree.order,
                    funs=list(vSize=function(...) sum(..., na.rm=TRUE)),
                    targets=list('employees'), drop=TRUE)
    res <- res[vSize > 0, ]
    setkeyv(res, c("total", tree.order))
    
    expect_equal(nrow(res)-1, nrow(tst))
    expect_equal(res$vSize[-1L], tst$vSize)
})
