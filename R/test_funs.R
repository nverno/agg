## Should all be the same if names match
##' @name agg
agg <- function(a, b) sum(sqrt((a - b)^2))

##' @name agg1
agg1 <- function(income, expense) sum(income / expense) * 100

##' @name agg2
agg2 <- function(expense, income) sum(income / expense) * 100

##' @name agg3
agg3 <- function(a, b) sum(a / b) * 100

##' @name agg4
agg4 <- function(b, a) sum(a / b) * 100

## income <- 1:10
## expense <- 10:1

## agg1(income, expense)
## agg2(income, expense)
## agg3(income, expense)
## agg4(income, expense)

## do.call('agg1', list(income, expense))
## do.call('agg2', list(income=income, expense))

## Returning lists in a column requires a double wrap
##' @name lstFn
lstFn <- function(a,b) list(list(a=a, b=b))

## load testing variables
load_vars <- function() {
    tree.order = c('education', 'status', 'gender', 'residence')    
    list2env(list(
        tree.order = tree.order,
        valCols = c('mean', 'meanfrac'),
        dtree = df2dtree(income, tree.order=tree.order,
                          funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                                    meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                          targets=list(c('income', 'expense'), c('income', 'expense')))
      ), envir=globalenv())
}

