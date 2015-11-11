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

## res <- df2dtree(income, tree.order=c('education', 'gender'),
##                 funs=list(tst='lstFn'), targets=list(c('income', 'expense')))



## A data.table like this with ids and levels
library(data.table)
dat <- data.table(level = rep(1:4, times=2^(0:3)), id = 1:15)

## my normal way, not using data table would involve a split and rep
levs <- split(dat$id, dat$level)
nodes <- unlist(mapply(function(a,b) rep(a, length.out=b), head(levs, -1L),
                       tail(lengths(levs), -1L)), use.names = FALSE)

## Desired result
res <- cbind(nodes, dat$id[-1L])

## To visualize
library(igraph)
plot(graph_from_edgelist(cbind(nodes, dat$id[-1L])), layout=layout.reingold.tilford,
     asp=0.6)
