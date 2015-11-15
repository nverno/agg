library(microbenchmark)
require(treemap)
require(dplyr)
require(lazyeval)

## data(business, package="treemap")  # data with huge numbers of levels
## tree.order <- c("NACE1", "NACE2")
## funs <- list(vSize = function(...) sum(..., na.rm=TRUE))
## targets <- list('employees')

## microbenchmark(
## { pdf(NULL);
##     tst <- treemap(business,
##                    index = tree.order,
##                    vSize="employees",
##                    title.legend="number of NACE4 categories",
##                    type="value")
##     dev.off()
## },
## {
##     res <- df2dtree(business, tree.order=tree.order,
##                     funs=list(vSize=function(...) sum(..., na.rm=TRUE)),
##                     targets=list('employees'), drop=TRUE)
## })

## tree.order <- c('ELEVCL', 'ASPCL', 'PPLOT', 'SPLOT')
## funs <- list(vSize = function(...) sum(..., na.rm=TRUE))
## targets <- list('BA')

## tst <- df2dtree(pp, tree.order, funs, targets, drop=TRUE)

## treemap(pp, tree.order, vSize='BA', type='value')

## Compare with dplyr 
tree.order <- c('ASPCL', 'ELEVCL', 'PPLOT', 'SPEC')
fn <- function(HT, BA) IQR(HT / BA, na.rm=TRUE)
funs <- list(out = "fn")
targets <- list(c('HT', 'BA'))

microbenchmark(
{
  res1 <- bind_rows(
    pp %>% dplyr::summarise(out = do.call(fn, list(HT, BA)), level=1, count=n()),
    pp %>% group_by_(tree.order[1L]) %>%
      dplyr::summarise(out=do.call(fn, list(HT, BA)), level=2, count=n()),
    pp %>% group_by_(.dots=tree.order[1:2]) %>% 
      dplyr::summarise(out=do.call(fn, list(HT, BA)), level=3, count=n()),
    pp %>% group_by_(.dots=tree.order[1:3]) %>%
      dplyr::summarise(out=do.call(fn, list(HT, BA)), level=3, count=n()),
    pp %>% group_by_(.dots=tree.order) %>%
      dplyr::summarise(out=do.call(fn, list(HT, BA)), level=4, count=n())
  ) %>% arrange_(c("level", tree.order))
},
{
  res <- df2dtree(pp, tree.order=tree.order, funs=funs,
                  targets=targets, drop.levels=TRUE)
})



set.seed(0)
dat <- data.frame(a=runif(10), b=runif(10), grp=factor(1:2))
fn <- function(x, y) IQR(x / y, na.rm = TRUE)
funs <- list(fn="fn")
targs <- list("a", "b")

dat %>% group_by(grp) %>% summarise_(out = do.call(fn, unname(.[unlist(targs)])))
dat %>% group_by(grp) %>% summarise_(out = do.call(fn, lapply(targs, function(x) .[[x]])))
dat %>% group_by(grp) %>% summarise(out = do.call(fn, list(a, b)))

