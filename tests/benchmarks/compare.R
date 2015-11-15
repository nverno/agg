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
funs <- list(out = fn)
targets <- list(c('HT', 'BA'))

## Some sample data, function, and variables to interpolate
set.seed(0)
dat <- data.frame(a=runif(10), b=runif(10))
func <- function(x, y) { print(x); print(y); IQR(x / y, na.rm = TRUE) }
fns <- list(fn="func")
targs <- list("a", "b")

library(dplyr)
cl <- interp(~do.call(fn, xs),
             .values=list(
               fn=fns$fn,
               xs = list(. = targs)))

# ~do.call("fn", list("a", "b"))

dat %>%
  dplyr::summarize_(out = eval(cl))

dat %>% summarise(out = do.call(funs$fn, unname(.[unlist(targs)])))
dat %>% summarise(out = do.call(fn, lapply(targs, function(x) .[[x]])))

## Expected result
dat %>%
  summarise(out = do.call(fn, list(a, b)))
#        out
# 1 1.084402

fn <- function(x, y) { print(x); print(y); IQR(x / y, na.rm = TRUE) }
dat %>%
  summarise_(out = interp(~do.call(fn, xs), fn=funs$fn, xs=targs))
# [1] "a"
# [1] "b"
# Error: non-numeric argument to binary operator


microbenchmark(
{
    res1 <- bind_rows(
        pp %>% dplyr::summarise_(out = interp(~do.call(fn ,targs), 
                                              .values = list(fn=fn, targs=targets)))

        pp %>% group_by(ASPCL) %>% dplyr::summarise(out=fn(HT, BA), level=2, count=n()),
        pp %>% group_by(ASPCL, ELEVCL) %>% 
            dplyr::summarise(out=fn(HT, BA), level=3, count=n()),
        pp %>% group_by(ASPCL, ELEVCL, PPLOT) %>%
            dplyr::summarise(out=fn(HT, BA), level=3, count=n()),
        pp %>% group_by(ASPCL, ELEVCL, PPLOT, SPEC) %>%
            dplyr::summarise(out=fn(HT, BA), level=4, count=n())
    ) %>% arrange(level, ASPCL, ELEVCL, PPLOT, SPEC)

},
{
    res <- df2dtree(pp, tree.order=tree.order, funs=funs, targets=targets, drop.levels=TRUE, in.place=TRUE)
})


################################################################################
##
##                                  Scratch
##
################################################################################
library(data.table)

# Potential species names
pot.spp <- apply(expand.grid(letters[1:10], letters[1:10]),1, paste, collapse="")

# Potential sites
pot.site <- apply(expand.grid(1:5, 1:5),1, paste, collapse="-")

# Size of data set
nsamp <- 1E5

# species, sites, years
spp <- sample(pot.spp, nsamp, replace=TRUE, prob=(1:length(pot.spp)))
site <- sample(pot.site, nsamp, replace=TRUE)
year <- sample(1:20, nsamp, replace=TRUE)

# data set
test <- data.table(year=year, spp=spp, site=site, key=c("year", "spp", "site"), val=rnorm(nsamp))

sdcols <- c('spp','site','year')
setkeyv(test, sdcols)
dat <- unique(test[, sdcols, with=FALSE])

dat[, count:=.N, by="spp,site"]


fs <- lapply(test[, sdcols, with=FALSE], unique)

res <- dat[spp %in% sample(spp, 2), ][
  site %in% sample(site, 3), ][
    year %in% sample(year, 2), ]

table(res$spp, res$site, res$year)

dat[, ]
