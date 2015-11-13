##' @include utils.R
NULL

##' @title DF/DT to tree
##' @param data The data (required).
##' @param tree.order The order of categorical variables to aggregate by.  If empty, a total aggregation only.
##' @param funs A list of named functions to apply to each grouping of the data.  Can be NULL.
##' @param targets A list of target variables, an element for each function in \code{funs}.
##' @param drop drop empty factor combinations (default is FALSE)
##' @import data.table
##' @return Returns a tree in data.table form (includes all factor combos even when NA values).
##' @export

df2dtree <- function(data, tree.order='', funs=NULL, targets=NULL, drop=FALSE) {
    if (!is.null(funs) && is.null(targets)) {
        stop('No targets supplied for functions.')
    } else if (!is.null(funs) && !is.null(targets) && length(funs) != length(targets))
        stop('Length of "funs" needs to match length of "targets".')
    if (is.null(funs) && !is.null(targets)) {
        warning('Targets are being ignored since no functions were defined.')
        targets <- NULL
    }
    ns <- make.unique(c("total", "level", "count", names(data)))  # unique names
    total <- ns[[1L]]
    level <- ns[[2L]]
    count <- ns[[3L]]
    ord <- nonEmpty(c(total, tree.order)) # ordering variables
    depth <- length(ord)                  # depth of tree (+1 for total)
    
    ## Ensure we have unique names for output columns
    if (!is.null(funs)) {
        if (is.null(names(funs))) {
            names(funs) <- paste0('output', seq_along(funs))
        } else if (any((inds <- names(funs) == ""))) {
            names(funs)[inds] <- paste0('output', seq_along(inds))
        }
    }
    outnames <- make.unique(c(names(funs), ns))[0:length(funs)]

    ## Factor category columns
    dat <- as.data.table(data)
    dat[, get('total') := factor('Total')]
    dat[, get("ord") := lapply(ord, function(x) factor(dat[[x]]))]

    ## Function to apply to each aggregation
    ## function arguments will be matched before, no need to match in do.call
    FUN <- function(fn, vars) do.call(fn, unname(vars))

    if (!drop) {                    
        ## indices for each aggregation level, so then can do in place
        ii <- cumsum(c(0, cumprod(lengths(lapply(dat[, ord, with=FALSE], levels)))))

        ## Get all level combinations and merge with data
        allLevs <- do.call("CJ", args=c("Total", lapply(dat[,ord[-1L],with=FALSE], levels)))
        setnames(allLevs, names(allLevs), ord)
        dat <- dat[allLevs, on=ord]  # merge
    } else {
        ii <- integer(length(sdcols)+1)  # calculate number of combinations
        for (i in seq_along(sdcols))
            ii[i+1] <- uniqueN(dat[, ord[1:i], with=FALSE])
        ii <- cumsum(ii)
    }

    ## Allocate result
    m <- tail(ii, 1L)  # number of rows in output
    res <- as.data.table(lapply(ord, function(i)
        factor(integer(m), levels=c(NA, levels(dat[[i]])))))
    setnames(res, ord)
    setkeyv(res, ord)

    ## Aggregate and apply functions
    for (i in seq.int(depth)) {
        res[(ii[i] + 1L):ii[i + 1L],
            c(ord[1:i], get('level'), get('count'),
              get("outnames")) := dat[, {
                  c(i, .N, Map(FUN, funs, lapply(targets, function(x) .SD[, x, with=FALSE])))
              }, keyby=eval(ord[1:i])]]
    }
    
    setattr(res, "lev", get("level"))  # track the column storing level
    return( res[] )
}

## ## Testing
## data <- income
## tree.order <- names(data)[-c(1:2)]
## funs <- list(sum="sum", sum="sum")
## targets <- list(c("income"), "expense")

## tree.order <- ''
## funs <- NULL
## targets <- NULL

## library(treemap)
## tree.order <- c("education", "status",    "gender",    "residence")
## pdf(NULL)
## tst <- treemap(income, index=tree.order, vSize='income', type='value')
## tst <- as.data.table(tst$tm)
## setkeyv(tst, c("level", tree.order))
## make_labels(tst, tree.order, 'labels')
## dev.off()

## dat <- as.data.table(diamonds)
## setkeyv(dat, sdcols)
## tst <- vector('numeric', length(sdcols))
## for (i in 1:length(sdcols)) tst[[i]] <- rleidv(dat[, sdcols[1:i], with=FALSE])


lst <- lapply(1:1000, seq)

library(microbenchmark)
m <- max(lengths(lst))

microbenchmark(
    t(`dim<-`(unlist(vapply(lst, FUN=`length<-`, (m <- max(lengths(lst)))),
                     FUN.VALUE='integer'),
              c(m ,m))),
    do.call(rbind, lapply(lst, "length<-", max(lengths(lst))))
)

x <- Map(":", 1, 1:4)

aperm(simplify2array(x), c(4,4))

t(x)
