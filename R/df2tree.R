##' @include utils.R
NULL

##' @title DF/DT to tree
##' @param data The data (required).
##' @param tree.order The order of categorical variables to aggregate by.  If empty, a total aggregation only.
##' @param funs A list of named functions to apply to each grouping of the data.  Can be NULL.
##' @param targets A list of target variables, an element for each function in \code{funs}.
##' @import data.table
##' @return Returns a tree in data.table form (includes all factor combos even when NA values).
##' @export

df2dtree <- function(data, tree.order='', funs=NULL, targets=NULL) {
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
    
    ## indices for each aggregation level, so then can do in place
    ii <- cumsum(c(0, cumprod(lengths(lapply(dat[, ord, with=FALSE], levels)))))

    ## Construct skeleton output
    m <- tail(ii, 1L)  # number of rows in output
    allLevs <- do.call("CJ", args=c("Total", lapply(dat[,ord[-1L],with=FALSE], levels)))
    setnames(allLevs, names(allLevs), ord)
    dat <- dat[allLevs, on=ord]  # add empty levels to make graphing easier
    
    res <- as.data.table(lapply(ord, function(i) factor(integer(m), levels=c(NA, levels(dat[[i]])))))
    setnames(res, ord)
    setkeyv(res, ord)
    
    ## function to mapply at each level
    ## proper column names are already found, don't need to match function arguments in do.call
    FUN <- function(fn, vars) do.call(fn, unname(vars)) 
    for (i in seq.int(depth)) {
        res[(ii[i] + 1L):ii[i + 1L],
            c(ord[1:i], get('level'), get('count'),
              get("outnames")) := dat[, {
                  c(i, .N, Map(FUN, funs, lapply(targets, function(x) .SD[, x, with=FALSE])))
              }, by=eval(ord[1:i])]]
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
