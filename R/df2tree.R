##' @include utils.R
NULL

##' @title DF/DT to tree
##' @param data The data (required).
##' @param tree.order The order of categorical variables to aggregate by. (required)
##' @param funs A list of named functions to apply to each grouping of the data.  Can be NULL.
##' @param targets A list of target variables, an element for each function in \code{funs}.
##' @param drop.levels drop empty factor combinations (default is FALSE)
##' @param drop.cols drop extra columns, ie. those not used in aggregation or functions (default to TRUE)
##' @import data.table
##' @return Returns a tree in data.table form (includes all factor combos even when NA values).
##' @export

df2dtree <- function(data, tree.order, funs=NULL, targets=NULL, drop.levels=FALSE,
                     drop.cols=TRUE) {
    if (!is.null(funs) && is.null(targets)) {
        stop('No targets supplied for functions.')
    } else if (!is.null(funs) && !is.null(targets) && length(funs) != length(targets))
        stop('Length of "funs" needs to match length of "targets".')
    if (is.null(funs) && !is.null(targets)) {
        warning('Targets are being ignored since no functions were defined.')
        targets <- NULL
    }
     if (length(nonEmpty(tree.order)) == 0 && length(nonEmpty(targets)) == 0)
        stop('No aggregation variables or targets were supplied.')
    
    dat <- as.data.table(data)
    if (drop.cols && length((ns <- names(dat)[!names(dat) %in% c(tree.order, unlist(targets))])))
        dat[, ns := NULL, with=FALSE]

    ns <- make.unique(c("level", "count", names(data)))  # unique names
    level <- ns[[1L]]
    count <- ns[[2L]]
    ord <- nonEmpty(tree.order) # ordering variables
    depth <- length(ord)+1      # depth of tree (+1 for total)
    
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
    dat[, get("ord") := lapply(ord, function(x) factor(dat[[x]]))]

    ## Function to apply to each aggregation
    ## function arguments will be matched before, no need to match in do.call
    FUN <- function(fn, vars) do.call(fn, unname(vars))

    if (!drop.levels) {                    
        ## Get all level combinations and merge with data
        allLevs <- do.call("CJ", args=lapply(dat[,ord,with=FALSE], levels))
        setnames(allLevs, names(allLevs), ord)
        dat <- dat[allLevs, on=ord]   # merge
    }

    ## build list of aggregations and rbindlist them together
    res <- vector("list", depth)
    res[[1L]] <- dat[, c(.N, Map(FUN, funs, lapply(targets, function(x) dat[, x, with=FALSE])))]
    setnames(res[[1L]], names(res[[1L]]), c(count, outnames))
    for (i in seq.int(2L, depth)) {
        res[[i]] <- dat[,c(
            .N, Map(FUN, funs, lapply(targets, function(x) .SD[, x, with=FALSE]))),
            by=eval(ord[1:(i-1L)])]
        setnames(res[[i]], names(res[[i]]), c(ord[1:(i-1L)], count, outnames))
    }
    levs <- sapply(res, uniqueN)
    res <- rbindlist(res, fill=TRUE)
    res[, get('level') := rep(1:depth, times=levs)]
    setkeyv(res, ord)

    setcolorder(res, c(ord, level, count, outnames))
    setattr(res, "level", get("level"))  # track the column storing level
    setattr(res, "drop.levels", drop.levels)  # were factor levels dropped? for graphing
    return( res[] )
}

