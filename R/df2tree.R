##' @include utils.R
NULL

##' @title DF/DT to tree
##' @param data The data (required).
##' @param tree.order The order of categorical variables to aggregate by.  If empty, a total aggregation only.
##' @param funs A list of named functions to apply to each grouping of the data.  Can be NULL.
##' @param targets A list of target variables, an element for each function in \code{funs}.
##' @import data.table
##' @return Returns a tree in data.table form.
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
    ns <- make.unique(c("total", "index", "count", "level", names(data)))  # unique names
    total <- ns[[1L]]
    index <- ns[[2L]]
    count <- ns[[3L]]
    level <- ns[[4L]]
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
    res <- do.call("CJ", args=c("Total", lapply(dat[,ord[-1L],with=FALSE], levels)))
    setnames(res, names(res), ord)
    as.data.table(list(total='Total', lapply(1:(depth-1), function(i)
        rep(NA, ii[i+1]+1)))

                  
                  
    setkeyv(res, ord)
    res[, c(get("level"), get("count")) := list(rep(1:depth, times=diff(ii)), NA_integer_)]

    
    ## function to mapply at each level
    FUN <- function(fn, vars) { if (lengths(vars)) do.call(fn, unname(vars)) else NA }
    for (i in seq.int(depth)) {
        res[(ii[i] + 1L):ii[i + 1L],
            c(ord[1:i], get('level'), get('count'),
              get("outnames")) := dat[, {
                  i, .N, c(Map(FUN, funs, lapply(targets, function(x) .SD[, x, with=FALSE])))
            }, by=eval(ord[1:i])]]
    }
    return( res[] )
}




## A data.table like this with ids and levels
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


example.data <- data.frame(letters = sample(x = LETTERS,size = 20,replace = T),
                           numbers = sample(x = 0:9,size = 20,replace = T))

attributes(obj = example.data)

