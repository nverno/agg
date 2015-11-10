## Convert a dtree to an igraph
## dtree are ordered hierarchy from left to right by column
##' @import igraph
##' @import data.table

dtree2graph <- function(dtree, value=NULL, all.cat=TRUE) {
    catCols <- sapply(dtree, class) == "factor"
    depth <- which(!catCols)[[1L]] - 1L
    if (!inherits(dtree, "data.table")) dtree <- as.data.table(dtree)
    setkey(dtree, level)
    dtree[, `:=`(id = 1:.N, reps = rep(0:3, cumprod(lengths(levs))))]
    
    ## Pull out the labels
    levs <- sapply(1:depth, function(i) levels(droplevels(dtree[[i]])))
    labels <- mapply(function(a, b) rep(a, length.out=b), levs, cumprod(lengths(levs)))

    g <- graph.tree(0) + vertices(ids = 1:nrow(dtree), label=unlist(labels))
    for (i in 1:depth) {
        
    }
}


## Testing
library(igraph)
dtree <- df2dtree(income, tree.order = c('education', 'status', 'gender', 'residence'),
                funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                          meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                targets=list(c('income', 'expense'), c('income', 'expense')))

## cbind(dtree$id[head(dtree$level,-1L) == dtree$level[-1L]])

## dtree[id[level < 4], .(level, id), by=level]

dat <- data.table(level = rep(1:4, times=2^(0:3)), id = 1:15)

## my normal way, not using data table would involve a split and rep
levs <- split(dat$id, dat$level)
nodes <- unlist(mapply(function(a,b) rep(a, length.out=b), head(levs, -1L),
                       tail(lengths(levs), -1L)), use.names = FALSE)

## Desired result
res <- cbind(nodes, dtree$id[-1L])


make_tree_edgelist = function(lev) cbind( rep(seq(2^(lev-1)-1), each=2), seq(2, 2^lev-1) )



