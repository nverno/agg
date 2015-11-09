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

mapply(function(a,b))

## Testing
library(igraph)
dtree <- df2dtree(income, tree.order = c('education', 'gender', 'residence'),
                funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                          meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                targets=list(c('income', 'expense'), c('income', 'expense')))

cbind(dtree$id[head(dtree$level,-1L) == dtree$level[-1L]])

dtree[id[level < 4], .(level, id), by=level]
