## Convert a dtree to an igraph
## dtree are ordered hierarchy from left to right by column
##' @import igraph
##' @import data.table
##' @export
dtree2graph <- function(dtree, value=NULL, all.cat=TRUE) {
    catCols <- sapply(dtree, class) == "factor"
    depth <- which(!catCols)[[1L]] - 1L  # dtrees have have aggregated columns first
    if (!inherits(dtree, "data.table")) dtree <- as.data.table(dtree)
    level <- attr(dtree, "lev")
    dtree[, id := 1:.N]  # node ids
    setkeyv(dtree, c(get(level), names(dtree[catCols])))

    ## Create edgelist
    levs <- sapply(dtree[, catCols, with=FALSE], levels)
    lens <- lengths(levs)
    inds <- cumprod(lens)
    nodes <- mapply(function(a, b, c) rep(sequence(a), b)+c,
                    head(lens, -1L), head(inds, -1L), head(cumsum(lens) - lens, -1L))
    
    tst2 <- dtree[, .N, by = .(level = level - 1)][dtree, on='level', nomatch=0][
        , .(nodes = rep(id, length.out = N[1])), by = level]
    
    dtree[, `:=`(id = 1:.N, reps = rep(0:3, cumprod(lengths(levs))))]

    dtree[, id := 1:.N]
    
    ## Pull out the labels
    levs <- sapply(1:depth, function(i) levels(droplevels(dtree[[i]])))
    labels <- mapply(function(a, b) rep(a, length.out=b), levs, cumprod(lengths(levs)))

    g <- graph.tree(0) + vertices(ids = 1:nrow(dtree), label=unlist(labels))
    for (i in 1:depth) {
        
    }
    
}

## Get edgelist given sequence of branching factors
edge_list <- function(levs) {
    lens <- lengths(levs, use.names=FALSE)
    mult <- cumprod(lens)
    nodes <- mapply(function(a, b, c) rep(a:(a+b), length.out=c),
                    head(cumsum(lens), -1L), head(lens, -1L), mult[-1L])
}

## ## Testing
## library(igraph)
dtree <- df2dtree(income, tree.order = c('education', 'status', 'gender', 'residence'),
                funs=list(mean=function(...) mean(c(...), na.rm=TRUE),
                          meanfrac=function(income, expense) mean(income/expense, na.rm=TRUE)),
                targets=list(c('income', 'expense'), c('income', 'expense')))

## ## cbind(dtree$id[head(dtree$level,-1L) == dtree$level[-1L]])

## ## dtree[id[level < 4], .(level, id), by=level]

## dat <- data.table(level = rep(1:4, times=2^(0:3)), id = 1:15)

## ## my normal way, not using data table would involve a split and rep
## levs <- split(dat$id, dat$level)
## nodes <- unlist(mapply(function(a,b) rep(a, length.out=b), head(levs, -1L),
##                        tail(lengths(levs), -1L)), use.names = FALSE)

## ## Desired result
## res <- cbind(nodes, dtree$id[-1L])


## make_tree_edgelist = function(lev) cbind( rep(seq(2^(lev-1)-1), each=2), seq(2, 2^lev-1) )


                       
