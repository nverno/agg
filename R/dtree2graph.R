## Convert a dtree to an igraph
## dtree are ordered hierarchy from left to right by column
##' @import igraph
##' @import data.table
##' @param dtree data.table/data.frame output from df2dtree (ie has been aggegregated)
##' @param value Values to keep as attributes in graph (defaults to all)
##' @param cols Aggregated columns (strings) to use for graph (defaults to all)
##' @export
dtree2graph <- function(dtree, values=TRUE, cols=TRUE) {
    catCols <- sapply(dtree, class) == "factor" & (isTRUE(cols) | names(dtree) %in% cols)
    valCols <- !catCols & (isTRUE(values) | names(dtree) %in% values)
    sdcols <- names(catCols)[catCols]
    valcols <- names(valCols)[valCols]
    
    if (!inherits(dtree, "data.table")) dtree <- as.data.table(dtree)
    level <- attr(dtree, "lev")                            # level column
    dtree[, c(sdcols, valcols), with=FALSE]
    setkeyv(dtree, c(get(level), names(dtree[catCols])))

    ## Create edgelist
    ns <- make.unique(c("id1", "id2", names(dtree)))[1:2]  # head/tail node ids
    levs <- lapply(dtree[, catCols, with=FALSE], levels)   # list of levels
    dtree[, get("ns") := edge_list(levs)]                  # add edges to tree

    ## Extract labels: colInd
    labels <- dtree[, colInd := sum(!is.na(.SD)), by=1:nrow(dtree), .SDcols=sdcols][
      , as.character(.SD[[.BY[[1]]]]), by=colInd, .SDcols=sdcols]$V1

    ## Make graph
    ## id2 rows contain values corresponding to nodes == id2
    g <- graph.tree(0) +
        do.call(vertices,  # create vertices with all of the value attributes
                c(list(id=dtree$id2, label=labels), as.list(dtree[, valcols, with=FALSE]))) +
        edges(c(rbind(dtree$id1[-1L], dtree$id2[-1L])))  # add edges
    return( g )
}

## Return ids for edgelist given list of factor levels
##' @title edge_list
##' @param levs list of levels of factors in tree
##' @return list of tails and head in edgelist
##' @export
edge_list <- function(levs) {
    lens <- lengths(levs, use.names=FALSE)
    mult <- cumprod(lens)
    starts <- c(0, cumsum(mult))
    id1 <- unlist(mapply(function(a, b, c) rep.int(seq.int(a)+b, c),
                           head(mult, -1L), head(starts, -2L), lens[-1L]))
    list(id1=c(NA, id1), id2=seq_len(sum(mult)))
}

## ## Data.table version for nodes
## tst2 <- dtree[, .N, by = .(level = level - 1)][dtree, on='level', nomatch=0][
##   , .(nodes = rep(id, length.out = N[1])), by = level]
## ns <- tst2$nodes
## edges <- edge_list(levs)
## g <- graph_from_edgelist(do.call(cbind, edges))
## plot(g, layout=layout.reingold.tilford)






