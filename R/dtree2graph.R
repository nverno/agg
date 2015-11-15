##
## In utils.R: get_labels, make_labels edge_list 
##' @include utils.R
NULL

## Convert a dtree to an igraph
## dtree are ordered hierarchy from left to right by column
##' @import igraph
##' @import data.table
##' @param dtree data.table/data.frame output from df2dtree (ie has been aggegregated)
##' @param values Values to keep as attributes in graph (defaults to all)
##' @param tree.depth levels of tree to add to graph (defaults to TRUE meaning all). Can be an integer or the name of the last category to include.
##' @param copy Copy the data or modify in place (if data.table)
##' @export
dtree2graph <- function(dtree, values=TRUE, tree.depth=TRUE, copy=TRUE) {
    if (is.null(tree.depth) || length(tree.depth)==0 ||
        (is.character(tree.depth) && !(tree.depth %in% names(dtree))) ||
        (is.numeric(tree.depth) && tree.depth < 1))
        stop("Graph must have some depth.  'tree.depth' must be 'TRUE', a positive integer, or the name of one of the categorical columns.")
    if (length(tree.depth) > 1) {
        warning(sprintf("tree.depth takes a single value, using '%s'", as.character(tree.depth)))
        tree.depth <- tree.depth[[1]]
    }
    if (is.null(values)) values <- FALSE
    if (is.null(copy)) copy <- TRUE
    
    catCols <- sapply(dtree, class) == "factor"
    maxDepth <- match(FALSE, catCols) - 1
    depth <- if (is.character(tree.depth)) {
                  which(names(catCols) == tree.depth)
              } else if (is.numeric(tree.depth)) {
                      tree.depth
              } else maxDepth
    catCols <- catCols & names(dtree) %in% names(dtree)[1:depth]
    valCols <- !catCols & (isTRUE(values) | names(dtree) %in% values)
    sdcols <- names(catCols)[catCols]
    valcols <- names(valCols)[valCols]
    
    if (!inherits(dtree, "data.table")) {
        dtree <- as.data.table(dtree)
    } else if (copy) dtree <- copy(dtree)
    
    level <- attr(dtree, "level")                            # level column
    delcols <- names(dtree)[!(names(dtree) %in% c(sdcols, valcols, level))]
    if (length(delcols)) dtree[, get("delcols") := NULL]   # remove columns
    if (depth < maxDepth) dtree <- dtree[get("level") <= depth]
    setkeyv(dtree, c(get(level), sdcols))

    ## Make some unique names for output
    ns <- as.list(make.unique(c("id1", "id2", "label", names(dtree))))
    names(ns)[1:3] <- c("id1", "id2", "label")
    
    ## Create edgelist
    levs <- lapply(dtree[, catCols, with=FALSE], levels)                            # list of levels
    dtree[, unlist(get("ns")[c("id1", "id2")], use.names=FALSE) := edge_list(levs)] # add edges to tree

    ## Add labels
    make_labels(dtree, sdcols=sdcols, colname=ns[[3L]])

    ## Make graph
    ## id2 rows contain values corresponding to nodes == id2
    g <- graph.tree(0) +
        do.call(vertices,  # create vertices with all of the value attributes
                c(list(id=dtree[[ns$id2]], label=dtree[[ns$label]]),
                  as.list(dtree[, valcols, with=FALSE]))) +
        edges(c(rbind(dtree[[ns$id1]][-1L], dtree[[ns$id2]][-1L])))  # add edges
    return( g )
}

## ## Data.table version for nodes
## tst2 <- dtree[, .N, by = .(level = level - 1)][dtree, on='level', nomatch=0][
##   , .(nodes = rep(id, length.out = N[1])), by = level]
## ns <- tst2$nodes
## edges <- edge_list(levs)
## g <- graph_from_edgelist(do.call(cbind, edges))
## plot(g, layout=layout.reingold.tilford)

