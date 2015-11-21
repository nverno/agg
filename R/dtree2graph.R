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

dtree2graph <- function(data, values=TRUE, tree.depth=TRUE, copy=TRUE) {
    if (is.null(tree.depth) || length(tree.depth)==0 ||
        (is.character(tree.depth) && !(tree.depth %in% names(data))) ||
        (is.numeric(tree.depth) && tree.depth < 2))
        stop("Graph must have some depth > 1.  'tree.depth' must be 'TRUE', a positive integer > 1, or the name of one of the categorical columns.")
    if (length(tree.depth) > 1) {
        warning(sprintf("tree.depth takes a single value, using '%s'", as.character(tree.depth[[1]])))
        tree.depth <- tree.depth[[1]]
    }
    if (is.null(values)) values <- FALSE
    if (is.null(copy)) copy <- TRUE
    
    level <- attr(data, "level")          # level column
    dropped <- attr(data, "drop.levels")  # full tree?
    catCols <- lapply(data, class) == "factor"
    depth <- if (is.character(tree.depth)) {
                  which(names(catCols) == tree.depth)
              } else if (is.numeric(tree.depth)) {
                      tree.depth
              } else max(data[[level]])
    catCols <- catCols & names(data) %in% names(data)[1:(depth-1)]
    valCols <- !catCols & (isTRUE(values) | names(data) %in% values)
    sdcols <- names(catCols)[catCols]
    valcols <- names(valCols)[valCols]
    
    if (!inherits(data, "data.table")) {
        res <- as.data.table(data)
    } else if (copy) res <- copy(data)

    delcols <- names(res)[!(names(res) %in% c(sdcols, valcols, level))]
    if (length(delcols)) res[, get("delcols") := NULL]   # remove columns
    if (depth < max(res[[level]])) res <- res[get("level") <= depth]

    ## Make some unique names for output
    ns <- as.list(make.unique(c("id1", "id2", "label", names(res))))
    names(ns)[1:3] <- c("id1", "id2", "label")

    ## If a partial tree, collapse, make_labels and return graph
    if (dropped) {
      make_labels(res, sdcols, ns$label)
      collapse(res, sdcols=sdcols, coln=c(ns$id1, ns$id2), copy=FALSE)
      g <- graph_from_data_frame(
        res[-1L, c(ns$id1, ns$id2), with=FALSE],     # edges
        vertices = res[, c(ns$id2, ns$label, valcols), with=FALSE])  # vertices (id2 has unique names)
      return( g )
    }

    ## Otherwise, construct edges (is this more efficient??)
    setkeyv(res, c(level, sdcols))
    
    ## Create edgelist
    levs <- lapply(res[, catCols, with=FALSE], levels)                            # list of levels
    levs <- c(total='Total', levs)
    res[, unlist(get("ns")[c("id1", "id2")], use.names=FALSE) := edge_list(levs)] # add edges to tree
    ## Add labels
    make_labels(res, sdcols=sdcols, colname=ns[[3L]])

    ## Make graph
    ## id2 rows contain values corresponding to nodes == id2
    setcolorder(res, c(ns$id1, ns$id2, setdiff(names(res), unlist(ns[c('id1', 'id2')]))))
    g <- graph_from_data_frame(
      res[-1L, c(ns$id1, ns$id2), with=FALSE],     # edges
      vertices = res[, c(ns$id2, ns$label, valcols), with=FALSE])  # vertices (id2 has unique names)

    ## Alternative, using edgelist, looks worse at a glance
    ## g <- graph.tree(0) +
    ##     do.call(vertices,  # create vertices with all of the value attributes
    ##             c(list(id=res[[ns$id2]], label=res[[ns$label]]),
    ##               as.list(res[, valcols, with=FALSE]))) +
    ##     edges(c(rbind(res[[ns$id1]][-1L], res[[ns$id2]][-1L])))  # add edges
    return( g )
}

## testing
load_graph <- function() {
    tree.order <- c('education', 'status', 'gender')
    funs <- list(out=mean)
    targets <- list('income')
    
    ## Two trees, w and w/o full levels
    dtree <- df2dtree(income, tree.order=tree.order, funs=funs, targets=targets,
                      drop.levels = FALSE)
    dtree1 <- df2dtree(income, tree.order = tree.order, funs=funs, targets=targets,
                       drop.levels = TRUE)
    list2env(
        list(
            values=TRUE,
            copy=TRUE,
            tree.depth=TRUE,
            tree.order=tree.order,
            data=income,
            funs=funs,
            targets=targets,
            dtree=dtree,
            dtree1=dtree1
        ), envir = baseenv()
    )
}
