##' @name nonEmpty
##' @param x 
nonEmpty <- function(x) x[nzchar(x)]

## From Hadleys book somewhere
##' @name compact
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

################################################################################
##
##                                Graph utils
##
################################################################################
## Return ids for edgelist given list of factor levels
##' @title edge_list
##' @param levs list of levels of factors in tree
##' @return list of tails and head in edgelist for full tree (all combinations)
##' @export
edge_list <- function(levs) {
    lens <- lengths(levs, use.names=FALSE)
    mult <- cumprod(lens)
    starts <- c(0, cumsum(mult))
    id1 <- unlist(mapply(function(a, b, c) rep.int(seq.int(a)+b, c),
                           head(mult, -1L), head(starts, -2L), lens[-1L]))
    list(id1=c(1, id1), id2=seq_len(sum(mult)))  # return c(1, 1) for root
}

## Return the rightmost non-missing values in data.table for each row
## http://stackoverflow.com/questions/33664659/extract-last-non-missing-value-in-row-with-data-table/33674200#33674200
##' @param x `as.list(data.table)`
##' @param ans ignore (recursive)
##' @param wh ignore (recursive)
##' @return character vector of labels
get_labels <- function(x, ans = rep_len(NA, length(x[[1L]])),
                       wh = seq_len(length(x[[1L]])))
{
    if(!length(wh)) return(ans)
    ans[wh] = as.character(x[[length(x)]])[wh]
    Recall(x[-length(x)], ans, wh[is.na(ans[wh])])
}

## Modification of 
## http://stackoverflow.com/questions/33664659/extract-last-non-missing-value-in-row-with-data-table/33674200#33674200
## Retrieves the right-most non-missing value from columns, and modifies in place
##' @param x data.table to add labels to
##' @param sdcols Columns to operate over (in the order of aggregation)
##' @param colname name of new column to add (the label column name)
make_labels <- function(x, sdcols, colname) {
    x[, get("colname") := NA_character_]
    wh = x[, .I]
    for (v in rev(sdcols)) {
      if (!length(wh)) break
      set(x, j=colname, i=wh, v = x[[v]][wh])
      wh = wh[is.na(x[[colname]][wh])]
    }
    if (is.na(x[[get('colname')]][[1L]]))
      x[1L, get('colname') := 'Total']
}

## Collapse data.table categorical columns into two columns: head,
## tail, common form for making graphs, ie with googleVis
##' @importFrom stringi stri_replace_all stri_replace_last
##' @param dtree A data.table in tree format
##' @param coln names of head -> tail columns
##' @param copy logical: should the data.table be copied
collapse <- function(dtree, sdcols, coln = c("id1", "id2"), copy=TRUE) {
  x <- if (copy) copy(dtree) else dtree
  val1 <- do.call(paste, c('Total', as.list(x[, sdcols, with=FALSE]), sep="."))
  val1 <- stringi::stri_replace_all(val1, '', regex='[.]?NA[.]?')
  val2 <- stringi::stri_replace_last(val1, '', regex='[.]{1}[^.]*$')
  x[, get('coln') := list(val2, val1)]
  ## x[1, coln[[2]] := NA_character_]
  x[, get('sdcols') := NULL]
  setcolorder(x, c(coln, setdiff(names(x), coln)))
}



