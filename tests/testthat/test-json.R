context('json conversion')
library(data.table)
library(jsonlite)
library(d3treeR)

## res <- d3treeR:::convert_treemap(as.data.frame(dtree), "test")
if (!require(treemap)) skip("package 'treemap' not found.")
data(business)
{ pdf(NULL);
    tst <- treemap(business,
            index=c("NACE1", "NACE2"),
            vSize="employees",
            title.legend="number of NACE4 categories",
            type="value")
    dev.off()
}

## s <- function(...) sum(c(...), na.rm=TRUE)
## dtst <- df2dtree(business, tree.order=c('NACE1', 'NACE2'),
##                  funs=list(vSize='s'), targets=list('employees'))
## dtst <- dtst[vSize > 0]

## tst <- {sink(file='NULL'); hist(1:10); sink()}
