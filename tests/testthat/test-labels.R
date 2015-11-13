context('extracting labels')
library(data.table)

## http://stackoverflow.com/questions/33664659/extract-last-non-missing-value-in-row-with-data-table
set.seed(0)
dat <- sapply(split(letters[1:25], rep.int(1:5, 5)), sample, size=8, replace=TRUE)
dat[upper.tri(dat)] <- NA
dat[4:5, 4:5] <- NA                              # the real data isnt nice and upper.triangular
dat <- data.frame(dat, stringsAsFactors = TRUE)  # factor columns
setDT(dat)
expect <- c("u", "q", "w", "h", "r", "t", "e", "t")

test_that('make_labels produces the correct labels', {
    make_labels(dat, sdcols=1:5, colname="labels")
    expect_equivalent(dat[,labels], expect)
})
