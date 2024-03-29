% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbRake.R
\name{add.random.fraction.to.cols}
\alias{add.random.fraction.to.cols}
\title{Add random fraction for sorting}
\usage{
add.random.fraction.to.cols(df, my_col)
}
\arguments{
\item{df}{a dataframe (e.g., VarRow (e.g., for "LHA"), whichever columns being raked over, "Min")}

\item{my_col}{a column in df (i.e., "Min" which is the minimum value for each row). This is the
column that the rows need to be sorted by. That is, the df needs to be sorted with the VarRow with
the smallest minimum value to the one with the largest minimum value, in preparation for raking.}
}
\value{
original dataframe without original my_col, but with a new sort_rows column with the
order needed to sort rows
}
\description{
Add a random number to a specified column, "my_col", then sort rows based on my_col, with the
random fraction used to break any ties. This is a helper function used in \code{\link{dbRake}}
within the raking algorithm functions (\code{\link{allowNegsnoMargin}}, \code{\link{noNegsnoMargin}},
and \code{\link{noNegsneedMargin}}), when there are more than two row groups (e.g., Regions, 5-year
Age Groups, Ages, more than two sexes (when Stats Can adds more than Male and Female)).
}
\seealso{
The overall raking function: \code{\link{dbRake}}()

Other raking helpers: 
\code{\link{adjustSex}()},
\code{\link{allowNegsnoMargin}()},
\code{\link{calc.cols}()},
\code{\link{multiRake}()},
\code{\link{noNegsneedMargin}()},
\code{\link{noNegsnoMargin}()},
\code{\link{prep.prorate.col}()},
\code{\link{prorate.col}()},
\code{\link{prorate.row}()},
\code{\link{prorate}()},
\code{\link{read.inputs}()},
\code{\link{real.to.int}()},
\code{\link{rename.age.grps}()},
\code{\link{rounded}()}
}
\concept{raking helpers}
