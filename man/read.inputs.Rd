% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbRake.R
\name{read.inputs}
\alias{read.inputs}
\title{Read xlsx or csv file}
\usage{
read.inputs(inputFile)
}
\arguments{
\item{inputFile}{is a string of the name of an xlsx or csv file to be read in}
}
\description{
reads in xlsx or csv input files, detecting those file types from file extension, from inputs
folder. This is a helper function used in \code{\link{dbRake}}.
}
\seealso{
The overall raking function: \code{\link{dbRake}}()

Other raking helpers: 
\code{\link{add.random.fraction.to.cols}()},
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
\code{\link{real.to.int}()},
\code{\link{rename.age.grps}()},
\code{\link{rounded}()}
}
\concept{raking helpers}
