\name{SweepFilesToDataFrame}
\alias{SweepFilesToDataFrame}
\title{Summarise multiple sweep files into a single dataframe}
\usage{
SweepFilesToDataFrame(ff)
}
\arguments{
  \item{ff}{paths to a set of sweep files}
}
\value{
dataframe with rows for each sweep file
}
\description{
Note that this is still a little fragile if the lists
produced by SummariseSweepFile do not have consistent field
names
}
\author{
jefferis
}
\seealso{
SummariseSweepFile
}

