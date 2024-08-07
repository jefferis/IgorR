% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadNclamp.R
\name{UpdateSweepDataFrame}
\alias{UpdateSweepDataFrame}
\title{Update the CSV file summarising the sweeps in an Nclamp data folder}
\usage{
UpdateSweepDataFrame(
  folder,
  outfile = NULL,
  action = c("update", "force"),
  DryRun = FALSE
)
}
\arguments{
\item{folder}{path to the folder.}

\item{outfile}{path to outfile (default: \code{/path/to/datafolder/datafolder.csv}).}

\item{action}{update newer (default) or force update (not yet implemented).}

\item{DryRun}{if \code{TRUE}, report which files would be processed, but do
nothing.}
}
\value{
\code{TRUE} if something happened, \code{FALSE} otherwise.
}
\description{
Update the CSV file summarising the sweeps in an Nclamp data folder
}
\author{
jefferis
}
