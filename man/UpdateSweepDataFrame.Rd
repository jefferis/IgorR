\name{UpdateSweepDataFrame}
\alias{UpdateSweepDataFrame}
\title{Update the csv file summarising the sweeps in an Nclamp data folder}
\usage{
UpdateSweepDataFrame(folder, outfile = NULL, action = c("update", "force"),
  DryRun = FALSE)
}
\arguments{
  \item{folder}{Path to the folder}

  \item{outfile}{Path to outfile (default:
  /path/to/datafolder/datafolder.csv)}

  \item{action}{TODO update newer (default) or force update
  (not implemented)}

  \item{DryRun}{Report which files would be processed, but
  do nothing}
}
\value{
TRUE if something happened, FALSE otherwise
}
\description{
Update the csv file summarising the sweeps in an Nclamp
data folder
}
\author{
jefferis
}

