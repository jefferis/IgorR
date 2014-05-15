\name{SummariseSweepFile}
\alias{SummariseSweepFile}
\title{Extract summary information from an Nclamp/Igor Sweep File}
\usage{
SummariseSweepFile(f, Verbose = FALSE)
}
\arguments{
  \item{f}{path to an Nclamp/Igor PXP format sweep file.}

  \item{Verbose}{if \code{TRUE}, print details while
  parsing underlying PXP file.}
}
\value{
A list of about 25 fields summarising the sweep file.
}
\description{
e.g. for import into Physiology database
}
\examples{
l <- SummariseSweepFile(system.file("igor", "WedJul407c2_001.pxp", package="IgorR"))
cat("There are", l$NumWaves, "waves in the file each of total duration", l$StimWaveLength,
 "ms and sample duration", l$StimSampleInterval, "ms \\n")
}
\author{
jefferis
}

