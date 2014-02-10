\name{SummariseSweepFile}
\alias{SummariseSweepFile}
\title{Extract summary information from an Nclamp/Igor Sweep File}
\usage{
SummariseSweepFile(f, Verbose = F)
}
\arguments{
  \item{f}{path to an Nclamp/Igor pxp format sweep file}

  \item{Verbose}{print details while parsing underlying pxp
  file}
}
\value{
a list of about 25 fields summarising the sweep file
}
\description{
e.g. for import into Physiology database
}
\examples{
l=SummariseSweepFile(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
cat("There are",l$NumWaves,"waves in the file each of total duration",l$StimWaveLength,
 "ms and sample duration",l$StimSampleInterval,"ms \\n")
}
\author{
jefferis
}

