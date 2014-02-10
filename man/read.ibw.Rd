\name{read.ibw}
\alias{read.ibw}
\title{Read binary files in the Igor Binary Wave format (IBW)}
\usage{
read.ibw(wavefile, Verbose = FALSE, ReturnTimeSeries = FALSE,
  MakeWave = FALSE, HeaderOnly = FALSE)
}
\arguments{
  \item{wavefile}{Either a character vector containing the
  path to a file or an R \link{connection}}

  \item{Verbose}{Whether to print status information while
  reading the file}

  \item{ReturnTimeSeries}{Return as an R time series
  (package \code{\link{ts}})}

  \item{MakeWave}{Assign wave to a list in the global user
  environment}

  \item{HeaderOnly}{Only return the header of the Igor
  Wave}
}
\value{
returns a vector containing the wave data OR returns the
name of a new R vector containing the data which has been
made in the user environment
}
\description{
Read binary files in the Igor Binary Wave format (IBW)
}
\examples{
# return a list containing the wave
wavedata=read.ibw(system.file("igor","version5.ibw",package="IgorR"))
sum(wavedata)

# make a list containing the wave's data in the users's environment
wavename=read.ibw(system.file("igor","version5.ibw",package="IgorR"),MakeWave=TRUE)
sum(get(wavename))
}
\author{
jefferis
}

