\name{tsp.igorwave}
\alias{tsp.igorwave}
\title{Return tsp attribute of igor wave (start, end, frequency)}
\usage{
tsp.igorwave(wave)
}
\arguments{
  \item{wave}{Igor wave loaded by read.ibw or read.pxp}
}
\value{
numeric vector with elements start, end, frequency
}
\description{
Note that end = (npts-1) * deltat
}
\author{
jefferis
}
\seealso{
\code{tsp}
}

