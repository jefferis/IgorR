\name{WaveToTimeSeries}
\alias{WaveToTimeSeries}
\title{Convert an Igor wave (or list of waves) loaded by read.ibw into an R time series}
\usage{
WaveToTimeSeries(WaveData, ReturnOriginalDataOnError = TRUE)
}
\arguments{
  \item{WaveData,}{a wave or list of waves}

  \item{ReturnOriginalDataOnError}{If we can't make a time
  series, return return original data (default TRUE)}
}
\value{
a time series or multi time series (ts, mts)
}
\description{
Where there are multiple waves, they are assumed to be of
compatible lengths so that they can be joined together by
cbind.
}
\author{
jefferis
}

