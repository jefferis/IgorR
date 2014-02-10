\name{read.pxp}
\alias{read.pxp}
\title{Reads an Igor Pro Packed Experiment (.pxp) file}
\usage{
read.pxp(pxpfile, regex, ReturnTimeSeries = FALSE, Verbose = FALSE,
  StructureOnly = FALSE, ExtractText = FALSE, IgorPlatform = NULL, ...)
}
\arguments{
  \item{pxpfile}{Character vector naming a PXP file or an R
  \link{connection}}

  \item{regex}{only read records (e.g. waves) in the pxp
  file whose names match a \link{regex}}

  \item{ReturnTimeSeries}{Igor waves are returned as a ts
  object with sensible x scaling (FALSE by default)}

  \item{Verbose}{whether to print information to console
  during loading (numeric values are also allowed 0=none,
  1=basic, 2=all)}

  \item{StructureOnly}{TODO Only the structure of the pxp
  file for inspection}

  \item{ExtractText}{Whether to extract procedures,
  recreation macros, history and plain text notebooks
  (FALSE by default)}

  \item{IgorPlatform}{OS on which Igor file was saved
  (windows or macintosh)}

  \item{...}{Optional parameters passed to \link{read.ibw}}
}
\value{
A list containing all the individual waves or variables in
the pxp file
}
\description{
Note that pxp files are only partially documented so some
contents cannot be parsed (e.g. image data). This function
currently reads data records (Igor waves and variables),
history, procedures, recreation macros and plain text
notebooks. Formatted notebooks cannot be read.
}
\details{
\code{IgorPlatform} will determine in which encoding text
is read (WINDOWS-1252 for windows and macintosh for
macintosh). Unique abbreviations are acceptable. Defaults
to "windows" on windows, "macintosh" otherwise. Note that
Igor Pro 5.5 added a PlatformRecord to the pxp file format
which is used to determine the file's platform of origin
when available. Since this is informatino straight from the
horse's mouth it will override the \code{IgorPlatform}
argument.
}
\examples{
r=read.pxp(system.file("igor","testexpt.pxp",package="IgorR"))
}
\author{
jefferis
}

