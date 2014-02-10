\name{ReadNclampLogTable}
\alias{ReadNclampLogTable}
\title{Read the log table produced by Nclamp acquisition software for Igor}
\usage{
ReadNclampLogTable(f, Verbose = FALSE)
}
\arguments{
  \item{f}{Path to the log file}

  \item{Verbose}{Whether to print status information while
  reading the file}
}
\value{
A dataframe containing a row for each acquisition protocol
run
}
\description{
log tables are special Igor .pxp files that contain only
variables. Each entry corresponds to a single run of an
Nclamp protocol, storing information like protocol name,
run time etc.
}
\author{
jefferis
}

