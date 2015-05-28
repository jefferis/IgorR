## Test environments
* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0
* winbuilder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs on any platform.

There were 3 NOTEs on win-builder devel

http://win-builder.r-project.org/aSRc7wR7Blt3/00check.log

relating to 3 issues:

1. Possibly mis-spelled words in DESCRIPTION:
  Wavemetrics (9:16)
  ibw (10:53)
  pxp (10:24, 12:13)

The first is a company name, the other two are file formats.

2. Found the following assignments to the global environment:
File 'IgorR/R/ReadIgorBinary.R':
  assign(WaveName, rval, envir = .GlobalEnv)

This has been discussed in previous submissions and is not a default behaviour
but must be explicitly requested by the user by setting a documented argument
(?read.ibw). The rationale is that some users like to be able to replicate the
behaviour of Igor Pro in which individual records (aka waves) within a single
file are available as individual objects in the user environment. Once again
this must be explicitly requested.

3. No repository set, so cyclic dependency check skipped

For information only.

## Downstream dependencies
There are presently no downstream dependencies on CRAN.
