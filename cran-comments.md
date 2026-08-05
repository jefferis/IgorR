## Changes since last CRAN release

* replaces deprecated special argument names in `structure()` test fixtures
* fixes some URLs in README

## Test environments

* winbuilder (devel)
* macbuilder (release)
* local macosx install, R 4.4.1
* github actions:
  * macosx-latest
  * ubuntu-latest (devel, release, oldrel-1)
  * windows-latest

## R CMD check results
There were no ERRORs or WARNINGs on any platform.

There was 1 NOTE on win-builder devel

https://win-builder.r-project.org/OyigICeuuqIH/00check.log

* checking R code for possible problems ... NOTE
Found the following assignments to the global environment:
File 'IgorR/R/ReadIgorBinary.R':
  assign(WaveName, rval, envir = .GlobalEnv)

This has been discussed in previous submissions and is not a default behaviour
but must be explicitly requested by the user by setting a documented argument
(?read.ibw). The rationale is that some users like to be able to replicate the
behaviour of Igor Pro in which individual records (aka waves) within a single
file are available as individual objects in the user environment. Once again
this must be explicitly requested.

## Downstream dependencies
There are presently no downstream dependencies on CRAN.
