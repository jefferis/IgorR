## Changes since last CRAN release

* overhauls datetime handling to fix a test failure on macosx arm:

  https://cran-archive.r-project.org/web/checks/2021/2021-10-05_check_results_IgorR.html

  that I was unable to address back in 2021 resulting in archival. I have 
  carried out cross-platform testing as below, so I am hopeful that there will
  be no build issues on CRAN platforms.
  
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

There were 2 NOTEs on win-builder devel

https://win-builder.r-project.org/4y3OYF9MRDBd/00check.log

relating to 2 issues:

1. X-CRAN-Comment: Archived on 2021-10-05 as check problems were not
    corrected in time.

2. * checking R code for possible problems ... NOTE
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
