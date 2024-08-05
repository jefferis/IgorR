# IgorR 0.9.0
* fix: handling of Igor dates using timechange library (#6)
* remove warnings due to recent changes in how readChar handles strings 
  containing embedded nulls
* fix URLs for CRAN
* dev: switch to github actions for R CMD check and pkgdown (and remove travis)

# IgorR 0.8.2
* minor: package docs, spelling, author formatting

# IgorR 0.8.1
* Fix namespace and canonical URL issues for CRAN

# IgorR 0.8
* fix bug in reading igor waves with embedded non-null terminated strings (#3)
* fix bug in reading igor pxp files with names that are not valid R names e.g. 
  begining with an underscore. (#2)
* Thanks to Troy Margrie, Mateo Velez and Charly Rousseau for the reports/test
  data
* Minor description fixes for R 3.2.0
* dev: switch to tests in tests/testthat (making them an optional install) but 
  keeping some igor waves required for examples in inst/igor
* dev: switch to new style travis config
* dev: upgrade to roxygen2 4.1.1

# IgorR 0.7.2
* minor doc fixes

# IgorR 0.7.1
* Fix encoding issue of Igor pxp files containing high bytes.
  Pointed out by Brian Ripley on Solaris, fixed across all platforms with help
  from Ashley Manton.
* Fix error on R<3.0.0 caused by use of new keep.source argument for parse().

# IgorR 0.7
* Add support for extracting history, procedures and plain text notebooks
  (contributed by Thomas Braun). See `read.pxp()` ExtractText argument.
* dev: added support for travis continuous integration
* dev: repo now includes man files to allow devtools installation
* dev: disable unreliable test of date-time parsing

# IgorR 0.6
* Some significant speed optimisations without any change in functionality
  up to 2x faster for small waves (<1Mb) on my system
* Now imports bitops, tools

# IgorR 0.5-4
* Add regular expression to restrict `ReadAllNclampLogTables()` to log files
* Package documentation

# IgorR 0.5-3
* Minor fix to error checks in `tsp.igorwave()`

# IgorR 0.5-2
* Minor fixes to remove outstanding NOTES from R 2.15's check

# IgorR 0.5-1
* Option to return Igor wave data as time series when reading pxp files.
* Fix problem with empty waves often encountered when reading waves in PXP files
  as time series. 

# IgorR 0.5
* rename ReadIgorPackedExperiment and ReadIgorBinary functions to
  `read.pxp()` and `read.ibw()`, respectively. 
* Fix an encoding bug on Windows MAC->macintosh
  (pointed out by Brian Ripley)
* Teach `read.pxp()` to read string variables with non ASCII characters
* Teach `read.pxp()` to read only waves matching a regex
* Teach `read.pxp()` to read structure only for waves when requested
* Documentation changes for github 

# IgorR 0.4
* First version on CRAN
