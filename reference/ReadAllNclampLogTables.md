# Read all Nclamp log tables from a directory into a list

Read all Nclamp log tables from a directory into a list

## Usage

``` r
ReadAllNclampLogTables(logfiledir, pattern = "_log[0-9]+[.]pxp$", ...)
```

## Arguments

- logfiledir:

  path to directory containing log files (pxp files).

- pattern:

  optional regular expression – see
  [`list.files`](https://rdrr.io/r/base/list.files.html).

- ...:

  additional parameters for
  [`ReadNclampLogTable`](https://jefferis.github.io/IgorR/reference/ReadNclampLogTable.md).

## Value

A named list containing one dataframe for each parsed log file.

## Author

jefferis

## Examples

``` r
if (FALSE) { # \dontrun{
logfiles <- ReadAllNclampLogTables("/GD/projects/PhysiologyData/logs")
str(logfiles)
} # }
```
