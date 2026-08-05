# Read the log table produced by Nclamp acquisition software for Igor

log tables are special Igor .pxp files that contain only variables. Each
entry corresponds to a single run of an Nclamp protocol, storing
information like protocol name, run time etc.

## Usage

``` r
ReadNclampLogTable(f, Verbose = FALSE)
```

## Arguments

- f:

  path to the log file.

- Verbose:

  whether to print status information while reading the file.

## Value

A dataframe containing a row for each acquisition protocol run.

## Author

jefferis
