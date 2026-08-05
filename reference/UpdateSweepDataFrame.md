# Update the CSV file summarising the sweeps in an Nclamp data folder

Update the CSV file summarising the sweeps in an Nclamp data folder

## Usage

``` r
UpdateSweepDataFrame(
  folder,
  outfile = NULL,
  action = c("update", "force"),
  DryRun = FALSE
)
```

## Arguments

- folder:

  path to the folder.

- outfile:

  path to outfile (default: `/path/to/datafolder/datafolder.csv`).

- action:

  update newer (default) or force update (not yet implemented).

- DryRun:

  if `TRUE`, report which files would be processed, but do nothing.

## Value

`TRUE` if something happened, `FALSE` otherwise.

## Author

jefferis
