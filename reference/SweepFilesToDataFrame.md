# Summarise multiple sweep files into a single dataframe

Note that this is still a little fragile if the lists produced by
[`SummariseSweepFile`](https://jefferis.github.io/IgorR/reference/SummariseSweepFile.md)
do not have consistent field names.

## Usage

``` r
SweepFilesToDataFrame(ff)
```

## Arguments

- ff:

  paths to a set of sweep files.

## Value

A dataframe with rows for each sweep file.

## See also

SummariseSweepFile

## Author

jefferis
