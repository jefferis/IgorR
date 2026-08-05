# Extract summary information from an Nclamp/Igor Sweep File

e.g. for import into Physiology database

## Usage

``` r
SummariseSweepFile(f, Verbose = FALSE)
```

## Arguments

- f:

  path to an Nclamp/Igor PXP format sweep file.

- Verbose:

  if `TRUE`, print details while parsing underlying PXP file.

## Value

A list of about 25 fields summarising the sweep file.

## Author

jefferis

## Examples

``` r
l <- SummariseSweepFile(system.file("igor", "WedJul407c2_001.pxp", package="IgorR"))
cat("There are", l$NumWaves, "waves in the file each of total duration", l$StimWaveLength, 
 "ms and sample duration", l$StimSampleInterval, "ms \n") 
#> There are 100 waves in the file each of total duration 20 ms and sample duration 0.025 ms 
```
