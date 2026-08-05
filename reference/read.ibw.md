# Read binary files in the Igor Binary Wave format (IBW)

Read binary files in the Igor Binary Wave format (IBW)

## Usage

``` r
read.ibw(
  wavefile,
  Verbose = FALSE,
  ReturnTimeSeries = FALSE,
  MakeWave = FALSE,
  HeaderOnly = FALSE
)
```

## Arguments

- wavefile:

  either a character vector containing the path to a file or an R
  [connection](https://rdrr.io/r/base/connections.html).

- Verbose:

  if `TRUE`, print status information while reading the file.

- ReturnTimeSeries:

  if `TRUE`, return as an R time series (package
  [`ts`](https://rdrr.io/r/stats/ts.html)).

- MakeWave:

  if `TRUE`, assign wave to a list in the global user environment.

- HeaderOnly:

  if `TRUE`, only return the header of the Igor Wave.

## Value

A vector containing the wave data or, if `MakeWave == TRUE`, returns the
name of a new R vector containing the data which has been made in the
user environment

## See also

Other igor-io:
[`WaveToTimeSeries()`](https://jefferis.github.io/IgorR/reference/WaveToTimeSeries.md),
[`read.pxp()`](https://jefferis.github.io/IgorR/reference/read.pxp.md)

## Author

jefferis

## Examples

``` r
# return a list containing the wave
wavedata=read.ibw(system.file("igor","version5.ibw",package="IgorR")) 
sum(wavedata)
#> [1] 15

# make a list containing the wave's data in the users's environment
wavename=read.ibw(system.file("igor","version5.ibw",package="IgorR"),MakeWave=TRUE) 
sum(get(wavename))
#> [1] 15
```
