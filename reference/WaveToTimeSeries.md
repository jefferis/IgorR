# Convert an Igor wave (wave list) loaded by read.ibw into an R time series

Where there are multiple waves, they are assumed to be of compatible
lengths so that they can be joined together by `cbind`.

## Usage

``` r
WaveToTimeSeries(WaveData, ReturnOriginalDataOnError = TRUE)
```

## Arguments

- WaveData, :

  a wave or list of waves

- ReturnOriginalDataOnError:

  If we can't make a time series, return return original data (default
  TRUE)

## Value

a time series or multi time series
([`ts`](https://rdrr.io/r/stats/ts.html), `mts`)

## See also

Other igor-io:
[`read.ibw()`](https://jefferis.github.io/IgorR/reference/read.ibw.md),
[`read.pxp()`](https://jefferis.github.io/IgorR/reference/read.pxp.md)

## Author

jefferis
