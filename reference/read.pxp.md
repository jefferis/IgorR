# Reads an Igor Pro Packed Experiment (.pxp) file

Note that PXP files are only partially documented so some contents
cannot be parsed (e.g. image data). This function currently reads data
records (Igor waves and variables), history, procedures, recreation
macros and plain text notebooks. Formatted notebooks cannot be read.

## Usage

``` r
read.pxp(
  pxpfile,
  regex,
  ReturnTimeSeries = FALSE,
  Verbose = FALSE,
  StructureOnly = FALSE,
  ExtractText = FALSE,
  IgorPlatform = NULL,
  ...
)
```

## Arguments

- pxpfile:

  character vector naming a PXP file or an R
  [connection](https://rdrr.io/r/base/connections.html).

- regex:

  if `TRUE`, only read records (e.g. waves) in the PXP file whose names
  match a [regex](https://rdrr.io/r/base/regex.html).

- ReturnTimeSeries:

  if `TRUE`, Igor waves are returned as a `link{ts}` object with
  sensible x scaling (`FALSE` by default).

- Verbose:

  whether to print information to console during loading (numeric values
  are also allowed 0=none, 1=basic, 2=all).

- StructureOnly:

  (TODO) if `TRUE`, only the structure of the PXP file for inspection.

- ExtractText:

  whether to extract procedures, recreation macros, history and plain
  text notebooks (`FALSE` by default).

- IgorPlatform:

  OS on which Igor file was saved (windows or macintosh).

- ...:

  optional parameters passed to
  [read.ibw](https://jefferis.github.io/IgorR/reference/read.ibw.md).

## Value

A list containing all the individual waves or variables in the PXP file.

## Details

`IgorPlatform` will determine in which encoding text is read
(WINDOWS-1252 for windows and macintosh for macintosh). Unique
abbreviations are acceptable. Defaults to `"windows"` on Windows,
`"macintosh"` otherwise. Note that Igor Pro 5.5 added a PlatformRecord
to the PXP file format which is used to determine the file's platform of
origin when available. Since this is information straight from the
horse's mouth it will override the `IgorPlatform` argument.

## See also

Other igor-io:
[`WaveToTimeSeries()`](https://jefferis.github.io/IgorR/reference/WaveToTimeSeries.md),
[`read.ibw()`](https://jefferis.github.io/IgorR/reference/read.ibw.md)

## Author

jefferis

## Examples

``` r
r=read.pxp(system.file("igor","testexpt.pxp",package="IgorR"))
```
