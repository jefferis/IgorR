# Package index

## Package Help

- [`IgorR-package`](https://jefferis.github.io/IgorR/reference/IgorR-package.md)
  [`IgorR`](https://jefferis.github.io/IgorR/reference/IgorR-package.md)
  : Read binary files saved by Igor Pro (including Neuromatic data)
- [`.ReadPackedHeader()`](https://jefferis.github.io/IgorR/reference/IgorR-private.md)
  : Private functions in IgorR Package

## Igor

Basic functions to read Igor Pro files

- [`read.ibw()`](https://jefferis.github.io/IgorR/reference/read.ibw.md)
  : Read binary files in the Igor Binary Wave format (IBW)
- [`read.pxp()`](https://jefferis.github.io/IgorR/reference/read.pxp.md)
  : Reads an Igor Pro Packed Experiment (.pxp) file
- [`WaveToTimeSeries()`](https://jefferis.github.io/IgorR/reference/WaveToTimeSeries.md)
  : Convert an Igor wave (wave list) loaded by read.ibw into an R time
  series
- [`tsp.igorwave()`](https://jefferis.github.io/IgorR/reference/tsp.igorwave.md)
  : Return tsp attribute of Igor wave (start, end, frequency)

## NClamp

Functions to handle NClamp data acquisition files

- [`SummariseSweepFile()`](https://jefferis.github.io/IgorR/reference/SummariseSweepFile.md)
  : Extract summary information from an Nclamp/Igor Sweep File
- [`SweepFilesToDataFrame()`](https://jefferis.github.io/IgorR/reference/SweepFilesToDataFrame.md)
  : Summarise multiple sweep files into a single dataframe
- [`UpdateSweepDataFrame()`](https://jefferis.github.io/IgorR/reference/UpdateSweepDataFrame.md)
  : Update the CSV file summarising the sweeps in an Nclamp data folder
- [`ReadNclampLogTable()`](https://jefferis.github.io/IgorR/reference/ReadNclampLogTable.md)
  : Read the log table produced by Nclamp acquisition software for Igor
- [`ReadAllNclampLogTables()`](https://jefferis.github.io/IgorR/reference/ReadAllNclampLogTables.md)
  : Read all Nclamp log tables from a directory into a list
