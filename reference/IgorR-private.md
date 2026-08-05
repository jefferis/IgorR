# Private functions in IgorR Package

`.ReadPackedHeader` reads the short record header from the current
location in a PXP file

## Usage

``` r
.ReadPackedHeader(con, endian)
```

## Arguments

- con:

  an R connection to the file we are reading

- endian:

  either little or big

## Value

a list containing information about the current record

## Details

Note that the `recordType` will be one of the constants from Igor's enum
`PackedFileRecordType`

## Author

jefferis
