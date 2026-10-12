# Transform a `data.table` in the long-format to a `data.table` in the start-stop format

This function transforms a `data.table` in the long-format (one row per
person per time point) to a `data.table` in the start-stop format (one
row per person-specific period in which no variables changed).

## Usage

``` r
long2start_stop(data, id, time, varying, overlap=FALSE,
                check_inputs=TRUE)
```

## Arguments

- data:

  A `data.table` or an object that can be coerced to a `data.table`
  (such as a `data.frame`) including data in the long-format.

- id:

  A single character string specifying a unique person identifier
  included in in `data`.

- time:

  A single character string specifying a time variable included in in
  `data` coded as integers starting from 1.

- varying:

  A character vector specifying names of variables included in in `data`
  that may change over time.

- overlap:

  Specifies whether the intervals should overlap or not. If `TRUE`, the
  `"stop"` column is simply increased by one, as compared to the output
  when `overlap=FALSE`. This means that changes for a given \\t\\ are
  recorded at the start of the next interval, but the previous interval
  ends on that same day.

- check_inputs:

  Whether to check if the user input is correct or not. Can be turned
  off by setting it to `FALSE` to save computation time.

## Details

This function relies on `data.table` syntax to make the data
transformation as RAM efficient and fast as possible.

## Value

Returns a `data.table` containing the columns `.id` (the unique person
identifier), `.time` (an integer variable encoding the time) and all
other variables included in the input `data` in the long format.

## Author

Robin Denz

## Examples

``` r
library(simDAG)
library(data.table)

# generate example data in long format
long <- data.table(.id=rep(seq_len(10), each=5),
                   .time=rep(seq_len(5), 10),
                   A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                           TRUE),
                   B=FALSE)
setkey(long, .id, .time)

# transform to start-stop format
long2start_stop(data=long, id=".id", time=".time", varying=c("A", "B"))
#>       .id start  stop      A      B
#>     <int> <int> <int> <lgcl> <lgcl>
#>  1:     1     1     5  FALSE  FALSE
#>  2:     2     1     5  FALSE  FALSE
#>  3:     3     1     5  FALSE  FALSE
#>  4:     4     1     5  FALSE  FALSE
#>  5:     5     1     5  FALSE  FALSE
#>  6:     6     1     5  FALSE  FALSE
#>  7:     7     1     5  FALSE  FALSE
#>  8:     8     1     5  FALSE  FALSE
#>  9:     9     1     3  FALSE  FALSE
#> 10:     9     4     5   TRUE  FALSE
#> 11:    10     1     3  FALSE  FALSE
#> 12:    10     4     5   TRUE  FALSE
```
