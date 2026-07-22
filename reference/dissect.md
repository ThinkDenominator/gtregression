# Dissect a dataset before regression

Returns a tidy summary of each variable's structure, missingness,
uniqueness, and suitability for use in regression models.

## Usage

``` r
dissect(data, verbose = FALSE, format = c("tibble", "gt", "flextable"))
```

## Arguments

- data:

  A data frame.

- verbose:

  Logical; if `TRUE`, print the summary and interpretation notes. The
  tibble is returned invisibly only when printed by the console.

- format:

  Output format. One of `"tibble"`, `"gt"`, or `"flextable"`. The
  default `"tibble"` keeps the output easy to use in pipelines.

## Value

A tibble, `gt_tbl`, or `flextable`, depending on `format`. The tibble
has columns: Variable, Type, Missing ( Levels, Compatibility, and Hint.

## Examples

``` r
dissect(data_birthwt)
#> # A tibble: 10 × 7
#>    Variable Type    `Missing (%)` Unique Levels Compatibility Hint              
#>    <chr>    <chr>   <chr>          <int> <chr>  <chr>         <chr>             
#>  1 low      integer 0%                 2 -      maybe         Two numeric value…
#>  2 age      integer 0%                24 -      compatible    Numeric variable …
#>  3 lwt      integer 0%                75 -      compatible    Numeric variable …
#>  4 race     integer 0%                 3 -      compatible    Numeric variable …
#>  5 smoke    integer 0%                 2 -      maybe         Two numeric value…
#>  6 ptl      integer 0%                 4 -      compatible    Numeric variable …
#>  7 ht       integer 0%                 2 -      maybe         Two numeric value…
#>  8 ui       integer 0%                 2 -      maybe         Two numeric value…
#>  9 ftv      integer 0%                 6 -      compatible    Numeric variable …
#> 10 bwt      integer 0%               131 -      compatible    Numeric variable …
dissect(data_birthwt, format = gt)


  


Dataset dissection before regression
```
