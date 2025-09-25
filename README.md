
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rpc)](https://CRAN.R-project.org/package=rpc)
<!-- badges: end -->

Helper functions to work assist routine class scheduling tasks.

## Installation

You can install the development version of rpc from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("laijasmine/rpc")
```

## Generate class information

Provide a start date and number of sessions to generate a draft
schedule:

``` r
rpc::get_classes("2025-09-15", 9, TRUE)
#> [1] "2025-09-15" "2025-09-29" "2025-10-20" "2025-11-03" "2025-11-17"
#> [6] "2025-12-01" "2025-12-15" "2025-12-29" "2026-01-12"
```

Then use the generated class schedule and format it into a valid
dataframe for Google Calendar

``` r
class_schedule <- rpc::get_class_schedule(sheet = "2025 Fall")
calendar_df <- rpc::create_calendar_events(class_schedule)
calendar_df
#> # A tibble: 75 × 4
#>    Subject              `Start Time` `End Time` `Start Date`
#>    <glue>               <chr>        <chr>      <chr>       
#>  1 Monday public class  6:00 PM      9:00 PM    2025-09-15  
#>  2 Monday public class  6:00 PM      9:00 PM    2025-09-22  
#>  3 Monday public class  6:00 PM      9:00 PM    2025-09-29  
#>  4 Monday public class  6:00 PM      9:00 PM    2025-10-06  
#>  5 Monday public class  6:00 PM      9:00 PM    2025-10-20  
#>  6 Monday public class  6:00 PM      9:00 PM    2025-10-27  
#>  7 Monday public class  6:00 PM      9:00 PM    2025-11-03  
#>  8 Monday public class  6:00 PM      9:00 PM    2025-11-10  
#>  9 Monday public class  6:00 PM      9:00 PM    2025-11-17  
#> 10 Tuesday public class 10:00 AM     1:00 PM    2025-09-16  
#> # ℹ 65 more rows
```

Upload the saved csv file to Google:

``` r
readr::write_csv(calendar_df, tempfile())
```
