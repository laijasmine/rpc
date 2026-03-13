# create_calendar_events

This generates a dataframe that conforms to the google calendar import
requirements. The dataframe can be saved as a csv then imported into the
desired calendar.

## Usage

``` r
create_calendar_events(class_schedule)
```

## Arguments

- class_schedule:

  (dataframe) class schedule table

## Value

google calendar import compatible data.frame

## Details

The import accepted columns: Subject (Required) The name of the event
Start Date (Required) The first day of the event Example: 05 / 30 / 2020
Start Time - The time the event begins Example: 10:00 AM End Date The
last day of the event Example: 05 / 30 / 2020 End Time

## Examples

``` r
class_schedule <- get_class_schedule(sheet = "2025 Fall")
#> Error in gs4_auth(): Can't get Google credentials.
#> ℹ Are you running googlesheets4 in a non-interactive session? Consider:
#> • Call `gs4_deauth()` to prevent the attempt to get credentials.
#> • Call `gs4_auth()` directly with all necessary specifics.
#> ℹ See gargle's "Non-interactive auth" vignette for more details:
#> ℹ <https://gargle.r-lib.org/articles/non-interactive-auth.html>
calendar_df <- create_calendar_events(class_schedule)
#> Error: object 'class_schedule' not found
#readr::write_csv(calendar_df, tempfile())
```
