# create_class Given a start date create class schedule

create_class Given a start date create class schedule

## Usage

``` r
create_class(start_date, sessions, class_frequency = "weekly")
```

## Arguments

- start_date:

  (chr) in the format of YYYY-MM-DD

- sessions:

  (int) number of sessions

- class_frequency:

  (str) frequency of classes, Options = "weekly", "biweekly", Default =
  "weekly"

## Value

tibble containing end_date, exclusions
