#' create_class
#' Given a start date create class schedule
#'
#' @param start_date (chr) in the format of YYYY-MM-DD
#' @param sessions (int) number of sessions
#' @param class_frequency (str) frequency of classes, Options = "weekly", "biweekly", Default = "weekly"
#'
#' @returns tibble containing end_date, exclusions
#'
#' @examples
#' @export
create_class <- function(start_date, sessions, class_frequency = "weekly") {
  classes <- get_classes(start_date, sessions, class_frequency)
  overlap_dates <- get_exclusions(start_date, sessions, class_frequency)

  tibble::tibble(
    end_date = classes[length(classes)],
    exclusions = overlap_dates
  )
}


#' get classes vector
#'
#' @param start_date (chr) in the format of YYYY-MM-DD
#' @param sessions (int) number of sessions
#' @param class_frequency (str) frequency of classes, Options = "weekly", "biweekly", Default = "weekly"
#'
#' @returns
#'
#' @export
#' @examples
get_classes <- function(start_date, sessions, class_frequency = "weekly") {
  start_date <- as.Date(start_date)
  class_interval <- "weeks"
  sessions <- sessions - 1

  if (class_frequency == "biweekly") {
    sessions <- sessions * 2
    class_interval <- "2 weeks"
  }

  classes <- seq(
    start_date,
    start_date + weeks(sessions),
    by = class_interval
  )

  if (any(.holidays %in% classes)) {
    overlap_dates <- .holidays[.holidays %in% classes]
    additional_classes <- sessions + length(overlap_dates)

    # extend the classes the additional week(s)
    if (class_frequency == "biweekly") {
      before_holiday <- seq(
        start_date,
        overlap_dates - weeks(),
        by = class_interval
      )

      for (h in seq_along(overlap_dates)) {
        after_holiday <- seq(
          overlap_dates[h] + weeks(),
          overlap_dates[h] + weeks(sessions - (length(before_holiday) * 2) + 1),
          by = class_interval
        )

        classes <- c(before_holiday, after_holiday)
      }
    } else {
      classes <- seq(
        start_date,
        start_date + weeks(additional_classes),
        by = class_interval
      )

      # drop overlap classes
      classes <- classes[-which(classes %in% overlap_dates)]
    }
  }

  classes
}


#' get exclusion dates
#'
#' @param start_date (date) start date
#' @param sessions (integer) number of sessions
#' @param class_frequency (str) frequency of classes, Options = "weekly", "biweekly", Default = "weekly"
#'
#' @returns
#'
#' @export
#' @examples
get_exclusions <- function(start_date, sessions, class_frequency = "weekly") {
  overlap_dates <- NA

  # get classes
  start_date <- as.Date(start_date)
  class_interval <- "weeks"
  sessions <- sessions - 1

  if (class_frequency == "biweekly") {
    sessions <- sessions * 2
    class_interval <- "2 weeks"
  }

  classes <- seq(
    start_date,
    start_date + weeks(sessions),
    by = class_interval
  )

  if (any(.holidays %in% classes)) {
    overlap_dates <- glue_collapse(
      .holidays[.holidays %in% classes],
      sep = ", "
    )
  }

  overlap_dates
}

#' get_instructors
#'
#' @params sheet (str) name of sheet to use
#'
#' @returns data.frame class schedule dataframe
#'
#' @export
#' @examples
get_instructors <- function(sheet) {
  ssid <- googlesheets4::as_sheets_id(
    "https://docs.google.com/spreadsheets/d/1vivUrj8WSWI2hHTOlgdWfiHdyTRd0zCEIX9xJnHAf54/edit?gid=0#gid=0"
  )

  suppressMessages(
    googlesheets4::read_sheet(ssid, sheet = sheet, col_types = "ccccddD--")
  )
}

#' get_class_schedule
#'
#' @param instructor (chr) name of instructor
#' @param sheet (str) name of sheet to use
#'
#' @returns tibble
#'
#' @export
#' @examples \dontrun{get_class_schedule("First Last", sheet = "2026 Winter")}
get_class_schedule <- function(instructor = NULL, sheet) {
  instructor_schedule <- get_instructors(sheet)

  if (!is.null(instructor)) {
    instructor_schedule <- instructor_schedule |>
      filter(instructor == {{ instructor }})
  }

  instructor_schedule |>
    mutate(
      class_frequency = ifelse(class_type == "member", "biweekly", "weekly")
    ) |>
    rowwise() |>
    mutate(class = create_class(start_date, sessions, class_frequency)) |>
    tidyr::unnest(class) |>
    select(-class_frequency)
}

#' Public class pick up date
#' Sets the pick up date 2 weeks after the last public class
#'
#' @param last_class the date of the last class
#'
#' @returns date
#'
#' @export
#' @examples
get_pick_up_date <- function(last_class) {
  sat <- last_class + (7 - wday(last_class)) + weeks(2)

  c(sat, (sat + lubridate::days()))
}


#' create_calendar_events
#'
#' This generates a dataframe that conforms to the google calendar import requirements.
#' The dataframe can be saved as a csv then imported into the desired calendar.
#'
#' The import accepted columns:
#' Subject (Required) The name of the event
#' Start Date (Required) The first day of the event
#' Example: 05 / 30 / 2020
#' Start Time - The time the event begins
#' Example: 10:00 AM
#' End Date The last day of the event
#' Example: 05 / 30 / 2020
#' End Time
#'
#' @param class_schedule (dataframe) class schedule table
#'
#' @returns google calendar import compatible data.frame
#'
#' @export
#' @examples
#' class_schedule <- get_class_schedule(sheet = "2025 Fall")
#' calendar_df <- create_calendar_events(class_schedule)
#' #readr::write_csv(calendar_df, tempfile())
create_calendar_events <- function(class_schedule) {
  schedule <- class_schedule |>
    mutate(
      Subject = glue::glue("{day} {class_type} class"),
      class_frequency = ifelse(class_type == "member", "biweekly", "weekly")
    ) |>
    dplyr::rowwise() |>
    mutate(
      Weeks = purrr::pmap(
        list(start_date, sessions, class_frequency),
        get_classes
      )
    ) |>
    tidyr::unnest(Weeks) |>
    tidyr::separate_wider_delim(
      class_time,
      delim = "-",
      names = c("Start Time", "End Time")
    ) |>
    dplyr::rename_with(~ stringr::str_replace(.x, "_", " ")) |>
    dplyr::rename_with(stringr::str_to_title) |>
    select(Subject, `Start Time`, `End Time`, `Start Date` = Weeks) |>
    mutate(dplyr::across(tidyselect::everything(), \(x) {
      trimws(x, which = "both")
    }))
}
