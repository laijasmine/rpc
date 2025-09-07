#' create_class
#' Given a start date create class schedule
#'
#' @param start_date (chr) in the format of YYYY-MM-DD
#' @param sessions (int) number of sessions
#' @param biweekly (bool) weekly or bi-weekly classes Default = FALSE
#'
#' @returns tibble containing end_date, exclusions
#'
#' @examples
#' @export
create_class <- function(start_date, sessions, biweekly = FALSE) {
  start_date <- as.Date(start_date)
  sessions <- sessions - 1 # not including start date
  class_interval <- weeks(1:sessions)
  if (biweekly) {
    class_interval <- class_interval * 2
  }

  classes <- start_date + class_interval

  overlap_dates <- NA
  if (any(.holidays %in% classes)) {
    overlap_dates <- .holidays[.holidays %in% classes]
    additional_classes <- sessions + length(overlap_dates)
    classes <- start_date + weeks(1:additional_classes)
    overlap_dates <- glue_collapse(overlap_dates, sep = ", ")
  }

  tibble::tibble(
    end_date = classes[length(classes)],
    exclusions = overlap_dates
  )
}

#' get_instructors
#'
#' @returns data.frame
#'
#' @export
#' @examples
get_instructors <- function() {
  # googlesheets4::gs4_auth()
  ssid <- googlesheets4::as_sheets_id(
    "https://docs.google.com/spreadsheets/d/1ws1-H2vXkpDJXjL6v6j6azvW7dGJzsIk6MUK6z5dB2g"
  )
  googlesheets4::read_sheet(ssid, col_types = "ccccddD")
}

#' get_class_schedule
#'
#' @param instructor (chr) name of instructor
#'
#' @returns tibble
#'
#' @export
#' @examples get_class_schedule("David Liu")
get_class_schedule <- function(instructor = NULL) {
  instructor_schedule <- get_instructors()

  if (!is.null(instructor)) {
    instructor_schedule <- instructor_schedule |>
      filter(instructor == {{ instructor }})
  }

  instructor_schedule |>
    mutate(biweekly = ifelse(class_type == "member", TRUE, FALSE)) |>
    rowwise() |>
    mutate(class = create_class(start_date, sessions, biweekly)) |>
    unnest(class) |>
    select(-biweekly)
}

pick_up_date <- function(last_class) {
  sat <- last_class + (7 - wday(last_class)) + weeks(2)

  c(sat, (sat + days()))
}
