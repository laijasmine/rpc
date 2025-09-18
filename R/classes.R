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
  classes <- get_classes(start_date, sessions, biweekly)
  overlap_dates <- get_exclusions(classes)

  tibble::tibble(
    end_date = classes[length(classes)],
    exclusions = overlap_dates
  )
}

#' get classes vector
get_classes <- function(start_date, sessions, biweekly) {
  start_date <- as.Date(start_date)
  sessions <- sessions - 1 # not including start date
  class_interval <- weeks(1:sessions)
  if (biweekly) {
    class_interval <- class_interval * 2
  }

  classes <- start_date + class_interval

  if (any(.holidays %in% classes)) {
    overlap_dates <- .holidays[.holidays %in% classes]
    additional_classes <- sessions + length(overlap_dates)
    # TODO fix this could be simplified
    classes <- c(start_date, (start_date + weeks(1:additional_classes)))
    classes <- classes[-which(classes %in% .holidays)]
  }

  classes
}

# get exclusion dates
get_exclusions <- function(classes) {
  overlap_dates <- NA

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
#' @returns data.frame
#'
#' @export
#' @examples
get_instructors <- function(sheet) {
  # googlesheets4::gs4_auth()
  ssid <- googlesheets4::as_sheets_id(
    #"https://docs.google.com/spreadsheets/d/1ws1-H2vXkpDJXjL6v6j6azvW7dGJzsIk6MUK6z5dB2g/edit?gid=0#gid=0"
    # classes documents
    "https://docs.google.com/spreadsheets/d/1vivUrj8WSWI2hHTOlgdWfiHdyTRd0zCEIX9xJnHAf54/edit?gid=0#gid=0"
  )
  googlesheets4::read_sheet(ssid, sheet = sheet, col_types = "ccccddD")
}

#' get_class_schedule
#'
#' @param instructor (chr) name of instructor
#'
#' @returns tibble
#'
#' @export
#' @examples get_class_schedule("David Liu")
get_class_schedule <- function(instructor = NULL, sheet) {
  instructor_schedule <- get_instructors(sheet)

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


#' Subject (Required) The name of the event
#' Start Date (Required) The first day of the event
#' Example: 05 / 30 / 2020
#' Start Time - The time the event begins
#' Example: 10:00 AM
#' End Date The last day of the event
#' Example: 05 / 30 / 2020
#' End Time
create_calendar_event <- function(class_schedule) {
  class_schedule |>
    mutate(
      Subject = glue("{day} {class_type} class"),
      class_dates = get_classes(start_date, sessions, TRUE)
    ) |>
    tidyr::separate_wider_delim(
      class_time,
      delim = "-",
      names = c("Start Time", "End Time")
    ) |>
    rename_with(~ stringr::str_replace(.x, "_", " ")) |>
    rename_with(stringr::str_to_title) |>
    select(Subject, `Start Time`, `End Time`, `Start Date`, `End Date`) #|>
  #readr::write_csv("test_gc.csv")
}
