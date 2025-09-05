#' @import lubridate

# @param start_date description
# @return data.frame containing start_date, end_date, exclusions
create_class <- function(start_date, sessions) {
  start_date <- as.Date(start_date)
  sessions   <- sessions - 1
  classes <- start_date + weeks(1:sessions)

  holiday_year <- year(.holidays) |> unique()

  if (year(start_date) != holiday_year) {
    get_holidays(year(start_date))
  }

  overlap_dates <- NA
  if (any(.holidays %in% classes)) {
    overlap_dates <- .holidays[.holidays %in% classes]
    additional_classes <- sessions + length(overlap_dates)
    classes <- start_date + weeks(1:additional_classes)
    overlap_dates <- list(overlap_dates)
  }

  tibble::tibble(
    start_date = start_date,
    end_date   = classes[length(classes)],
    exclusions = overlap_dates)
}

get_instructors <- function(sheet) {
  # read sheet

}

get_class_schedule <- function(instructor = NULL) {
  create_class()
  get_instructors()
}

create_contract <- function(instructor) {
  
}

create_instructor_package <- function(season) {
  create_contract()
  get_class_schedule()
}

create_city_document <- function(season) {
  create_contract()
  get_class_schedule()
}
