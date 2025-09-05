#' @import lubridate

# @param start_date description
# @return data.frame containing start_date, end_date, exclusions
# @export
create_class <- function(start_date, sessions = 9) {
  start_date <- as.Date(start_date)
  sessions   <- sessions - 1
  classes <- start_date + weeks(1:sessions)

  holiday_year <- year(.holidays) |> unique()

  # TODO how to update only once
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

get_instructors <- function() {
  # read sheet
  # googlesheets4::gs4_auth()
  ssid <- googlesheets4::as_sheets_id("https://docs.google.com/spreadsheets/d/1ws1-H2vXkpDJXjL6v6j6azvW7dGJzsIk6MUK6z5dB2g")
  googlesheets4::read_sheet(ssid)
}

get_class_schedule <- function(instructor = NULL) {
  instructor_schedule <- get_instructors()
  new_schedule <- lapply(instructor_schedule$start_date, create_class) |> 
    dplyr::bind_rows()

  dplyr::bind_cols(instructor_schedule, new_schedule, )
  
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
