# @param start_date description
# @return data.frame containing start_date, end_date, exclusions
create_class <- function(start_date, sessions) {
  start_date <- as.Date(start_date)
  sessions   <- sessions - 1
  classes <- start_date + weeks(1:sessions)

  holiday_path <- "data/holidays.rds"
  holidays     <- readRDS(holiday_path)
  holiday_year <- year(holidays) |> unique()

  if (year(start_date) != holiday_year) {
    get_holidays(year(start_date))
    holidays <- readRDS(holiday_path)
  }

  overlap_dates <- NA
  if (any(as.Date(holidays) %in% classes)) {
    overlap_dates <- holidays[as.Date(holidays) %in% classes]
    additional_classes <- sessions + length(overlap_dates)
    classes <- start_date + weeks(1:additional_classes)
    overlap_dates <- list(overlap_dates)
  }

  tibble(
    start_date = start_date,
    end_date   = classes[length(classes)],
    exclusions = overlap_dates)
}

get_holidays <- function(year, save_location = "data/holidays.rds") {
  req <- request("https://canada-holidays.ca/api/v1/") |> 
    req_url_path_append("provinces") |> 
    req_url_path_append("BC") |> 
    req_url_query(year=year) |> 
    req_perform()

  resp <- resp_body_json(req)
  dates <- lapply(resp$province$holidays, function(x) x["observedDate"]) |> 
    unlist()

  saveRDS(dates, save_location)
}



