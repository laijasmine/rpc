library(httr2)
library(lubridate)

get_holidays <- function(year) {
  req <- request("https://canada-holidays.ca/api/v1/") |> 
    req_url_path_append("provinces") |> 
    req_url_path_append("BC") |> 
    req_url_query(year=year) |> 
    req_perform()

  resp <- resp_body_json(req)
  lapply(resp$province$holidays, function(x) x["observedDate"]) |> 
    unlist() |> 
    unname()
}

holiday_data <- lapply(2025:2031, get_holidays)
.holidays <- holiday_data |> unlist() |> as.Date()
usethis::use_data(.holidays, internal = TRUE, overwrite = TRUE)
