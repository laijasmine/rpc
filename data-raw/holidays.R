get_holidays <- function(year) {
  req <- httr2::request("https://canada-holidays.ca/api/v1/") |> 
    httr2::req_url_path_append("provinces") |> 
    httr2::req_url_path_append("BC") |> 
    httr2::req_url_query(year=year) |> 
    httr2::req_perform()

  resp <- httr2::resp_body_json(req)
  .holidays <- lapply(resp$province$holidays, function(x) as.Date(x["observedDate"])) |> 
    unlist() |> 
    unname()

  usethis::use_data(.holidays, internal = TRUE, overwrite = TRUE)
}
