library(dplyr)
library(googlesheets4)
library(glue)
library(tidyr)
# googlesheets4::gs4_auth()

# Variables
# test sheet
volunteer_sheet <- "https://docs.google.com/spreadsheets/d/1iFxPH5qV7jh8YYKW2_WpNnq-PtmgAx-r3hDk5WRkOVc/edit?gid=0#gid=0"
session <- "Winter 2026"

class_schedule <- get_class_schedule()

# Class Rep
volunteer_sign_up <- class_schedule |>
  mutate(
    Volunteer = ifelse(
      (instructor == "David Liu") & (class_type != "member"),
      "Loren",
      ""
    ),
    day = glue("{day} {class_type} class rep")
  ) |>
  select(day, time = class_time, date = start_date, Volunteer)

# Pick up date
last_class <- max(class_schedule$end_date)

cleanup <- data.frame(
  day = c("Clean up (2x volunteers on agreed upon date after last class)"),
  time = c("TBA"),
  date = last_class
)

pickup <- data.frame(
  day = c("Pick up", "Pick up"),
  time = c("14:00 - 16:00", "14:00 - 16:00"),
  date = pick_up_date(last_class)
)

ssid <- as_sheets_id(volunteer_sheet)
sheet_write(
  bind_rows(volunteer_sign_up, cleanup, pickup),
  ss = ssid,
  sheet = session
)

## Contracts
library(purrr)
library(quarto)
library(rpc)
library(glue)

year         <- 2025
session      <- "winter"
hourly_rate  <- 36
class_length <- 3
class_prep   <- 3
class_schedule <- get_class_schedule()
instructors <- unique(class_schedule$instructor)

# per instructor
i <- "David Liu"
instructor_classes <- class_schedule |>
  filter(instructor == {{ i }}, class_type == "public") |>
  select(-class_type, -instructor, -cost)

number_of_courses <- NROW(instructor_classes)
kiln <- ifelse(
  i == "David Liu",
  "+ 2 hours kiln loading per course",
  ""
)
kiln_hours <- ifelse(i == "David Liu", 2, 0) * number_of_courses

class_prep <- class_prep_hours * number_of_courses
total_sessions <- sum(as.numeric(instructor_classes$sessions))
total_class_time <- (class_length * total_sessions) +
  class_prep +
  kiln_hours
total_contract_amount <- hourly_rate * total_class_time

quarto_render(
  input = glue("{getwd()}/inst/contract_template.qmd"),
  output_file = glue("{year}_{session}_{i}.pdf"),
  output_format = "pdf",
  execute_dir = getwd(),
  execute_params = list(
    instructor = i,
    day = instructor_classes$day,
    sessions = instructor_classes$sessions,
    class_time = instructor_classes$class_time,
    start_date = as.character(instructor_classes$start_date),
    end_date = as.character(instructor_classes$end_date) #,
    #exclusions = instructor_classes$exclusions,
    class_prep = class_prep,
    total_sessions = total_sessions,
    total_class_time = total_class_time,
    total_contract_amount = total_contract_amount
  )
)
