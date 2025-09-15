library(dplyr)
library(googlesheets4)
library(glue)
library(tidyr)
library(purrr)
library(quarto)
library(rpc)
library(lubridate)

# Variables
year <- 2025
session <- "Winter"

# Class info
class_schedule <- get_class_schedule(sheet = glue("{year} {session}"))
instructors <- unique(class_schedule$instructor)

# test sheet
volunteer_sheet <- "https://docs.google.com/spreadsheets/d/1iFxPH5qV7jh8YYKW2_WpNnq-PtmgAx-r3hDk5WRkOVc/edit?gid=0#gid=0"
session_yr <- glue("{session} {year}")

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
  sheet = session_yr
)

## Contracts per instructor
for (i in instructors) {
  instructor_classes <- class_schedule |>
    filter(instructor == {{ i }}, class_type == "public") |>
    mutate(exclusions = ifelse(is.na(exclusions), "None", exclusions)) |>
    select(-class_type, -instructor, -cost)

  quarto_render(
    input = glue("{getwd()}/inst/contract_template.qmd"),
    output_file = glue(
      "{year}_{session}_public_contract_invoice_{i}.pdf"
    ),
    output_format = "pdf",
    execute_dir = getwd(),
    execute_params = list(
      instructor = i,
      day = instructor_classes$day,
      sessions = instructor_classes$sessions,
      class_time = instructor_classes$class_time,
      start_date = as.character(instructor_classes$start_date),
      end_date = as.character(instructor_classes$end_date),
      exclusions = instructor_classes$exclusions,
      course_description = "Wheel Throwing and Hand Building - Beginner and Intermediate Level"
    )
  )
}

#member class
member_class <- class_schedule |>
  filter(class_type == "member")
member_instructor <- unique(member_class$instructor)

instructor_classes <- member_class |>
  mutate(exclusions = ifelse(is.na(exclusions), "None", exclusions)) |>
  select(-class_type, -instructor, -cost)

quarto_render(
  input = glue("{getwd()}/inst/contract_template.qmd"),
  output_file = glue(
    "{year}_{session}_member_contract_invoice_{member_instructor}.pdf"
  ),
  output_format = "pdf",
  execute_dir = getwd(),
  execute_params = list(
    instructor = member_instructor,
    day = instructor_classes$day,
    sessions = instructor_classes$sessions,
    class_time = instructor_classes$class_time,
    start_date = as.character(instructor_classes$start_date),
    end_date = as.character(instructor_classes$end_date),
    exclusions = instructor_classes$exclusions,
    course_description = "Member Intermediate Classes"
  )
)
