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
