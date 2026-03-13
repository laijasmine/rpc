calculate_instructor_pay <- function(
  hourly_rate,
  class_type,
  number_of_courses,
  instructor
) {
  sessions <- 9
  class_length <- 3
  class_prep_hours <- 3

  tech_total_hours <- 0
  tech_hours_per_week <- 5

  if (class_type == "member") {
    sessions <- 6
    class_prep_hours <- 6
  }

  class_prep <- class_prep_hours * number_of_courses
  total_sessions <- sessions * number_of_courses
  total_hours <- (class_length * total_sessions) + class_prep

  if ((class_type == "public") && (instructor == "David Liu")) {
    kiln_hours <- ifelse(
      (instructor == "David Liu") &&
        (class_type != "member"),
      2,
      0
    ) *
      number_of_courses

    tech_weeks <- 12
    tech_total_hours <- tech_hours_per_week * tech_weeks

    total_hours <- total_hours + tech_total_hours + kiln_hours
  } else if ((class_type == "member") && (instructor == "David Liu")) {
    kiln_hours <- ifelse(
      (instructor == "David Liu"),
      2,
      0
    ) *
      number_of_courses

    total_hours <- total_hours + kiln_hours
  }

  return(hourly_rate * total_hours)
}
