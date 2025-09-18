test_that("class dates correct", {
  monday <- create_class("2025-09-15", 9)
  expect_equal(monday$end_date, as.Date("2025-11-17"))
  expect_equal(unlist(monday$exclusions), c("2025-10-13"))

  tuesday <- create_class("2025-09-16", 9)
  expect_equal(tuesday$end_date, as.Date("2025-11-25"))
  expect_equal(unlist(tuesday$exclusions), "2025-09-30, 2025-11-11")

  wednesday <- create_class("2025-09-17", 9)
  expect_equal(wednesday$end_date, as.Date("2025-11-12"))
  expect_equal(wednesday$exclusions, NA)

  saturday <- create_class("2025-09-20", 9)
  expect_equal(saturday$end_date, as.Date("2025-11-15"))
  expect_equal(saturday$exclusions, NA)

  member_mon <- create_class("2025-09-22", 6, TRUE)
  expect_equal(member_mon$end_date, as.Date("2025-12-01"))
  expect_equal(member_mon$exclusions, NA)

  member_fri <- create_class("2025-09-19", 6, TRUE)
  expect_equal(member_fri$end_date, as.Date("2025-11-28"))
  expect_equal(member_fri$exclusions, NA)
})

test_that("pick up date is correct", {
  expect_equal(
    pick_up_date(as.Date("2025-11-17")),
    c(as.Date("2025-12-06"), as.Date("2025-12-07"))
  )
})

test_that("calendar import is generated", {
  class_schedule <- get_class_schedule(sheet = "2025 Fall")
  calendar_df <- create_calendar_event(class_schedule)

  #expect_equal(NROW(calendar_df), 75)
  #expect_equal(
  #  names(calendar_df),
  #  c("Subject", "Start Date", "Start Time", "End Time")
  #)
})
