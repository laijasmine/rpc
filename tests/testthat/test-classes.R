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
})

