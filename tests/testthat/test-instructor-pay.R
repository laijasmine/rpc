test_that("public pay calculation", {
  jean <- calculate_instructor_pay(
    hourly_rate = 36,
    class_type = "public",
    number_of_courses = 1,
    instructor = "Jean Fung"
  )
  expect_equal(jean, 1080)

  evan <- calculate_instructor_pay(
    hourly_rate = 36,
    class_type = "public",
    number_of_courses = 3,
    instructor = "Evan Leung"
  )
  expect_equal(evan, 3240)

  david <- calculate_instructor_pay(
    hourly_rate = 36,
    class_type = "public",
    number_of_courses = 3,
    instructor = "David Liu"
  )
  expect_equal(david, 5616)
})

test_that("member pay calculation", {
  david <- calculate_instructor_pay(
    hourly_rate = 36,
    class_type = "member",
    number_of_courses = 1,
    instructor = "David Liu"
  )
  expect_equal(david, 936)
})
