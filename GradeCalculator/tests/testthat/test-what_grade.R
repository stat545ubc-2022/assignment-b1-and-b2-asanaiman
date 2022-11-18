test_that("Basic Testing", {
  expect_equal(what_grade(50, 50, 50), "50%")
  expect_equal(what_grade(c(20, 30, 20), c(80, 75, 70), 75), "75%") #Different because taking a list of numbers
  expect_equal(what_grade(c(20,25), c(20,25), 90), "Oops! Better Luck Next Time :(") #output exceeds 100% so puts out string
  expect_error(what_grade("Cat", 50, 75)) #inputting anything that's not a numeric should put out error
  expect_equal(what_grade(NA, 50, 75), NA) #inputting NA outputs NA
})
