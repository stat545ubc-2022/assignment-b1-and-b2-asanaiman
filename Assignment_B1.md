Assignment_B1
================
Asana
2022-11-03

### Loading Appropriate Packages

``` r
suppressPackageStartupMessages(library("devtools"))
suppressPackageStartupMessages(library("testthat"))
```

# Exercise 1: Make a Function (25 points)

### In this exercise, you’ll be making a function and fortifying it. The function need not be complicated. The function need not be “serious”, but shouldn’t be nonsense.

``` r
#' Final Exam Grade Calculator
#' 
#' This function calculates for its user the percent they need on their final exam to get their desired final grade. It takes in the weight of different assignments in a course, the percent grade received on each of them,and the desired final course grade and returns what percent is need on the final to achieve that grade. If the percent needed is higher than 100% it will return "Oops! Better Luck Next Time :(". Note: the weight of the assignments and the assignment grades need to be inputted in the same order. 
#' 
#'
#' @param AssignmentsWeigh 
#' This variable takes in a numeric that represents the weight of the assignments completed by user. 
#' Name justification: This argument is named AssignmentsWeigh because it takes in the percent value each assignment weighs.
#' @param GradesRecieved 
#' This variables takes in a numeric that represents the percentage received on the assignments.
#' Name justification: This argument is named GradesRecieved because it takes in the percent value receieved on each assignment inputted into AssignmentsWeigh.
#' @param DesiredFinalGrade 
#' This argument takes in a numberic that the final percentage the user wants in the course. 
#' Name justification: This argument is named DesiredFinalGrade because it takes in the final percent value the user wants in the course.
#'
#' @return This function returns the percent needed on the final exam for the user to get their desired final grade in the course. If this value is greater than 100 it will return "Oops! Better Luck Next Time :("


WhatGrade <- function(AssignmentsWeigh, GradesRecieved, DesiredFinalGrade) {
  CurrentGrades <- AssignmentsWeigh*(GradesRecieved/100)
  SumCurrentGrades <- sum(CurrentGrades)
  FinalWeight <- 100 - sum(AssignmentsWeigh)
  GradeNeeded = ((DesiredFinalGrade-SumCurrentGrades)/FinalWeight) *100
  ifelse(GradeNeeded > 100, "Oops! Better Luck Next Time :(", paste0(GradeNeeded, "%"))
}
```

# Exercise 2: Document Your Function (20 points)

### In the same code chunk where you made your function, document the function using roxygen2 tags.

Included in the code chunk above.

# Exercise 3: Exercise 3: Include examples (15 points)

### Demonstrate the usage of your function with a few examples. Use one or more new code chunks, describing what you’re doing.

<u>**Example 1:**</u> **Inputting only a single assignment.**

There is only 1 assignment in the course worth 50% prior to the final
exam. Student received 70%, and they want a final grade of 80%.

``` r
WhatGrade(50, 70, 80) 
```

    ## [1] "90%"

Returns 90%, meaning a grade of 90% is required on the final exam to get
a final grade of 80%.

<u>**Example 2:**</u> **Inputting a list of assignments.**

There are 5 assignments in the course worth 10%, 20%, 5%, 30%, 5% prior
to the final exam. Student received 70%, 80%, 100%, 60%, 60%
respectively and they want a final grade of 78%.

***Note:*** *the weight of the assignments and the assignment grades
need to be inputted in the same order.*

``` r
WhatGrade(c(10, 20, 5, 30, 5), c(70, 80, 100, 60, 60), 78) 
```

    ## [1] "96.6666666666667%"

Returns 96.6666666666667%, meaning a grade of 90% is required on the
final exam to get a final grade of 78%.

<u>**Example 3:**</u> **Wanting a grade in the course that requires a
score above 100% on the final exam.**

There are 3 assignments in the course worth 25%, 25%, 25% prior to the
final exam. Student received 20%, 50%, 50% on the assignments
respectively and they want a final grade of 70% in the course.

***Note:*** *the weight of the assignments and the assignment grades
need to be inputted in the same order.*

``` r
WhatGrade(c(25,25,25), c(20,50,50), 70)
```

    ## [1] "Oops! Better Luck Next Time :("

Returns “Oops! Better Luck Next Time :(”, meaning the value calculated
is over 100% and not possible.

<u>**Example 4:**</u> **Inputting a non-numeric value.**

Inputting any value other than a numeric value in any of the variables
will produce an *error*.

``` r
WhatGrade("cat", 50,78)
```

    ## Error in AssignmentsWeigh * (GradesRecieved/100): non-numeric argument to binary operator

# Exercise 4: Test the Function (25 points)

### Running examples is a good way of checking by-eye whether your function is working as expected. But, having a formal “yes or no” check is useful when you move on to other parts of your analysis.

``` r
test_that("Basic Testing", {
  expect_equal(WhatGrade(50, 50, 50), "50%")
  expect_equal(WhatGrade(c(20, 30, 20), c(80, 75, 70), 75), "75%") #Different because taking a list of numbers
  expect_equal(WhatGrade(c(20,25), c(20,25), 90), "Oops! Better Luck Next Time :(") #output exceeds 100% so puts out string
  expect_error(WhatGrade("Cat", 50, 75)) #inputting anything that's not a numeric should put out error
  expect_equal(WhatGrade(NA, 50, 75), NA) #inputting NA outputs NA
})
```

    ## Test passed 😸
