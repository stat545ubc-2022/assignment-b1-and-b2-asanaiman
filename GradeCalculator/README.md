
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GradeCalculator

<!-- badges: start -->
<!-- badges: end -->

The goal of GradeCalculator is to give ‘whatGrade()’ which makes
calculating the final exam grade you need to achieve a certain grade in
a course easier.

## Installation

You can install the development version of GradeCalculator like so:

``` r
library(devtools)
install_github("stat545ubc-2022/assignment-b1-and-b2-asanaiman/GradeCalculator"))

# Once package is downloaded can load the package and use!
library(GradeCalculator)
```

## Example

This is a basic example which shows you how to solve a common problem:

There are 5 assignments in the course worth 10%, 20%, 5%, 30%, 5% prior
to the final exam. Student received 70%, 80%, 100%, 60%, 60%
respectively and they want a final grade of 78%.

***Note:*** *the weight of the assignments and the assignment grades
need to be inputted in the same order.*

``` r
library(GradeCalculator)
whatGrade(c(10, 20, 5, 30, 5), c(70, 80, 100, 60, 60), 78) 
#> [1] "96.6666666666667%"
```