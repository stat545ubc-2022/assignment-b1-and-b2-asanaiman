#' Calculate Final Exam Grade Calculator
#'
#' This function calculates for its user the percent they need on their final exam to get their desired final grade.
#' It takes in the weight of different assignments in a course, the percent grade received on each of them, and the desired final
#' course grade and returns what percent is need on the final to achieve that grade. If the percent needed is higher than 100%
#' it will return "Oops! Better Luck Next Time :(". Note: the weight of the assignments and the assignment grades need to be
#' inputted in the same order.
#' @param AssignmentsWeight
#' This variable takes in a numeric that represents the weight of the assignments completed by user.
#' Name justification: This argument is named AssignmentsWeight because it takes in the percent value each assignment weighs.
#' @param GradesRecieved
#' This variables takes in a numeric that represents the percentage received on the assignments.
#' Name justification: This argument is named GradesRecieved because it takes in the percent value receieved on each assignment inputted into AssignmentsWeight.
#' @param DesiredFinalGrade
#' This argument takes in a numeric that the final percentage the user wants in the course.
#' Name justification: This argument is named DesiredFinalGrade because it takes in the final percent value the user wants in the course.
#' @return This function returns a numeric vector representing the percent needed on the final exam for the user to get their desired final grade in the course.
#' If this value is greater than 100 it will return "Oops! Better Luck Next Time :("
#' @examples
#' whatGrade(50, 70, 80)
#' whatGrade(c(10, 20, 5, 30, 5), c(70, 80, 100, 60, 60), 78)
#' whatGrade(c(25,25,25), c(20,50,50), 70)
#' @export
whatGrade <- function(AssignmentsWeight, GradesRecieved, DesiredFinalGrade) {
  CurrentGrades <- AssignmentsWeight*(GradesRecieved/100)
  SumCurrentGrades <- sum(CurrentGrades)
  FinalWeight <- 100 - sum(AssignmentsWeight)
  GradeNeeded = ((DesiredFinalGrade-SumCurrentGrades)/FinalWeight) *100
  ifelse(GradeNeeded > 100, "Oops! Better Luck Next Time :(", paste0(GradeNeeded, "%"))
}
