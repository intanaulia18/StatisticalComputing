#' Geometry package: perimeter, circumference, and area calculations
#'
#' @docType package
#' @name geometry
#'
#' @importFrom stats mean
#'

#' @importFrom stats sum

#' @importFrom stats pi
#' Calculate perimeter and area of a square
#'
#' @param s Length of a side of the square
#'
#' @return A list with perimeter and area
#' @export
square <- function(s) {
  perimeter <- 4 * s
  area <- s^2
  list(perimeter = perimeter, area = area)
}

#' Calculate perimeter and area of a triangle
#'
#' @param a Length of side a
#' @param b Length of side b
#' @param c Length of side c
#'
#' @return A list with perimeter and area
#' @export
triangle <- function(a, b, c) {
  perimeter <- a + b + c
  s <- perimeter / 2
  area <- sqrt(s * (s - a) * (s - b) * (s - c))
  list(perimeter = perimeter, area = area)
}

#' Calculate circumference and area of a circle
#'
#' @param r Radius of the circle
#'
#' @return A list with circumference and area
#' @export
circle <- function(r) {
  circumference <- 2 * pi * r
  area <- pi * r^2
  list(circumference = circumference, area = area)
}

#' Calculate perimeter and area of a rectangle
#'
#' @param p Length of the rectangle
#' @param l Width of the rectangle
#'
#' @return A list with perimeter and area
#' @export
rectangle <- function(p, l) {
  perimeter <- 2 * (p + l)
  area <- p * l
  list(perimeter = perimeter, area = area)
}
