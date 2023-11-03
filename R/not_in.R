#' Not In
#'
#' The inverse of %in%
#'
#' @param a vector. values to search for
#' @param b vector. values to search within
#'
#' @return logical TRUE or FALSE
#'
#' @export
`%not_in%` <- function(a, b){
  !(a %in% b)
}
