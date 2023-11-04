#' Timestamp
#'
#' Returns a timestamp in a format convenient for filename versioning
#'
#' @return string in the format "year month day _ hour minute second"
#' @export
#'
t_ts <- function(){
  format.Date(Sys.time(), format = "%Y%m%d_%H%M%S")
}
