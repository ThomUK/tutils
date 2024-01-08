#' Convert a date to the financial year it sits within
#'
#' @param date date. The date you are converting
#'
#' @return string. A string of the form "2020/21"
#' @export
#'
t_as_fy <- function(date){

  if(!inherits(date, "Date")) stop("The argument 'date' must be of class 'Date'.")

  mth <- format(date, "%m")
  yr <- as.numeric(format(date, "%Y"))

  if(mth <= "03") {
    base_year <- yr - 1
  } else {
    base_year <- yr
  }

  result <- paste0(base_year, "/", stringr::str_sub(base_year + 1, start = -2))

}
