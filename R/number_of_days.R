#' Return the number of days in a month for a given year
#'
#' @param date either a `character` or a `Date` of length 1. See examples below.
#' 
#' @param format a `character` of length 1. Used to specify the format of the 
#'   date. Default is `"%Y-%m-%d"` (i.e. 2024-12-25). See examples below.
#'
#' @return An `integer` corresponding to the number of days in the month for 
#' the specified year.
#' 
#' @export
#'
#' @examples
#' ## Default ----
#' number_of_days("2024-02-01")
#' 
#' ## Day doesn't matter ----
#' number_of_days("2024-02-23")
#' 
#' ## Year matters ----
#' number_of_days("2023-02-23")
#' 
#' ## Change the format ----
#' number_of_days("2024/02/23", format = "%Y/%m/%d")
#' 
#' ## French format ----
#' number_of_days("23/02/2024", format = "%d/%m/%Y")

number_of_days <- function(date, format = "%Y-%m-%d") {
  
  ## Check args ----
  
  if (missing(date)) {
    stop("Argument 'date' is required", call. = FALSE)
  }
  
  if (!is.character(date) && !inherits(date, "Date")) {
    stop("Argument 'date' must be either a character or a Date", call. = FALSE)
  }
  
  if (length(date) != 1) {
    stop("Argument 'date' must be of length 1", call. = FALSE)
  }
  
  if (nchar(as.character(date)) != 10) {
    stop("Wrong 'date'. Year must have 4 characters (e.g. 2024), month 2 ", 
         "characters (e.g. 01), and day 2 characters (e.g. 01)", call. = FALSE)
  }
  
  if (!inherits(date, "Date")) {
    date <- as.Date(date, format = format)
  }
  
  
  if (is.na(date)) {
    stop("Error in converting 'date'. Please use the argument 'format' to ", 
         "specify the appropriate format. See '?strptime' for further ", 
         "information", call. = FALSE)
  }
  
  month <- format(date, format = "%m")
  
  while (format(date, format = "%m") == month) {
    
    date <- date + 1
  }
  
  as.integer(format(date - 1, format = "%d"))
}
