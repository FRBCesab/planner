#' List and order days of the week
#' 
#' @return A two-column `data.frame` with:
#' - `weekday`, the name of the week day,
#' - `x`, the order of the week day (the week starts on Monday).

#' @noRd

weekdays <- function() {
  
  data.frame("weekday"  = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday", 
                            "Sunday"),
             "x"        = 1:7)
}
