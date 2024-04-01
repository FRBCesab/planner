#' Get holidays category
#'
#' @param data a `data.frame` with the following column: `type`,
#'   the category of the holiday. Typically, the output of `get_holidays()`.
#'
#' @return A `character` with the categories of holidays.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Get holidays for United Kingdom in 2024 ----
#' holidays <- get_holidays(country = "UK", year = 2024, month = 4)
#' 
#' ## Get types of holidays ----
#' get_holiday_types(holidays)
#' }

get_holiday_types <- function(data) {
  
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("Argument 'data' must have at least one row (holiday)", 
         call. = FALSE)
  }
  
  if (!("type" %in% colnames(data))) {
    stop("Column 'type' (category of the holiday) is missing from 'data'",
         call. = FALSE)
  }
  
  sort(unique(data$"type"))
}
