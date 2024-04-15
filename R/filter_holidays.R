#' Filter holidays by category
#'
#' @param data a `data.frame` with at least the following column: `type`,
#'   the category of the holiday. Typically, the output of `get_holidays()`.
#'
#' @param types a `character` with the types of holidays to keep.
#'   See `get_holiday_types()`.
#'   
#' @return A `data.frame`, same as the input but with only holidays matching the
#' `type` category.
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
#' 
#' ## Filter holidays ----
#' filter_holidays(holidays, types = "Common Local Holiday")
#' }

filter_holidays <- function(data, types) {
  
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
  
  if (missing(types)) {
    stop("Argument 'types' is required", call. = FALSE)
  }
  
  if (!is.character(types)) {
    stop("Argument 'types' must be a character", call. = FALSE)
  }
  
  # if (any(!(types %in% data$"type"))) {
  #   stop("Some holiday types in 'types' are missing from 'data'. Please use ",
  #        "'get_holiday_types()'", call. = FALSE)
  # }
  
  data <- data[data$"type" %in% types, ]
  rownames(data) <- NULL
  
  data
}
