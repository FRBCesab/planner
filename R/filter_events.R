#' Filter calendar events to match the extent on the calendar
#'
#' @param data a `data.frame` with at least the following columns: `from`, the
#'   start of the event, `to`, the end of the event, `event`, the name of the 
#'   event, and `category`, the category of the event (used for different 
#'   colors).
#'   
#' @param year either an `integer` or a `character` of length 1. Must have 4 
#'   characters (e.g. '2024' and not '24'). Default is the current year.
#'   
#' @param month either an `integer` or a `character` of length 1. Must have 1 
#'   or 2 characters (e.g. '01' or '1'). Default is the current month.
#'   
#' @param format a `character` of length 1. Used to specify the format of the 
#'   date. Default is `"%Y-%m-%d"` (i.e. 2024-12-25).
#' 
#' @param weekend a `logical`. If `TRUE` keeps Saturdays and Sundays. Default is
#'   `FALSE`.
#'   
#' @return A `data.frame`, same as the input but with only events matching the
#' extent on the calendar.
#' 
#' @export
#'
#' @examples
#' ## No example ---

filter_events <- function(data, year = format(Sys.Date(), "%Y"), 
                          month = format(Sys.Date(), "%m"), 
                          format = "%Y-%m-%d", weekend = FALSE) {
  
  ## Check args ----
  
  if (!is.character(year) && !is.numeric(year)) {
    stop("Argument 'year' must either a character or an integer", call. = FALSE)
  }
  
  if (length(year) > 1) {
    stop("Argument 'year' must be of length 1", call. = FALSE)
  }
  
  if (nchar(year) != 4) {
    stop("Argument 'year' must be of the form 'YYYY'", call. = FALSE)
  }
  
  if (!is.character(month) && !is.numeric(month)) {
    stop("Argument 'month' must either a character or an integer", 
         call. = FALSE)
  }
  
  if (length(month) > 1) {
    stop("Argument 'month' must be of length 1", call. = FALSE)
  }
  
  if (nchar(month) == 1) {
    month <- paste0("0", month)
  }
  
  if (nchar(month) != 2) {
    stop("Argument 'month' must be of the form 'MM' or 'M' (e.g. '01' or '1')",
         call. = FALSE)
  }
  
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("Argument 'data' must have at least one row (calendar event)", 
         call. = FALSE)
  }
  
  if (!("from" %in% colnames(data))) {
    stop("Column 'from' (start of the event) is missing from 'data'",
         call. = FALSE)
  }
  
  if (!("to" %in% colnames(data))) {
    stop("Column 'to' (end of the event) is missing from 'data'",
         call. = FALSE)
  }
  
  if (!("event" %in% colnames(data))) {
    stop("Column 'event' (name of the event) is missing from 'data'",
         call. = FALSE)
  }
  
  if (!("category" %in% colnames(data))) {
    stop("Column 'category' (category of the event) is missing from 'data'",
         call. = FALSE)
  }
  
  if (!inherits(data$"from", "Date")) {
    data$"from" <- as.Date(data$"from", format = format)
  }
  
  
  if (any(is.na(data$"from"))) {
    stop("Error in converting dates. Please use the argument 'format' to ", 
         "specify the appropriate format. See '?strptime' for further ", 
         "information", call. = FALSE)
  }
  
  if (!inherits(data$"to", "Date")) {
    data$"to" <- as.Date(data$"to", format = format)
  }
  
  
  if (any(is.na(data$"to"))) {
    stop("Error in converting dates. Please use the argument 'format' to ", 
         "specify the appropriate format. See '?strptime' for further ", 
         "information", call. = FALSE)
  }
  
  if (!is.character(format)) {
    stop("Argument 'format' must be a character", call. = FALSE)
  }
  
  if (length(format) != 1) {
    stop("Argument 'format' must be of length 1", call. = FALSE)
  }
  
  if (!is.logical(weekend)) {
    stop("Argument 'weekend' must be a logical ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(weekend) != 1) {
    stop("Argument 'weekend' must be a logical ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  
  ## Filter event dates ----
  
  calendar <- get_calendar(year, month, weekend)
  
  events <- data.frame()
  
  for (i in 1:nrow(data)) {
    
    dates <- seq(data[i, "from"], data[i, "to"], by = "days")
    
    dates <- dates[which(dates %in% calendar$"date")]
    
    if (length(dates) > 0) {
      
     events <- rbind(events,
                     data.frame("event"    = data[i, "event"],
                                "from"     = as.character(min(dates)),
                                "to"       = as.character(max(dates)),
                                "category" = data[i, "category"]))
    }
  }
  
  
  ## Order events by category ----
  
  if (nrow(events) > 0) {

    events <- events[with(events, order(from, to, event)), ]
    
    rownames(events) <- NULL
  }
  
  events
}
