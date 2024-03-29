#' Get the calendar for a given month and year
#'
#' @param year either an `integer` or a `character` of length 1. Must have 4 
#'   characters (e.g. '2024' and not '24'). Default is the current year.
#'   
#' @param month either an `integer` or a `character` of length 1. Must have 1 
#'   or 2 characters (e.g. '01' or '1'). Default is the current month.
#'   
#' @param weekend a `logical`. If `TRUE` keeps Saturdays and Sundays. Default is
#'   `FALSE`.
#'   
#' @return A `data.frame` with the following columns:
#' - `date`: the date of the day (`YYYY-MM-DD`),
#' - `year`: the year of the day (`integer`),
#' - `month`: the month of the day (`integer`),
#' - `day`: the day (`integer`),
#' - `week`: the number of week (`integer`),
#' - `weekday`: the name of the day of the week (`character`),
#' - `month_name`: the name of month (`character`),
#' - `x`: position on the x-axis (used by `plot_calendar()`),
#' - `y`: position on the y-axis (used by `plot_calendar()`).
#' 
#' @export
#'
#' @examples
#' ## Calendar for the current month ----
#' head(get_calendar())
#' 
#' ## Calendar for December (current year) ----
#' head(get_calendar(month = 12))
#' 
#' ## Calendar for April (current year) ----
#' head(get_calendar(month = 4))
#' 
#' ## Calendar for January 1970 ----
#' head(get_calendar(year = 1970, month = 1))

get_calendar <- function(year = format(Sys.Date(), "%Y"), 
                         month = format(Sys.Date(), "%m"), weekend = FALSE) {
  
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
  
  ## Switch to US locale ----
  
  locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  on.exit(Sys.setlocale("LC_TIME", locale))
  
  
  ## Create sequence of days ----
  
  n_days <- number_of_days(paste(year, month, "01", sep = "-"))
  
  dates <- seq(as.Date(paste(year, month, "01", sep = "-")),
               as.Date(paste(year, month, n_days, sep = "-")), 
               by = "days")
  
  
  ## Extract date info ----
  
  calendar <- data.frame("date"       = dates,
                         "year"       = as.integer(format(dates, "%Y")),
                         "month"      = as.integer(format(dates, "%m")),
                         "day"        = as.integer(format(dates, "%d")),
                         "month_name" = tools::toTitleCase(format(dates, "%B")),
                         "weekday"    = tools::toTitleCase(format(dates, "%A")),
                         "week"       = as.integer(format(dates, "%W")))
  
  
  ## Add position on x-axis (day of the week) ----
  
  calendar <- merge(calendar, weekdays(), by = "weekday", all = TRUE)
  
  
  ## Remove weekend (if required) ----
  
  if (!weekend) {
    calendar <- calendar[!(calendar$"weekday" %in% c("Saturday", "Sunday")), ]  
  }
  
  
  ## Add position on y-axis (week of the month) ----
  
  calendar$"y" <- calendar$"week"  - min(calendar$"week") + 1
  calendar$"y" <- abs(calendar$"y" - max(calendar$"y"))   + 1
  
  
  ## Correct y-axis position (reverse axis) ----
  
  while (max(calendar$"y") < 6) {
    calendar$"y" <- calendar$"y" + 1
  }
  
  
  ## Order by days ----
  
  calendar <- calendar[order(calendar$"date"), ]
  
  rownames(calendar) <- NULL
  
  
  ## Order columns ----
  
  calendar <- calendar[ , c("date", "year", "month", "day", "week", "weekday",
                            "month_name", "x", "y")]
  
  calendar
}
