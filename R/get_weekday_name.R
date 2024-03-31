#' Get the weekday name of a date
#'
#' @param date either a `character` or a `Date` of length 1. The date to extract
#'   the weekday name. See examples below.
#' 
#' @param format a `character` of length 1. Used to specify the format of the 
#'   date. Default is `"%Y-%m-%d"` (i.e. 2024-12-25). See examples below.
#' 
#' @param lang a `character` of length 1. Used to change the default locale
#'   (i.e. the language). Default is `NULL` (i.e. use the current locale).
#'   See examples below. Depending on the OS and the locale, the output can be
#'   weird.
#'
#' @return A `character` of length 1.
#' 
#' @export
#'
#' @examples
#' ## Default ----
#' get_weekday_name("2024-04-01")
#' 
#' ## Change date format ----
#' get_weekday_name("01/04/2024", format = "%d/%m/%Y")
#' 
#' \dontrun{
#' ## Get Spanish name ----
#' get_weekday_name("2024-04-01", lang = "Spanish")
#' 
#' ## Get Finnish name ----
#' get_weekday_name("2024-04-01", lang = "Finnish")
#' }

get_weekday_name <- function(date, format = "%Y-%m-%d", lang = NULL) {
  
  if (missing(date)) {
    stop("Argument 'date' is required", call. = FALSE)
  }
  
  if (!is.character(date) && !inherits(date, "Date")) {
    stop("Argument 'date' must be either a character or a Date", call. = FALSE)
  }
  
  if (length(date) != 1) {
    stop("Argument 'date' must be of length 1", call. = FALSE)
  }
  
  if (!inherits(date, "Date")) {
    date <- as.Date(date, format = format)
  }
  
  if (any(is.na(date))) {
    stop("Error in converting date. Please use the argument 'format' to ", 
         "specify the appropriate format. See '?strptime' for further ", 
         "information", call. = FALSE)
  }
  
  
  ## Switch locale ----
  
  if (!is.null(lang)) {
    
    o_warn <- options()$"warn"
    
    lc_time    <- Sys.getlocale("LC_TIME")
    lc_ctype   <- Sys.getlocale("LC_CTYPE")
    lc_collate <- Sys.getlocale("LC_COLLATE")
    
    on.exit(options("warn" = o_warn), add = TRUE)
    on.exit(Sys.setlocale("LC_TIME",    lc_time), add = TRUE)
    on.exit(Sys.setlocale("LC_CTYPE",   lc_ctype), add = TRUE)
    on.exit(Sys.setlocale("LC_COLLATE", lc_collate), add = TRUE)
    
    options("warn" = -1)
    Sys.setlocale("LC_TIME",    lang)
    Sys.setlocale("LC_CTYPE",   lang)
    Sys.setlocale("LC_COLLATE", lang)
  }

  
  ## Get weekday name ----
  
  tools::toTitleCase(format(as.Date(date), "%A"))
}
