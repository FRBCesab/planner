#' Get the name of a month
#'
#' @param month an `integer` of length 1. The month to convert in letters.
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
#' get_month_name(month = 4)
#' 
#' \dontrun{
#' ## Get Spanish name ----
#' get_month_name(month = 4, lang = "Spanish")
#' 
#' ## Get Finnish name ----
#' get_month_name(month = 4, lang = "Finnish")
#' }

get_month_name <- function(month, lang = NULL) {
  
  if (missing(month)) {
    stop("Argument 'month' is required", call. = FALSE)
  }
  
  if (!is.numeric(month)) {
    stop("Argument 'month' must be a numeric", call. = FALSE)
  }
  
  if (length(month) != 1) {
    stop("Argument 'month' must be a numeric of length 1", call. = FALSE)
  }
  
  if (!(month %in% 1:12)) {
    stop("Argument 'month' must be between 1 and 12", call. = FALSE)
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
    
  
  ## Create date ----
  
  day <- paste(1970, month, 1, sep = "-")
  
  
  ## Get month name ----
  
  tools::toTitleCase(format(as.Date(day), "%B"))
}
