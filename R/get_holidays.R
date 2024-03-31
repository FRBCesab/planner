#' Get holidays for a given country and a given year
#'
#' @description
#' Scraps the site <https://www.timeanddate.com> to retrieve holidays data.
#'
#' @param country a `character` of length 1. The name of the country 
#'   (e.g. `'France'`).
#'
#' @param year either an `integer` or a `character` of length 1. Must have 4 
#'   characters (e.g. '2024' and not '24').
#'   
#' @return A `data.frame` with the following columns:
#' - `date`: the date of the holiday (`YYYY-MM-DD`),
#' - `name`: the name of the holiday (`character`).
#' 
#' @export
#'
#' @examples
#' ## Get holidays for France in 2024 ----
#' get_holidays("France", 2024)

get_holidays <- function(country, year) {

  ## Check args ----
  
  if (missing(country)) {
    stop("Argument 'country' is required", call. = FALSE)
  }
  
  if (!is.character(country)) {
    stop("Argument 'country' must be a character", call. = FALSE)
  }
  
  if (length(country) != 1) {
    stop("Argument 'country' must be of length 1", call. = FALSE)
  }
  
  if (missing(year)) {
    stop("Argument 'year' is required", call. = FALSE)
  }
  
  if (!is.character(year) && !is.numeric(year)) {
    stop("Argument 'year' must be either a character or an integer", 
         call. = FALSE)
  }
  
  if (length(year) != 1) {
    stop("Argument 'year' must be of length 1", call. = FALSE)
  }
  
  if (nchar(year) != 4) {
    stop("Argument 'year' must be of the form 'YYYY'", call. = FALSE)
  }

  
  ## Change user agent ----
  
  ua <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:124.0) ",
               "Gecko/20100101 Firefox/124.0")
  
  
  ## Switch to US locale ----
  
  lc_time    <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", lc_time), add = TRUE)
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  
  
  ## Get data ----
  
  url <- paste0("https://www.timeanddate.com/holidays/", 
                tolower(country), "/", year)
  
  page <- httr::GET(url, 
                    httr::add_headers(`Accept-Language` = "en"),
                    httr::user_agent(ua))
  
  if (httr::status_code(page) != 200) {
    stop("Unable to retrieve holidays. Check country name or check your ", 
         "internet connexion or wait a few minutes", call. = FALSE)
  }
  
  content <- httr::content(page)
  
  
  ## Clean data ----
  
  content <- rvest::html_nodes(content, ".table")
  content <- rvest::html_table(content, fill = TRUE)
  content <- data.frame(content)
  content <- content[-1, c(1, 3:4)]
  content <- content[grep("national", tolower(content$"Type")), -3]
  
  content$"Date" <- paste(content$"Date", year)
  content$"Date" <- as.character(as.Date(content$"Date", format = "%d %b %Y"))
  
  colnames(content) <- tolower(colnames(content))
  
  content$"name" <- gsub(" / .*", "", content$"name")
  
  content <- content[!duplicated(content$"date"), ]
  
  content
}
