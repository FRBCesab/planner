#' Get moon phases for a given year
#'
#' @description
#' Scraps the site <https://www.timeanddate.com> to retrieve moon phases data.
#'
#' @param year either an `integer` or a `character` of length 1. Must have 4 
#'   characters (e.g. '2024' and not '24'). Default is the current year.
#'   
#' @return A `data.frame` with the following columns:
#' - `new_moon`: the date of new moons (`YYYY-MM-DD`),
#' - `first_quarter`: the date of first quarters (`YYYY-MM-DD`),
#' - `full_moon`: the date of full moons (`YYYY-MM-DD`),
#' - `third_quarter`: the date of third quarters (`YYYY-MM-DD`).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Get moon phases for 2024 ----
#' get_moon_phases(2024)
#' }

get_moon_phases <- function(year) {
  
  ## Check args ----
  
  if (missing(year)) {
    stop("Argument 'year' is required", call. = FALSE)
  }
  
  if (!is.character(year) && !is.numeric(year)) {
    stop("Argument 'year' must be either a character or an integer", 
         call. = FALSE)
  }
  
  if (length(year) > 1) {
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
  
  url <- paste0("https://www.timeanddate.com/moon/phases/?year=", year)
  
  page <- httr::GET(url, 
                    httr::add_headers(`Accept-Language` = "en"),
                    httr::user_agent(ua))
  
  if (httr::status_code(page) != 200) {
    stop("Unable to retrieve moon phases. Please wait a few minutes or check ", 
         "you connexion.", call. = FALSE)
  }
  
  content <- httr::content(page)
  
  
  ## Clean data ----
  
  content <- rvest::html_nodes(content, "table")
  content <- rvest::html_table(content, fill = TRUE)
  content <- content[[2]][-1, c("New Moon", "First Quarter", "Full Moon", 
                                "Third Quarter")]
  content <- as.data.frame(content)
  content <- content[grep("^[0-9]", content[ , 1]), ]
  content <- content[grep("^[0-9]", content[ , 2]), ]
  content <- content[grep("^[0-9]", content[ , 3]), ]
  content <- content[grep("^[0-9]", content[ , 4]), ]
  colnames(content) <- c("new_moon", "first_quarter", "full_moon", 
                         "third_quarter")
  
  content$"new_moon"      <- paste(content$"new_moon",  year)
  content$"first_quarter" <- paste(content$"first_quarter", year)
  content$"full_moon"     <- paste(content$"full_moon", year)
  content$"third_quarter" <- paste(content$"third_quarter", year)
  
  content$"new_moon"      <- as.character(as.Date(content$"new_moon", 
                                                  format = "%d %b %Y"))
  content$"first_quarter" <- as.character(as.Date(content$"first_quarter", 
                                                  format = "%d %b %Y"))
  content$"full_moon"     <- as.character(as.Date(content$"full_moon", 
                                                  format = "%d %b %Y"))
  content$"third_quarter" <- as.character(as.Date(content$"third_quarter", 
                                                  format = "%d %b %Y"))
  
  content
}
