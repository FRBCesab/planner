#' List French Holidays
#' 
#' @param year either an `integer` or a `character` of length 1. Used to filter
#'   holidays for a given year. Optional. Default is `NULL`.
#' 
#' @return A two-column `data.frame` with:
#' - `date`, the day of the holiday and,
#' - `event`, the name of the holiday.
#' 
#' @noRd

days_off <- function(year = NULL) {
  
  ## 2024 ----
  
  days_24 <- data.frame()
  
  days_24 <- rbind(days_24,
                   data.frame("date"  = "01-01",
                              "event" = "New Year's Day"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "04-01",
                              "event" = "Easter Monday"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "05-01", 
                              "event" = "Labour Day"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "05-08", 
                              "event" = "Victory 1945"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "05-09", 
                              "event" = "Ascension"))
  # days_24 <- rbind(days_24,
  #                  data.frame("date"  = "05-20", 
  #                             "event" = "Pentecost"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "07-14", 
                              "event" = "National Day"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "08-15", 
                              "event" = "Assumption"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "11-01", 
                              "event" = "All Saints' Day"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "11-11", 
                              "event" = "Armistice 1918"))
  days_24 <- rbind(days_24,
                   data.frame("date"  = "12-25",
                              "event" = "Christmas Day"))
  
  days_24$"date" <- paste("2024", days_24$"date", sep = "-")
  
  
  ## 2025 ----
  
  days_25 <- data.frame()
  
  days_25 <- rbind(days_25,
                   data.frame("date"  = "01-01",
                              "event" = "New Year's Day"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "04-21",
                              "event" = "Easter Monday"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "05-01", 
                              "event" = "Labour Day"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "05-08", 
                              "event" = "Victory 1945"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "05-29", 
                              "event" = "Ascension"))
  # days_25 <- rbind(days_25,
  #                  data.frame("date"  = "06-09", 
  #                             "event" = "Pentecost"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "07-14", 
                              "event" = "National Day"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "08-15", 
                              "event" = "Assumption"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "11-01", 
                              "event" = "All Saints' Day"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "11-11", 
                              "event" = "Armistice 1918"))
  days_25 <- rbind(days_25,
                   data.frame("date"  = "12-25",
                              "event" = "Christmas Day"))
  
  days_25$"date" <- paste("2025", days_25$"date", sep = "-")
  
  
  ## 2026 ----
  
  days_26 <- data.frame()
  
  days_26 <- rbind(days_26,
                   data.frame("date"  = "01-01",
                              "event" = "New Year's Day"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "04-06",
                              "event" = "Easter Monday"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "05-01", 
                              "event" = "Labour Day"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "05-08", 
                              "event" = "Victory 1945"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "05-14", 
                              "event" = "Ascension"))
  # days_26 <- rbind(days_26,
  #                  data.frame("date"  = "05-25", 
  #                             "event" = "Pentecost"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "07-14", 
                              "event" = "National Day"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "08-15", 
                              "event" = "Assumption"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "11-01", 
                              "event" = "All Saints' Day"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "11-11", 
                              "event" = "Armistice 1918"))
  days_26 <- rbind(days_26,
                   data.frame("date"  = "12-26",
                              "event" = "Christmas Day"))
  
  days_26$"date" <- paste("2026", days_26$"date", sep = "-")
  
  
  days <- rbind(days_24, days_25, days_26)
  
  
  ## Subset by year ----
  
  if (!is.null(year)) {
    
    if (length(year) > 1) {
      stop("Argument 'year' must be of length 1", call. = FALSE)
    }
    
    if (nchar(year) != 4) {
      stop("Argument 'year' must be of the form 'YYYY'", call. = FALSE)
    }
    
    years <- unique(substr(days$"date", 1, 4))
    
    if (!(year %in% years)) {
      stop("Argument 'year' must be one of '", paste0(years, collapse = "', '"),
           "'", call. = FALSE)
    }
    
    days <- days[grep(paste0("^", year), days$"date"), ]
    
    rownames(days) <- NULL
  }
  
  days
}
