#' Duplicate multi-week events and order events
#' 
#' @note
#' For internal purpose only
#' 
#' @noRd

multiweek_events <- function(data, year, month, weekend) {
  
  events <- data.frame()
  
  if (nrow(data) > 0) {
    
    calendar <- get_calendar(year, month, weekend)
    calendar <- calendar[ , c("date", "x", "y")]
    
    for (i in 1:nrow(data)) {
      
      days <- data.frame("date" = as.character(seq(as.Date(data[i, "from"]), 
                                                   as.Date(data[i, "to"]), 
                                                   by = "days")))
      
      days <- merge(days, calendar, by = "date", all = FALSE)
      
      dates <- tapply(days$"date", days$"y", function(x) {
        data.frame("from" = as.character(min(as.Date(x))),
                   "to"   = as.character(max(as.Date(x))))
      })
      
      days <- lapply(dates, function(x) {
        data[i, "from"] <- x[1]
        data[i, "to"]   <- x[2]
        data[i, ]
      })
      
      days <- do.call(rbind.data.frame, days)
      
      days <- days[order(days$"from", decreasing = FALSE), ]
      
      if (nrow(days) > 1) {
        days[-nrow(days), "event"] <- paste(days[-nrow(days), "event"], 
                                           "(continued)")
      }
      
      events <- rbind(events, days)
    }
    
    events$"n_days" <- (as.Date(events$"to") - as.Date(events$"from"))
    
    events <- events[with(events, order(from, -n_days, event)), ]
    
    rownames(events) <- NULL
  }
  
  events
}
