multiweek_events <- function(data, year, month) {
  
  events <- data.frame()
  
  if (nrow(data) > 0) {
    
    calendar <- get_calendar(year, month)
    calendar <- calendar[ , c("date", "x", "y")]
    
    data$"n_days" <- (as.Date(data$"to") - as.Date(data$"from"))
    
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
    
    # events$"n_days" <- (as.Date(events$"to") - as.Date(events$"from"))
    
    events <- events[with(events, order(from, -n_days, event)), ]
    
    rownames(events) <- NULL
  }
  
  events
}
