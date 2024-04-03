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
      
      case <- ifelse(is.na(data[i, "to"]), "single_day", "multi_day")
      
      if (case == "single_day") {
        
        days <- data.frame("date" = as.character(seq(as.Date(data[i, "from"]), 
                                                     as.Date(data[i, "from"]), 
                                                     by = "days")))
      } else {
        
        days <- data.frame("date" = as.character(seq(as.Date(data[i, "from"]), 
                                                     as.Date(data[i, "to"]), 
                                                     by = "days")))
      }
            
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
        days[-1, "name"] <- paste(days[-1, "name"], "(continued)")
      }
      
      days$"n_days" <- (as.Date(days$"to") - as.Date(days$"from"))
      
      if(case == "single_day") {
        days$"to" <- NA 
      }
      
      events <- rbind(events, days)
    }
    
    events <- events[with(events, order(from, -n_days, to, name)), ]
    
    rownames(events) <- NULL
  }
  
  events
}
