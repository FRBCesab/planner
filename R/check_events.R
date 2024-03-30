#' Check if the max number of events by day is < 5
#' 
#' @note
#' For internal purpose only
#' 
#' @noRd

check_events <- function(data) {
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (nrow(data) > 0) {
    
    if (!("from" %in% colnames(data))) {
      stop("Column 'from' (start of the event) is missing from 'data'",
           call. = FALSE)
    }
    
    if (!("to" %in% colnames(data))) {
      stop("Column 'to' (end of the event) is missing from 'data'",
           call. = FALSE)
    }
    
    days <- unlist(lapply(1:nrow(data), function(i) {
      as.character(seq(as.Date(data[i, "from"]), as.Date(data[i, "to"]), 
                       by = "days"))
    }))
    
    n_events_by_day <- table(days)
    
    if (max(n_events_by_day) > 4) {
      stop("Too many events by day. For aesthetic purpose, only 4 four events ",
           "by day is supported.", call. = FALSE)
    }
  }
  
  invisible(NULL)
}
