#' Export a monthly calendar as pdf
#'
#' @param year either an `integer` or a `character` of length 1. Must have 4 
#'   characters (e.g. '2024' and not '24'). Default is the current year.
#'   
#' @param month either an `integer` or a `character` of length 1. Must have 1 
#'   or 2 characters (e.g. '01' or '1'). Default is the current month.
#'   
#' @param path a `character` of length 1. The directory to save the `pdf` file.
#'   Must exist. Default is the current directory.
#' 
#' @param filename a `character` of length 1. The name of the `pdf` file. 
#'   Default is `calendar-YYYY-MM.pdf` (e.g. `calendar-2024-04.pdf`).
#' 
#' @param title a `character` of length 1. The title of the calendar. Default is
#'   `Month YYYY` (e.g. `April 2024`).
#' 
#' @param events an optional `data.frame` with the following columns: `event`,
#'   the name of the event, `from`, the starting date of the event, `to`, the 
#'   ending date of the event, and `category`, the category of the event.
#' 
#' @param weekend a `logical`. If `TRUE` keeps Saturdays and Sundays. Default is
#'   `FALSE`.
#'   
#' @param palette a `character` vector of colors for the events. If only one 
#'   color is provided (default), all events will have this color. If
#'   several colors are provided, the number of colors in `palette` must be 
#'   equal to the total number of event categories (duplicated colors are 
#'   accepted). Moreover, the `palette` argument must be a named vector, where 
#'   names match event categories. For example, let's that the `events` object
#'   contains two categories (`cat_a` and `cat_b`), the `palette` argument must
#'   be equal to `palette = c("cat_a" = "black", "cat_b" = "red")`.
#'
#' @return No return value. The calendar will exported as a `pdf` file in 
#' `path`.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Add an example ----
#' }

plot_calendar <- function(year = format(Sys.Date(), "%Y"), 
                          month = format(Sys.Date(), "%m"), 
                          path = getwd(), filename = NULL, title = NULL, 
                          events = NULL, weekend = FALSE, palette = "#990000") {
  
  ## Check year ----
  
  if (!is.character(year) && !is.numeric(year)) {
    stop("Argument 'year' must either a character or an integer", call. = FALSE)
  }
  
  if (length(year) > 1) {
    stop("Argument 'year' must be of length 1", call. = FALSE)
  }
  
  if (nchar(year) != 4) {
    stop("Argument 'year' must be of the form 'YYYY'", call. = FALSE)
  }
  
  
  ## Check month ----
  
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
    stop("Argument 'month' must be of the form 'MM' (e.g. '01' instead of '1')",
         call. = FALSE)
  }
  
  
  ## Check path ----
  
  if (!is.character(path)) {
    stop("Argument 'path' must be a character", call. = FALSE)
  }
  
  if (length(path) != 1) {
    stop("Argument 'path' must be a character of length 1", call. = FALSE)
  }
  
  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist", call. = FALSE)
  }
  
  
  ## Check filename ----
  
  if (!is.null(filename)) {
    
    if (!is.character(filename)) {
      stop("Argument 'filename' must be a character", call. = FALSE)
    }
    
    if (length(filename) != 1) {
      stop("Argument 'filename' must be a character of length 1", call. = FALSE)
    }
  }
  
  
  ## Check title ----
  
  if (!is.null(title)) {
    
    if (!is.character(title)) {
      stop("Argument 'title' must be a character", call. = FALSE)
    }
    
    if (length(title) != 1) {
      stop("Argument 'title' must be a character of length 1", call. = FALSE)
    }
  }
  
  
  ## Check events ----
  
  if (!is.null(events)) {
    
    if (!is.data.frame(events)) {
      stop("Argument 'events' must be a data.frame", call. = FALSE)
    }
    
    if (nrow(events) == 0) {
      stop("Argument 'events' must have at least one row (calendar event)", 
           call. = FALSE)
    }
    
    if (!("from" %in% colnames(events))) {
      stop("Column 'from' (start of the event) is missing from 'events'",
           call. = FALSE)
    }
    
    if (!("to" %in% colnames(events))) {
      stop("Column 'to' (end of the event) is missing from 'events'",
           call. = FALSE)
    }
    
    if (!("event" %in% colnames(events))) {
      stop("Column 'event' (name of the event) is missing from 'events'",
           call. = FALSE)
    }
    
    if (!("category" %in% colnames(events))) {
      stop("Column 'category' (category of the event) is missing from 'events'",
           call. = FALSE)
    }
  }
  
  
  ## Check weekend ----
  
  if (!is.logical(weekend)) {
    stop("Argument 'weekend' must be a logical ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(weekend) != 1) {
    stop("Argument 'weekend' must be a logical ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  
  ## Check palette ----
  
  if (!is.character(palette)) {
    stop("Argument 'palette' must be a character (color)", call. = FALSE)
  }
  
  if (length(palette) > 1 && !is.null(events)) {
    
    if (any(names(palette) == "")) {
      stop("Some colors in 'palette' don't have name", call. = FALSE)
    }
    
    categories <- unique(events$"category")
    
    if (any(!(categories %in% names(palette)))) {
      stop("Some event categories don't have color. Please check the ", 
           "argument 'palette'", call. = FALSE)
    }
  }
  
  
  ## Create filename ----
  
  if (is.null(filename)) {
    filename <- paste("calendar", year, month, sep = "-")
  }
  
  filename <- paste0(filename, ".pdf")
  filename <- gsub("\\.pdf\\.pdf", ".pdf", filename)
  
  
  ## Get calendar data ----
  
  calendar <- get_calendar(year, month, weekend)
  offs     <- days_off(year)
  
  
  ## Create title ----
  
  if (is.null(title)) {
    title <- paste(unique(calendar[ , "month_name"]), year)
  }
  
  
  ## Define x-axis range ----
  
  x_lim <- c(0, length(unique(calendar$"weekday")))  
 
  
  ## Define y-axis range ----
  
  n_weeks <- length(unique(calendar$"week"))
  
  y_lim <- c(NA, 6)
  
  y_lim[1] <- ifelse(n_weeks == 6, 0, y_lim[1])
  y_lim[1] <- ifelse(n_weeks == 5, 1, y_lim[1])
  y_lim[1] <- ifelse(n_weeks == 4, 2, y_lim[1])
  
  
  ## Graphical parameters ----
  
  pdf(file       = file.path(path, filename),
      width      = 11.69291,
      height     =  8.26772,
      paper      = "a4r",
      pointsize  = 16)
  
  par(family   = "serif", 
      mar      = c(0.5, 0.5, 2.5, 0.5), 
      xaxs     = "i", 
      yaxs     = "i", 
      lend     = "square",
      cex.axis = 1.25, 
      mgp      = c(1.5, 0.2, 0), 
      tcl      = -0.25, 
      xpd      = FALSE,
      new      = FALSE, 
      fig      = c(0, 1, 0, 1), 
      fg       = "#333333", 
      col      = "#333333", 
      col.axis = "#333333",
      col.lab  = "#333333", 
      font.lab = 2, 
      cex.axis = 1.0)
  
  plot(0, 
       type = "n", 
       bty  = "n", 
       axes = FALSE, 
       ann  = FALSE, 
       xlim = x_lim, 
       ylim = y_lim)
  
  
  ## Add calendar data (days and holidays) ----
  
  for (i in 1:nrow(calendar)) {
    
    rect(xleft   = calendar[i, "x"] - 1,
         xright  = calendar[i, "x"],
         ybottom = calendar[i, "y"] - 1,
         ytop    = calendar[i, "y"],
         col     = "#ffffff",
         lwd     = 0.75,
         xpd     = TRUE)
    
    ## Add weekend ----
    
    if (calendar[i, "weekday"] %in% c("Saturday", "Sunday")) {
      
      rect(xleft   = calendar[i, "x"] - 1,
           xright  = calendar[i, "x"],
           ybottom = calendar[i, "y"] - 1,
           ytop    = calendar[i, "y"],
           col     = "#efefef",
           lwd     = 0.75,
           xpd     = TRUE)
    }
    
    
    ## Add holidays ----
    
    if (calendar[i, "date"] %in% offs$"date") {
      
      rect(xleft   = calendar[i, "x"] - 1,
           xright  = calendar[i, "x"],
           ybottom = calendar[i, "y"] - 1,
           ytop    = calendar[i, "y"],
           col     = "#efefef",
           lwd     = 0.75,
           xpd     = TRUE)
      
      text(x      = calendar[i, "x"] - 0.50,
           y      = calendar[i, "y"] - 0.75,
           labels = paste0("OFF - ", offs[which(offs$"date" == 
                                                  calendar[i, "date"]), 
                                          "event"]),
           cex    = 0.65,
           font   = 2,
           col    = "#666666")
    }
    
    
    ## Add day number ----
    
    text(x      = calendar[i, "x"] - 1.04,
         y      = calendar[i, "y"] - 0.10,
         labels = calendar[i, "day"],
         pos    = 4,
         cex    = 0.35)
  }
  
  
  ## Add day names (top axis) ----
  
  for (i in 1:nrow(weekdays())) {
    
    text(x      = weekdays()[i, "x"] - 0.5,
         y      = 5.95,
         labels = weekdays()[i, "weekday"],
         cex    = 0.65,
         pos    = 3,
         font   = 2,
         xpd    = TRUE)
  }
  
  
  ## Add title ----
  
  mtext(title,
        side = 3,
        line = 1,
        cex    = 1.25,
        font   = 2)
  
  
  ## Add events ----
  
  if (!is.null(events)) {
    
    add_events(events, year, month, palette, weekend)
  }
  
  
  invisible(dev.off())
  
  message("Calendar successfully exported as '", file.path(path, filename), "'")
  
  invisible(NULL)
}
