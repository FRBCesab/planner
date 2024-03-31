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
#' @param weekend a `logical`. If `TRUE` (default) keeps Saturdays and Sundays.
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
#' @param lang a `character` of length 1. Used to change the default locale
#'   (i.e. the language). Default is `NULL` (i.e. use the current locale).
#'   Depending on the OS and the locale, the output can be weird.
#' 
#' @param country a `character` of length 1. The name of the country 
#'   (e.g. `'France'`) used to retrieve holidays. Default is `NULL`.
#'   
#' @param moon a `logical`. If `TRUE` adds new/full moon glyph. 
#'   Default is `FALSE`.
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
                          events = NULL, weekend = TRUE, palette = "#990000",
                          lang = NULL, country = NULL, moon = FALSE) {
  
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
  
  
  ## Check moon ----
  
  if (!is.logical(moon)) {
    stop("Argument 'moon' must be a logical ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  if (length(moon) != 1) {
    stop("Argument 'moon' must be a logical ('TRUE' or 'FALSE')", 
         call. = FALSE)
  }
  
  
  ## Check lang ----
  
  if (!is.null(lang)) {
    if (!is.character(lang)) {
      stop("Argument 'lang' must be a character", call. = FALSE)
    }
    
    if (length(lang) != 1) {
      stop("Argument 'lang' must be of length 1", call. = FALSE)
    }
  }
  
  
  ## Check country ----
  
  if (!is.null(country)) {
    if (!is.character(country)) {
      stop("Argument 'country' must be a character", call. = FALSE)
    }
    
    if (length(country) != 1) {
      stop("Argument 'country' must be of length 1", call. = FALSE)
    }
  }
  
  
  ## Get calendar data ----
  
  calendar <- get_calendar(year, month, weekend, lang = lang)
  
  
  ## Create file name ----
  
  if (is.null(filename)) {
    filename <- paste("calendar", year, month, sep = "-")
  }
  
  filename <- paste0(filename, ".pdf")
  filename <- gsub("\\.pdf\\.pdf", ".pdf", filename)
  
  
  ## Create title ----
  
  if (is.null(title)) {
    title <- paste(unique(calendar[ , "user_month_name"]), year)
  }
  
  
  ## Get holidays ----
  
  if (!is.null(country)) {
    offs <- get_holidays(country, year)
  }
    

  ## Get moon phases dates ----
  
  if (moon) {
    moon_dates <- get_moon_phases(year)
  }
  
  
  ## Define x-axis range ----
  
  x_lim <- c(0, length(unique(calendar$"en_weekday")))  
 
  
  ## Define y-axis range ----
  
  n_weeks <- length(unique(calendar$"week"))
  
  y_lim <- c(NA, 6)
  
  y_lim[1] <- ifelse(n_weeks == 6, 0, y_lim[1])
  y_lim[1] <- ifelse(n_weeks == 5, 1, y_lim[1])
  y_lim[1] <- ifelse(n_weeks == 4, 2, y_lim[1])
  
  
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
  
  
  ## Graphical parameters ----

  cairo_pdf(filename   = file.path(path, filename),
            width      = 11.69291,
            height     =  8.26772,
            # paper      = "a4r",
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
    
    if (calendar[i, "en_weekday"] %in% c("Saturday", "Sunday")) {
      
      rect(xleft   = calendar[i, "x"] - 1,
           xright  = calendar[i, "x"],
           ybottom = calendar[i, "y"] - 1,
           ytop    = calendar[i, "y"],
           col     = "#efefef",
           lwd     = 0.75,
           xpd     = TRUE)
    }
    
    
    ## Add holidays ----
    
    if (!is.null(country)) {
    
      if (calendar[i, "date"] %in% offs$"date") {
        
        rect(xleft   = calendar[i, "x"] - 1,
             xright  = calendar[i, "x"],
             ybottom = calendar[i, "y"] - 1,
             ytop    = calendar[i, "y"],
             col     = "#efefef",
             lwd     = 0.75,
             xpd     = TRUE)
        
        text(x      = calendar[i, "x"] - 0.50,
             y      = calendar[i, "y"] - 0.85,
             labels = paste0("OFF\n", offs[which(offs$"date" == 
                                                   calendar[i, "date"]), 
                                           "name"]),
             cex    = 0.65,
             font   = 2,
             col    = "#666666")
      } 
    }
    
    
    ## Add day number ----
    
    x_at <- ifelse(length(unique(calendar$"en_weekday")) == 7, 0.02, 0.03)
    
    text(x      = calendar[i, "x"] - (1 + x_at),
         y      = calendar[i, "y"] - 0.10,
         labels = calendar[i, "day"],
         pos    = 4,
         cex    = 0.35)
    
    
    ## Add moon phases ----
    
    if (moon) {
     
      x_at <- ifelse(length(unique(calendar$"en_weekday")) == 7, 0.10, 0.06)
      
      if (calendar[i, "date"] %in% moon_dates$"new_moon") {

        points(x   = calendar[i, "x"] - x_at,
               y   = calendar[i, "y"] - 0.10,
               pch = 21,
               col = "#333333",
               bg  = "#333333",
               cex = 1)   
      }
      
      if (calendar[i, "date"] %in% moon_dates$"full_moon") {
        
        points(x   = calendar[i, "x"] - x_at,
               y   = calendar[i, "y"] - 0.10,
               pch = 21,
               col = "#333333",
               bg  = "#ffffff",
               cex = 1)   
      }
    }
  }
  
  
  ## Add day names (top axis) ----
  
  for (i in 1:nrow(weekdays())) {
    
    label <- which(calendar$"en_weekday" == weekdays()[i, "weekday"])
    label <- calendar[label[1], "user_weekday"]
    
    text(x      = weekdays()[i, "x"] - 0.5,
         y      = 5.95,
         labels = label,
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
