#' Add event on a calendar
#' 
#' @note
#' For internal purpose only
#' 
#' @noRd

add_events <- function(data, year, month, palette, weekend) {
  
  calendar <- get_calendar(year, month, weekend = weekend)
  data     <- filter_events(data, year, month, weekend = weekend)
  
  check_events(data)
  
  if (nrow(data) > 0) {
    
    ## Add color to data ----
    
    categories <- unique(data$"category")
    
    if (length(palette) > 1) {
      
      if (any(names(palette) == "")) {
        stop("Some colors in the palette don't have name", call. = FALSE)
      }
      
      if (any(!(categories %in% names(palette)))) {
        stop("Some event categories don't have color. Please check the ", 
             "argument 'palette'", call. = FALSE)
      }
      
      palette <- data.frame("category" = names(palette), 
                            "color"    = palette)
      
      data <- merge(data, palette, by = "category", all.x = TRUE, all.y = FALSE)
      
    } else {
      
      data$"color" <- palette
    }
    
    
    ## Multiweek events ----
    
    data <- multiweek_events(data, year, month, weekend = weekend)
    
    
    ## Add events ----
    
    for (i in 1:nrow(data)) {
      
      x_lft  <- calendar[which(calendar[ , "date"] == data[i, "from"]), "x"]
      x_rght <- calendar[which(calendar[ , "date"] == data[i, "to"]), "x"]
      
      y_btm  <- calendar[which(calendar[ , "date"] == data[i, "from"]), "y"]
      y_top <- calendar[which(calendar[ , "date"] == data[i, "to"]), "y"]
      
      if (i == 1) {
        
        coords <- data.frame("x" = x_lft:x_rght,
                             "y" = rep(y_top, length(x_lft:x_rght)),
                             "n" = 1)
        
        coords$"key" <- paste(coords$"x", coords$"y", sep = "-")
        
        y_line <- 1
        
      } else {
        
        coord <- data.frame("x" = x_lft:x_rght,
                            "y" = rep(y_top, length(x_lft:x_rght)),
                            "n" = 1)
        
        coord$"key" <- paste(coord$"x", coord$"y", sep = "-")
        
        pos <- which(coord$"key" %in% coords$"key")
        
        if (length(pos) > 0) {
          
          sop <- which(coords$"key" %in% coord$"key")
          coords[sop, "n"] <- coords[sop, "n"] + 1
          coords <- rbind(coords, coord[-pos, ])
          
        } else {
          
          coords <- rbind(coords, coord)
        }
        
        y_line <- max(coords[which(coords$"key" %in% coord$"key"), "n"])
      }
      
      # if (data[i, "from"] != data[i, "to"]) {
        
        rect(xleft   = x_lft - 1 + 0.05,
             xright  = x_rght - 0.05,
             ybottom = y_btm - (0.18 * y_line + 0.02 * (y_line - 1)) - 0.18,
             ytop    = y_top - (0.18 * y_line + 0.02 * (y_line - 1)),
             col     = data[i, "color"],
             border  = "white",
             lwd     = 0.75,
             xpd     = TRUE)
        
        text(x      = ((x_lft - 1 + 0.1) + (x_rght - 0.1)) / 2,
             y      = y_btm - (0.18 * y_line + 0.02 * (y_line - 1)) - 0.18 / 2,
             labels = data[i, "event"],
             cex    = 0.65,
             font   = 2,
             col    = "#ffffff") 
      
      # } else {
        
        # points(x   = x_lft - 1 + 0.075,
        #        y   = y_btm - (0.18 * y_line + 0.02 * (y_line - 1)) - 0.18 / 2,
        #        pch = 19,
        #        cex = 0.85,
        #        col = data[i, "color"]) 
        # 
        # text(x      = x_lft - 1 + 0.075,
        #      y      = y_btm - (0.18 * y_line + 0.02 * (y_line - 1)) - 0.18 / 2 - 0.01,
        #      labels = data[i, "event"],
        #      cex    = 0.65,
        #      font   = 2,
        #      pos    = 4,
        #      col    = data[i, "color"]) 
      # }
    }
  }
  
  invisible(NULL)
}
