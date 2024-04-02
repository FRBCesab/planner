#' Add a JPG/PNG to a plot
#'
#' @param image ...
#' @param file ...
#' @param x ...
#' @param y ...
#' @param adj ...
#' @param size ...
#' @param col ...
#' @param alpha ...
#' @param angle ...
#' @param fill ...
#' @param border ...
#' @param padding ...
#' @param add ...
#' @param ... ...

add_image <- function(image, file, x, y, adj = c(0.5, 0.5), size = NULL, 
                      col = NA, alpha = NULL, angle = 0, fill = NA, border = NA, 
                      padding = NULL, add = TRUE, ...) {
  
  if (missing(image)) image <- NULL
  if (missing(file)) file <- NULL
  
  if (missing(x)) x <- NULL
  if (missing(y)) y <- NULL
  
  if (is.null(image) & is.null(file)) {
    stop("One of 'image' or 'file' is required.")  
  }
  
  if (!is.null(image) & !is.null(file)) {
    stop("Only one of 'image' or 'file' is required.")  
  }
  
  if (is.null(image)) {
    
    if (sum(is.na(file))) {
      stop("Argument 'file' cannot contain missing value.")
    }
    
    if (length(file) != 1 | !is.character(file)) {
      stop("Argument 'file' must be a character (filename) of length 1.")
    }
    
    file_formats <- c("jpg$", "jpeg$", "png$")
    file_format  <- unlist(lapply(file_formats, grepl, file))
    
    if (!sum(file_format)) stop("Picture must be a JP(E)G or a PNG.")
    
    if (grepl("^https?://", file)) {
      
      file_ext <- switch(which(file_format), "jpg", "jpg", "png")
      
      path <- tempfile(fileext = file_ext)
      utils::download.file(url = file, destfile = path, quiet = TRUE)
      
      file <- path
      
    } else {
      
      if (!file.exists(file)) stop("File does not exist.")
    }
    
    image <- switch(
      which(file_format),
      jpeg::readJPEG(file, native = FALSE),
      jpeg::readJPEG(file, native = FALSE),
      png::readPNG(file, native = FALSE)
    )
  }
  
  if (!inherits(image, "array")) {
    stop("Argument 'image' must be a '3D array' (use `native = FALSE`).")
  }
  
  if (dim(image)[3] < 3 | dim(image)[3] > 4) {
    stop("Wrong image format: Must a 3-channels (RGB) or 4-channels (RGBA).")
  }
  
  if (is.null(padding)) padding <- rep(0, 4)
  
  if (sum(is.na(padding))) stop("Argument 'padding' cannot contain NA.")
  if (!is.numeric(padding)) stop("Argument 'padding' must be a numeric.")
  
  if (length(padding) == 1) {
    padding <- rep(padding, 4)
    
  } else {
    
    if (length(padding) == 2) {
      padding <- rep(padding, 2)
      
    } else {
      
      if (length(padding) != 4) {
        stop("Argument 'padding' must be a numeric of length 1, 2, or 4.") 
      }
    }
  }
  
  if (!add) {
    
    plot(0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", ann = FALSE, 
         bty = "n", axes = FALSE)
  }
  
  if (is.null(dev.list())) stop("No open graphical device (use `add = TRUE`).")
  
  if (is.null(x)) x <- par()$usr[1] + (par()$usr[2] - par()$usr[1]) / 2
  if (is.null(y)) y <- par()$usr[3] + (par()$usr[4] - par()$usr[3]) / 2
  
  if (sum(is.na(x))) {
    stop("Argument 'x' cannot contain missing value.")
  }
  
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a numeric.")
  }
  
  if (sum(is.na(y))) {
    stop("Argument 'y' cannot contain missing value.")
  }
  
  if (!is.numeric(y)) {
    stop("Argument 'y' must be a numeric.")
  }
  
  if (length(x) != length(y)) {
    stop("Arguments 'x and 'y' must have the same length.")
  }
  
  
  aspect_ratio <- dim(image)[1] / dim(image)[2]
  
  
  if (is.null(size)) size <- 100
  size <- rep(size, length(x))
  
  if (!is.na(col)) col <- rep(col, length(x))
  if (!is.null(alpha)) alpha <- rep(alpha, length(x))
  
  original <- image
  
  for (i in 1:length(x)) {
    
    image <- original
    
    ## Change Image Colors (monochromatic transformation) ---
    
    if (!is.na(col[i])) {
      
      col_i <- col2rgb(col[i], alpha = TRUE)
      
      for (channel in 1:3) {
        
        image[ , , channel][image[ , , channel] != 1] <- col_i[channel] / 255
      }
      
      if (dim(image)[3] == 4) image[ , , 4][image[ , , 4] != 0] <- col_i[4] / 255
    }
    
    if (!is.null(alpha)) {
      
      alpha_i <- alpha[i]
      
      if (alpha_i > 1) alpha_i <- alpha_i / 255
      
      if (dim(image)[3] == 4) image[ , , 4][image[ , , 4] != 0] <- alpha_i
    }
    
    
    ## Adjusting Image Dimensions ----
    
    size_i <- size[i] / 100
    
    width  <- ifelse(aspect_ratio > 1, NA, size_i * (par()$usr[2] - par()$usr[1]))
    height <- ifelse(aspect_ratio <= 1, NA, size_i * (par()$usr[4] - par()$usr[3]))
    
    if (is.na(width)) {
      
      height_in <- height * par()$pin[2] / (par()$usr[4] - par()$usr[3])
      width_in  <- height_in / aspect_ratio
      width     <- width_in * (par()$usr[2] - par()$usr[1]) / par()$pin[1]
      
    }
    
    if (is.na(height)) {
      
      width_in  <- width / (par()$usr[2] - par()$usr[1]) * par()$pin[1]
      height_in <- width_in * aspect_ratio
      height    <- height_in / par()$pin[2] * (par()$usr[4] - par()$usr[3])
    }
    
    ## Adjusting Image Position ----
    
    x1 <- x[i] - ((1 - adj[1]) * width)
    y1 <- y[i] - ((1 - adj[2]) * height)
    x2 <- x1 + width
    y2 <- y1 + height
    
    
    ## Add a background ----
    
    if (!is.na(fill) || !is.na(border)) {
      
      padding[1] <- padding[1] * height
      padding[2] <- padding[2] * width
      padding[3] <- padding[3] * height
      padding[4] <- padding[4] * width
      
      rect(
        xleft       = x1 - padding[2],
        xright      = x2 + padding[4],
        ybottom     = y1 - padding[1],
        ytop        = y2 + padding[3],
        col         = fill, 
        border      = border,
        ...
      )
    }
    
    
    ## Plot Image ----
    
    rasterImage(
      image       = as.raster(image),
      xleft       = x1,
      xright      = x2,
      ybottom     = y1,
      ytop        = y2,
      interpolate = TRUE,
      angle       = angle
    )
  }  
  
  invisible(c(x1, y1, x2, y2))
}
