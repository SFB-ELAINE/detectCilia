#' @title addNumberToImage
#' @description Adds a number to the image layer
#' @details Adds an integer to a x-y-3(rgb)-representation of an image.
#' @aliases addnumbertoimage addNumbertoimage addNumberToimage 
#' addnumberToImage addnumbertoImage
#' @author Kai Budde
#' @export addNumberToImage
#' @param image An three-dimensional array of numbers between 0 and 1
#' @param number A number (integer to be drawn/copied)
#' @param pos_x A number (x-value for starting pixel (upper left corner))
#' @param pos_y A number (y-value for starting pixel (upper left corner))
#' @param number_size_factor A number (factor for resizing the number)
#' @param number_color A string (color of thr number to be drawn)

addNumberToImage <- function(image = NULL,
                             number = NULL,
                             pos_x = NULL,
                             pos_y = NULL,
                             number_size_factor = NULL,
                             number_color = "red"){
  
  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }
  if(is.null(number)){
    print(paste("Please call the function with a number", sep=""))
    return()
  }
  
  if(is.null(pos_x)){
    pos_x <- 1
  }
  if(is.null(pos_y)){
    pos_y <- 1
  }
  
  # Read in correct integers -----------------------------------------------
  list_of_digits <- unlist(strsplit(x = toString(number), split = ""))
  number_of_digits <- length(list_of_digits)
  
  # Add the digits to the image --------------------------------------------
  
  for(i in 1:number_of_digits){
    digit <- list_of_digits[i]
    digit_path <- paste("inst/digits/", digit, ".tiff", sep="")
    digit_image <- tiff::readTIFF(source = digit_path, convert = TRUE,
                            info = FALSE)
    
    # Only keep the black layer
    digit_image <- digit_image[,,4]
    if(i == 1){
      image_with_numbers <- image
    }
    
    # Resize image
    if(is.null(number_size_factor)){
      min_repeating_number <- min(dim(image_with_numbers)[1]/dim(digit_image)[1],
                                  dim(image_with_numbers)[2]/dim(digit_image)[2])
      
      # It should be possible to have every number at least ten times in
      # every direction
      number_size_factor <- min_repeating_number / 10
      
    }
    
    digit_image <- resizeImage(digit_image, number_size_factor)
    
    # Choose layer of image where the number should be added to
    if(tolower(number_color) == "red"){
      image_layer <- 1
    }
    
    # Adapt the starting position so the digits will be completely seen
    digit_image_size_x <- dim(digit_image)[1]
    digit_image_size_y <- dim(digit_image)[2]
    
    if( (pos_x + digit_image_size_x - 1) > dim(image_with_numbers)[1]){
      pos_x <- dim(image_with_numbers)[1] - digit_image_size_x
    }
    
    # Copy smaller matrix into bigger one
    for(row in 1:digit_image_size_x){
      for(col in 1:digit_image_size_y){
        image_with_numbers[pos_x + row, pos_y + col, image_layer] <-
          min(digit_image[row, col] +
                image_with_numbers[pos_x + row, pos_y + col, image_layer], 1)
      }
    }
    
    
  }
  
  return(image_with_numbers)
}
