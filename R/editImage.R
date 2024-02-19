#' Calculate binary mask
#' 
#' `editImage()` masks an image layer depending on a threshold.
#' 
#' @param image An three-dimensional array of numbers between 0 and 1.
#' @param object_color A character being the color of the staining of the
#' object to be masked.
#' @param threshold A number that determines the brightness of a pixel to
#' be counted as a cilium pixel.
#' 
#' @returns A binary image.

editImage <- function(image = NULL,
                      object_color = NULL,
                      threshold = NULL){

  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }
  if(is.null(threshold)){
    threshold <- 0.1
  }

  if(is.null(object_color) || missing(object_color)){
    object_color <- "red"
  }

  object_color <- tolower(object_color)

  # Extract the layer of the cilia -----------------------------------------

  if(length(dim(image))==3){
    if(object_color == "red" | object_color == "r"){
      image_cilia <- image[,,1]
    }else if(object_color == "green" | object_color == "g"){
      image_cilia <- image[,,2]
    }else if(object_color == "blue" | object_color == "b"){
      image_cilia <- image[,,3]
    }else{
      print(paste("Please enter a color (red, green or blue) for the cilia.",
                  sep=""))
      return()
    }
  }else{
    image_cilia <- image
  }
  

  # Calculate binary mask of cilia pixels ----------------------------------

  # Making sure that the calculated threshold is not above 1 or below 0
  if(threshold > 1){
    threshold <- 1
  }
  if(threshold < 0){
    threshold <- 0
  }
  
  if(threshold != 1){
    image_cilia[image_cilia > threshold] <- 1
    image_cilia[image_cilia <= threshold] <- 0
  }else{
    image_cilia[image_cilia >= threshold] <- 1
    image_cilia[image_cilia < threshold] <- 0
  }
  

  return(image_cilia)
}
