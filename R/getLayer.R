#' Get a specific layer of the image
#' 
#' `getLayer()` extracts a specific layer of an image (being x-y-3(rgb)-
#' representation).

#' @param image An three-dimensional array of numbers between 0 and 1.
#' @param layer A character being the color of the layer.

#' @returns An array (specific channel of image).

getLayer <- function(image = NULL,
                     layer = NULL){
  
  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }
  
  if(missing(layer)){
    print("Layer is missing and is given the value red.")
    layer <- "red"
  }
  
  if(is.null(layer)){
    return(drop(image))
  }
  
  layer <- tolower(layer)
  
  # Extract the layer of the cilia -----------------------------------------
  
  if(layer == "red" | layer == "r"){
    image_layer <- image[,,1]
  }else if(layer == "green" | layer == "g"){
    image_layer <- image[,,2]
  }else if(layer == "blue" | layer == "b"){
    image_layer <- image[,,3]
  }else{
    print(paste("Please enter a color (red, green or blue) for the cilia.",
                sep=""))
    return()
  }
  
  return(image_layer)
}
