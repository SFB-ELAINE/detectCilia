#' @title meanIntensityImage
#' @description Get tmean intensity of cilia
#' @details By using an x-y-3(rgb)-representation of an image, you can
#' extract the layer of the cilia and calculate the mean intensity
#' disregarding all pixels that are black.
#' @aliases meanintensityImage meanIntensityimage meanintensityimage
#' @author Kai Budde
#' @export meanIntensityImage
#' @param image An three-dimensional array of numbers between 0 and 1
#' @param cilium_color A character (color of the staining of the cilia)

meanIntensityImage <- function(image = NULL,
                      cilium_color = NULL){
  
  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }

  if(missing(cilium_color)){
    cilium_color <- "red"
  }
  
  cilium_color <- tolower(cilium_color)
  
  # Extract the layer of the cilia -----------------------------------------
  
  if(cilium_color == "red" | cilium_color == "r"){
    image_cilia <- image[,,1]
  }else if(cilium_color == "green" | cilium_color == "g"){
    image_cilia <- image[,,2]
  }else if(cilium_color == "blue" | cilium_color == "b"){
    image_cilia <- image[,,3]
  }else{
    print(paste("Please enter a color (red, green or blue) for the cilia.",
                sep=""))
    return()
  }
  
  # Calculate mean intensity -----------------------------------------------
  image_cilia[image_cilia == 0] <- NA
  mean_intensity <- mean(image_cilia, na.rm = TRUE)
  
  return(mean_intensity)
}
