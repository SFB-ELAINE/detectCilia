#' @title editImage
#' @description Get the layer of the primary cilia
#' @details By using an x-y-3(rgb)-representation of an image, you can
#' extract the information of the cilia in an image.
#' @aliases editimage imageedit imageEdit
#' @author Kai Budde
#' @export editImage
#' @param image An three-dimensional array of numbers between 0 and 1
#' @param cilia_color A character (color of the staining of the cilia)
#' @param threshold A character (that determine tha brightness of a pixel
#' to be counted)

editImage <- function(image = NULL,
                      cilia_color = NULL,
                      threshold = NULL){

  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }
  if(is.null(threshold)){
    threshold <- 0.1
  }

  if(missing(cilia_color)){
    cilia_color <- "blue"
  }

  cilia_color <- tolower(cilia_color)

  # Extract the layer of the cilia -----------------------------------------

  if(cilia_color == "red" | cilia_color == "r"){
    image_cilia <- image[,,1]
  }else if(cilia_color == "green" | cilia_color == "g"){
    image_cilia <- image[,,2]
  }else if(cilia_color == "blue" | cilia_color == "b"){
    image_cilia <- image[,,3]
  }else{
    print(paste("Please enter a color (red, green or blue) for the cilia.",
                sep=""))
    return()
  }

  # higher contrast of the cilia -------------------------------------------
  image_cilia[,][image_cilia[,] >= threshold] <- 1
  image_cilia[,][image_cilia[,] < threshold] <- 0

  return(image_cilia)
}
