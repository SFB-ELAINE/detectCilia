#' @title editImage
#' @description Get the layer of the primary cilia
#' @details By using an x-y-3(rgb)-representation of an image, you can
#' extract the information of the cilia in an image and only use those pixel
#' that are above a certain threshold.
#' @aliases editimage imageedit imageEdit
#' @author Kai Budde
#' @export editImage
#' @param image An three-dimensional array of numbers between 0 and 1
#' @param object_color A character (color of the staining of the object)
#' @param threshold A number (that determines the brightness of a pixel to
#' be counted as cilium pixel)

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

  if(missing(object_color)){
    object_color <- "blue"
  }

  object_color <- tolower(object_color)

  # Extract the layer of the cilia -----------------------------------------

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

  # higher contrast of the cilia -------------------------------------------
  #threshold <- threshold * sum(image_cilia[,])/sum(image_cilia[,]>0)
  #print(threshold)
  #print(sum(image_cilia[,])/sum(image_cilia[,]>0))
  #print(threshold)
  
  # Making sure that the calculated threshold is not above 1 or below 0
  if(threshold > 1){
    threshold <- 1
  }
  if(threshold < 0){
    threshold <- 0
  }
  
  image_cilia[image_cilia > threshold] <- 1
  image_cilia[image_cilia <= threshold] <- 0

  return(image_cilia)
}
