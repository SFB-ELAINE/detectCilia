#' @title summarizeCiliaInformation
#' @description Summarizes the information of found cilia in images
#' @details Input should be the result of detectCilia.R.
#' The output is a data frame with the lengths of the found cilia.
#' @aliases summarizeCiliumInformation summarizeciliainformation
#' @author Kai Budde-Sagert
#' @export summarizeCiliaInformation
#' @import graphics
#' @import stats
#' @param df_cilium_information A data frame
#' @param min_cilium_area A number (min number of pixels needed for a layer being part of a cilium)
#' @param pixel_size A number (size of one pixel in micrometer)
#' @param slice_distance A number (distance of two consecutive slices in
#' z-direction in micrometer)
#' @examples
#' \dontrun{
#' # Get the length of the cilia
#' df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
#'                                                0.219647,
#'                                                0.20944)
#' }
#'

summarizeCiliaInformation <- function(
  df_cilium_information = df_cilium_information,
  min_cilium_area = min_cilium_area,
  pixel_size = pixel_size,
  slice_distance = slice_distance) {

  number_of_cilia <- unique(df_cilium_information$ciliumNumber)

  df_cilium_summary <- data.frame("cilium" = sort(number_of_cilia),
                                  vertical_length_in_um = NA,
                                  vertical_length_in_layers = NA,
                                  horizontal_length_in_um = NA,
                                  horizontal_length_in_pixels = NA,
                                  total_length_in_um = NA)

  # Go through every cilium and find the length of the z-projection --------
  for(i in number_of_cilia){

    df_cilium_projection <- df_cilium_information[
      df_cilium_information$ciliumNumber == i,]
    # height of cilium (projection in vertical direction)
    
    layers_larger_than_min_size <- unique(df_cilium_projection$layer[df_cilium_projection$layer > 0])[
      as.logical(table(df_cilium_projection$layer[df_cilium_projection$layer > 0]) > min_cilium_area)] #in pixels
    
    lower_layer <- min(layers_larger_than_min_size)
    upper_layer <- max(layers_larger_than_min_size)

    vertical_length_in_layers <- upper_layer-lower_layer + 1 # in z stack layers
    vertical_length_in_um <- (upper_layer - lower_layer + 1) * slice_distance # in \mu m

    # Find pixels of z-projection ##
    # Only keep non-duplicated columns pos_x and pos_y (all unique positions)
    df_cilium_projection <- df_cilium_projection[,c(1,2)]
    df_cilium_projection <- df_cilium_projection[
      !duplicated(df_cilium_projection), ]

    df_cilium_projection <- dplyr::arrange(df_cilium_projection, .data$pos_x, .data$pos_y)

    # Find a linear regression that fits best through the points -----------
    number_of_pos_x_points <- length(unique(df_cilium_projection$pos_x))
    number_of_pos_y_points <- length(unique(df_cilium_projection$pos_y))

    # The side with more points shall represent the x-axis in the regression
    if(number_of_pos_y_points > number_of_pos_x_points){
      # print(paste("Cilium Nr. ", i, " is elongated in x-direction more than in y-direction.", sep=""))
      # column is x-axis (as usually)
      linear_model <- lm(pos_x ~ pos_y, df_cilium_projection)
      slope <- as.numeric(linear_model$coefficients[2])

      # plot(df_cilium_projection$pos_y, -df_cilium_projection$pos_x)
      # abline(-linear_model$coefficients[1], -linear_model$coefficients[2])

      # Length of the line
      x2 <- max(df_cilium_projection$pos_y)
      x1 <- min(df_cilium_projection$pos_y)
      horizontal_length_in_pixels <- sqrt(slope*slope + 1) * (x2 - x1) # in pixels
      horizontal_length_in_um <- horizontal_length_in_pixels * pixel_size # in \mu m

    }else{
      # print(paste("Cilium Nr. ", i, " is elongated in y-direction more than in x-direction.", sep=""))
      # pos_x is x-axis
      linear_model <- lm(pos_y ~ pos_x, df_cilium_projection)
      slope <- as.numeric(linear_model$coefficients[2])

      #plot(df_cilium_projection$pos_y, -df_cilium_projection$pos_x)
      #abline(linear_model$coefficients[1]/linear_model$coefficients[2], -1/linear_model$coefficients[2])

      # Length of the line
      x2 <- max(df_cilium_projection$pos_x)
      x1 <- min(df_cilium_projection$pos_x)
      horizontal_length_in_pixels <- sqrt(slope*slope + 1) * (x2 - x1) # in pixels
      horizontal_length_in_um <- horizontal_length_in_pixels * pixel_size # in \mu m

    }

    # Total length of the cilium
    total_length_in_um <- sqrt(horizontal_length_in_um*horizontal_length_in_um + vertical_length_in_um*vertical_length_in_um)
    
    df_cilium_summary$vertical_length_in_um[df_cilium_summary$cilium == i] <- vertical_length_in_um
    df_cilium_summary$vertical_length_in_layers[df_cilium_summary$cilium == i] <- vertical_length_in_layers
    df_cilium_summary$horizontal_length_in_um[df_cilium_summary$cilium == i] <- horizontal_length_in_um
    df_cilium_summary$horizontal_length_in_pixels[df_cilium_summary$cilium == i] <- horizontal_length_in_pixels
    df_cilium_summary$total_length_in_um[df_cilium_summary$cilium == i] <- total_length_in_um


  }

  return(df_cilium_summary)

}
