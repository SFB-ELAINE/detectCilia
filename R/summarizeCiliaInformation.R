#' Summarize the information of found cilia
#' 
#' `summarizeCiliaInformation()` summarizes the information of cilia found
#' by the main function detectCilia.R. The output is a data frame with the
#' lengths of the found cilia.

#' @param df_cilium_information A data frame with found cilia.
#' @param min_cilium_area A number for the minimum number of pixels needed
#' to represent a cilium.
#' @param pixel_size A number depicting the size of one pixel in micrometers.
#' @param slice_distance A number depicting the distance of two consecutive
#' z-stack layers in micrometers.
#' @param number_of_z_stack_layers A number telling how many z-stack layers
#' there are in the image.
#' 
#' @import graphics
#' @import stats
#' 
#' @returns A tibble with summarized cilium information (lengths).

summarizeCiliaInformation <- function(
  df_cilium_information,
  min_cilium_area,
  pixel_size,
  slice_distance,
  number_of_z_stack_layers) {

  number_of_cilia <- unique(df_cilium_information$ciliumNumber)

  df_cilium_summary <- data.frame(cilium = sort(number_of_cilia),
                                  cilium_shape = NA,
                                  cilium_projection_area_in_pixels = NA,
                                  cilium_projection_area_in_um2 = NA,
                                  number_of_z_stack_layers = number_of_z_stack_layers,
                                  lowest_cilium_layer = NA,
                                  uppermost_cilium_layer = NA,
                                  vertical_length_in_layers = NA,
                                  vertical_length_in_um = NA,
                                  horizontal_length_in_pixels = NA,
                                  horizontal_length_in_um = NA,
                                  total_length_in_um = NA)

  # Go through every cilium and find the length of the z-projection --------
  for(i in number_of_cilia){

    df_cilium_projection <- df_cilium_information[
      df_cilium_information$ciliumNumber == i,]
    
    cilium_shape <- df_cilium_projection$cilium_shape[1]
    cilium_area <- sum(df_cilium_projection$layer == -99)
    cilium_area_in_um <- round(x = (cilium_area * pixel_size * pixel_size), digits = 1)
    
    # height of cilium (projection in vertical direction)
    layers_larger_than_min_size <- unique(df_cilium_projection$layer[df_cilium_projection$layer > 0])[
      as.logical(table(df_cilium_projection$layer[df_cilium_projection$layer > 0]) > min_cilium_area)] #in pixels
    
    if(length(layers_larger_than_min_size) > 0){
      lower_layer <- min(layers_larger_than_min_size)
      upper_layer <- max(layers_larger_than_min_size)
      
      vertical_length_in_layers <- upper_layer-lower_layer + 1 # in z stack layers
      vertical_length_in_um <- (upper_layer - lower_layer + 1) * slice_distance # in \mu m
    }else{
      lower_layer <- NA
      upper_layer <- NA
      
      vertical_length_in_layers <- NA
      vertical_length_in_um     <- NA
    }
    
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
      # print(paste("Cilium no. ", i, " is elongated in x-direction more than in y-direction.", sep=""))
      # column is x-axis (as usually)
      linear_model <- lm(pos_x ~ pos_y, df_cilium_projection)
      slope <- as.numeric(linear_model$coefficients[2])

      # plot(df_cilium_projection$pos_y, -df_cilium_projection$pos_x)
      # abline(-linear_model$coefficients[1], -linear_model$coefficients[2])

      # Length of the line
      x2 <- max(df_cilium_projection$pos_y)
      x1 <- min(df_cilium_projection$pos_y)
      # print(paste0("slope: ", slope))
      horizontal_length_in_pixels <- sqrt(slope*slope + 1) * (x2 - x1 + 1) # in pixels
      horizontal_length_in_um <- horizontal_length_in_pixels * pixel_size # in \mu m

    }else{
      # print(paste("Cilium no. ", i, " is elongated in y-direction more than in x-direction.", sep=""))
      # pos_x is x-axis
      linear_model <- lm(pos_y ~ pos_x, df_cilium_projection)
      slope <- as.numeric(linear_model$coefficients[2])

      #plot(df_cilium_projection$pos_y, -df_cilium_projection$pos_x)
      #abline(linear_model$coefficients[1]/linear_model$coefficients[2], -1/linear_model$coefficients[2])

      # Length of the line
      x2 <- max(df_cilium_projection$pos_x)
      x1 <- min(df_cilium_projection$pos_x)
      horizontal_length_in_pixels <- sqrt(slope*slope + 1) * (x2 - x1 + 1) # in pixels
      horizontal_length_in_um <- horizontal_length_in_pixels * pixel_size # in \mu m

    }

    # Total length of the cilium
    total_length_in_um <- sqrt(horizontal_length_in_um*horizontal_length_in_um + vertical_length_in_um*vertical_length_in_um)
    
    df_cilium_summary$cilium_shape[df_cilium_summary$cilium == i]                <- cilium_shape
    df_cilium_summary$cilium_projection_area_in_pixels[
      df_cilium_summary$cilium == i]                                             <- cilium_area
    df_cilium_summary$cilium_projection_area_in_um2[df_cilium_summary$cilium == i]<- cilium_area_in_um
    df_cilium_summary$lowest_cilium_layer[df_cilium_summary$cilium == i]         <- lower_layer
    df_cilium_summary$uppermost_cilium_layer[df_cilium_summary$cilium == i]      <- upper_layer
    df_cilium_summary$vertical_length_in_layers[df_cilium_summary$cilium == i]   <- vertical_length_in_layers
    df_cilium_summary$vertical_length_in_um[df_cilium_summary$cilium == i]       <- vertical_length_in_um
    df_cilium_summary$horizontal_length_in_pixels[df_cilium_summary$cilium == i] <- horizontal_length_in_pixels
    df_cilium_summary$horizontal_length_in_um[df_cilium_summary$cilium == i]     <- horizontal_length_in_um
    df_cilium_summary$total_length_in_um[df_cilium_summary$cilium == i]          <- total_length_in_um
  }
  
  number_of_digits <- floor(sqrt(min_cilium_area))
  
  df_cilium_summary$vertical_length_in_um   <- round(x = df_cilium_summary$vertical_length_in_um, digits = number_of_digits)
  df_cilium_summary$horizontal_length_in_um <- round(x = df_cilium_summary$horizontal_length_in_um, digits = number_of_digits)
  df_cilium_summary$horizontal_length_in_pixels <- round(x = df_cilium_summary$horizontal_length_in_pixels, digits = 1)
  df_cilium_summary$total_length_in_um      <- round(x = df_cilium_summary$total_length_in_um, digits = number_of_digits)
  
  return(df_cilium_summary)

}
