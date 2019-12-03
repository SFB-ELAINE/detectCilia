#' @title summarizeCiliaInformation
#' @description Summarizes the information of found cilia in images
#' @details Input should be the result of detectCilia.R.
#' The output is a data frame with the lengths of the found cilia.
#' @aliases summarizeCiliumInformation summarizeciliainformation
#' @author Kai Budde
#' @export summarizeCiliaInformation
#' @import graphics
#' @import stats
#' @param df_cilium_information A data fram
#' @param pixel_size A number (size of one pixel in micrometer)
#' @param sclice_distance A number (distance of two consecutive slices in
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
  pixel_size = pixel_size,
  sclice_distance = sclice_distance) {

  number_of_cilia <- unique(df_cilium_information$ciliumNumber)

  df_cilium_summary <- data.frame("cilium" = sort(number_of_cilia),
                                  vertical_length = NA,
                                  horizontal_length = NA,
                                  total_length = NA)

  # Go through every cilium and find the length of the z-projection --------
  for(i in number_of_cilia){

    df_cilium_projection <- df_cilium_information[
      df_cilium_information$ciliumNumber == i,]
    # height of cilium (projection in vertical direction)
    lower_layer <- min(df_cilium_projection$layer[df_cilium_projection$layer > 0])
    upper_layer <- max(df_cilium_projection$layer[df_cilium_projection$layer > 0])

    vertical_length <- (upper_layer-lower_layer) * sclice_distance # in \mu m

    # Find pixels of z-projection ##
    # Only keep non-duplicated columns row and col
    df_cilium_projection <- df_cilium_projection[,c(1,2)]
    df_cilium_projection <- df_cilium_projection[
      !duplicated(df_cilium_projection), ]

    df_cilium_projection <- dplyr::arrange(df_cilium_projection, row, col)

    # Find a linear regression that fits best through the points -----------
    number_of_row_points <- length(unique(df_cilium_projection$row))
    number_of_col_points <- length(unique(df_cilium_projection$col))

    # The side with more points shall represent the x-axis in the regression
    if(number_of_col_points > number_of_row_points){
      print(paste("Cilium Nr. ", i, " is elongated in x-direction more than in y-direction.", sep=""))
      # column is x-axis (as usually)
      linear_model <- lm(row ~ col, df_cilium_projection)
      slope <- as.numeric(linear_model$coefficients[2])

      plot(df_cilium_projection$col, -df_cilium_projection$row)
      abline(-linear_model$coefficients[1], -linear_model$coefficients[2])

      # Length of the line
      x2 <- max(df_cilium_projection$col)
      x1 <- min(df_cilium_projection$col)
      horizontal_length <- sqrt(slope*slope + 1) * (x2 - x1) # in pixels
      horizontal_length <- horizontal_length * pixel_size # in \mu m

    }else{
      print(paste("Cilium Nr. ", i, " is elongated in y-direction more than in x-direction.", sep=""))
      # row is x-axis
      linear_model <- lm(col ~ row, df_cilium_projection)
      slope <- as.numeric(linear_model$coefficients[2])

      plot(df_cilium_projection$col, -df_cilium_projection$row)
      abline(linear_model$coefficients[1]/linear_model$coefficients[2], -1/linear_model$coefficients[2])

      # Length of the line
      x2 <- max(df_cilium_projection$row)
      x1 <- min(df_cilium_projection$row)
      horizontal_length <- sqrt(slope*slope + 1) * (x2 - x1) # in pixels
      horizontal_length <- horizontal_length * pixel_size # in \mu m

    }

    # Total length of the cilium
    total_length <- sqrt(horizontal_length*horizontal_length + vertical_length*vertical_length)

    df_cilium_summary$vertical_length[df_cilium_summary$cilium == i] <- vertical_length
    df_cilium_summary$horizontal_length[df_cilium_summary$cilium == i] <- horizontal_length
    df_cilium_summary$total_length[df_cilium_summary$cilium == i] <- total_length


  }

  return(df_cilium_summary)

}
