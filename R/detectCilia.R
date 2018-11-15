#' @title detectCilia
#' @description Main function for detecting cilia in images
#' @details Input should be tif-format.
#' At first, the cilia will be located by using the max intensity method
#' and reconnecting them with nearby pixels with the lower threshold.
#' Afterwards a linear regression is being calculated for each cilium
#' projection which determines the length a (horizontal plane). The length b
#' in the vertical plane is being determined by the numbers of z-slices
#' where the cilium can be found. The entire length of the cilium is being
#' calculated with c=sqrt(a^2+b^2).
#' The output will be written in the current directory.
#' Please be aware that the coordinate system is turned by 90° to the right.
#' The origin of the x- and y-axes is in the upper-left corner of the image.
#' (The x-axis points downwards and the y-axis to the right.)
#' @aliases ciliaDetect detectcilia
#' @author Kai Budde
#' @export detectCilia
#' @param input_dir A character (directory that contains all images)
#' @param cilia_color A character (color of the cilia staining)
#' @param threshold_find A number (minimum intensity to find cilia)
#' @param threshold_connect A number (minimum intensity to connect to cilia)
#' @param vicinity A number (neighborhood to look for pixels that belong to
#' a given cilium)
#' @examples
#' \dontrun{
#' # Obtain all positions of cilia in every z-layer
#' df_cilium_information <- detectCilia(input_dir = "inst/testFiles",
#'                                      cilia_color = "red",
#'                                      threshold_find = 0.5,
#'                                      threshold_connect = 0.1)
#'
#' # Get the length of the cilia
#' df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
#'                                                0.219647,
#'                                                0.20944)
#' }
#'

detectCilia <- function(input_dir = NULL,
                        cilia_color = "blue",
                        threshold_find = 0.5,
                        threshold_connect = 0.1,
                        vicinity = NULL) {


  # Basics and sourcing functions ------------------------------------------
  .old.options <- options()
  on.exit(options(.old.options))

  options(stringsAsFactors = FALSE, warn=-1)

  # ---------------------------------------------------------------------- #
  # ---------------------- Data acquisition ------------------------------ #
  # ---------------------------------------------------------------------- #

  # Parameter input --------------------------------------------------------

  if(is.null(vicinity)){
    vicinity <- 3
  }
  # Data input -------------------------------------------------------------

  # Input directory must be submitted. If not: close function call.
  if(is.null(input_dir)){
    print(paste("Please call the function with an input directory ",
                "which contains images of cells with colored cilia",
                sep=""))
    return()
  }

  # Save the file names (tifs) ----------------------------------
  file_names <- list.files(path = input_dir)
  file_names <- file_names[grepl("tif", file_names)]

  # Data output ------------------------------------------------------------
  # Make a new subdirectory inside the input directory
  if(grepl("\\\\", input_dir)){
    input_dir <- gsub("\\$", "", input_dir)
    output_dir <- paste(input_dir, "\\output\\", sep="")
  }else{
    input_dir <- gsub("/$", "", input_dir)
    output_dir <- paste(input_dir, "/output/", sep="")
  }

  dir.create(output_dir, showWarnings = FALSE)


  # ---------------------------------------------------------------------- #
  # ---------------------- Data manipulation ----------------------------- #
  # ---------------------------------------------------------------------- #


  # Save a matrix that contains all cilia information of all images

  # i = 1 .. n(images) Go through all images (tifs) ------------------------
  print("Connecting all images.")
  for(i in 1:length(file_names)){
    print(paste("Dealing with file ", file_names[i], ". (It is now ",
                Sys.time(), ".)", sep=""))


    # Save the row number of data.frame which contains the current
    # image.

    image_name <- gsub("(.*)\\.tif*", "\\1", file_names[i])
    image_path <- paste(input_dir, file_names[i], sep="/")


    # Read, manipulate and save cilia image --------------------------------

    image <- tiff::readTIFF(source = image_path, info = FALSE)

    # Save only color layer of cilia
    image_cilia <- editImage(image = image, cilia_color = cilia_color,
                             threshold = threshold_find)

    # Save the cilia image (The calculation is equal to the z-projection
    # method where all pixels in an overlay are being found whose intensity
    # exceed the threshold_find.)
    if(i == 1){
      sum_image_cilia <- image_cilia
    }else{
      sum_image_cilia <- sum_image_cilia + image_cilia
    }
  }

  # Sum of all cilia
  sum_image_cilia[,][sum_image_cilia[,] > 1] <- 1
  rm(image, image_cilia, image_name, image_path)


  # Save information where there have been found cilia ---------------------
  list_of_cilium_points <- which(sum_image_cilia==1, arr.ind = T)

  # Test whether there are cilia in the image
  if(length(list_of_cilium_points) > 0){
    df_cilium_points <- data.frame(list_of_cilium_points)
    rm(list_of_cilium_points)

    # Determine the cilium number for each cilium found ####################

    df_cilium_points$ciliumNumber <- 0
    df_cilium_points$ciliumNumber[1] <- 1

    if(nrow(df_cilium_points) > 1){
      i <- 2
      while(!is.na(which(df_cilium_points$ciliumNumber==0)[1])){
        .row_distance <- df_cilium_points$row[i] -
          df_cilium_points$row

        .col_distance <- df_cilium_points$col[i] -
          df_cilium_points$col

        .row_distance[abs(.row_distance) <= vicinity] <- 0
        .col_distance[abs(.col_distance) <= vicinity] <- 0

        .distance <- abs(.row_distance) + abs(.col_distance)

        # Get the cilium number
        ciliumNumber_dummy <-
          unique(df_cilium_points$ciliumNumber[.distance == 0])

        if(length(ciliumNumber_dummy) == 1 && ciliumNumber_dummy == 0){
          # Advance Cilium number
          ciliumNumber <- ciliumNumber + 1
        }else{
          # Points belong to already existing cilium
          ciliumNumber <- ciliumNumber_dummy[!(ciliumNumber_dummy == 0)]
        }


        if(length(ciliumNumber) > 1)
        {
          print("Something very wrong happened with the cilia numbering!")
        }else{
          df_cilium_points$ciliumNumber[.distance == 0] <- ciliumNumber
        }

        # Advance i to the next row which contains 0 as the cilium number
        i <- which(df_cilium_points$ciliumNumber==0)[1]
      }
    }

    df_cilium_points$layer <- -1

    # Save information in big data frame, which contains all layers
    # -1 is the sum of all layers
    df_cilium_information <- df_cilium_points


    # # # # # --------------------------------------------------------------
    # Go through every image and find cilia there that are connected to the
    # projection of all images

    # i = 1 .. n(images) Go through all images (tifs) ----------------------
    print("Finding cilia in every layer.")
    for(i in 1:length(file_names)){
      print(paste("Dealing with file ", file_names[i], ". (It is now ",
                  Sys.time(), ".)", sep=""))


      # Save the row number of data.frame which contains the current
      # image.

      image_name <- gsub("(.*)\\.tif*", "\\1", file_names[i])
      image_path <- paste(input_dir, file_names[i], sep="/")


      # Read, manipulate and save cilia image ------------------------------

      image <- tiff::readTIFF(source = image_path, info = FALSE)

      # Start with combining all layers to identify the cilia

      # Load image with the threshold_connect
      image_cilia_connect <- editImage(image = image,
                                       cilia_color = cilia_color,
                                       threshold = threshold_connect)

      # Get positions of the cilia
      list_of_cilium_points_connect <- which(image_cilia_connect==1,
                                             arr.ind = T)
      df_cilium_points_connect <- data.frame(list_of_cilium_points_connect)
      rm(list_of_cilium_points_connect)

      if(nrow(df_cilium_points_connect) > 0){
        df_cilium_points_connect$cilium <- FALSE

        # Go through the list of detected cilia and find connections in list
        # with lower threshold

        df_cilium_points_connect$ciliumNumber <- 0

        for(j in 1:nrow(df_cilium_points_connect)){

          .row_distance <- df_cilium_points_connect$row[j] -
            df_cilium_points$row

          .col_distance <- df_cilium_points_connect$col[j] -
            df_cilium_points$col

          .row_distance[abs(.row_distance) <= vicinity] <- 0
          .col_distance[abs(.col_distance) <= vicinity] <- 0

          .distance <- abs(.row_distance) + abs(.col_distance)

          # if zero is in distance, then the point of
          # df_cilium_points_connect[j] is in the vicinity of a "real"
          # cilium point

          if(0 %in% .distance){
            df_cilium_points_connect$cilium[j] <- TRUE

            # Get the cilium number from the df_cilium_points (sum of
            # all images)
            df_cilium_points_connect$ciliumNumber[j] <-
              df_cilium_points$ciliumNumber[which(.distance == 0)[1]]
          }
        }

        # Delete rows that are no true cilia
        df_cilium_points_connect <- df_cilium_points_connect[
          !df_cilium_points_connect$cilium == FALSE,]

        if(nrow(df_cilium_points_connect) > 0){
          row.names(df_cilium_points_connect) <-
            1:nrow(df_cilium_points_connect)

          # Drop information that it is cilia
          df_cilium_points_connect <- df_cilium_points_connect[-3]

          # Save the layer and append it to big data frame ###################
          if(nchar(image_name) > 2){
            .dummy <- strsplit(image_name, "")[[1]]
            layer_number <- as.integer(
              paste(.dummy[length(.dummy)-1], .dummy[length(.dummy)], sep=""))
          }else{
            layer_number <- as.integer(image_name)
          }

          df_cilium_points_connect$layer <- layer_number

          # Save the positions of the cilia
          df_cilium_information <- rbind(df_cilium_information,
                                         df_cilium_points_connect)

          # Save image with marked cilia
          for(k in 1:length(df_cilium_points_connect$row)){
            image[df_cilium_points_connect$row[k], df_cilium_points_connect$col[k], 1] <- 1
            image[df_cilium_points_connect$row[k], df_cilium_points_connect$col[k], 2] <- 1
          }

          tiff::writeTIFF(what = image,
                          where = paste(output_dir, image_name,
                                        "_cilia_layer.tif", sep = ""),
                          bits.per.sample = 8L, compression = "none",
                          reduce = TRUE)
        }

      }

    }

    return(df_cilium_information)
  }else{
    return(0)
  }
}
