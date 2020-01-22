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
#' @param cilium_color A character (color of the cilia staining)
#' @param threshold_find A number (minimum intensity to find cilia compared)
#' @param threshold_connect A number (minimum intensity to connect to
#' already detected cilium)
#' @param vicinity A number (neighborhood to look for pixels that belong to
#' a given cilium)
#' @param min_size A number (gives the minimum size of a cilium to be
#' detected)
#' @param max_size A number (gives the maximum size of a cilium to be
#' detected)
#' @param number_size_factor A number (factor for resizing the digit image)

detectCilia <- function(input_dir = NULL,
                        cilium_color = "red",
                        threshold_find = 0.9,
                        threshold_connect = 0.5,
                        vicinity = NULL,
                        min_size = 3,
                        max_size = 100,
                        number_size_factor = NULL) {
  
  
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
  
  image_stack <- stackImages::stackImages(input_dir = input_dir,
                                          stackMethod = "max")
  image_stack_copy <- image_stack
  
  # Save only color layer of cilia
  image_cilia <- editImage(image = image_stack, cilium_color = cilium_color,
                           threshold = threshold_find)
  
  
  # Save information where there have been found cilia ---------------------
  list_of_cilium_points <- which(image_cilia > 0, arr.ind = T)
  
  if(length(list_of_cilium_points) == 0){
    print("No cilium found.")
    return()
  }
  
  df_cilium_points <- data.frame(list_of_cilium_points)
  df_cilium_points <- dplyr::arrange(df_cilium_points, row, col)
  rm(list_of_cilium_points)
  
  # Delete all found pixels that have now other found pixels in the
  # neighborhood (+-1)
  df_cilium_points$possibleCilium <- FALSE
  
  for(i in 1:length(df_cilium_points$row)){
    
    .row_distance <- df_cilium_points$row[i] -
      df_cilium_points$row
    
    .col_distance <- df_cilium_points$col[i] -
      df_cilium_points$col
    
    .row_distance[abs(.row_distance) <= 1] <- 0
    .col_distance[abs(.col_distance) <= 1] <- 0
    
    .distance <- abs(.row_distance) + abs(.col_distance)
    if(sum(.distance == 0) > 1){
      df_cilium_points$possibleCilium[.distance == 0] <- TRUE
    }
  }
  
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  
  
  # Determine the cilium number for each cilium found ######################
  
  df_cilium_points$ciliumNumber <- 0
  df_cilium_points$ciliumNumber[1] <- 1
  
  
  # Start with the second entry in the data frame
  i <- 2
  while(!is.na(which(df_cilium_points$ciliumNumber==0)[1])){
    
    # Calculate Distance of current pixel to all other pixel that might
    # be a Cilium
    .row_distance <- df_cilium_points$row[i] -
      df_cilium_points$row
    
    .col_distance <- df_cilium_points$col[i] -
      df_cilium_points$col
    
    .row_distance[abs(.row_distance) <= vicinity] <- 0
    .col_distance[abs(.col_distance) <= vicinity] <- 0
    
    .distance <- abs(.row_distance) + abs(.col_distance)
    
    # Get the cilium number (close cilium that has been detected)
    ciliumNumber_dummy <-
      unique(df_cilium_points$ciliumNumber[.distance == 0])
    
    if(length(ciliumNumber_dummy) == 1 && ciliumNumber_dummy == 0){
      # Advance Cilium number because there is no Cilium close by
      ciliumNumber <- max(df_cilium_points$ciliumNumber) + 1
    }else{
      # Points belong to already existing cilium
      ciliumNumber <- ciliumNumber_dummy[!(ciliumNumber_dummy == 0)]
    }
    
    
    if(length(ciliumNumber) > 1)
    {
      print("The following cilia are now one:")
      print(ciliumNumber)
      for(j in 2:length(ciliumNumber)){
        df_cilium_points$ciliumNumber[
          df_cilium_points$ciliumNumber == ciliumNumber[j]] <-
          ciliumNumber[1]
      }
      
    }else{
      df_cilium_points$ciliumNumber[.distance == 0] <- ciliumNumber
    }
    
    # Advance i to the next row which contains 0 as the cilium number
    i <- which(df_cilium_points$ciliumNumber==0)[1]
  }
  
  # Delete all structures that are too small and therefore may not be a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) < min_size){
      df_cilium_points <- df_cilium_points[!(df_cilium_points$ciliumNumber == i),]
    }
  }
  
  # Delete all structures that are too big and therefore may not be a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) > max_size){
      df_cilium_points <- df_cilium_points[!(df_cilium_points$ciliumNumber == i),]
    }
  }
  
  # Renumber the cilia ###################################################
  .number <- 1
  for(i in unique(df_cilium_points$ciliumNumber)){
    df_cilium_points$ciliumNumber[df_cilium_points$ciliumNumber == i] <-
      .number
    .number <- .number + 1
  }
  rm(.number)
  
  # Drop possibleCilium column
  df_cilium_points <-
    df_cilium_points[,!names(df_cilium_points)=="possibleCilium"]
  
  
  # Save image with marked cilia
  for(k in 1:length(df_cilium_points$row)){
    image_stack[df_cilium_points$row[k], df_cilium_points$col[k], 1] <- 1
    image_stack[df_cilium_points$row[k], df_cilium_points$col[k], 2] <- 1
  }
  
  
  # Save image after looking for the brightest spots
  tiff::writeTIFF(what = image_stack,
                  where = paste(output_dir, "stack_cilia_first.tif", sep = ""),
                  bits.per.sample = 8L, compression = "none",
                  reduce = TRUE)
  
  
  # # # # # --------------------------------------------------------------
  # Go through every image and find cilia there that are connected to the
  # projection of all images
  
  # Add layer information to data frame
  df_cilium_points$layer <- -1
  
  # Save information in big data frame, which contains all layers
  # -1 is the sum of all layers
  df_cilium_information <- df_cilium_points
  
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
    
    image <- tiff::readTIFF(source = image_path, convert = TRUE, info = FALSE)
    
    # Start with combining all layers to identify the cilia
    
    # Load image with the threshold_connect
    image_cilia_connect <- editImage(image = image,
                                     cilium_color = cilium_color,
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
        
        # Save the layer and append it to big data frame #################
        
        # Find the layer name (The file name should be of the form
        # "..z01..","..z02..", ... for files obtained from ZEN oder of the
        # form "0001", "0002", ... for files obtained from ImageJ)
        if(grepl(pattern = ".*z([[:digit:]]+).*",
                  x = image_name, ignore.case = TRUE)){
          layer_number <- gsub(".*z([[:digit:]]+).*","\\1", image_name)
        }else if(grepl(pattern = ".*([[:digit:]]{2})$",
                       x = image_name, ignore.case = TRUE)){
          layer_number <- gsub(".*([[:digit:]]{2,})$","\\1", image_name)
        }else{
          print("The naming of the files is bad.")
          return(0)
        }
        
        layer_number <- as.integer(layer_number)
        
        df_cilium_points_connect$layer <- layer_number
        
        # Save the positions of the cilia
        df_cilium_information <- rbind(df_cilium_information,
                                       df_cilium_points_connect)
        
        # Save image with marked cilia
        for(k in 1:length(df_cilium_points_connect$row)){
          image[df_cilium_points_connect$row[k],
                df_cilium_points_connect$col[k], 1] <- 1
          image[df_cilium_points_connect$row[k],
                df_cilium_points_connect$col[k], 2] <- 1
        }
        
        tiff::writeTIFF(what = image,
                        where = paste(output_dir, image_name,
                                      "_cilia_layer.tif", sep = ""),
                        bits.per.sample = 8L, compression = "none",
                        reduce = TRUE)
      }
      
    }
    
  }
  
  # Save the stack with cilium information of all layers ###################
  df_cilium_all <- df_cilium_information
  df_cilium_all$rowcol <- paste(df_cilium_all$row,
                                df_cilium_all$col,
                                sep = ",")
  df_cilium_all <- df_cilium_all[df_cilium_all$rowcol == unique(df_cilium_all$rowcol),]
  df_cilium_all <- df_cilium_all[,-c(5)]
  df_cilium_all$layer <- -99
  df_cilium_all <- dplyr::arrange(df_cilium_all, ciliumNumber, row, col)
  
  # Add information of all cilium coordinates as layer -99 to data frame
  df_cilium_information <- rbind(df_cilium_information, df_cilium_all)
  
  # Mark all cilia coordinates in a new stack image
  image_stack_all_cilia <- image_stack_copy
  
  for(k in 1:length(df_cilium_all$row)){
    image_stack_all_cilia[df_cilium_all$row[k], df_cilium_all$col[k], 1] <- 1
    image_stack_all_cilia[df_cilium_all$row[k], df_cilium_all$col[k], 2] <- 1
  }
  
  tiff::writeTIFF(what = image_stack_all_cilia,
                  where = paste(output_dir, "stack_cilia_all.tif", sep = ""),
                  bits.per.sample = 8L, compression = "none",
                  reduce = TRUE)
  
  # Add numbers to image
  image_stack_numbers <- image_stack_all_cilia
  
  for(i in 1:length(unique(df_cilium_all$ciliumNumber))){
    ciliumNumber <- unique(df_cilium_all$ciliumNumber)[i]
    pos_x <- df_cilium_all$row[df_cilium_all$ciliumNumber == i][
      length(df_cilium_all$row[df_cilium_all$ciliumNumber == i])]
    pos_y <- df_cilium_all$col[df_cilium_all$ciliumNumber == i][
      length(df_cilium_all$col[df_cilium_all$ciliumNumber == i])]
    
    image_stack_numbers <- addNumberToImage(image = image_stack_numbers,
                                            number = ciliumNumber,
                                            pos_x = pos_x,
                                            pos_y = pos_y,
                                            number_size_factor = number_size_factor,
                                            number_color = "red")
    
  }

  tiff::writeTIFF(what = image_stack_numbers,
                  where = paste(output_dir, "stack_cilia_all_numbers.tif", sep = ""),
                  bits.per.sample = 8L, compression = "none",
                  reduce = TRUE)
  
  
  return(df_cilium_information)
}
