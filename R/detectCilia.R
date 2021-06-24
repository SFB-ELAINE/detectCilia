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
#' @param input_dir_tif A character (directory that contains z-stack sclices
#' in the tif format)
#' @param input_file_czi A character (a file with the z-stack image as czi)
#' @param cilium_color A character (color of the cilium staining)
#' @param nucleus_color A character (color of the nuclei staining)
#' @param threshold_by_density_of_cilium_pixels A Boolean (disregard the
#' threshold values if true and instead use a custom function to calculate
#' the thresholds by looking at the density of cilium color pixels found in
#' the image)
#' @param threshold_find A number (minimum intensity to find cilia)
#' @param threshold_connect A number (minimum intensity to connect to
#' already detected cilium)
#' @param vicinity A number (neighborhood to look for pixels that belong to
#' a given cilium)
#' @param min_size A number (gives the minimum size of a cilium to be
#' detected)
#' @param max_size A number (gives the maximum size of a cilium to be
#' detected)
#' @param number_size_factor A number (factor for resizing the digit image)
#' @param pixel_size A number (size of one pixel in micrometer)
#' @param slice_distance A number (distance of two consecutive slices in
#' z-direction in micrometer)
#' @param nuc_mask_width_heigth A number (width and heigth for detecting
#' nuclei)

detectCilia <- function(input_dir_tif = NULL,
                        input_file_czi = NULL,
                        cilium_color = "red",
                        nucleus_color = "blue",
                        threshold_by_density_of_cilium_pixels = FALSE,
                        threshold_find = 0.01,
                        threshold_connect = 0.005,
                        vicinity = NULL,
                        min_size = 3,
                        max_size = 100,
                        number_size_factor = NULL,
                        pixel_size = NULL,
                        slice_distance = NULL,
                        nuc_mask_width_heigth = 100) {
  
  
  # Basics and sourcing functions ------------------------------------------
  .old.options <- options()
  on.exit(options(.old.options))
  
  options(stringsAsFactors = FALSE, warn=-1)
  
  # ---------------------------------------------------------------------- #
  # ---------------------- Data acquisition ------------------------------ #
  # ---------------------------------------------------------------------- #
  
  # Parameter input --------------------------------------------------------
  
  if(is.null(vicinity)){
    vicinity <- 1
  }
  # Data input -------------------------------------------------------------
  
  # Input directory must be submitted. If not: close function call.
  if(is.null(input_dir_tif) && is.null(input_file_czi)){
    print(paste("Please call the function with an input directory ",
                "which contains z-stack tif images with colored cilia or",
                "with an input czi file.",
                sep=""))
    return()
  }
  
  # Save information whether it is a directory with tif images or a czi file
  if(is.null(input_dir_tif) && !is.null(input_file_czi)){
    image_format <- "czi"
  }else if(!is.null(input_dir_tif) && is.null(input_file_czi)){
    image_format <- "tif"
  }
  
  # Save the file names of tif images --------------------------------------
  
  if(image_format == "tif"){
    file_names_tif <- list.files(path = input_dir_tif)
    file_names_tif <- file_names_tif[grepl("tif", file_names_tif)]
  }
  
  # Data output ------------------------------------------------------------
  # Make a new subdirectory inside the input directory
  if(image_format == "tif"){
    if(grepl("\\\\", input_dir_tif)){
      input_dir_tif <- gsub("\\$", "", input_dir_tif)
      input_file_name <- gsub("(.+)\\.tif", "\\1", file_names_tif[1])
      output_dir <- paste(input_dir_tif, "\\output\\", sep="")
    }else{
      input_dir_tif <- gsub("/$", "", input_dir_tif)
      input_file_name <- gsub("(.+)\\.tif", "\\1", file_names_tif[1])
      output_dir <- paste(input_dir_tif, "/output/", sep="")
    }
  }else if(image_format == "czi"){
    if(grepl("\\\\", input_file_czi)){
      input_dir <- gsub("(.+)\\.+\\.czi", "\\1", input_file_czi)
      input_file_name <- gsub(".+\\(.+)\\.czi", "\\1", input_file_czi)
      output_dir <- paste(input_dir, "\\", input_file_name, "_output\\", sep="")
      
    }else{
      input_dir <- gsub("(.+)/.+\\.czi", "\\1", input_file_czi)
      input_file_name <- gsub(".+/(.+)\\.czi", "\\1", input_file_czi)
      output_dir <- paste(input_dir, "/", input_file_name, "_output/", sep="")
    }
  }
  
  dir.create(output_dir, showWarnings = FALSE)
  
  
  # ---------------------------------------------------------------------- #
  # ---------------------- Data manipulation ----------------------------- #
  # ---------------------------------------------------------------------- #
  # Read image data an put into one array if necessary
  if(image_format == "tif"){
    
    # i = 1 .. n go through all slices and append them to image_data -------
    print("Finding cilia in every layer.")
    for(i in 1:length(file_names_tif)){
      
      print(paste("Dealing with file ", file_names_tif[i], ". (It is now ",
                  Sys.time(), ".)", sep=""))
      
      # Read, manipulate and save cilia image ------------------------------
      image_path <- paste(input_dir_tif, "/", file_names_tif[i], sep="")
      
      #image <- tiff::readTIFF(source = image_path, convert = TRUE,
      #                        info = FALSE)
      image <- EBImage::readImage(files = image_path, type = "tiff")
      
    
      # Find the layer name (The file name should be of the form
      # "..z01..","..z02..", ... for files obtained from ZEN oder of the
      # form "0001", "0002", ... for files obtained from ImageJ)
      if(grepl(pattern = ".*z([[:digit:]]+).*",
               x = file_names_tif[i], ignore.case = TRUE)){
        layer_number <- gsub(".*z([[:digit:]]+).*","\\1", file_names_tif[i])
      }else if(grepl(pattern = ".*([[:digit:]]{2})$",
                     x = file_names_tif[i], ignore.case = TRUE)){
        layer_number <- gsub(".*([[:digit:]]{2,})$","\\1", file_names_tif[i])
      }else{
        print("The naming of the files is bad.")
        return(0)
      }
      
      layer_number <- as.integer(layer_number)
      
      if(layer_number != i){
        print("The numbering of the layers is inaccurate of layers are missing.")
      }
      
      if(i == 1){
        image_data <- array(0, dim = c(dim(image),length(file_names_tif)))
      }
      
      image_data[,,,i] <- image
    }
    
    Image_data <- EBImage::Image(data = image_data, colormode = "Color")
    
    rm(i)
    
  }else if(image_format == "czi"){
    image_data <- readCzi::readCzi(input_file = input_file_czi)
    Image_data <- EBImage::Image(data = image_data, colormode = "Color")
  }
  
  
  # Stack images
  # if(image_format == "tif"){
  #   # Get a stack of all layers and recalculate thresholds if required -----
  #   #image_stack <- stackImages::stackImages(input_dir = input_dir,
  #   #                                        stackMethod = "addAndNormalize")
  #   #image_stack_max <- stackImages::stackImages(input_dir = input_dir,
  #   #                                            stackMethod = "max")
  #   
  #   image_stack <- stackImages::stackImages(input_dir = input_dir,
  #                                           stackMethod = "average")
  #   
  #   Image_stack <- EBImage::Image(data = image_stack, colormode = "Color")
  #   
  # }else if(image_format == "czi"){
  #   
    # Create empty stack image
    image_stack <- array(0, dim = dim(Image_data)[1:3])
    #Image_stack <- EBImage::Image(data = array(0, dim = dim(Image_data)[1:3]),
    #                              colormode = "Color")
    
    # Stack with mean values
    for(i in 1:dim(image_data)[3]){
      image_stack[,,i] = apply(image_data[,,i,], c(1,2), mean)
    }
    
    Image_stack <- EBImage::Image(data = image_stack, colormode = "Color")
    
    rm(i)
    
  # }
  
  # Enhance contrast of stack image
  Image_stack_histogram_equalization <- EBImage::clahe(x = Image_stack)
  
  # Find the nuclei --------------------------------------------------------
  
  # Save only color layer of nuclei
  Image_nuclei <- getLayer(image = Image_stack, layer = nucleus_color)
  
  # Reduce background noise
  #Image_nuclei[Image_nuclei < quantile(Image_nuclei, 0.1)] <- 0
  
  # Blur the image
  Image_nuclei <-  medianFilter(x = Image_nuclei, size = 5)
  #Image_nuclei <- EBImage::gblur(Image_nuclei, sigma = 3)
  
  # Make the image brighter
  Image_nuclei <- 10*Image_nuclei
  #Image_nuclei <- Image_nuclei/max(Image_nuclei)
  
  #display(Image_nuclei)
  #display(Image_nuclei, method = "raster", all = TRUE)
  
  #nmask <- EBImage::thresh(Image_nuclei, w=500, h=500, offset=0.05)
  nmask <- EBImage::thresh(x = Image_nuclei,
                           w = nuc_mask_width_heigth,
                           h = nuc_mask_width_heigth,
                           offset = 0.05)
  
  # Morphological opening to remove objects smaller than the structuring element
  # (disc of sice 25)
  nmask <- EBImage::opening(nmask, makeBrush(13, shape='disc'))
  # Fill holes
  nmask <- EBImage::fillHull(nmask)
  # Label each connected set of pixels with a distinct ID
  nmask <- EBImage::bwlabel(nmask)
  
  #display(nmask)
  
  # Record all nuclei that are at the edges of the image
  left  <- table(nmask[1, 1:dim(nmask)[2]])
  top   <- table(nmask[1:dim(nmask)[1],1])
  right <- table(nmask[dim(nmask)[1], 1:dim(nmask)[2]])
  bottom <- table(nmask[1:dim(nmask)[1],dim(nmask)[2]])
  
  left <- as.integer(names(left))
  top <- as.integer(names(top))
  right <- as.integer(names(right))
  bottom <- as.integer(names(bottom))
  
  nuclei_at_borders <- unique(c(left, top, right, bottom))
  # delete 0
  nuclei_at_borders <- nuclei_at_borders[nuclei_at_borders != 0]
  
  # Delete all nuclei at border
  if(length(nuclei_at_borders) > 0){
    for(i in 1:length(nuclei_at_borders)){
      imageData(nmask)[imageData(nmask) == nuclei_at_borders[i]] <- 0
    }
    rm(i)
  }
  
  #display(nmask)
  
  # Delete all remaining nuclei that are smaller than 5% of the median size
  # object sizes
  # barplot(table(nmask)[-1])
  
  nmask <-  EBImage::watershed( distmap(nmask), 1 )

  table_nmask <- table(nmask)
  nuc_min_size <- 0.1*median(table_nmask[-1])
  
  # remove objects that are smaller than min_nuc_size
  to_be_removed <- as.integer(names(which(table_nmask < nuc_min_size)))
  if(length(to_be_removed) > 0){
    for(i in 1:length(to_be_removed)){
      imageData(nmask)[imageData(nmask) == to_be_removed[i]] <- 0
    }
    rm(i)
  }
  
  # Recount nuclei
  nmask <- EBImage::bwlabel(nmask)
  #display(nmask)
  
  # Watershed in order to distinct nuclei that are too close to each other
  nmask_watershed <-  EBImage::watershed( distmap(nmask), 1 )
  #display(colorLabels(nmask_watershed), all=TRUE)
  
  # Count number of cells
  nucNo <- max(nmask_watershed)
  
  # Calculate "threshold_find" and "threshold_connect" if ------------------
  # "threshold_by_density_of_cilium_pixels == TRUE"
  # The thresholds will be calculated with the contrast-enhanced stack image
  
  if(threshold_by_density_of_cilium_pixels == TRUE){
    
    print(paste("Recalculating the thresholds, because ",
                "threshold_by_density_of_cilium_pixels was given as ",
                "TRUE.", sep=""))
    
    Image_cilia_layer <- getLayer(image = Image_stack,
                                  layer = cilium_color)
    Image_cilia_layer_histeq <- getLayer(image = Image_stack_histogram_equalization,
                                         layer = cilium_color)
    
    #ratio_of_cilia_pixels <- nucNo * ((max_size - min_size) / 2) /
    #  (dim(Image_stack)[1]*dim(Image_stack)[2])
    ratio_of_cilia_pixels <- nucNo * (2 * min_size) /
      (dim(Image_stack)[1]*dim(Image_stack)[2])
    
    #ratio_of_cilia_pixels <- 1.1*ratio_of_cilia_pixels
    
    print(paste("Intensity quantile to find cilia: ",
                (1-ratio_of_cilia_pixels), sep=""))
    threshold_find_avg <- quantile(Image_cilia_layer, (1-ratio_of_cilia_pixels) )
    threshold_find_avg <- as.numeric(threshold_find_avg)
    
    # Set min intensity 0
    #Image_cilia_layer_histeq[
    #  Image_cilia_layer_histeq == min(Image_cilia_layer_histeq)] <-
    #  Image_cilia_layer_histeq - min(Image_cilia_layer_histeq)
    
    threshold_find_histeq <- quantile(Image_cilia_layer_histeq, (1-ratio_of_cilia_pixels))
    threshold_find_histeq <- as.numeric(threshold_find_histeq)
    
    #threshold_find <- min(threshold_find_avg, threshold_find_histeq)
    # threshold_find in histeq-image
    threshold_find <- threshold_find_histeq
    
    #zeros_of_cilia  <- sum(image_cilia_layer==0)
    #sum_of_cilia    <- sum(image_cilia_layer)
    #points_of_cilia <- dim(image_cilia_layer)[1]*dim(image_cilia_layer)[2]
    #average_cilia   <- sum_of_cilia/(points_of_cilia-zeros_of_cilia)
    
    # threshold_find in avg-image
    threshold_connect <- threshold_find_avg
    
    print(paste("The new threshold values are: threshold_find = ",
                threshold_find, " and threshold_connect = ",
                threshold_connect,
                ".",
                sep=""))
    
    rm(list = c("Image_cilia_layer", "Image_cilia_layer_histeq"))
  }
  
  # # Calculate threshold find and threshold connect if
  # # threshold_by_density_of_cilium_pixels == TRUE
  # if(threshold_by_density_of_cilium_pixels == TRUE){
  #   print(paste("Recalculating the thresholds, because ",
  #               "threshold_by_density_of_cilium_pixels was given as ",
  #               "TRUE.", sep=""))
  #   
  #   image_cilia_layer <- getLayer(image = image_stack,
  #                                 layer = cilium_color)
  #   image_cilia_layer_max <- getLayer(image = image_stack_max,
  #                                     layer = cilium_color)
  #   
  #   #number_of_cilium_pixels <- sum(image_cilia_layer > 0)
  #   #density_of_cilium_pixels <- number_of_cilium_pixels /
  #   #  (dim(image_cilia_layer)[1] * dim(image_cilia_layer)[2])
  #   
  #   
  #   ## Custom formula (found by analyzing a few images) for calculating the
  #   ## thresholds
  #   #threshold_find <- (density_of_cilium_pixels + 0.006) / 0.867
  #   #threshold_find <- round(threshold_find, 4)
  #   #threshold_connect <- threshold_find / 2
  #   
  #   #threshold_find <- (2*quantile(image_cilia_layer[image_cilia_layer>0], 0.95) +
  #   #                     max(image_cilia_layer)) / 3
  #   threshold_find_avg <- quantile(image_cilia_layer[image_cilia_layer>0], 0.997)
  #   threshold_find_avg <- as.numeric(threshold_find_avg)
  #   
  #   threshold_find_max <- quantile(image_cilia_layer_max[image_cilia_layer_max>0], 0.997)
  #   threshold_find_max <- as.numeric(threshold_find_max)
  #   
  #   threshold_find <- min(threshold_find_avg, threshold_find_max)
  #   
  #   if(threshold_find < 0.1){
  #     threshold_find <- 0.1
  #   }
  #   
  #   zeros_of_cilia  <- sum(image_cilia_layer==0)
  #   sum_of_cilia    <- sum(image_cilia_layer)
  #   points_of_cilia <- dim(image_cilia_layer)[1]*dim(image_cilia_layer)[2]
  #   average_cilia   <- sum_of_cilia/(points_of_cilia-zeros_of_cilia)
  #   
  #   threshold_connect <- threshold_find/10
  #   
  #   print(paste("The new threshold values are: threshold_find = ",
  #               threshold_find, " and threshold_connect = ",
  #               threshold_connect,
  #               ".",
  #               sep=""))
  #   
  #   rm(image_cilia_layer)
  # }
  
  # Save information where there have been found cilia ---------------------
  
  # Save only color layer of cilia
  Image_cilia <- editImage(image = Image_stack_histogram_equalization,
                           object_color = cilium_color,
                           threshold = threshold_find)
  
  #display(Image_cilia)
  
  list_of_cilium_points <- which(Image_cilia > 0, arr.ind = T)
  
  if(length(list_of_cilium_points) == 0){
    print("No cilium found.")
    
    # (The next few lines are also found in the end of this function)
    Image_stack_histogram_equalization_normalized <-
      EBImage::normalize(Image_stack_histogram_equalization)
    
    EBImage::writeImage(x = Image_stack_histogram_equalization_normalized,
                        files = paste(output_dir, input_file_name,
                                      "_stack_cilia_all_histogram_equalized_normalized.tif",
                                      sep = ""),
                        bits.per.sample = 8,
                        type = "tiff")
    
    # Save all parameters in a csv
    
    # Original parameter
    function_call <- paste(deparse(match.call()), collapse = "")
    function_call <- gsub(pattern = " +", replacement = " ", x = function_call)
    df_OriginalParameterList <- data.frame(
      "parameterNames" = "Function call",
      "parameterValues" = function_call)
    
    # Final parameter values
    parameters <- as.list(match.call())
    parameter_names <- names(parameters)[names(parameters) != ""]
    
    # Add "threshold_find" and "threshold_connect" to parameterlist if they
    # are not already in the list
    if( ! ("threshold_find" %in% parameter_names) ){
      parameter_names <-  c(parameter_names, "threshold_find")
    }
    if( ! ("threshold_connect" %in% parameter_names) ){
      parameter_names <-  c(parameter_names, "threshold_connect")
    }
    
    df_FinalParameterList <- data.frame("parameterNames" = parameter_names,
                                        "parameterValues" = NA)
    
    # Go through every parameterName and save current value
    for(i in 1:length(parameter_names)){
      df_FinalParameterList$parameterValues[
        df_FinalParameterList$parameterNames == parameter_names[i]] <-
        as.character(get(parameter_names[i]))
    }
    rm(i)
    
    # Combine both data frames
    df_parameterList <- rbind(df_OriginalParameterList, df_FinalParameterList)
    
    if(!is.null(df_parameterList)){
      write.csv(df_parameterList,
                file = paste(output_dir, "parameter_list.csv", sep=""), row.names = FALSE)
      write.csv2(df_parameterList,
                 file = paste(output_dir, "parameter_list_de.csv", sep=""), row.names = FALSE)
    }
    
    # Save the number of nuclei
    df_number_nuclei <- data.frame("numberOfNuclei" = nucNo)
    if(!is.null(df_number_nuclei)){
      write.csv(df_number_nuclei,
                file = paste(output_dir, "nuclei_number.csv", sep=""), row.names = FALSE)
      write.csv2(df_number_nuclei,
                 file = paste(output_dir, "nuclei_number_de.csv", sep=""), row.names = FALSE)
    }
    
    # Get the length of the cilia
    df_cilium_summary <- data.frame("cilium" = NA, "vertical_length" = 0,
                                    "horizontal_length" = 0,
                                    "total_length" = 0)
    
    if(!is.null(df_cilium_summary)){
      write.csv(df_cilium_summary,
                file = paste(output_dir, "cilium_summary.csv", sep=""), row.names = FALSE)
      
      write.csv2(df_cilium_summary,
                 file = paste(output_dir, "cilium_summary_de.csv", sep=""), row.names = FALSE)
    }
    
    # Save an excel sheet that contains information of all csv files
    # in separate sheets
    write.xlsx(df_number_nuclei,
               file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
               sheetName = "NucleiNumber", row.names = FALSE,
               append = TRUE)
    
    write.xlsx(df_parameterList,
               file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
               sheetName = "ParameterList", row.names = FALSE,
               append = TRUE)
    
    return(NULL)
  }
  
  df_cilium_points <- data.frame(list_of_cilium_points)
  df_cilium_points <- dplyr::arrange(df_cilium_points, row, col)
  rm(list_of_cilium_points)
  
  # Delete all found pixels that have no other found pixels in the ---------
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
  rm(i)
  
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  
  # Determine the cilium number for each cilium found ----------------------
  
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
  
  # Delete all structures that are too small and therefore may not be ------
  # a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) < min_size){
      df_cilium_points <- df_cilium_points[
        !(df_cilium_points$ciliumNumber == i),]
    }
  }
  rm(i)
  
  # Delete all structures that are too big and therefore may not be --------
  # a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) > max_size){
      df_cilium_points <- df_cilium_points[
        !(df_cilium_points$ciliumNumber == i),]
    }
  }
  rm(i)
  
  # Return function because no cilium could be found -----------------------
  if(length(unique(df_cilium_points$ciliumNumber)) == 0){
    print(paste("Please change your input parameter values or image file. ",
                "No cilium could be found.",
                sep=""))
    
    # (The next few lines are also found in the end of this function)
    Image_stack_histogram_equalization_normalized <-
      EBImage::normalize(Image_stack_histogram_equalization)
    
    EBImage::writeImage(x = Image_stack_histogram_equalization_normalized,
                        files = paste(output_dir, input_file_name,
                                      "_stack_cilia_all_histogram_equalized_normalized.tif",
                                      sep = ""),
                        bits.per.sample = 8,
                        type = "tiff")
    
    # Save all parameters in a csv
    
    # Original parameter
    function_call <- paste(deparse(match.call()), collapse = "")
    function_call <- gsub(pattern = " +", replacement = " ", x = function_call)
    df_OriginalParameterList <- data.frame(
      "parameterNames" = "Function call",
      "parameterValues" = function_call)
    
    # Final parameter values
    parameters <- as.list(match.call())
    parameter_names <- names(parameters)[names(parameters) != ""]
    
    # Add "threshold_find" and "threshold_connect" to parameterlist if they
    # are not already in the list
    if( ! ("threshold_find" %in% parameter_names) ){
      parameter_names <-  c(parameter_names, "threshold_find")
    }
    if( ! ("threshold_connect" %in% parameter_names) ){
      parameter_names <-  c(parameter_names, "threshold_connect")
    }
    
    df_FinalParameterList <- data.frame("parameterNames" = parameter_names,
                                        "parameterValues" = NA)
    
    # Go through every parameterName and save current value
    for(i in 1:length(parameter_names)){
      df_FinalParameterList$parameterValues[
        df_FinalParameterList$parameterNames == parameter_names[i]] <-
        as.character(get(parameter_names[i]))
    }
    rm(i)
    
    # Combine both data frames
    df_parameterList <- rbind(df_OriginalParameterList, df_FinalParameterList)
    
    if(!is.null(df_parameterList)){
      write.csv(df_parameterList,
                file = paste(output_dir, "parameter_list.csv", sep=""), row.names = FALSE)
      write.csv2(df_parameterList,
                 file = paste(output_dir, "parameter_list_de.csv", sep=""), row.names = FALSE)
    }
    
    # Save the number of nuclei
    df_number_nuclei <- data.frame("numberOfNuclei" = nucNo)
    if(!is.null(df_number_nuclei)){
      write.csv(df_number_nuclei,
                file = paste(output_dir, "nuclei_number.csv", sep=""), row.names = FALSE)
      write.csv2(df_number_nuclei,
                 file = paste(output_dir, "nuclei_number_de.csv", sep=""), row.names = FALSE)
    }
    
    # Get the length of the cilia
    df_cilium_summary <- data.frame("cilium" = NA, "vertical_length" = 0,
                                    "horizontal_length" = 0,
                                    "total_length" = 0)
    
    if(!is.null(df_cilium_summary)){
      write.csv(df_cilium_summary,
                file = paste(output_dir, "cilium_summary.csv", sep=""), row.names = FALSE)
      
      write.csv2(df_cilium_summary,
                 file = paste(output_dir, "cilium_summary_de.csv", sep=""), row.names = FALSE)
    }
    
    # Save an excel sheet that contains information of all csv files
    # in separate sheets
    write.xlsx(df_number_nuclei,
               file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
               sheetName = "NucleiNumber", row.names = FALSE,
               append = TRUE)
    
    write.xlsx(df_parameterList,
               file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
               sheetName = "ParameterList", row.names = FALSE,
               append = TRUE)
    
    return(NULL)
  }
  
  # Renumber the cilia -----------------------------------------------------
  .number <- 1
  for(i in unique(df_cilium_points$ciliumNumber)){
    df_cilium_points$ciliumNumber[df_cilium_points$ciliumNumber == i] <-
      .number
    .number <- .number + 1
  }
  rm(i)
  rm(.number)
  
  # Drop possibleCilium column
  df_cilium_points <-
    df_cilium_points[,!names(df_cilium_points)=="possibleCilium"]
  
  
  # Save image with marked cilia
  Image_stack_cilia <- Image_stack
  for(k in 1:length(df_cilium_points$row)){
    Image_stack_cilia[df_cilium_points$row[k], df_cilium_points$col[k], 1] <- 1
    Image_stack_cilia[df_cilium_points$row[k], df_cilium_points$col[k], 2] <- 1
  }
  
  
  # Save image after looking for the brightest spots
  # tiff::writeTIFF(what = Image_stack_cilia,
  #                 where = paste(output_dir,
  #                               "stack_cilia_unconnected.tif",
  #                               sep = ""),
  #                 bits.per.sample = 8L, compression = "none",
  #                 reduce = TRUE)
  EBImage::writeImage(x = Image_stack_cilia,
                      files = paste(output_dir,
                                    input_file_name,
                                    "_stack_cilia_unconnected.tif",
                                    sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  # ---------------------------------------------------------------------- #
  # Go through every image layer (i = 1 ... n) and find cilia there that   #
  # are connected to the projection of all images                          #
  # ---------------------------------------------------------------------- #
  
  print("Connecting all images.")
  
  # Add layer information to data frame
  df_cilium_points$layer <- -1
  
  # Save information in big data frame, which contains all layers
  # -1 is the sum of all layers
  df_cilium_information <- df_cilium_points
  
  # i = 1 .. n (layers) Go through all layers ------------------------------
  print("Finding cilia in every layer.")
  for(i in 1:dim(image_data)[4]){
    
    print(paste("Dealing with layer ", i, ". (It is now ",
                Sys.time(), ".)", sep=""))
    
    
    # Save Image for every layer 
    #Image <-  Image_data[,,,i]
    Image <-  image_data[,,,i]
    
    # Image of each layer with cilium channel
    Image_cilia_connect <- editImage(image = Image,
                                     object_color = cilium_color,
                                     threshold = threshold_connect)
    
    # Get positions of the cilia
    list_of_cilium_points_connect <- which(Image_cilia_connect==1,
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
        
        df_cilium_points_connect$layer <- i
        
        # Save the layer and append it to big data frame -------------------
        
        # Save the positions of the cilia
        df_cilium_information <- rbind(df_cilium_information,
                                       df_cilium_points_connect)
        
        # Save image with marked cilia
        for(k in 1:length(df_cilium_points_connect$row)){
          Image[df_cilium_points_connect$row[k],
                df_cilium_points_connect$col[k], 1] <- 1
          Image[df_cilium_points_connect$row[k],
                df_cilium_points_connect$col[k], 2] <- 1
        }
        
        # tiff::writeTIFF(what = Image,
        #                 where = paste(output_dir, input_file_name,
        #                               "_cilia_layer_", i, ".tif", sep = ""),
        #                 bits.per.sample = 8L, compression = "none",
        #                 reduce = TRUE)
        Image <- EBImage::Image(data = Image, colormode = "color")
        EBImage::writeImage(x = Image,
                            files = paste(output_dir, input_file_name,
                                          "_cilia_layer_", i, ".tif", sep = ""),
                            bits.per.sample = 8,
                            type = "tiff")
      }
      
    }
    
  }
  
  rm(i)
  rm(Image)
  
  
  # Save the stack with cilium information of all layers -------------------
  
  df_cilium_all <- df_cilium_information
  df_cilium_all$rowcol <- paste(df_cilium_all$row,
                                df_cilium_all$col,
                                sep = ",")
  df_cilium_all <- df_cilium_all[
    df_cilium_all$rowcol == unique(df_cilium_all$rowcol),]
  df_cilium_all <- df_cilium_all[,-c(5)]
  df_cilium_all$layer <- -99
  df_cilium_all <- dplyr::arrange(df_cilium_all, ciliumNumber, row, col)
  row.names(df_cilium_all) <- NULL
  
  # Add information of all cilium coordinates as layer -99 to data frame
  df_cilium_information <- rbind(df_cilium_information, df_cilium_all)
  
  # Mark all cilia coordinates in a new stack image
  #Image_stack_copy  <- Image_stack
  #Image_stack_all_cilia <- Image_stack_copy
  Image_stack_cilia_connected <- Image_stack
  
  
  for(k in 1:length(df_cilium_all$row)){
    Image_stack_cilia_connected[df_cilium_all$row[k], df_cilium_all$col[k], 1] <- 1
    Image_stack_cilia_connected[df_cilium_all$row[k], df_cilium_all$col[k], 2] <- 1
  }
  rm(k)
  
  # tiff::writeTIFF(what = Image_stack_cilia_connected,
  #                 where = paste(output_dir, input_file_name,
  #                               "_stack_cilia_connected.tif", sep = ""),
  #                 bits.per.sample = 8L, compression = "none",
  #                 reduce = TRUE)
  EBImage::writeImage(x = Image_stack_cilia_connected,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_connected.tif", sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  
  # Add cilium numbers to image --------------------------------------------
  Image_stack_numbers <- Image_stack_cilia_connected
  image_stack_numbers <- as.array(Image_stack_numbers)
  
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
  rm(i)
  
  # tiff::writeTIFF(what = image_stack_numbers,
  #                 where = paste(output_dir, input_file_name,
  #                               "_stack_cilia_all_numbers.tif", sep = ""),
  #                 bits.per.sample = 8L, compression = "none",
  #                 reduce = TRUE)
  Image_stack_numbers <- EBImage::Image(data = image_stack_numbers,
                                        colormode = "color")
  EBImage::writeImage(x = Image_stack_numbers,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_all_numbers.tif", sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  
  # Save stack with marked nuclei ------------------------------------------
  
  # Include numbers of nuclei
  table_nmask_watershed <- table(nmask_watershed)
  
  if(length(table_nmask_watershed[-1]) > 0){
    # remove 0
    nuc_numbers <- as.integer(names(table_nmask_watershed[-1]))
    
    for(i in 1:length(nuc_numbers)){
      
      # Find approximate midpoint of every nucleus
      dummy_coordinates <- which(
        imageData(nmask_watershed) == nuc_numbers[i], arr.ind = TRUE)
      
      
      pos_x <- round(mean(dummy_coordinates[,1]))
      pos_y <- round(mean(dummy_coordinates[,2]))
      
      image_stack_numbers <- addNumberToImage(image = image_stack_numbers,
                                              number = i,
                                              pos_x = pos_x,
                                              pos_y = pos_y,
                                              number_size_factor = number_size_factor,
                                              number_color = "green")
      image_stack_numbers <- addNumberToImage(image = image_stack_numbers,
                                              number = i,
                                              pos_x = pos_x,
                                              pos_y = pos_y,
                                              number_size_factor = number_size_factor,
                                              number_color = "blue")
    }
    rm(i)
  }
  
  # Add border of nuclei and save file
  Image_stack_numbers <- EBImage::Image(image_stack_numbers)
  colorMode(Image_stack_numbers) <- "color"
  colorMode(nmask_watershed) <- "gray"
  
  Image_stack_numbers <- paintObjects(x = nmask_watershed,
                                      tgt = Image_stack_numbers,
                                      col='#ff00ff')
  
  # Display the number of nuclei
  print(paste("Number of nuclei: ", nucNo, sep=""))
  
  # tiff::writeTIFF(what = Image_stack_numbers,
  #                 where = paste(output_dir, input_file_name,
  #                               "_stack_cilia_all_numbers_nuclei.tif",
  #                               sep = ""),
  #                 bits.per.sample = 8L, compression = "none",
  #                 reduce = TRUE)
  EBImage::writeImage(x = Image_stack_numbers,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_all_numbers_nuclei.tif",
                                    sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  Image_stack_histogram_equalization_normalized <- EBImage::normalize(Image_stack_histogram_equalization)
  EBImage::writeImage(x = Image_stack_histogram_equalization_normalized,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_all_histogram_equalized_normalized.tif",
                                    sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  # Output of all information ----------------------------------------------
  # test <- function(a=1, b=2, c=3){
  #   a=2
  # 
  #   # Original parameter
  #   parameters <- deparse(match.call())
  #   df_OriginalParameterList <- data.frame("parameterNames" = "Original function call", "parameterValues" = parameters)
  # 
  #   # Final parameter values
  #   parameters <- as.list(match.call())
  #   print(parameters)
  #   parameter_names <- names(parameters)[names(parameters) != ""]
  #   df_FinalParameterList <- data.frame("parameterNames" = parameter_names, "parameterValues" = NA)
  # 
  #   # Go through every parameterName and save current value
  #   for(i in 1:length(parameter_names)){
  #     df_FinalParameterList$parameterValues[df_FinalParameterList$parameterNames == parameter_names[i]] <- as.character(get(parameter_names[i]))
  #   }
  # 
  #   # Combine both data frames
  #   df_parameterList <- rbind(df_OriginalParameterList, df_FinalParameterList)
  # 
  # 
  #   print(df_parameterList)
  # }
  # 
  # test(a = 1, b = 2, c = 4)
  
  
  # Save all parameters in a csv
  
  # Original parameter
  function_call <- paste(deparse(match.call()), collapse = "")
  function_call <- gsub(pattern = " +", replacement = " ", x = function_call)
  df_OriginalParameterList <- data.frame(
    "parameterNames" = "Function call",
    "parameterValues" = function_call)
  
  # Final parameter values
  parameters <- as.list(match.call())
  print(parameters)
  parameter_names <- names(parameters)[names(parameters) != ""]
  
  # Add "threshold_find" and "threshold_connect" to parameterlist if they
  # are not already in the list
  if( ! ("threshold_find" %in% parameter_names) ){
    parameter_names <-  c(parameter_names, "threshold_find")
  }
  if( ! ("threshold_connect" %in% parameter_names) ){
    parameter_names <-  c(parameter_names, "threshold_connect")
  }
  
  df_FinalParameterList <- data.frame("parameterNames" = parameter_names,
                                      "parameterValues" = NA)
  
  # Go through every parameterName and save current value
  for(i in 1:length(parameter_names)){
    df_FinalParameterList$parameterValues[
      df_FinalParameterList$parameterNames == parameter_names[i]] <-
      as.character(get(parameter_names[i]))
  }
  rm(i)
  
  # Combine both data frames
  df_parameterList <- rbind(df_OriginalParameterList, df_FinalParameterList)
  
  if(!is.null(df_parameterList)){
    write.csv(df_parameterList,
              file = paste(output_dir, "parameter_list.csv", sep=""), row.names = FALSE)
    write.csv2(df_parameterList,
               file = paste(output_dir, "parameter_list_de.csv", sep=""), row.names = FALSE)
  }
  
  # Save the number of nuclei
  df_number_nuclei <- data.frame("numberOfNuclei" = nucNo)
  if(!is.null(df_number_nuclei)){
    write.csv(df_number_nuclei,
              file = paste(output_dir, "nuclei_number.csv", sep=""), row.names = FALSE)
    write.csv2(df_number_nuclei,
               file = paste(output_dir, "nuclei_number_de.csv", sep=""), row.names = FALSE)
  }
  
  
  
  # Get the length of the cilia
  df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                                 pixel_size,
                                                 slice_distance)
  
  if(!is.null(df_cilium_summary)){
    write.csv(df_cilium_summary,
              file = paste(output_dir, "cilium_summary.csv", sep=""), row.names = FALSE)
    
    write.csv2(df_cilium_summary,
               file = paste(output_dir, "cilium_summary_de.csv", sep=""), row.names = FALSE)
  }
  
  
  # Save an excel sheet that contains information of all csv files
  # in separate sheets
  write.xlsx(df_cilium_summary,
             file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
             sheetName = "CiliumInformation", row.names = FALSE,
             append = FALSE)
  
  write.xlsx(df_number_nuclei,
             file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
             sheetName = "NucleiNumber", row.names = FALSE,
             append = TRUE)
  
  write.xlsx(df_parameterList,
             file = paste(output_dir, "detect_cilium_summary.xlsx", sep=""),
             sheetName = "ParameterList", row.names = FALSE,
             append = TRUE)
  
  
  # Combine all data.frame to a list
  output_list <- list("df_parameterlist" = df_parameterList,
                      "df_cilium_information" = df_cilium_information,
                      "df_cilium_summary" = df_cilium_summary)
  
  return(output_list)
}
