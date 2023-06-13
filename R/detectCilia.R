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
#' @param projection_method A character (defines the method for the z projection:
#' either "mean" or "max")
#' @param threshold_by_density_of_cilium_pixels A Boolean (disregard the
#' threshold values if true and instead use a custom function to calculate
#' the thresholds by looking at the density of cilium color pixels found in
#' the image)
#' @param threshold_find A number (minimum intensity to find cilia)
#' @param threshold_connect A number (minimum intensity to connect to
#' already detected cilium)
#' @param vicinity_combine A number (neighborhood to look for structures
#' belonging to the same cilium (exclusion volume))
#' @param vicinity_connect A number (neighborhood to look for pixels that
#' belong to a given cilium (inclusion volume))
#' @param min_cilium_area A number (gives the minimum area of a cilium to be
#' detected)
#' @param max_cilium_area A number (gives the maximum area of a cilium to be
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
                        projection_method = "mean",
                        threshold_by_density_of_cilium_pixels = TRUE,
                        threshold_find = NULL,
                        threshold_connect = NULL,
                        vicinity_combine = NULL,
                        vicinity_connect = NULL,
                        min_cilium_area = NULL,
                        max_cilium_area = NULL,
                        number_size_factor = NULL,
                        pixel_size = NULL,
                        slice_distance = NULL,
                        nuc_mask_width_heigth = NULL) {
  
  
  # Basics -----------------------------------------------------------------
  .old.options <- options()
  on.exit(options(.old.options))
  
  options(stringsAsFactors = FALSE, warn=-1)
  
  # ---------------------------------------------------------------------- #
  # ----------------------- 1. Image preparation ------------------------- #
  # ---------------------------------------------------------------------- #
  
  # Read image data --------------------------------------------------------
  
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
  
  # Save the file names of tif images
  if(image_format == "tif"){
    file_names_tif <- list.files(path = input_dir_tif)
    file_names_tif <- file_names_tif[grepl("tif", file_names_tif)]
  }
  
  # Make a new sub-directory inside the input directory for data output
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
  
  
  # Read image data and put it into one array if necessary (for TIFs)
  if(image_format == "tif"){
    
    # Check whether required parameter values are given
    if(is.null(pixel_size)){
      print("Please call the function with specifying pixel_size in um.")
      return()
    }
    if(is.null(slice_distance)){
      print("Please call the function with specifying slice_distance in um.")
      return()
    }
    
    # i = 1 .. n go through all slices and append them to image_data
    for(i in 1:length(file_names_tif)){
      
      print(paste("Dealing with file ", file_names_tif[i], ". (It is now ",
                  Sys.time(), ".)", sep=""))
      
      image_path <- paste(input_dir_tif, "/", file_names_tif[i], sep="")
      
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
    
    rm(i)
    
  }else if(image_format == "czi"){
    
    # Read data
    image_data <- readCzi::readCzi(input_file = input_file_czi)
    
    
    # Read metadata and get missing parameter values -----------------------
    df_metadata <- readCzi::readCziMetadata(input_file = input_file_czi,
                                            save_metadata = FALSE)
    
    if(df_metadata$scaling_x == df_metadata$scaling_y){
      # df_metadata$scaling_x given in um
      pixel_size_dummy <- df_metadata$scaling_x #* 1e6
      
      if(is.null(pixel_size)){
        pixel_size <- pixel_size_dummy
      }else if(pixel_size != pixel_size_dummy){
        print("The given pixel size is different to the one obtained from the metadata.")
        return()
      }
    }else{
      print("Scaling is wrong.")
    }
    if(is.null(slice_distance)){
      # df_metadata$scaling_x given in um
      slice_distance <- df_metadata$scaling_z #* 1e6
    }else{
      if(slice_distance != df_metadata$scaling_z){
        print("The given slize distance is different to the one obtained from the metadata.")
        return()
      }
    }
    
    
  }
  
  Image_data <- EBImage::Image(data = image_data, colormode = "Color")
  
  
  # Calculate missing input parameter values -------------------------------
  
  
  # Determine min and max sizes of a primary cilium
  # (We assume it to be between 1um and 5um long and 0.5um and 2um wide.)
  
  max_cilium_area_in_um2 <- 5*2
  min_cilium_area_in_um2 <- 1*0.5
  
  if(is.null(min_cilium_area)){
    min_cilium_area <- floor(x = min_cilium_area_in_um2 / pixel_size)
  }
  
  if(is.null(max_cilium_area)){
    max_cilium_area <- ceiling(x = max_cilium_area_in_um2 / pixel_size)
  }
  
  rm(list = c("max_cilium_area_in_um2", "min_cilium_area_in_um2"))
  
  # Determine the vicinity to combine cilium points during the first search (find)
  if(is.null(vicinity_combine)){
    vicinity_combine <- ceiling(x = max_cilium_area / 2)
  }
  
  # Determine the vicinity to connect cilium points during connecting phase
  # (a newly found point may not be further away than this many points
  # from an existing cilium)
  if(is.null(vicinity_connect)){
    vicinity_connect <- ceiling(x = min_cilium_area / 2)
  }
  
  # Determine the nuclei mask area
  # (We assume a nucleus area of 15um*15um and require the moving rectangle
  # to be 3 times as larges)
  nuc_length <- 15
  
  if(is.null(nuc_mask_width_heigth)){
    nuc_mask_width_heigth <- 3*ceiling(nuc_length/pixel_size)
  }
  
  # Determine thresholds for finding cilia if not to be determined depending
  # on the pixel intensities
  if(!threshold_by_density_of_cilium_pixels){
    if(is.null(threshold_find)){
      threshold_find <- 0.01
    }
    if(is.null(threshold_connect)){
      threshold_connect <- 0.005
    }
  }
  
  
  # Calculate the projection of the z-stack layers -------------------------
  
  # Create empty stack image
  image_stack <- array(0, dim = dim(image_data)[1:3])
  #Image_stack <- EBImage::Image(data = array(0, dim = dim(Image_data)[1:3]),
  #                              colormode = "Color")
  
  if(projection_method == "max"){
    
    # Stack with max values
    for(i in 1:dim(image_data)[3]){
      image_stack[,,i] = apply(image_data[,,i,], c(1,2), max)
    }
    
  }else if(projection_method == "mean"){
    
    # Stack with mean values
    for(i in 1:dim(image_data)[3]){
      image_stack[,,i] = apply(image_data[,,i,], c(1,2), mean)
    }
    
  }else{
    print("Please choose either max or mean as zstack projection method.")
  }
  
  rm(i)
  
  Image_stack <- EBImage::Image(data = image_stack, colormode = "Color")
  
  # Enhance contrast of stack image
  Image_stack_histogram_equalization <- EBImage::clahe(x = Image_stack)
  
  # ---------------------------------------------------------------------- #
  # ------------------------ 2. Nuclei detection ------------------------- #
  # ---------------------------------------------------------------------- #
  
  # Find and count all nuclei -----------------------------------------------
  
  # Save only color layer of nuclei
  Image_nuclei <- getLayer(image = Image_stack, layer = nucleus_color)
  
  # Smooth the image
  Image_nuclei <-  EBImage::medianFilter(x = Image_nuclei, size = 5)
  
  # Make the image brighter for detection
  Image_nuclei <- 10*Image_nuclei
  
  #display(Image_nuclei)
  #display(Image_nuclei, method = "raster", all = TRUE)
  
  # Calculate the nucleus mask
  nmask <- EBImage::thresh(x = Image_nuclei,
                           w = nuc_mask_width_heigth,
                           h = nuc_mask_width_heigth,
                           offset = 0.05)
  
  # Morphological opening to remove objects smaller than the structuring element
  # (disc of size 13)
  nmask <- EBImage::opening(nmask, makeBrush(13, shape='disc'))
  # Fill holes
  nmask <- EBImage::fillHull(nmask)
  # Label each connected set of pixels with a distinct ID
  nmask <- EBImage::bwlabel(nmask)
  
  #display(nmask)
  
  # Record all nuclei that are at the borders of the image
  left  <- table(nmask[1, 1:dim(nmask)[2]])
  top   <- table(nmask[1:dim(nmask)[1],1])
  right <- table(nmask[dim(nmask)[1], 1:dim(nmask)[2]])
  bottom <- table(nmask[1:dim(nmask)[1],dim(nmask)[2]])
  
  left <- as.integer(names(left))
  top <- as.integer(names(top))
  right <- as.integer(names(right))
  bottom <- as.integer(names(bottom))
  
  nuclei_at_borders <- unique(c(left, top, right, bottom))
  # delete 0s
  nuclei_at_borders <- nuclei_at_borders[nuclei_at_borders != 0]
  
  # Delete all nuclei at border
  if(length(nuclei_at_borders) > 0){
    for(i in 1:length(nuclei_at_borders)){
      imageData(nmask)[imageData(nmask) == nuclei_at_borders[i]] <- 0
    }
    rm(i)
  }
  
  #display(nmask)
  
  # Delete all remaining nuclei that are smaller than 10% of the median size
  # object sizes
  # barplot(table(nmask)[-1])
  
  nmask <-  EBImage::watershed( distmap(nmask), 1 )
  
  table_nmask <- table(nmask)
  nuc_min_area <- 0.1*median(table_nmask[-1])
  
  # remove objects that are smaller than min_nuc_size
  to_be_removed <- as.integer(names(which(table_nmask < nuc_min_area)))
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
  #display(nmask_watershed)
  
  # Count number of cells
  nucNo <- max(nmask_watershed)
  
  # ---------------------------------------------------------------------- #
  # ------------------------- 3. Cilia detection ------------------------- #
  # ---------------------------------------------------------------------- #
  
  # Calculate the cilia detection thresholds -------------------------------
  
  # Calculate "threshold_find" and "threshold_connect" if
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
    
    ratio_of_cilia_pixels <- nucNo * ((min_cilium_area + max_cilium_area)/2) /
      (dim(Image_stack)[1]*dim(Image_stack)[2])
    
    print(paste("Intensity quantile to find cilia: ",
                (1-ratio_of_cilia_pixels), sep=""))
    
    # threshold_find
    threshold_find_histeq <- quantile(Image_cilia_layer_histeq,
                                      (1-ratio_of_cilia_pixels), na.rm = TRUE)
    threshold_find_histeq <- as.numeric(threshold_find_histeq)
    
    threshold_find <- threshold_find_histeq
    
    # threshold_connect
    threshold_connect <- quantile(Image_cilia_layer,
                                  (1-ratio_of_cilia_pixels), na.rm = TRUE)
    threshold_connect <- as.numeric(threshold_connect)
    
    
    print(paste("The new threshold values are: threshold_find = ",
                threshold_find, " and threshold_connect = ",
                threshold_connect,
                ".",
                sep=""))
  }
  
  # Save a mask of possible cilia ------------------------------------------
  
  # Save only color layer of cilia
  Image_cilia_histeq <- editImage(image = Image_stack_histogram_equalization,
                                  object_color = cilium_color,
                                  threshold = threshold_find)
  
  #display(Image_cilia_histeq)
  
  list_of_cilium_points <- which(Image_cilia_histeq > 0, arr.ind = T)
  
  
  # Exit function because no cilium could be found
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
      "Original_function_call" = function_call)
    
    # All parameter values
    parameter_names <- c("input_dir_tif", "input_file_czi", "cilium_color",
                         "nucleus_color", "projection_method",
                         "threshold_by_density_of_cilium_pixels",
                         "threshold_find", "threshold_connect",
                         "vicinity_combine", "vicinity_connect", "min_cilium_area", "max_cilium_area",
                         "number_size_factor", "pixel_size", "slice_distance",
                         "nuc_mask_width_heigth")
    
    df_FinalParameterList <- setNames(data.frame(
      matrix(ncol = length(parameter_names), nrow = 1)),
      parameter_names)
    
    # Go through every parameterName and save current value
    for(i in 1:length(parameter_names)){
      
      par_value <- ifelse(test = is.null(get(parameter_names[i])),
                          yes = NA,
                          no = get(parameter_names[i]) )
      
      df_FinalParameterList[[parameter_names[i]]] <- par_value
      
    }
    
    rm(i)
    
    # Combine both data frames
    df_parameterList <- cbind(df_OriginalParameterList, df_FinalParameterList)
    
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
    
    df_cilium_summary <- data.frame("cilium" = NA,
                                    "vertical_length_in_um" = NA,
                                    "vertical_length_in_layers" = NA,
                                    "horizontal_length_in_um" = NA,
                                    "horizontal_length_in_pixels" = NA,
                                    "total_length_in_um" = NA)
    
    if(!is.null(df_cilium_summary)){
      write.csv(df_cilium_summary,
                file = paste(output_dir, "cilium_summary.csv", sep=""), row.names = FALSE)
      
      write.csv2(df_cilium_summary,
                 file = paste(output_dir, "cilium_summary_de.csv", sep=""), row.names = FALSE)
    }
    
    return(NULL)
  }
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  
  # Clean cilium pixels ----------------------------------------------------
  
  df_cilium_points <- data.frame(list_of_cilium_points)
  
  # Rename columns (x: from left to right, y: from top to bottom)
  names(df_cilium_points) <- c("pos_x", "pos_y")
  
  df_cilium_points <- dplyr::arrange(df_cilium_points, pos_y, pos_x) #NEW
  rm(list_of_cilium_points)
  
  # Delete all found pixels that have no other found pixels in the
  # neighborhood (+-1)
  df_cilium_points$possibleCilium <- FALSE
  
  for(i in 1:length(df_cilium_points$pos_x)){
    
    .pos_x_distance <- df_cilium_points$pos_x[i] -
      df_cilium_points$pos_x
    
    .pos_y_distance <- df_cilium_points$pos_y[i] -
      df_cilium_points$pos_y
    
    .pos_x_distance[abs(.pos_x_distance) <= 1] <- 0
    .pos_y_distance[abs(.pos_y_distance) <= 1] <- 0
    
    .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
    if(sum(.distance == 0) > 1){
      df_cilium_points$possibleCilium[.distance == 0] <- TRUE
    }
  }
  rm(i)
  
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  
  # Determine the cilium number for each cilium found (Combine all cilium
  # pixels that are no less than vicinity_combine apart)
  
  df_cilium_points$ciliumNumber <- 0
  df_cilium_points$ciliumNumber[1] <- 1
  
  
  # Start with the second entry in the data frame
  i <- 2
  while(!is.na(which(df_cilium_points$ciliumNumber==0)[1])){
    
    # Calculate Distance of current pixel to all other pixel that might
    # be a Cilium
    .pos_x_distance <- df_cilium_points$pos_x[i] -
      df_cilium_points$pos_x
    
    .pos_y_distance <- df_cilium_points$pos_y[i] -
      df_cilium_points$pos_y
    
    .pos_x_distance[abs(.pos_x_distance) <= vicinity_combine] <- 0
    .pos_y_distance[abs(.pos_y_distance) <= vicinity_combine] <- 0
    
    .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
    
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
  
  # Delete all pixels that are not bright enough (less than 0.75*median)
  for(i in unique(df_cilium_points$ciliumNumber)){
    
    indices <- cbind(df_cilium_points$pos_x[df_cilium_points$ciliumNumber == i],
                     df_cilium_points$pos_y[df_cilium_points$ciliumNumber == i])
    
    cilium_intensities <- Image_cilia_layer[indices]
    
    lower_limit <- 0.75 * median(cilium_intensities, na.rm = TRUE)
    keep_pixels <- cilium_intensities > lower_limit 
    
    df_cilium_points$possibleCilium[df_cilium_points$ciliumNumber == i] <- keep_pixels
  }
  
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  # Mark all structures that are too small and therefore may not be a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) < min_cilium_area){
      df_cilium_points$possibleCilium[df_cilium_points$ciliumNumber == i] <- FALSE
      # df_cilium_points <- df_cilium_points[
      #   !(df_cilium_points$ciliumNumber == i),]
    }
  }
  rm(i)
  
  # Mark all structures that are too large and therefore may not be a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) > (1.5 * max_cilium_area)){
      df_cilium_points$possibleCilium[df_cilium_points$ciliumNumber == i] <- FALSE
    }
  }
  rm(i)
  
  # Delete all structures that are too small or too big
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  
  # Delete all cilia that touch a border of the image
  # (including one pixel in between)
  
  # Coordinate system:
  # x-axis: bottom left to bottom right
  # y-axis: bottom left to top left
  
  # row = pos_x: left to right (= from x=0 to x_max)
  # col = pos_y: top to bottom (= from y_max to y=0)
  
  # Record all cilia that are at the borders of the image
  dim_image_x <- dim(image_data)[2]
  dim_image_y <- dim(image_data)[1]
  
  cilia_at_left_border    <- unique(df_cilium_points$ciliumNumber[df_cilium_points$pos_x==1 | df_cilium_points$pos_x==2])
  cilia_at_top_border     <- unique(df_cilium_points$ciliumNumber[df_cilium_points$pos_y==1 | df_cilium_points$pos_y==2])
  cilia_at_right_border   <- unique(df_cilium_points$ciliumNumber[df_cilium_points$pos_x==dim_image_x | df_cilium_points$pos_x==(dim_image_x-1)])
  cilia_at_bottom_border  <- unique(df_cilium_points$ciliumNumber[df_cilium_points$pos_y==dim_image_y | df_cilium_points$pos_y==(dim_image_y-1)])
  
  if(length(cilia_at_left_border) > 0){
    df_cilium_points <- df_cilium_points[!(df_cilium_points$ciliumNumber %in% cilia_at_left_border),]
  }
  if(length(cilia_at_top_border) > 0){
    df_cilium_points <- df_cilium_points[!(df_cilium_points$ciliumNumber %in% cilia_at_top_border),]
  }
  if(length(cilia_at_right_border) > 0){
    df_cilium_points <- df_cilium_points[!(df_cilium_points$ciliumNumber %in% cilia_at_right_border),]
  }
  if(length(cilia_at_bottom_border) > 0){
    df_cilium_points <- df_cilium_points[!(df_cilium_points$ciliumNumber %in% cilia_at_bottom_border),]
  }
  
  rm(list = c("cilia_at_left_border", "cilia_at_top_border",
              "cilia_at_right_border", "cilia_at_bottom_border"))
  
  # Delete separated cilium parts that are not connected to the brightest
  # cilium part.
  
  df_cilium_points$disconnectedPart <- FALSE
  
  indices <- cbind(df_cilium_points$pos_x, df_cilium_points$pos_y)
  df_cilium_points$fluorescence_intensity <- Image_cilia_layer_histeq[indices]
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    
    df_dummy <- df_cilium_points[df_cilium_points$ciliumNumber == i,]
    
    #Find cluster number
    df_dummy$ClusterNumber <- 0
    df_dummy$ClusterNumber[1] <- 1
    
    
    # Start with the second entry in the data frame
    j <- 2
    while(!is.na(which(df_dummy$ClusterNumber==0)[1])){
      
      # Calculate Distance of current pixel to all other pixel that might
      # be a cluster of that cilium
      .pos_x_distance <- df_dummy$pos_x[j] -
        df_dummy$pos_x
      
      .pos_y_distance <- df_dummy$pos_y[j] -
        df_dummy$pos_y
      
      .pos_x_distance[abs(.pos_x_distance) <= vicinity_connect] <- 0
      .pos_y_distance[abs(.pos_y_distance) <= vicinity_connect] <- 0
      
      .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
      
      # Get the cilium number (close cilium that has been detected)
      ClusterNumber_dummy <-
        unique(df_dummy$ClusterNumber[.distance == 0])
      
      if(length(ClusterNumber_dummy) == 1 && ClusterNumber_dummy == 0){
        # Advance Cilium number because there is no Cilium close by
        ClusterNumber <- max(df_dummy$ClusterNumber) + 1
      }else{
        # Points belong to already existing cilium
        ClusterNumber <- ClusterNumber_dummy[!(ClusterNumber_dummy == 0)]
      }
      
      if(length(ClusterNumber) > 1){
        print("The following clusters are now one:")
        print(ClusterNumber)
        print("of the cilium number:")
        print(i)
        for(j in 2:length(ClusterNumber)){
          df_dummy$ClusterNumber[
            df_dummy$ClusterNumber == ClusterNumber[j]] <-
            ClusterNumber[1]
        }
        
      }else{
        df_dummy$ClusterNumber[.distance == 0] <- ClusterNumber
      }
      
      # Advance i to the next row which contains 0 as the cilium number
      j <- which(df_dummy$ClusterNumber==0)[1]
    }
    rm(j)
    
    df_test <- df_dummy %>% 
      group_by(ClusterNumber) %>% 
      summarise(meanIntensity = mean(fluorescence_intensity))
    
    df_dummy$disconnectedPart[
      df_dummy$ClusterNumber != df_test$ClusterNumber[
        which(df_test$meanIntensity == max(df_test$meanIntensity))]] <- TRUE
    
    df_dummy <- df_dummy[!df_dummy$disconnectedPart,]
    df_dummy <- df_dummy[,!names(df_dummy)=="ClusterNumber"]
    
    # Keep only rows that have been found
    df_cilium_points <- rbind(df_cilium_points, df_dummy)
    
  }
  rm(i)
  rm(df_test)
  rm(df_dummy)
  
  # Delete duplicated lines
  df_cilium_points <- df_cilium_points[duplicated(df_cilium_points),]
  
  # Sort cilia
  df_cilium_points <- df_cilium_points %>% 
    arrange(ciliumNumber)
  
  # Drop disconnectedPart and column
  df_cilium_points <-
    df_cilium_points[,!names(df_cilium_points)=="disconnectedPart"]
  df_cilium_points <-
    df_cilium_points[,!names(df_cilium_points)=="fluorescence_intensity"]
  
  
  # Exit function because no cilium could be found
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
      "Original_function_call" = function_call)
    
    # All parameter values
    parameter_names <- c("input_dir_tif", "input_file_czi", "cilium_color",
                         "nucleus_color", "projection_method",
                         "threshold_by_density_of_cilium_pixels",
                         "threshold_find", "threshold_connect",
                         "vicinity_combine", "vicinity_connect", "min_cilium_area", "max_cilium_area",
                         "number_size_factor", "pixel_size", "slice_distance",
                         "nuc_mask_width_heigth")
    
    df_FinalParameterList <- setNames(data.frame(
      matrix(ncol = length(parameter_names), nrow = 1)),
      parameter_names)
    
    # Go through every parameterName and save current value
    for(i in 1:length(parameter_names)){
      
      par_value <- ifelse(test = is.null(get(parameter_names[i])),
                          yes = NA,
                          no = get(parameter_names[i]) )
      
      df_FinalParameterList[[parameter_names[i]]] <- par_value
      
    }
    
    rm(i)
    
    # Combine both data frames
    df_parameterList <- cbind(df_OriginalParameterList, df_FinalParameterList)
    
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
    
    df_cilium_summary <- data.frame("cilium" = NA,
                                    "vertical_length_in_um" = NA,
                                    "vertical_length_in_layers" = NA,
                                    "horizontal_length_in_um" = NA,
                                    "horizontal_length_in_pixels" = NA,
                                    "total_length_in_um" = NA)
    
    if(!is.null(df_cilium_summary)){
      write.csv(df_cilium_summary,
                file = paste(output_dir, "cilium_summary.csv", sep=""), row.names = FALSE)
      
      write.csv2(df_cilium_summary,
                 file = paste(output_dir, "cilium_summary_de.csv", sep=""), row.names = FALSE)
    }
    
    return(NULL)
  }
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  # Renumber the cilia
  .number <- 1
  df_cilium_points$ciliumNumber_NEW <- NA
  for(i in unique(df_cilium_points$ciliumNumber)){
    df_cilium_points$ciliumNumber_NEW[df_cilium_points$ciliumNumber == i] <-
      .number
    .number <- .number + 1
  }
  rm(i)
  rm(.number)
  
  df_cilium_points <- dplyr::select(df_cilium_points, -ciliumNumber)
  names(df_cilium_points)[names(df_cilium_points) == "ciliumNumber_NEW"] <- "ciliumNumber"
  
  # Drop possibleCilium column
  df_cilium_points <-
    df_cilium_points[,!names(df_cilium_points)=="possibleCilium"]
  
  
  # Save image with marked cilia
  Image_stack_cilia <- Image_stack
  for(k in 1:length(df_cilium_points$pos_x)){
    Image_stack_cilia[df_cilium_points$pos_x[k], df_cilium_points$pos_y[k], 1] <- 1
    Image_stack_cilia[df_cilium_points$pos_x[k], df_cilium_points$pos_y[k], 2] <- 1
  }
  
  EBImage::writeImage(x = Image_stack_cilia,
                      files = paste(output_dir,
                                    input_file_name,
                                    "_stack_cilia_unconnected.tif",
                                    sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  # Find in every z-stack layer the cilium pixels --------------------------
  # belonging to a specific cilium by going through every image layer
  # (i = 1 ... n) and finding cilium pixels that are connected to it
  
  print("Connecting all images.")
  
  # Add layer information to data frame
  df_cilium_points$layer <- -1
  
  # Save information in big data frame, which contains all layers
  # -1 is the sum of all layers
  df_cilium_information <- df_cilium_points
  
  
  # Add median fluorescence intensity per cilium
  df_cilium_median_intensity <- data.frame(ciliumNumber = unique(df_cilium_information$ciliumNumber),
                                           median_stack_intensity = NA)
  
  for(i in unique(df_cilium_information$ciliumNumber)){
    
    indices <- cbind(df_cilium_points$pos_x[df_cilium_points$ciliumNumber == i],
                     df_cilium_points$pos_y[df_cilium_points$ciliumNumber == i])
    
    cilium_intensities <- Image_cilia_layer[indices]
    
    df_cilium_median_intensity$median_stack_intensity[df_cilium_median_intensity$ciliumNumber == i] <- median(cilium_intensities, na.rm = TRUE)
  }
  rm(i)
  
  # Go through all layers (i = 1 .. n (layers) )
  print("Finding cilia in every layer.")
  for(i in 1:dim(image_data)[4]){
    
    print(paste("Dealing with layer ", i, ". (It is now ",
                Sys.time(), ".)", sep=""))
    
    
    # Save Image for every layer 
    Image <-  image_data[,,,i]
    
    Image_cilia_layer <- getLayer(image = Image, layer = cilium_color)
    
    # Image of each layer with cilium channel
    Image_cilia_connect <- editImage(image = Image,
                                     object_color = cilium_color,
                                     threshold = threshold_connect)
    # display(Image_cilia_connect)
    
    # Get positions of the cilia
    list_of_cilium_points_connect <- which(Image_cilia_connect==1,
                                           arr.ind = T)
    
    df_cilium_points_connect <- data.frame(list_of_cilium_points_connect)
    
    # Rename columns (x: from left to right, y: from top to bottom)
    names(df_cilium_points_connect) <- c("pos_x", "pos_y")
    
    rm(list_of_cilium_points_connect)
    
    # Cleaning of possible cilia pixels in every layer
    if(nrow(df_cilium_points_connect) > 0){
      df_cilium_points_connect$possibleCilium <- FALSE
      
      
      # Delete all pixels that have no connection to another pixel
      # Delete all found pixels that have no other found pixels in the
      # neighborhood (+-1)
      for(j in 1:length(df_cilium_points_connect$pos_x)){
        
        .pos_x_distance <- df_cilium_points_connect$pos_x[j] -
          df_cilium_points_connect$pos_x
        
        .pos_y_distance <- df_cilium_points_connect$pos_y[j] -
          df_cilium_points_connect$pos_y
        
        .pos_x_distance[abs(.pos_x_distance) <= 1] <- 0
        .pos_y_distance[abs(.pos_y_distance) <= 1] <- 0
        
        .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
        if(sum(.distance == 0) > 1){
          df_cilium_points_connect$possibleCilium[.distance == 0] <- TRUE
        }
      }
      rm(j)
      
      df_cilium_points_connect <- df_cilium_points_connect[
        df_cilium_points_connect$possibleCilium,]
      
      # Go through the list of detected cilia and find connections in list
      # with lower threshold
      if(nrow(df_cilium_points_connect) > 0){
        df_cilium_points_connect$ciliumNumber <- 0
        
        for(j in 1:nrow(df_cilium_points_connect)){
          
          .pos_x_distance <- df_cilium_points_connect$pos_x[j] -
            df_cilium_points$pos_x
          
          .pos_y_distance <- df_cilium_points_connect$pos_y[j] -
            df_cilium_points$pos_y
          
          .pos_x_distance[abs(.pos_x_distance) <= vicinity_connect] <- 0
          .pos_y_distance[abs(.pos_y_distance) <= vicinity_connect] <- 0
          
          .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
          
          # if zero is in distance, then the point of
          # df_cilium_points_connect[j] is in the vicinity of a "real"
          # cilium point
          
          if(0 %in% .distance){
            df_cilium_points_connect$possibleCilium[j] <- TRUE
            
            # Get the cilium number from the df_cilium_points (sum of
            # all images)
            df_cilium_points_connect$ciliumNumber[j] <-
              df_cilium_points$ciliumNumber[which(.distance == 0)[1]]
          }
        }
        
        # Delete rows that cannot be connected to an existing cilium
        df_cilium_points_connect <- df_cilium_points_connect[
          !df_cilium_points_connect$ciliumNumber == 0,]
        
        if(nrow(df_cilium_points_connect) > 0){
          
          row.names(df_cilium_points_connect) <- NULL
          
          # Delete all pixels that are not bright enough
          # (less than median intensity (of stack))
          df_cilium_points_connect$possibleCilium <- FALSE
          for(k in unique(df_cilium_points_connect$ciliumNumber)){
            
            indices <- cbind(df_cilium_points_connect$pos_x[
              df_cilium_points_connect$ciliumNumber == k],
              df_cilium_points_connect$pos_y[
                df_cilium_points_connect$ciliumNumber == k])
            
            cilium_intensities <- Image_cilia_layer[indices]
            
            lower_limit <- df_cilium_median_intensity$median_stack_intensity[
              df_cilium_median_intensity$ciliumNumber == k]
            keep_pixels <- cilium_intensities > lower_limit 
            
            df_cilium_points_connect$possibleCilium[
              df_cilium_points_connect$ciliumNumber == k] <- keep_pixels
          }
          rm(k)
          df_cilium_points_connect <- df_cilium_points_connect[
            df_cilium_points_connect$possibleCilium,]
          
          for(k in unique(df_cilium_points_connect$ciliumNumber)){
            if(sum(df_cilium_points_connect$ciliumNumber == k) < min_cilium_area){
              df_cilium_points_connect$possibleCilium[
                df_cilium_points_connect$ciliumNumber == k] <- FALSE
            }
          }
          rm(k)
          df_cilium_points_connect <- df_cilium_points_connect[
            df_cilium_points_connect$possibleCilium,]
          
          
          if(nrow(df_cilium_points_connect) > 0){
            # Drop information that it is cilia
            df_cilium_points_connect <- df_cilium_points_connect[-3]
            
            df_cilium_points_connect$layer <- i
            
            # Save the layer and append it to big data frame
            
            # Save the positions of the cilia
            df_cilium_information <- rbind(df_cilium_information,
                                           df_cilium_points_connect)
            
            # Save image with marked cilia
            for(k in 1:length(df_cilium_points_connect$pos_x)){
              Image[df_cilium_points_connect$pos_x[k],
                    df_cilium_points_connect$pos_y[k], 1] <- 1
              Image[df_cilium_points_connect$pos_x[k],
                    df_cilium_points_connect$pos_y[k], 2] <- 1
            }
            
            Image <- EBImage::Image(data = Image, colormode = "color")
            EBImage::writeImage(x = Image,
                                files = paste(output_dir, input_file_name,
                                              "_cilia_layer_", i, ".tif", sep = ""),
                                bits.per.sample = 8,
                                type = "tiff")
          }
        }
      }
    }
  }
  
  rm(i)
  rm(Image)
  rm(Image_cilia_layer)
  
  
  
  # Save a data frame with all cilia information ---------------------------
  
  # Save all locations of all cilia (independent from the z layer where it
  # was found being found)
  df_cilium_all <- df_cilium_information
  df_cilium_all$xy <- paste(df_cilium_all$pos_x,
                            df_cilium_all$pos_y,
                            sep = ",")
  # Keep only those coordinates that are not duplicated
  df_cilium_all <- df_cilium_all[
    !duplicated(df_cilium_all$xy),]
  
  df_cilium_all <- df_cilium_all[,-c(5)]
  df_cilium_all$layer <- -99
  
  df_cilium_all <- dplyr::arrange(df_cilium_all, ciliumNumber, pos_y, pos_x)
  
  row.names(df_cilium_all) <- NULL
  
  # Add information of all cilium coordinates as layer -99 to data frame
  df_cilium_information <- rbind(df_cilium_information, df_cilium_all)
  
  # Save the projection image with cilium information ----------------------
  
  # Mark all cilia coordinates in a new stack image
  Image_stack_cilia_connected <- Image_stack
  
  
  for(k in 1:length(df_cilium_all$pos_x)){
    Image_stack_cilia_connected[df_cilium_all$pos_x[k], df_cilium_all$pos_y[k], 1] <- 1
    Image_stack_cilia_connected[df_cilium_all$pos_x[k], df_cilium_all$pos_y[k], 2] <- 1
  }
  rm(k)
  
  EBImage::writeImage(x = Image_stack_cilia_connected,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_connected.tif", sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  
  # Add cilium numbers to image --------------------------------------------
  Image_stack_numbers <- Image_stack_cilia_connected
  image_stack_numbers <- as.array(Image_stack_numbers)
  
  for(i in unique(df_cilium_all$ciliumNumber) ){
    ciliumNumber <- i
    pos_x <- df_cilium_all$pos_x[df_cilium_all$ciliumNumber == i][
      length(df_cilium_all$pos_x[df_cilium_all$ciliumNumber == i])]
    pos_y <- df_cilium_all$pos_y[df_cilium_all$ciliumNumber == i][
      length(df_cilium_all$pos_y[df_cilium_all$ciliumNumber == i])]
    
    image_stack_numbers <- addNumberToImage(image = image_stack_numbers,
                                            number = ciliumNumber,
                                            pos_x = pos_x,
                                            pos_y = pos_y,
                                            number_size_factor = number_size_factor,
                                            number_color = "red")
  }
  rm(i)
  
  Image_stack_numbers <- EBImage::Image(data = image_stack_numbers,
                                        colormode = "color")
  EBImage::writeImage(x = Image_stack_numbers,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_all_numbers.tif", sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  
  # Add nuclei and cilium numbers to image ---------------------------------
  
  # Include numbers of nuclei
  table_nmask_watershed <- table(nmask_watershed)
  df_nuclei_positions <-  as.data.frame(table_nmask_watershed)
  names(df_nuclei_positions) <- c("nuc_number", "frequency")
  df_nuclei_positions$nuc_number <- as.integer(levels(df_nuclei_positions$nuc_number))
  
  if( (dim(df_nuclei_positions)[1]-1) > 0){
    
    # remove 0
    df_nuclei_positions <- df_nuclei_positions[!df_nuclei_positions$nuc_number == 0,]
    nuc_numbers <- df_nuclei_positions$nuc_number
    
    # Find approximate midpoint of every nucleus
    df_nuclei_positions$pos_x <- NA
    df_nuclei_positions$pos_y <- NA
    for(i in 1:length(nuc_numbers)){
      dummy_coordinates <- which(
        imageData(nmask_watershed) == nuc_numbers[i], arr.ind = TRUE)
      
      df_nuclei_positions$pos_x[i] <- round(mean(dummy_coordinates[,1]))
      df_nuclei_positions$pos_y[i] <- round(mean(dummy_coordinates[,2]))
      
    }
    
    # Rearrange nuclei (from left to right and top to bottom)
    df_nuclei_positions <- df_nuclei_positions %>% 
      dplyr::arrange(pos_y, pos_x)
    
    df_nuclei_positions$nuc_number_new <- NA
    df_nuclei_positions$nuc_number_new <- nuc_numbers
    
    # Add nuclei numbers to image
    for(i in 1:length(nuc_numbers)){
      
      image_stack_numbers <- addNumberToImage(
        image = image_stack_numbers,
        number = i,
        pos_x = df_nuclei_positions$pos_x[df_nuclei_positions$nuc_number_new == i],
        pos_y = df_nuclei_positions$pos_y[df_nuclei_positions$nuc_number_new == i],
        number_size_factor = number_size_factor,
        number_color = "green")
      
      image_stack_numbers <- addNumberToImage(
        image = image_stack_numbers,
        number = i,
        pos_x = df_nuclei_positions$pos_x[df_nuclei_positions$nuc_number_new == i],
        pos_y = df_nuclei_positions$pos_y[df_nuclei_positions$nuc_number_new == i],
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
  
  EBImage::writeImage(x = Image_stack_numbers,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_all_numbers_nuclei.tif",
                                    sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  # Normalize image --------------------------------------------------------
  
  Image_stack_histogram_equalization_normalized <- EBImage::normalize(
    Image_stack_histogram_equalization)
  
  EBImage::writeImage(x = Image_stack_histogram_equalization_normalized,
                      files = paste(output_dir, input_file_name,
                                    "_stack_cilia_all_histogram_equalized_normalized.tif",
                                    sep = ""),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  # Save function call and used parameter values ---------------------------

  # Save all parameters in a csv
  
  # Original parameter
  function_call <- paste(deparse(match.call()), collapse = "")
  function_call <- gsub(pattern = " +", replacement = " ", x = function_call)
  df_OriginalParameterList <- data.frame(
    "Original_function_call" = function_call)

  # All parameter values
  parameter_names <- c("input_dir_tif", "input_file_czi", "cilium_color",
                       "nucleus_color", "projection_method",
                       "threshold_by_density_of_cilium_pixels",
                       "threshold_find", "threshold_connect",
                       "vicinity_combine", "vicinity_connect",
                       "min_cilium_area", "max_cilium_area",
                       "number_size_factor", "pixel_size", "slice_distance",
                       "nuc_mask_width_heigth")
  
  
  df_FinalParameterList <- setNames(data.frame(
    matrix(ncol = length(parameter_names), nrow = 1)),
    parameter_names)
  
  # Go through every parameterName and save current value
  for(i in 1:length(parameter_names)){
    
    par_value <- ifelse(test = is.null(get(parameter_names[i])),
                        yes = NA,
                        no = get(parameter_names[i]) )
    
    df_FinalParameterList[[parameter_names[i]]] <- par_value
    
  }
  
  rm(i)
  
  # Combine both data frames
  df_parameterList <- cbind(df_OriginalParameterList, df_FinalParameterList)
  
  if(!is.null(df_parameterList)){
    write.csv(df_parameterList,
              file = paste(output_dir, "parameter_list.csv", sep=""),
              row.names = FALSE)
    
    write.csv2(df_parameterList,
               file = paste(output_dir, "parameter_list_de.csv", sep=""),
               row.names = FALSE)
  }
  
  # Save the number of nuclei ----------------------------------------------
  df_number_nuclei <- data.frame("numberOfNuclei" = nucNo)
  if(!is.null(df_number_nuclei)){
    write.csv(df_number_nuclei,
              file = paste(output_dir, "nuclei_number.csv", sep=""),
              row.names = FALSE)
    write.csv2(df_number_nuclei,
               file = paste(output_dir, "nuclei_number_de.csv", sep=""),
               row.names = FALSE)
  }
  
  
  
  # ---------------------------------------------------------------------- #
  # ---------------------- 4. Cilia measurement -------------------------- #
  # ---------------------------------------------------------------------- #
  
  # Save information of cilia ----------------------------------------------
  
  # Get the length of the cilia
  df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                                 min_cilium_area,
                                                 pixel_size,
                                                 slice_distance)
  
  if(!is.null(df_cilium_summary)){
    write.csv(df_cilium_summary,
              file = paste(output_dir, "cilium_summary.csv", sep=""),
              row.names = FALSE)
    
    write.csv2(df_cilium_summary,
               file = paste(output_dir, "cilium_summary_de.csv", sep=""),
               row.names = FALSE)
  }
  
  
  # Combine all data.frame to a list
  output_list <- list("df_parameterlist" = df_parameterList,
                      "df_cilium_information" = df_cilium_information,
                      "df_cilium_summary" = df_cilium_summary)
  
  return(output_list)
}
