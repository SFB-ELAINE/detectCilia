#' Detect cilia in images
#' 
#' `detectCilia()` is the main function to detect cilia or similar small
#' labelled objects in a CZI (ideally) or TIFF image. At first, the cilia
#' will be located by using a projection method. Afterward, the length of
#' a cilium is determined in the horizontal plane. The height in the
#' vertical plane is being determined by the number of z-stack layers
#' where a cilium is found. The entire length of the cilium is being
#' calculated using Pythagoras' theorem (\code{c=sqrt(a^2+b^2)}).
#' The output will be written in the current directory.
#' Please be aware that that the coordinate system is turned by 90° to the
#' right. The origin of the x- and y-axes is in the upper-left corner of the
#' image. (The x-axis points downwards and the y-axis to the right.)
#'
#' @param input_dir_tif Path to a directory containing z-stack slices in TIF
#' format as a string.
#' @param input_file_czi Path to a CZI file with the z-stack image as a string.
#' @param output_dir Path to a directory where the results are to be saved
#' as a string.
#' @param cilium_color Color of the cilium staining as a string or single
#' character ("r", "red", "g", "green", "b", or "blue").
#' @param nucleus_color Color of the nuclei staining as a string or single
#' character ("r", "red", "g", "green", "b", or "blue").
#' @param projection_method_for_threshold_calculation Method as a string for
#' calculating the z-stack projection which is used for calculating
#' threshold to find cilia positions (either "mean" or "max").
#' @param use_histogram_equalization_for_threshold_calculation A Boolean
#' stating whether to use a histogram equalization algorithm for determining
#' threshold values and finding cilia in projection.
#' @param threshold_by_density_of_cilium_pixels A Boolean.
#'   * `TRUE` (the default): Disregard the custom threshold values (if given)
#'   and, instead, use a function to calculate the binarization thresholds by
#'   looking at the density of cilium color pixels found in the entire image.
#'   * `FALSE`: Use custom threshold values.
#' @param threshold_find A number between 0 and 1 as the minimum
#' intensity of possible cilia pixels in the z-stack projection.
#' @param threshold_connect A number between 0 and 1 (and usually
#' lower than `threshold_find`) as the minimum intensity to find possible
#' cilia pixels in every z-stack layer. Only those pixels are kept which
#' are close to already found ciliary structures.
#' @param vicinity_combine Number of pixels (integer) defining the
#' neighborhood for combining found ciliary pixels in the z-stack
#' projection to one cilium.
#' @param vicinity_connect Number of pixels (integer) defining the
#' neighborhood for combining found ciliary pixels in every z-stack layer
#' to one cilium.
#' @param min_cilium_area_in_pixels Minimum number of pixels (integer) a cilium should
#' consist of.
#' @param max_cilium_area_in_pixels Maximum number of pixels (integer) a cilium should
#' consist of.
#' @param number_size_factor A number for resizing the number image
#' to be added.
#' @param pixel_size A number indicating the size (length and height) of one
#' pixel in micrometers
#' @param slice_distance A number showing the distance of two consecutive
#' z-stack layers in micrometers.
#' @param nuc_mask_width_height_in_pixels Number of pixels (integer) for nuclei detection.
#' @param export_normalized_images A Boolean.
#'   * `TRUE`: Also export the images with a normalized version of the original
#'   z-stack projection/z-stack layer.
#'   * `FALSE` (Default): Do not export normalized images.
#' @param number_of_expected_nuclei A number depicting the number of cells
#' (= max. number of cilia) to be found if no nuclei are present in image.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @returns A list with the following content: 
#' 1) tibble `df_parameterlist` with information about used parameters,
#' 2) tibble `df_cilium_information`with information about found cilia,
#' 3) tibble `df_cilium_summary` with lengths of found cilia.
#' @export detectCilia
#'
#' @examples
#' # Detect all cilia (red structures) (and nuclei (blue structures)) in
#' # a CZI image
#' detectCilia::detectCilia(input_file_czi = system.file("extdata",
#' "testImageCzi", "CiliaImage.czi", package = "detectCilia",
#' mustWork = TRUE))

detectCilia <- function(
  input_dir_tif = NULL,
  input_file_czi = NULL,
  output_dir = NULL,
  cilium_color = "red",
  nucleus_color = "blue",
  projection_method_for_threshold_calculation = "max",
  threshold_by_density_of_cilium_pixels = TRUE,
  use_histogram_equalization_for_threshold_calculation = FALSE,
  threshold_find = NULL,
  threshold_connect = NULL,
  vicinity_combine = NULL,
  vicinity_connect = NULL,
  min_cilium_area_in_pixels = NULL,
  max_cilium_area_in_pixels = NULL,
  number_size_factor = NULL,
  pixel_size = NULL,
  slice_distance = NULL,
  nuc_mask_width_height_in_pixels = NULL,
  export_normalized_images = FALSE,
  number_of_expected_nuclei = NULL) {
  
  
  # 0. Basics --------------------------------------------------------------
  .old.options <- options()
  on.exit(options(.old.options))
  
  options(stringsAsFactors = FALSE, warn=-1)
  
  if(!("EBImage" %in% utils::installed.packages())){
    print("Installing EBImage.")
    BiocManager::install("EBImage")
  }
  
  
  
  # ---------------------------------------------------------------------- #
  # ----------------------- 1. Image preparation ------------------------- #
  # ---------------------------------------------------------------------- #
  
  # 1. Image preparation ---------------------------------------------------
  # 1.1 Read image data ----------------------------------------------------
  
  # Input directory must be submitted. If not: close function call.
  if(is.null(input_dir_tif) && is.null(input_file_czi)){
    print(paste("Please call the function with an input directory ",
                "which contains z-stack TIF images with colored cilia or",
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
  
  # Save the file names of TIF images
  if(image_format == "tif"){
    file_names_tif <- list.files(path = input_dir_tif)
    file_names_tif <- file_names_tif[grepl("tif", file_names_tif)]
  }
  
  # Make a new sub-directory inside the input directory for data output
  if(image_format == "tif"){
    if(grepl("\\\\", input_dir_tif)){
      input_dir_tif <- gsub("\\$", "", input_dir_tif)
      input_file_name <- gsub("(.?)\\.tif", "\\1", file_names_tif[1])
      if(is.null(output_dir)){
        output_dir <- paste(input_dir_tif, "\\output\\", sep="") 
      }
    }else{
      input_dir_tif <- gsub("/$", "", input_dir_tif)
      input_file_name <- gsub("(.?)[[:digit:]]+\\.tif", "\\1", file_names_tif[1])
      if(is.null(output_dir)){
        output_dir <- paste(input_dir_tif, "/output/", sep="")
      }
    }
  }else if(image_format == "czi"){
    if(grepl("\\\\", input_file_czi)){
      input_dir <- gsub("(.+)\\.+\\.czi", "\\1", input_file_czi)
      input_file_name <- gsub(".+\\(.+)\\.czi", "\\1", input_file_czi)
      if(is.null(output_dir)){
        output_dir <- paste(input_dir, "\\", input_file_name, "_output\\", sep="")
      }
    }else{
      input_dir <- gsub("(.+)/.+\\.czi", "\\1", input_file_czi)
      input_file_name <- gsub(".+/(.+)\\.czi", "\\1", input_file_czi)
      if(is.null(output_dir)){
        output_dir <- paste(input_dir, "/", input_file_name, "_output/", sep="")
      }
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
      
      image_path <- file.path(input_dir_tif, file_names_tif[i])
      
      image <- EBImage::readImage(files = image_path, type = "tiff")
      
      
      # Find the layer name (The file name should be of the form
      # "..z01..","..z02..", ... for files obtained from ZEN or of the
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
      
      # if(layer_number != i){
      #   print("The numbering of the layers is inaccurate.")
      # }
      
      # Create empty z-stack image
      if(layer_number == 1){
        if(length(dim(image)) == 2){
          #Import grayscale image
          image_data <- array(0, dim = c(dim(image), length(file_names_tif)))
        }else if(length(dim(image)) == 3){
          image_data <- array(0, dim = c(dim(image),length(file_names_tif)))
        }else{
          print("Something is wrong with the input image dimension.")
        }
        
      }
      
      if(length(dim(image)) == 2){
        image_data[,,layer_number] <- image
      }else if(length(dim(image)) == 3){
        image_data[,,,layer_number] <- image
      }else{
        print("Something is wrong with the input image dimension.")
      }
      
    }
    rm(i)
    
    # Make it a colored image if it was grayscale
    if(length(dim(image_data)) == 3){
      
      cilium_color <- tolower(cilium_color)
      if(cilium_color == "red" || cilium_color == "r"){
        image_data <- EBImage::rgbImage(red = image_data)
      }else if(cilium_color == "green" || cilium_color == "g"){
        image_data <- EBImage::rgbImage(green = image_data)
      }else if(cilium_color == "blue" || cilium_color == "b"){
        image_data <- EBImage::rgbImage(blue = image_data)
      }else{
        print("Please call the function with a correct cilium color name.")
      }
      image_data <- as.array(image_data)
    }
    
  }else if(image_format == "czi"){
    
    # Read data
    image_data <- readCzi::readCzi(input_file = input_file_czi)
    
    
    # 1.2 Read metadata and get missing parameter values -------------------
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
  
  
  # 1.3 Calculate missing input parameter values ---------------------------
  
  
  # Determine min and max sizes of a primary cilium (in pixels)
  # (We assume it to be between 1um and 5um long and 0.25um and 1um wide.)
  
  max_cilium_area_in_um2 <- 5*1
  min_cilium_area_in_um2 <- 1*0.25
  
  if(is.null(min_cilium_area_in_pixels)){
    min_cilium_area_in_pixels <- floor(x = min_cilium_area_in_um2 / pixel_size / pixel_size)
  }
  
  if(is.null(max_cilium_area_in_pixels)){
    max_cilium_area_in_pixels <- ceiling(x = max_cilium_area_in_um2 / pixel_size / pixel_size)
  }
  
  rm(list = c("max_cilium_area_in_um2", "min_cilium_area_in_um2"))
  
  # Determine the vicinity to combine cilium points during the first search
  # (finding phase):
  # Use half of square root of large cilium area as max distance to combine
  # two cilium points
  if(is.null(vicinity_combine)){
    # Radius of a large cilium
    vicinity_combine <- ceiling(0.5 * sqrt(max_cilium_area_in_pixels))
  }
  
  # Determine the vicinity to connect cilium points during connecting phase
  # (a newly found point may not be further away than this many points
  # from an existing cilium):
  # Use half of square root of a small cilium area as max distance to combine
  # two cilium points
  if(is.null(vicinity_connect)){
    # vicinity_connect <- floor(0.5 + sqrt(min_cilium_area_in_pixels))
    vicinity_connect <- floor(0.5 * sqrt(min_cilium_area_in_pixels))
  }
  
  # Determine the nuclei mask area
  # (We assume a nucleus area of 15um*15um and require the moving rectangle
  # to be 3 times as larges)
  nuc_length <- 15
  
  if(is.null(nuc_mask_width_height_in_pixels)){
    nuc_mask_width_height_in_pixels <- 3*ceiling(nuc_length/pixel_size)
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
  
  
  # 1.4 Calculate the projection of the z-stack layers ---------------------
  
  # Create empty projection image
  image_projection_max <- array(0, dim = dim(image_data)[1:3])
  image_projection_mean <- array(0, dim = dim(image_data)[1:3])
  
  # Use max values for projection
  for(i in 1:dim(image_data)[3]){
    image_projection_max[,,i] = apply(image_data[,,i,], c(1,2), max)
  }
  
  # Use mean values for projection
  for(i in 1:dim(image_data)[3]){
    image_projection_mean[,,i] = apply(image_data[,,i,], c(1,2), mean)
  }
  
  rm(i)
  
  Image_projection_max <- EBImage::Image(data = image_projection_max,
                                         colormode = "Color")
  Image_projection_mean <- EBImage::Image(data = image_projection_mean,
                                          colormode = "Color")
  # if(dim(image_projection_max)[3] == 1){
  #   Image_projection_max <- EBImage::Image(data = image_projection_max,
  #                                          colormode = "Gray")
  #   Image_projection_mean <- EBImage::Image(data = image_projection_mean,
  #                                           colormode = "Gray")
  # }else{
  #   Image_projection_max <- EBImage::Image(data = image_projection_max,
  #                                          colormode = "Color")
  #   Image_projection_mean <- EBImage::Image(data = image_projection_mean,
  #                                           colormode = "Color")
  # }
  
  
  
  # ---------------------------------------------------------------------- #
  # ------------------------ 2. Nuclei detection ------------------------- #
  # ---------------------------------------------------------------------- #
  
  # 2. Nuclei detection ----------------------------------------------------
  #    Find and count all nuclei using the max projection
  if(is.null(nucleus_color)){
    nucNo <- number_of_expected_nuclei
    
  }else{
    # Save only color layer of nuclei
    Image_nuclei <- getLayer(image = Image_projection_max,
                             layer = nucleus_color)
    Image_nuclei <- EBImage::clahe(x = Image_nuclei, nx = 8)
    
    # Smooth the image
    filterSize <- floor(5*(0.22/pixel_size))
    Image_nuclei <-  EBImage::medianFilter(x = Image_nuclei, size = filterSize)
    
    # print(paste0("mean(Image_nuclei): ", mean(Image_nuclei)))
    # Make the image brighter for detection
    if(mean(Image_nuclei) < 0.05){
      Image_nuclei <- 10*Image_nuclei
    }else if(mean(Image_nuclei) < 0.1){
      Image_nuclei <- 5*Image_nuclei
    }else{
      Image_nuclei <- 2*Image_nuclei
    }
    
    #display(Image_nuclei)
    #display(Image_nuclei, method = "raster", all = TRUE)
    
    # Calculate the nucleus mask
    nmask <- EBImage::thresh(x = Image_nuclei,
                             w = nuc_mask_width_height_in_pixels,
                             h = nuc_mask_width_height_in_pixels,
                             offset = 0.05)
    # display(nmask)
    
    # Morphological opening to remove objects smaller than the structuring element
    # (disc of size 13)
    discSize <- floor(13*(0.22/pixel_size))
    nmask <- EBImage::opening(nmask, EBImage::makeBrush(discSize, shape='disc'))
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
        EBImage::imageData(nmask)[EBImage::imageData(nmask) == nuclei_at_borders[i]] <- 0
      }
      rm(i)
    }
    
    #display(nmask)
    
    # Delete all remaining nuclei that are smaller than 10% of the median size
    # object sizes
    # barplot(table(nmask)[-1])
    
    nmask <-  EBImage::watershed(EBImage::distmap(nmask), 1)
    
    table_nmask <- table(nmask)
    nuc_min_area_median  <- 0.1*median(table_nmask[-1])
    nuc_min_area_mean    <- 0.1*mean(table_nmask[-1])
    
    if(nuc_min_area_mean > (2*nuc_min_area_median) ){
      nuc_min_area <- 0.5 * (nuc_min_area_mean + nuc_min_area_median)
      print("Use combination of mean and median nucleus area for defining minimum nucleus size.")
    }else{
      nuc_min_area <- nuc_min_area_median
    }
    
    nuc_max_area_median <- 10 * median(table_nmask[-1])
    nuc_max_area_mean   <- 10 * mean(table_nmask[-1])
    
    if(nuc_max_area_mean > (2*nuc_max_area_median) ){
      nuc_max_area <- 0.5 * (nuc_max_area_mean + nuc_max_area_median)
      print("Use combination of mean and median nucleus area for defining maximum nucleus size.")
    }else{
      nuc_max_area <- nuc_max_area_median
    }
    
    # remove objects that are smaller than min_nuc_size
    to_be_removed <- c(as.integer(names(which(table_nmask < nuc_min_area))),
                       as.integer(names(which(table_nmask > nuc_max_area))))
    to_be_removed <- sort(to_be_removed)
    to_be_removed <- to_be_removed[!(to_be_removed == 0)]
    
    if(length(to_be_removed) > 0){
      for(i in 1:length(to_be_removed)){
        EBImage::imageData(nmask)[EBImage::imageData(nmask) == to_be_removed[i]] <- 0
      }
      rm(i)
    }
    
    # Recount nuclei
    nmask <- EBImage::bwlabel(nmask)
    total_nuclei_area <- sum(nmask != 0)
    #display(nmask)
    
    # Watershed in order to distinct nuclei that are too close to each other
    nmask_watershed <-  EBImage::watershed( EBImage::distmap(nmask), 1 )
    #display(colorLabels(nmask_watershed), all=TRUE)
    #display(nmask_watershed)
    
    # Count number of cells
    nucNo <- max(nmask_watershed)
    mean_nucleus_area_in_pixels <- round(total_nuclei_area / nucNo)
    
    if(export_normalized_images){
      EBImage::writeImage(x = nmask_watershed,
                          files = file.path(output_dir,
                                            paste(input_file_name,
                                                  "_nucleimask.tif",
                                                  sep = "")),
                          bits.per.sample = 8,
                          type = "tiff")
    }
    
  }
  
  
  # ---------------------------------------------------------------------- #
  # ------------ 3. Cilia detection in z-stack projection ---------------- #
  # ---------------------------------------------------------------------- #
  
  # 3. Cilia detection in z-stack projection -------------------------------
  # 3.1 Calculate the cilia detection thresholds ---------------------------
  
  # Calculate "threshold_find" and "threshold_connect" if
  # "threshold_by_density_of_cilium_pixels == TRUE"
  
  if(threshold_by_density_of_cilium_pixels == TRUE){
    
    print(paste("Recalculating the thresholds, because ",
                "threshold_by_density_of_cilium_pixels was given as ",
                "TRUE.", sep=""))
    
    # Enhance contrast of image projection
    if(dim(Image_projection_max)[1] %% 8 == 0 &&
       dim(Image_projection_max)[2] %% 8 == 0){
      clahe_nx <- 8
    }else if(dim(Image_projection_max)[1] %% 4 == 0 &&
             dim(Image_projection_max)[2] %% 4 == 0){
      clahe_nx <- 4
    }else if(dim(Image_projection_max)[1] %% 2 == 0 &&
             dim(Image_projection_max)[2] %% 2 == 0){
      clahe_nx <- 2
    }else{
      print("Something is wrong with the image dimension.")
    }
    
    
    if(projection_method_for_threshold_calculation == "max"){
      Image_projection_histogram_equalization <- EBImage::clahe(
        x = Image_projection_max, nx = clahe_nx)
      Image_projection <- Image_projection_max
    }else if(projection_method_for_threshold_calculation == "mean"){
      Image_projection_histogram_equalization <- EBImage::clahe(
        x = Image_projection_mean, nx = clahe_nx)
      Image_projection <- Image_projection_mean
    }else{
      print("Please choose either max or mean as zstack projection method.")
    }
    
    # Get image channel with ciliary pixels for finding cilia
    if(use_histogram_equalization_for_threshold_calculation){
      Image_cilia_layer_find <- getLayer(image = Image_projection_histogram_equalization,
                                         layer = cilium_color)
    }else{
      Image_cilia_layer_find <- getLayer(image = Image_projection,
                                         layer = cilium_color)
    }
    
    # Calculate ratio of possible cilium pixels determined by the number of
    # identified cells, the image size, and the geometric mean of the cilium area
    ratio_of_cilia_pixels <- nucNo * (sqrt(min_cilium_area_in_pixels * max_cilium_area_in_pixels)) /
      (dim(Image_projection)[1]*dim(Image_projection)[2])
    
    print(paste("Intensity quantile to find cilia: ",
                (1-ratio_of_cilia_pixels), sep=""))
    
    # threshold_find
    threshold_find <- quantile(Image_cilia_layer_find,
                               (1-ratio_of_cilia_pixels), na.rm = TRUE)
    threshold_find <- as.numeric(threshold_find)
    
    # Use the mean projection for finding threshold to connect cilia
    # in every z-stack layer
    Image_cilia_layer_connect <- getLayer(image = Image_projection_mean,
                                          layer = cilium_color)
    # if(is.null(cilium_color)){
    #   Image_cilia_layer_connect <- Image_projection_mean
    # }else{
    #   Image_cilia_layer_connect <- getLayer(image = Image_projection_mean,
    #                                         layer = cilium_color)
    # }
    
    # threshold_connect
    # threshold_connect <- quantile(Image_cilia_layer_connect,
    #                               (1-ratio_of_cilia_pixels), na.rm = TRUE)
    # threshold_connect <- as.numeric(threshold_connect)
    threshold_connect <- 0.1*threshold_find #0.5 0.2 # 0.1
    
    print(paste("The new threshold values are: threshold_find = ",
                threshold_find, " and threshold_connect = ",
                threshold_connect,
                ".",
                sep=""))
  }
  
  # 3.2 Save a mask of possible cilia --------------------------------------
  
  # Get mask of possible cilia
  if(use_histogram_equalization_for_threshold_calculation){
    Image_cilia <- editImage(
      image        = Image_projection_histogram_equalization,
      object_color = cilium_color,
      threshold    = threshold_find)
  }else{
    Image_cilia <- editImage(
      image        = Image_projection,
      object_color = cilium_color,
      threshold    = threshold_find)
  }
  
  #display(Image_cilia)
  
  list_of_cilium_points <- which(Image_cilia > 0, arr.ind = T)
  
  
  # Exit function because no cilium could be found
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if(length(list_of_cilium_points) == 0){
    
    print("No cilium found.")
    
    # (The next few lines are also found in the end of this function)
    Image_projection_histogram_equalization_normalized <-
      EBImage::normalize(Image_projection_histogram_equalization)
    
    EBImage::writeImage(x = Image_projection_histogram_equalization_normalized,
                        files = file.path(
                          output_dir,
                          paste(input_file_name,
                                "_projection_cilia_all_histogram_equalized_normalized.tif",
                                sep = "")),
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
                         "nucleus_color",
                         "projection_method_for_threshold_calculation",
                         "threshold_by_density_of_cilium_pixels",
                         "threshold_find", "threshold_connect",
                         "vicinity_combine", "vicinity_connect",
                         "min_cilium_area_in_pixels", "max_cilium_area_in_pixels",
                         "number_size_factor", "pixel_size",
                         "slice_distance", "nuc_mask_width_height_in_pixels")
    
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
      readr::write_csv(df_parameterList,
                       file = file.path(output_dir, "parameter_list.csv"))
      readr::write_csv2(df_parameterList,
                        file = file.path(output_dir, "parameter_list_de.csv"))
    }
    
    # Save the number of nuclei
    if(!is.null(nucleus_color)){
      # Add file name to tibble
      
      mean_nuclei_projection_diameter_in_um = sqrt(4 * mean_nucleus_area_in_pixels * pixel_size * pixel_size / pi)
      mean_nuclei_projection_diameter_in_um = round(x = mean_nuclei_projection_diameter_in_um, digits = 1)
      
      df_number_nuclei <- data.frame(
        "number_Of_nuclei" = nucNo,
        "mean_nuclei_projection_area_in_pixels" = mean_nucleus_area_in_pixels,
        "mean_nuclei_projection_equivalent_diameter_in_um" = mean_nuclei_projection_diameter_in_um)
      
      if(is.null(input_dir_tif)){
        df_number_nuclei$fileName <- basename(input_file_czi)
        df_number_nuclei <- df_number_nuclei %>% 
          dplyr::relocate(.data$fileName)
      }else if(is.null(input_file_czi)){
        df_number_nuclei$dirName <- basename(input_dir_tif)
        df_number_nuclei <- df_number_nuclei %>% 
          dplyr::relocate(.data$dirName)
      }
      
      if(!is.null(df_number_nuclei)){
        readr::write_csv(df_number_nuclei,
                         file = file.path(output_dir, "nuclei_number.csv"))
        readr::write_csv2(df_number_nuclei,
                          file = file.path(output_dir, "nuclei_number_de.csv"))
      }
    }
    
    df_cilium_summary <- data.frame(cilium = NA,
                                    cilium_shape = NA,
                                    cilium_projection_area_in_pixels = NA,
                                    cilium_projection_area_in_um2 = NA,
                                    number_of_z_stack_layers = dim(image_data)[4],
                                    lowest_cilium_layer = NA,
                                    uppermost_cilium_layer = NA,
                                    vertical_length_in_layers = NA,
                                    vertical_length_in_um = NA,
                                    horizontal_length_in_pixels = NA,
                                    horizontal_length_in_um = NA,
                                    total_length_in_um = NA)
    
    # Add file name to tibble
    if(is.null(input_dir_tif)){
      df_cilium_summary$fileName <- basename(input_file_czi)
      df_cilium_summary <- df_cilium_summary %>% 
        dplyr::relocate(.data$fileName)
    }else if(is.null(input_file_czi)){
      df_cilium_summary$dirName <- basename(input_dir_tif)
      df_cilium_summary <- df_cilium_summary %>% 
        dplyr::relocate(.data$dirName)
    }
    
    if(!is.null(df_cilium_summary)){
      readr::write_csv(df_cilium_summary,
                       file = file.path(output_dir, "cilium_summary.csv"))
      
      readr::write_csv2(df_cilium_summary,
                        file = file.path(output_dir, "cilium_summary_de.csv"))
    }
    
    return(NULL)
  }
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  
  # 3.2 Clean found cilium pixels ------------------------------------------
  
  df_cilium_points <- data.frame(list_of_cilium_points)
  
  # Rename columns (x: from left to right, y: from top to bottom)
  names(df_cilium_points) <- c("pos_x", "pos_y")
  
  df_cilium_points <- dplyr::arrange(df_cilium_points, pos_y, pos_x)
  rm(list_of_cilium_points)
  
  # Delete all found pixels that have no other found pixels in the
  # direct neighborhood (+-1)
  df_cilium_points$possibleCilium <- FALSE
  
  neighborhood <- vicinity_combine
  for(jx in -neighborhood:neighborhood){
    for(jy in -neighborhood:neighborhood){
      if(!(jx == 0 && jy == 0)){
        # Shift pixels accordingly and check for equality
        df_cilium_points_shifted <- df_cilium_points
        df_cilium_points_shifted$pos_x <-
          df_cilium_points_shifted$pos_x + jx
        df_cilium_points_shifted$pos_y <-
          df_cilium_points_shifted$pos_y + jy
        equal_positions <- paste(df_cilium_points$pos_x,
                                 df_cilium_points$pos_y, sep= ", ") %in%
          paste(df_cilium_points_shifted$pos_x,
                df_cilium_points_shifted$pos_y, sep= ", ")
        df_cilium_points$possibleCilium[equal_positions] <- TRUE
      }
    }
  }
  rm(list = c("jx", "jy", "df_cilium_points_shifted"))
  
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  
  # Determine the cilium number of each cilium found (Combine all cilium
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
    
    if(length(ciliumNumber) > 1){
      print(paste0("The following cilia are now one: ",
                   paste0(ciliumNumber, collapse = ", ")))
      for(j in 2:length(ciliumNumber)){
        df_cilium_points$ciliumNumber[
          df_cilium_points$ciliumNumber == ciliumNumber[j]] <-
          ciliumNumber[1]
      }
      rm(j)
    }else{
      df_cilium_points$ciliumNumber[.distance == 0] <- ciliumNumber
    }
    
    # Advance i to the next row which contains 0 as the cilium number
    i <- which(df_cilium_points$ciliumNumber==0)[1]
  }
  rm(list = c("i", "ciliumNumber"))
  
  
  # Delete all ciliary pixels that are not bright enough
  # (less than 0.75*median)
  min_intensity_factor <- 0.75
  for(i in unique(df_cilium_points$ciliumNumber)){
    
    indices <- cbind(df_cilium_points$pos_x[df_cilium_points$ciliumNumber == i],
                     df_cilium_points$pos_y[df_cilium_points$ciliumNumber == i])
    
    cilium_intensities <- Image_cilia_layer_find[indices]
    
    lower_limit <- min_intensity_factor * median(cilium_intensities, na.rm = TRUE)
    keep_pixels <- cilium_intensities > lower_limit 
    
    df_cilium_points$possibleCilium[df_cilium_points$ciliumNumber == i] <- keep_pixels
  }
  rm(list = c("i", "indices", "cilium_intensities", "lower_limit", "keep_pixels"))
  
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  
  # Mark all structures that are too small and therefore may not be a cilium
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) < min_cilium_area_in_pixels){
      df_cilium_points$possibleCilium[df_cilium_points$ciliumNumber == i] <- FALSE
      # df_cilium_points <- df_cilium_points[
      #   !(df_cilium_points$ciliumNumber == i),]
    }
  }
  rm(i)
  
  # Mark all structures that are too large and therefore may not be a cilium
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    if(sum(df_cilium_points$ciliumNumber == i) > max_cilium_area_in_pixels){
      df_cilium_points$possibleCilium[df_cilium_points$ciliumNumber == i] <- FALSE
    }
  }
  rm(i)
  
  # Delete all structures that are too small or too big
  df_cilium_points <- df_cilium_points[df_cilium_points$possibleCilium,]
  
  # Delete all cilia touching a border of the image
  # (including one pixel in between)
  
  # Coordinate system:
  # x-axis: bottom left to bottom right
  # y-axis: bottom left to top left
  
  # row = pos_x: left to right (= from x=0 to x_max)
  # col = pos_y: top to bottom (= from y_max to y=0)
  
  # Record all cilia that are at the borders of the image
  dim_image_x <- dim(image_data)[2]
  dim_image_y <- dim(image_data)[1]
  
  cilia_at_left_border    <- unique(df_cilium_points$ciliumNumber[
    df_cilium_points$pos_x==1 | df_cilium_points$pos_x==2])
  cilia_at_top_border     <- unique(df_cilium_points$ciliumNumber[
    df_cilium_points$pos_y==1 | df_cilium_points$pos_y==2])
  cilia_at_right_border   <- unique(df_cilium_points$ciliumNumber[
    df_cilium_points$pos_x==dim_image_x | df_cilium_points$pos_x==(dim_image_x-1)])
  cilia_at_bottom_border  <- unique(df_cilium_points$ciliumNumber[
    df_cilium_points$pos_y==dim_image_y | df_cilium_points$pos_y==(dim_image_y-1)])
  
  # Remove found cilia touching the border
  if(length(cilia_at_left_border) > 0){
    df_cilium_points <- df_cilium_points[
      !(df_cilium_points$ciliumNumber %in% cilia_at_left_border),]
  }
  if(length(cilia_at_top_border) > 0){
    df_cilium_points <- df_cilium_points[
      !(df_cilium_points$ciliumNumber %in% cilia_at_top_border),]
  }
  if(length(cilia_at_right_border) > 0){
    df_cilium_points <- df_cilium_points[
      !(df_cilium_points$ciliumNumber %in% cilia_at_right_border),]
  }
  if(length(cilia_at_bottom_border) > 0){
    df_cilium_points <- df_cilium_points[
      !(df_cilium_points$ciliumNumber %in% cilia_at_bottom_border),]
  }
  
  rm(list = c("cilia_at_left_border", "cilia_at_top_border",
              "cilia_at_right_border", "cilia_at_bottom_border"))
  
  
  # In every found cilium (cluster), delete separated cilium parts that
  # are not directly connected (being vicinity_connect apart) to the
  # brightest part of that cilium
  
  df_cilium_points$disconnectedPart <- FALSE
  
  indices <- cbind(df_cilium_points$pos_x, df_cilium_points$pos_y)
  df_cilium_points$fluorescence_intensity <- Image_cilia_layer_find[indices]
  
  df_cilium_points$ClusterNumber <- 0
  
  for(i in unique(df_cilium_points$ciliumNumber)){
    # print(i)
    df_dummy <- df_cilium_points[df_cilium_points$ciliumNumber == i,]
    
    df_dummy$ClusterNumber[1] <- 1
    
    if(length(df_dummy$ClusterNumber) > 1){
      j <- 2
      while(0 %in% df_dummy$ClusterNumber){
        
        .pos_x_distance <- df_dummy$pos_x[j] -
          df_dummy$pos_x
        
        .pos_y_distance <- df_dummy$pos_y[j] -
          df_dummy$pos_y
        
        .pos_x_distance[abs(.pos_x_distance) <= vicinity_connect] <- 0
        .pos_y_distance[abs(.pos_y_distance) <= vicinity_connect] <- 0
        
        .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
        
        # Get the cluster number (close cilium that has been detected)
        ClusterNumber_dummy <-
          unique(df_dummy$ClusterNumber[.distance == 0])
        
        if(length(ClusterNumber_dummy) == 1 && ClusterNumber_dummy == 0){
          # Advance cluster number number because there is no cluster close by
          ClusterNumber <- max(df_dummy$ClusterNumber) + 1
        }else{
          # Points belong to already existing cluster
          ClusterNumber <- ClusterNumber_dummy[!(ClusterNumber_dummy == 0)]
        }
        
        if(length(ClusterNumber) > 1){
          print(paste0("The following clusters of cilium ", i,
                       " are now one: ", paste0(ClusterNumber, collapse = ", ")))
          for(k in 2:length(ClusterNumber)){
            df_dummy$ClusterNumber[
              df_dummy$ClusterNumber == ClusterNumber[k]] <-
              ClusterNumber[1]
          }
          rm(k)
          
        }else{
          df_dummy$ClusterNumber[
            df_dummy$ClusterNumber == 0 & .distance == 0] <- ClusterNumber
        }
        
        j <- j+1
      }
      rm(list = c("j", "ClusterNumber"))
      
      df_test <- df_dummy %>% 
        dplyr::group_by(ClusterNumber) %>%
        dplyr::summarise(meanIntensity = mean(.data$fluorescence_intensity),
                         clusterSize = length(.data$fluorescence_intensity))
      
      # Remove all clusters that are too small
      df_test <- df_test[!(df_test$clusterSize < min_cilium_area_in_pixels),]
      
      cluster_number_with_heighest_mean_intensity <- df_test$ClusterNumber[
        df_test$meanIntensity == max(df_test$meanIntensity)]
      
      if(length(cluster_number_with_heighest_mean_intensity)>1){
        print("Please check the determination of the cluster number.")
      }
      
      df_dummy$disconnectedPart[
        df_dummy$ClusterNumber != cluster_number_with_heighest_mean_intensity] <- TRUE
      
      df_dummy <- df_dummy[!df_dummy$disconnectedPart,]
      df_dummy <- df_dummy[,!names(df_dummy)=="ClusterNumber"]
      
      # Keep only rows that have been found
      if(i == unique(df_cilium_points$ciliumNumber)[1]){
        df_cilium_points2 <- df_dummy
      }else{
        df_cilium_points2 <- rbind(df_cilium_points2, df_dummy)
      }
      
    }
    
  }
  rm(list = c("i", "df_test", "df_dummy"))
  
  # Delete old cilium point tibble
  df_cilium_points <- df_cilium_points2
  rm(df_cilium_points2)
  
  # Sort cilia
  df_cilium_points <- df_cilium_points %>% 
    dplyr::arrange(ciliumNumber, pos_x, pos_y)
  
  
  # Drop disconnectedPart and fluorescence intensity columns
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
    Image_projection_histogram_equalization_normalized <-
      EBImage::normalize(Image_projection_histogram_equalization)
    
    EBImage::writeImage(x = Image_projection_histogram_equalization_normalized,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_projection_cilia_all_histogram_equalized_normalized.tif",
                                                sep = "")),
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
                         "nucleus_color",
                         "projection_method_for_threshold_calculation",
                         "threshold_by_density_of_cilium_pixels",
                         "threshold_find", "threshold_connect",
                         "vicinity_combine", "vicinity_connect",
                         "min_cilium_area_in_pixels", "max_cilium_area_in_pixels",
                         "number_size_factor", "pixel_size",
                         "slice_distance", "nuc_mask_width_height_in_pixels")
    
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
      readr::write_csv(df_parameterList,
                       file = file.path(output_dir, "parameter_list.csv"))
      readr::write_csv2(df_parameterList,
                        file = file.path(output_dir, "parameter_list_de.csv"))
    }
    
    # Save the number of nuclei
    if(!is.null(nucleus_color)){
      # Add file name to tibble
      
      mean_nuclei_projection_diameter_in_um = sqrt(4 * mean_nucleus_area_in_pixels * pixel_size * pixel_size / pi)
      mean_nuclei_projection_diameter_in_um = round(x = mean_nuclei_projection_diameter_in_um, digits = 1)
      
      df_number_nuclei <- data.frame(
        "number_Of_nuclei" = nucNo,
        "mean_nuclei_projection_area_in_pixels" = mean_nucleus_area_in_pixels,
        "mean_nuclei_projection_equivalent_diameter_in_um" = mean_nuclei_projection_diameter_in_um)
      
      if(is.null(input_dir_tif)){
        df_number_nuclei$fileName <- basename(input_file_czi)
        df_number_nuclei <- df_number_nuclei %>% 
          dplyr::relocate(.data$fileName)
      }else if(is.null(input_file_czi)){
        df_number_nuclei$dirName <- basename(input_dir_tif)
        df_number_nuclei <- df_number_nuclei %>% 
          dplyr::relocate(.data$dirName)
      }
      
      if(!is.null(df_number_nuclei)){
        readr::write_csv(df_number_nuclei,
                         file = file.path(output_dir, "nuclei_number.csv"))
        readr::write_csv2(df_number_nuclei,
                          file = file.path(output_dir, "nuclei_number_de.csv"))
      }
    }
    
    df_cilium_summary <- data.frame(cilium = NA,
                                    cilium_shape = NA,
                                    cilium_projection_area_in_pixels = NA,
                                    cilium_projection_area_in_um2 = NA,
                                    number_of_z_stack_layers = dim(image_data)[4],
                                    lowest_cilium_layer = NA,
                                    uppermost_cilium_layer = NA,
                                    vertical_length_in_layers = NA,
                                    vertical_length_in_um = NA,
                                    horizontal_length_in_pixels = NA,
                                    horizontal_length_in_um = NA,
                                    total_length_in_um = NA)
    
    # Add file name to tibble
    if(is.null(input_dir_tif)){
      df_cilium_summary$fileName <- basename(input_file_czi)
      df_cilium_summary <- df_cilium_summary %>% 
        dplyr::relocate(.data$fileName)
    }else if(is.null(input_file_czi)){
      df_cilium_summary$dirName <- basename(input_dir_tif)
      df_cilium_summary <- df_cilium_summary %>% 
        dplyr::relocate(.data$dirName)
    }
    
    if(!is.null(df_cilium_summary)){
      readr::write_csv(df_cilium_summary,
                       file = file.path(output_dir, "cilium_summary.csv"))
      
      readr::write_csv2(df_cilium_summary,
                        file = file.path(output_dir, "cilium_summary_de.csv"))
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
  rm(list = c("i", ".number"))
  
  df_cilium_points <- dplyr::select(df_cilium_points, -ciliumNumber)
  names(df_cilium_points)[names(df_cilium_points) == "ciliumNumber_NEW"] <- "ciliumNumber"
  
  # Drop possibleCilium column
  df_cilium_points <-
    df_cilium_points[,!names(df_cilium_points)=="possibleCilium"]
  
  
  # ---------------------------------------------------------------------- #
  # -------------- 4. Cilia detection in z-stack layers ------------------ #
  # ---------------------------------------------------------------------- #
  
  # 4. Cilia detection in z-stack layers -----------------------------------
  # Find cilia pixels belonging to a specific cilium by going through every
  # image layer (i = 1 ... n) and looking for ciliary pixels in the same
  # region as existing cilia
  
  print("Connecting cilia in all z-stack layers.")
  
  # Add layer information to data frame (layer -1 == z-stack projection)
  df_cilium_points$layer <- -1
  
  # Save information in big data frame, which contains all layers
  # -1 is the sum of all layers
  df_cilium_information <- df_cilium_points
  
  
  # Add median fluorescence intensity per cilium of mean projection
  df_cilium_median_intensity <- data.frame(
    ciliumNumber = unique(df_cilium_information$ciliumNumber),
    median_mean_projection_intensity = NA)
  
  for(i in unique(df_cilium_information$ciliumNumber)){
    
    indices <- cbind(df_cilium_points$pos_x[df_cilium_points$ciliumNumber == i],
                     df_cilium_points$pos_y[df_cilium_points$ciliumNumber == i])
    
    cilium_intensities <- Image_cilia_layer_connect[indices]
    
    df_cilium_median_intensity$median_mean_projection_intensity[
      df_cilium_median_intensity$ciliumNumber == i] <-
      median(cilium_intensities, na.rm = TRUE)
  }
  rm(i)
  
  # Go through all layers (i = 1 .. n (layers) )
  
  print("Finding cilia in every layer.")
  for(i in 1:dim(image_data)[4]){
    
    print(paste("Dealing with layer ", i, ". (It is now ",
                Sys.time(), ".)", sep=""))
    
    
    # Get image of every layer 
    Image <-  image_data[,,,i]
    
    Image_cilia_layer <- getLayer(image = Image, layer = cilium_color)
    
    # Mask cilium channel
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
      neighborhood <- 1
      for(jx in -neighborhood:neighborhood){
        for(jy in -neighborhood:neighborhood){
          if(!(jx == 0 && jy == 0)){
            # Shift pixels accordingly and check for equality
            df_cilium_points_connect_shifted <- df_cilium_points_connect
            df_cilium_points_connect_shifted$pos_x <-
              df_cilium_points_connect_shifted$pos_x + jx
            df_cilium_points_connect_shifted$pos_y <-
              df_cilium_points_connect_shifted$pos_y + jy
            equal_positions <- paste(df_cilium_points_connect$pos_x,
                                     df_cilium_points_connect$pos_y, sep= ", ") %in%
              paste(df_cilium_points_connect_shifted$pos_x,
                    df_cilium_points_connect_shifted$pos_y, sep= ", ")
            df_cilium_points_connect$possibleCilium[equal_positions] <- TRUE
          }
        }
      }
      rm(list = c("jx", "jy", "df_cilium_points_connect_shifted"))
      
      
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
        rm(j)
        
        # Delete rows that cannot be connected to an existing cilium
        df_cilium_points_connect <- df_cilium_points_connect[
          !df_cilium_points_connect$ciliumNumber == 0,]
        
        if(nrow(df_cilium_points_connect) > 0){
          
          row.names(df_cilium_points_connect) <- NULL
          
          # Delete all pixels that are not bright enough
          # (less than median intensity (of projection))
          df_cilium_points_connect$possibleCilium <- FALSE
          
          for(k in unique(df_cilium_points_connect$ciliumNumber)){
            
            indices <- cbind(df_cilium_points_connect$pos_x[
              df_cilium_points_connect$ciliumNumber == k],
              df_cilium_points_connect$pos_y[
                df_cilium_points_connect$ciliumNumber == k])
            
            cilium_intensities <- Image_cilia_layer[indices]
            
            lower_limit <- df_cilium_median_intensity$median_mean_projection_intensity[
              df_cilium_median_intensity$ciliumNumber == k]
            keep_pixels <- cilium_intensities > lower_limit
            
            df_cilium_points_connect$possibleCilium[
              df_cilium_points_connect$ciliumNumber == k] <- keep_pixels
          }
          rm(k)
          
          df_cilium_points_connect <- df_cilium_points_connect[
            df_cilium_points_connect$possibleCilium,]
          
          # Mark all structures that are too small and therefore may not be a cilium
          for(k in unique(df_cilium_points_connect$ciliumNumber)){
            if(sum(df_cilium_points_connect$ciliumNumber == k) < min_cilium_area_in_pixels){
              df_cilium_points_connect$possibleCilium[
                df_cilium_points_connect$ciliumNumber == k] <- FALSE
            }
          }
          rm(k)
          
          # Mark all structures that are too large and therefore may not be a cilium
          for(k in unique(df_cilium_points_connect$ciliumNumber)){
            if(sum(df_cilium_points_connect$ciliumNumber == k) > max_cilium_area_in_pixels){
              df_cilium_points_connect$possibleCilium[
                df_cilium_points_connect$ciliumNumber == k] <- FALSE
            }
          }
          rm(k)
          
          
          df_cilium_points_connect <- df_cilium_points_connect[
            df_cilium_points_connect$possibleCilium,]
          
          
          if(nrow(df_cilium_points_connect) > 0){
            
            # Drop "possibleCilium" column
            df_cilium_points_connect <- 
              df_cilium_points_connect[ ,!names(df_cilium_points_connect) ==
                                          "possibleCilium"]
            
            # Save the layer and append it to big data frame
            df_cilium_points_connect$layer <- i
            df_cilium_information <- rbind(df_cilium_information,
                                           df_cilium_points_connect)
            
          }
        }
      }
    }
  }
  
  rm(list = c("i", "Image", "Image_cilia_layer"))
  
  # Check if only layer -1 (from z stack projection) is available ----------
  # This means that the found cilia in the z stack cilia were deleted
  # during the cleaning steps
  if(dim(image_data)[4] > 1){
    df_cilium_layers <- df_cilium_information %>%
      dplyr::group_by(ciliumNumber ) %>%
      dplyr::summarise(layer_mean = mean(.data$layer))
    
    # Remove all cilia with mean = -1 (no cilium in z stack layers)
    cilia_to_be_removed <- df_cilium_layers$ciliumNumber[df_cilium_layers$layer_mean == -1]
    
    df_cilium_information <- df_cilium_information[!df_cilium_information$ciliumNumber %in% cilia_to_be_removed,]
    rm(df_cilium_layers)
    
    
    # Exit function because no cilium could be found
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    if(length(unique(df_cilium_information$ciliumNumber)) == 0){
      print(paste("Please change your input parameter values or image file. ",
                  "No cilium could be found.",
                  sep=""))
      
      # (The next few lines are also found in the end of this function)
      Image_projection_histogram_equalization_normalized <-
        EBImage::normalize(Image_projection_histogram_equalization)
      
      EBImage::writeImage(x = Image_projection_histogram_equalization_normalized,
                          files = file.path(output_dir,
                                            paste(input_file_name,
                                                  "_projection_cilia_all_histogram_equalized_normalized.tif",
                                                  sep = "")),
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
                           "nucleus_color",
                           "projection_method_for_threshold_calculation",
                           "threshold_by_density_of_cilium_pixels",
                           "threshold_find", "threshold_connect",
                           "vicinity_combine", "vicinity_connect",
                           "min_cilium_area_in_pixels", "max_cilium_area_in_pixels",
                           "number_size_factor", "pixel_size",
                           "slice_distance", "nuc_mask_width_height_in_pixels")
      
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
        readr::write_csv(df_parameterList,
                         file = file.path(output_dir, "parameter_list.csv"))
        readr::write_csv2(df_parameterList,
                          file = file.path(output_dir, "parameter_list_de.csv"))
      }
      
      # Save the number of nuclei
      if(!is.null(nucleus_color)){
        # Add file name to tibble
        
        mean_nuclei_projection_diameter_in_um = sqrt(4 * mean_nucleus_area_in_pixels * pixel_size * pixel_size / pi)
        mean_nuclei_projection_diameter_in_um = round(x = mean_nuclei_projection_diameter_in_um, digits = 1)
        
        df_number_nuclei <- data.frame(
          "number_Of_nuclei" = nucNo,
          "mean_nuclei_projection_area_in_pixels" = mean_nucleus_area_in_pixels,
          "mean_nuclei_projection_equivalent_diameter_in_um" = mean_nuclei_projection_diameter_in_um)
        
        if(is.null(input_dir_tif)){
          df_number_nuclei$fileName <- basename(input_file_czi)
          df_number_nuclei <- df_number_nuclei %>% 
            dplyr::relocate(.data$fileName)
        }else if(is.null(input_file_czi)){
          df_number_nuclei$dirName <- basename(input_dir_tif)
          df_number_nuclei <- df_number_nuclei %>% 
            dplyr::relocate(.data$dirName)
        }
        
        if(!is.null(df_number_nuclei)){
          readr::write_csv(df_number_nuclei,
                           file = file.path(output_dir, "nuclei_number.csv"))
          readr::write_csv2(df_number_nuclei,
                            file = file.path(output_dir, "nuclei_number_de.csv"))
        }
      }
      
      df_cilium_summary <- data.frame(cilium = NA,
                                      cilium_shape = NA,
                                      cilium_projection_area_in_pixels = NA,
                                      cilium_projection_area_in_um2 = NA,
                                      number_of_z_stack_layers = dim(image_data)[4],
                                      lowest_cilium_layer = NA,
                                      uppermost_cilium_layer = NA,
                                      vertical_length_in_layers = NA,
                                      vertical_length_in_um = NA,
                                      horizontal_length_in_pixels = NA,
                                      horizontal_length_in_um = NA,
                                      total_length_in_um = NA)
      
      # Add file name to tibble
      if(is.null(input_dir_tif)){
        df_cilium_summary$fileName <- basename(input_file_czi)
        df_cilium_summary <- df_cilium_summary %>% 
          dplyr::relocate(.data$fileName)
      }else if(is.null(input_file_czi)){
        df_cilium_summary$dirName <- basename(input_dir_tif)
        df_cilium_summary <- df_cilium_summary %>% 
          dplyr::relocate(.data$dirName)
      }
      
      if(!is.null(df_cilium_summary)){
        readr::write_csv(df_cilium_summary,
                         file = file.path(output_dir, "cilium_summary.csv"))
        
        readr::write_csv2(df_cilium_summary,
                          file = file.path(output_dir, "cilium_summary_de.csv"))
      }
      
      return(NULL)
    }
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  }
  
  # Check if no connected cilium is larger than max cilium area ------------
  
  # Save all locations of all cilia (independent from the z layer where it
  # was found being found)
  df_cilium_all <- df_cilium_information
  df_cilium_all$xy <- paste(df_cilium_all$pos_x,
                            df_cilium_all$pos_y,
                            sep = ",")
  # Keep only those coordinates that are not duplicated
  df_cilium_all <- df_cilium_all[
    !duplicated(df_cilium_all$xy),]
  
  df_cilium_all <- df_cilium_all[,!names(df_cilium_all)=="xy"]
  df_cilium_all$layer <- -99
  
  df_cilium_all <- dplyr::arrange(df_cilium_all, ciliumNumber, pos_y, pos_x)
  
  row.names(df_cilium_all) <- NULL
  
  df_cilium_size <- df_cilium_all %>%
    dplyr::group_by(ciliumNumber ) %>%
    dplyr::summarise(ciliumSize = length(.data$pos_x))
  
  cilium_numbers_to_be_removed <- df_cilium_size$ciliumNumber[
    df_cilium_size$ciliumSize > max_cilium_area_in_pixels]
  
  if(length(cilium_numbers_to_be_removed) > 0){
    df_cilium_all <- df_cilium_all[
      !df_cilium_all$ciliumNumber %in% cilium_numbers_to_be_removed,]
    df_cilium_information <- df_cilium_information[
      !df_cilium_information$ciliumNumber %in% cilium_numbers_to_be_removed,]
  }
  
  ###new from here
  # Delete all cilia that are disconnected
  # In every found cilium (cluster), delete separated cilium parts that
  # are not directly connected (being vicinity_connect apart) to the
  # brightest part of that cilium
  
  df_cilium_all$disconnectedCilia <- FALSE
  df_cilium_all$ClusterNumber <- 0
  
  for(i in unique(df_cilium_all$ciliumNumber)){
    # print(i)
    df_dummy <- df_cilium_all[df_cilium_all$ciliumNumber == i,]
    
    # # Create a data frame with (x, y) coordinates
    # coordinates <- data.frame(x = df_dummy$pos_x, y = df_dummy$pos_y)
    # 
    # # Calculate pairwise distances
    # distances <- as.matrix(dist(coordinates, method = "manhattan"))
    # distances[!lower.tri(distances)] <- NA
    # 
    # # Set the threshold distance (e.g., 2 units)
    # threshold_distance <- 1
    # 
    # # Initialize labels
    # labels <- rep(NA, length(df_dummy$pos_x))
    # current_label <- 1
    # 
    # # Assign labels based on connectivity
    # for (k in 1:(length(x_coords) - 1)) {
    #   if(is.na(labels[k])) {
    #     neighbors <- which(distances[,k] <= threshold_distance)
    #     labels[neighbors] <- current_label
    #     current_label <- current_label + 1
    #   }
    # }
    # 
    # 
    # 
    # 
    # 
    # 
    # distances <- as.matrix(dist(cbind(df_dummy$pos_x, df_dummy$pos_y), upper = FALSE, method = "manhattan"))
    # distances[!lower.tri(distances)] <- NA
    # 
    # threshold_distance <- 1
    # 
    # connected_pairs <- which(distances <= threshold_distance, arr.ind = TRUE)
    # 
    # for (k in 1:nrow(connected_pairs)) {
    #   point1 <- connected_pairs[k, "row"]
    #   point2 <- connected_pairs[k, "col"]
    #   
    #   df_dummy
    # }
    
    j <- 1
    
    rows_gone_through <- j
    rows_to_go_through <- 1:length(df_dummy$pos_x)
    rows_to_go_through <- rows_to_go_through[rows_to_go_through != j]
    
    threshold_distance <- 1
    df_dummy$ClusterNumber[j] <- 1
    
    if(length(df_dummy$ClusterNumber) > 1){
      
      next_point <- rows_to_go_through[
        rows_to_go_through %in%
          which(df_dummy$pos_x %in% (df_dummy$pos_x[j] - threshold_distance) : (df_dummy$pos_x[j] + threshold_distance) &
                  df_dummy$pos_y %in% (df_dummy$pos_y[j] - threshold_distance) : (df_dummy$pos_y[j] + threshold_distance))][1]
      if(is.na(next_point)){
        j <- rows_to_go_through[1]
      }else{
        j <- next_point
      }
      
      while(0 %in% df_dummy$ClusterNumber){
        
        .pos_x_distance <- df_dummy$pos_x[j] -
          df_dummy$pos_x
        
        .pos_y_distance <- df_dummy$pos_y[j] -
          df_dummy$pos_y
        
        .pos_x_distance[abs(.pos_x_distance) <= threshold_distance] <- 0
        .pos_y_distance[abs(.pos_y_distance) <= threshold_distance] <- 0
        
        .distance <- abs(.pos_x_distance) + abs(.pos_y_distance)
        
        # Get the cluster number (close cilium that has been detected)
        ClusterNumber_dummy <-
          unique(df_dummy$ClusterNumber[.distance == 0])
        
        if(length(ClusterNumber_dummy) == 1 && ClusterNumber_dummy == 0){
          # Advance cluster number number because there is no cluster close by
          ClusterNumber <- max(df_dummy$ClusterNumber) + 1
        }else{
          # Points belong to already existing cluster
          ClusterNumber <- ClusterNumber_dummy[!(ClusterNumber_dummy == 0)]
        }
        
        # if(length(ClusterNumber) > 1){
        #   print(paste0("The following clusters of cilium ", i,
        #                " are now one: ", paste0(ClusterNumber, collapse = ", ")))
        #   for(k in 2:length(ClusterNumber)){
        #     df_dummy$ClusterNumber[
        #       df_dummy$ClusterNumber == ClusterNumber[k]] <-
        #       ClusterNumber[1]
        #   }
        #   rm(k)
        #   
        # }else{
          df_dummy$ClusterNumber[
            df_dummy$ClusterNumber == 0 & .distance == 0] <- ClusterNumber
        # }
        # 
          rows_to_go_through <- rows_to_go_through[rows_to_go_through != j]
          next_point <- rows_to_go_through[
            rows_to_go_through %in%
              which(df_dummy$pos_x %in% (df_dummy$pos_x[j] - threshold_distance) : (df_dummy$pos_x[j] + threshold_distance) &
                      df_dummy$pos_y %in% (df_dummy$pos_y[j] - threshold_distance) : (df_dummy$pos_y[j] + threshold_distance))][1]
          if(is.na(next_point)){
            j <- rows_to_go_through[1]
          }else{
            j <- next_point
          }
          
      }
      rm(list = c("j", "ClusterNumber"))
      
      # df_test <- df_dummy %>% 
      #   dplyr::group_by(ClusterNumber) %>%
      #   dplyr::summarise(meanIntensity = mean(.data$fluorescence_intensity),
      #                    clusterSize = length(.data$fluorescence_intensity))
      # 
      # # Remove all clusters that are too small
      # df_test <- df_test[!(df_test$clusterSize < min_cilium_area_in_pixels),]
      # 
      # cluster_number_with_heighest_mean_intensity <- df_test$ClusterNumber[
      #   df_test$meanIntensity == max(df_test$meanIntensity)]
      # 
      # if(length(cluster_number_with_heighest_mean_intensity)>1){
      #   print("Please check the determination of the cluster number.")
      # }
      # 
      # df_dummy$disconnectedPart[
      #   df_dummy$ClusterNumber != cluster_number_with_heighest_mean_intensity] <- TRUE
      # 
      # df_dummy <- df_dummy[!df_dummy$disconnectedPart,]
      # df_dummy <- df_dummy[,!names(df_dummy)=="ClusterNumber"]
      
      # # Keep only rows that have been found
      # if(i == unique(df_cilium_points$ciliumNumber)[1]){
      #   df_cilium_points2 <- df_dummy
      # }else{
      #   df_cilium_points2 <- rbind(df_cilium_points2, df_dummy)
      # }
      
      if(max(df_dummy$ClusterNumber) > 1){
        df_cilium_all$disconnectedCilia[df_cilium_all$ciliumNumber == i] <- TRUE
      }
      
    }
    
  }
  rm(list = c("i", "df_dummy"))
  
  # Remove cilia with disconnected cilia
  disconnectedCilia <- unique(df_cilium_all$ciliumNumber[df_cilium_all$disconnectedCilia])
  
  df_cilium_information <- df_cilium_information[!df_cilium_information$ciliumNumber %in% disconnectedCilia, ]
  df_cilium_all <- df_cilium_all[!df_cilium_all$ciliumNumber %in% disconnectedCilia,]
  
  # Drop extra columns
  df_cilium_all <-
    df_cilium_all[,!names(df_cilium_all)=="disconnectedCilia"]
  df_cilium_all <-
    df_cilium_all[,!names(df_cilium_all)=="ClusterNumber"]
  
  ###new up to here
  
  # Renumber cilia
  .number <- 1
  df_cilium_information$ciliumNumber_NEW <- NA
  for(i in unique(df_cilium_information$ciliumNumber)){
    df_cilium_information$ciliumNumber_NEW[
      df_cilium_information$ciliumNumber == i] <-
      .number
    df_cilium_all$ciliumNumber_NEW[df_cilium_all$ciliumNumber == i] <-
      .number
    .number <- .number + 1
  }
  rm(list = c("i", ".number"))
  
  df_cilium_information <- dplyr::select(df_cilium_information, -ciliumNumber)
  names(df_cilium_information)[
    names(df_cilium_information) == "ciliumNumber_NEW"] <- "ciliumNumber"
  
  df_cilium_all <- dplyr::select(df_cilium_all, -ciliumNumber)
  names(df_cilium_all)[names(df_cilium_all) == "ciliumNumber_NEW"] <- "ciliumNumber"
  
  # Save found cilia in every z-stack layer
  
  for(i in unique(df_cilium_information$layer[df_cilium_information$layer>0])){
    
    coordinates_layer1 <- matrix(
      data = c(df_cilium_information$pos_x[df_cilium_information$layer == i],
               df_cilium_information$pos_y[df_cilium_information$layer == i],
               rep(1, length(df_cilium_information$pos_x[df_cilium_information$layer == i]))),
      ncol = 3)
    coordinates_layer2 <- matrix(
      data = c(df_cilium_information$pos_x[df_cilium_information$layer == i],
               df_cilium_information$pos_y[df_cilium_information$layer == i],
               rep(2, length(df_cilium_information$pos_x[df_cilium_information$layer == i]))),
      ncol = 3)
    
    # if(dim(image_data)[3] == 1){
    #   coordinates_layer1 <- matrix(
    #     data = c(df_cilium_information$pos_x[df_cilium_information$layer == i],
    #              df_cilium_information$pos_y[df_cilium_information$layer == i]),
    #     ncol = 2)
    #   coordinates_layer2 <- matrix(
    #     data = c(df_cilium_information$pos_x[df_cilium_information$layer == i],
    #              df_cilium_information$pos_y[df_cilium_information$layer == i]),
    #     ncol = 2)
    # }else{
    #   coordinates_layer1 <- matrix(
    #     data = c(df_cilium_information$pos_x[df_cilium_information$layer == i],
    #              df_cilium_information$pos_y[df_cilium_information$layer == i],
    #              rep(1, length(df_cilium_information$pos_x[df_cilium_information$layer == i]))),
    #     ncol = 3)
    #   coordinates_layer2 <- matrix(
    #     data = c(df_cilium_information$pos_x[df_cilium_information$layer == i],
    #              df_cilium_information$pos_y[df_cilium_information$layer == i],
    #              rep(2, length(df_cilium_information$pos_x[df_cilium_information$layer == i]))),
    #     ncol = 3)
    # }
    
    
    Image <-  image_data[,,,i]
    
    if(export_normalized_images){
      Image_normalized <- image_data[,,,i]
      Image_normalized <- EBImage::normalize(Image_normalized)
      
      Image_normalized[coordinates_layer1] <- 1
      Image_normalized[coordinates_layer2] <- 1
      
      # if(length(dim(Image_normalized)) == 2){
      #   Image_normalized <- EBImage::Image(data = Image_normalized,
      #                                      colormode = "Gray")
      # }else{
      Image_normalized <- EBImage::Image(data = Image_normalized,
                                         colormode = "color")
      # }
      
      EBImage::writeImage(x = Image_normalized,
                          files = file.path(output_dir,
                                            paste(input_file_name,
                                                  "_cilia_layer_",
                                                  i, "_normalized.tif",
                                                  sep = "")),
                          bits.per.sample = 8,
                          type = "tiff")
    }
    
    Image[coordinates_layer1] <- 1
    Image[coordinates_layer2] <- 1
    
    Image <- EBImage::Image(data = Image, colormode = "color")
    EBImage::writeImage(x = Image,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_cilia_layer_",
                                                i, ".tif",
                                                sep = "")),
                        bits.per.sample = 8,
                        type = "tiff")
  }
  rm(i)
  
  
  rm(list = c("coordinates_layer1", "coordinates_layer2"))
  
  # Save unconnected cilia image (from projection but after this cleaning step)
  
  # Save image with labelled cilia
  Image_projection_cilia <- Image_projection
  
  coordinates_layer1 <- matrix(
    data = c(df_cilium_information$pos_x[df_cilium_information$layer == -1],
             df_cilium_information$pos_y[df_cilium_information$layer == -1],
             rep(1, length(df_cilium_information$pos_x[df_cilium_information$layer == -1]))),
    ncol = 3)
  coordinates_layer2 <- matrix(
    data = c(df_cilium_information$pos_x[df_cilium_information$layer == -1],
             df_cilium_information$pos_y[df_cilium_information$layer == -1],
             rep(2, length(df_cilium_information$pos_x[df_cilium_information$layer == -1]))),
    ncol = 3)
  
  
  if(export_normalized_images){
    Image_projection_cilia_normalized <- Image_projection_cilia
    Image_projection_cilia_normalized <- EBImage::normalize(
      Image_projection_cilia_normalized)
    
    Image_projection_cilia_normalized[coordinates_layer1] <- 1
    Image_projection_cilia_normalized[coordinates_layer2] <- 1
    
    EBImage::writeImage(x = Image_projection_cilia_normalized,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_projection_cilia_unconnected_normalized.tif",
                                                sep = "")),
                        bits.per.sample = 8,
                        type = "tiff")
  }
  
  
  Image_projection_cilia[coordinates_layer1] <- 1
  Image_projection_cilia[coordinates_layer2] <- 1
  
  rm(list = c("coordinates_layer1", "coordinates_layer2"))
  
  EBImage::writeImage(x = Image_projection_cilia,
                      files = file.path(output_dir,
                                        paste(input_file_name,
                                              "_projection_cilia_unconnected.tif",
                                              sep = "")),
                      bits.per.sample = 8,
                      type = "tiff")
  
  # ---------------------------------------------------------------------- #
  # -------------------------- 5. Save results --------------------------- #
  # ---------------------------------------------------------------------- #
  
  # 5. Save results --------------------------------------------------------
  # 5.1 Save a data frame with all cilia information -----------------------
  
  # Add information of all cilium coordinates as layer -99 to data frame
  # (Layer = -1 is from the projection)
  df_cilium_information <- rbind(df_cilium_information, df_cilium_all)
  
  # 5.2 Check if shape is cilium-like --------------------------------------
  df_cilium_information$cilium_shape <- FALSE
  
  for(i in unique(df_cilium_information$ciliumNumber)){
    x_max <- max(df_cilium_information$pos_x[
      df_cilium_information$ciliumNumber == i])
    x_min <- min(df_cilium_information$pos_x[
      df_cilium_information$ciliumNumber == i])
    
    y_max <- max(df_cilium_information$pos_y[
      df_cilium_information$ciliumNumber == i])
    y_min <- min(df_cilium_information$pos_y[
      df_cilium_information$ciliumNumber == i])
    
    # Calculate fill ratio
    number_of_cilium_pixels <- sum(df_cilium_information$layer[
      df_cilium_information$ciliumNumber == i] == -99)
    number_of_pixels_of_surrounding_rectangle <- abs((x_max-x_min+1)*(y_max-y_min+1))
    fill_ratio <- number_of_cilium_pixels/number_of_pixels_of_surrounding_rectangle
    
    if(fill_ratio < 0.8){
      df_cilium_information$cilium_shape[
        df_cilium_information$ciliumNumber == i] <- TRUE
    }else{
      # Calculate aspect ratio
      aspect_ratio <- abs((x_max-x_min+1)/(y_max-y_min+1))
      aspect_ratio <- ifelse(test = aspect_ratio < 1,
                             yes = 1/aspect_ratio,
                             no = aspect_ratio)
      if(aspect_ratio > 3/2){
        df_cilium_information$cilium_shape[
          df_cilium_information$ciliumNumber == i] <- TRUE
      }
    }
  }
  rm(i)
  
  # 5.2 Save the projection image with cilium information ------------------
  
  # Label all cilia coordinates in a new projection image
  if(use_histogram_equalization_for_threshold_calculation){
    Image_projection_cilia_connected <- Image_projection_histogram_equalization
  }else if(projection_method_for_threshold_calculation == "max"){
    Image_projection_cilia_connected <- Image_projection_max
  }else if(projection_method_for_threshold_calculation == "mean"){
    Image_projection_cilia_connected <- Image_projection_mean
  }
  
  # Cleaned ciliary objects
  Image_projection_cilia_connected_copy <- Image_projection_cilia_connected
  df_cilium_all_cilia <- df_cilium_information[
    df_cilium_information$layer == -99 & df_cilium_information$cilium_shape,]
  coordinates_layer1 <- matrix(
    data = c(df_cilium_all_cilia$pos_x,
             df_cilium_all_cilia$pos_y,
             rep(1, length(df_cilium_all_cilia$pos_x))),
    ncol = 3)
  coordinates_layer2 <- matrix(
    data = c(df_cilium_all_cilia$pos_x,
             df_cilium_all_cilia$pos_y,
             rep(2, length(df_cilium_all_cilia$pos_x))),
    ncol = 3)
  
  Image_projection_cilia_connected[coordinates_layer1] <- 1
  Image_projection_cilia_connected[coordinates_layer2] <- 1
  
  rm(list = c("coordinates_layer1", "coordinates_layer2", "df_cilium_all_cilia"))
  
  EBImage::writeImage(x = Image_projection_cilia_connected,
                      files = file.path(output_dir,
                                        paste(input_file_name,
                                              "_projection_cilia_connected_cleaned.tif",
                                              sep = "")),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  # All ciliary objects
  Image_projection_cilia_connected <- Image_projection_cilia_connected_copy
  coordinates_layer1 <- matrix(
    data = c(df_cilium_all$pos_x,
             df_cilium_all$pos_y,
             rep(1, length(df_cilium_all$pos_x))),
    ncol = 3)
  coordinates_layer2 <- matrix(
    data = c(df_cilium_all$pos_x,
             df_cilium_all$pos_y,
             rep(2, length(df_cilium_all$pos_x))),
    ncol = 3)
  
  if(export_normalized_images){
    Image_projection_cilia_connected_normalized <- Image_projection_cilia_connected
    Image_projection_cilia_connected_normalized <- EBImage::normalize(
      Image_projection_cilia_connected_normalized)
    
    Image_projection_cilia_connected_normalized[coordinates_layer1] <- 1
    Image_projection_cilia_connected_normalized[coordinates_layer2] <- 1
    
    EBImage::writeImage(x = Image_projection_cilia_connected_normalized,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_projection_cilia_connected_normalized.tif",
                                                sep = "")),
                        bits.per.sample = 8,
                        type = "tiff")
  }
  
  
  Image_projection_cilia_connected[coordinates_layer1] <- 1
  Image_projection_cilia_connected[coordinates_layer2] <- 1
  
  rm(list = c("coordinates_layer1", "coordinates_layer2", "Image_projection_cilia_connected_copy"))
  
  EBImage::writeImage(x = Image_projection_cilia_connected,
                      files = file.path(output_dir,
                                        paste(input_file_name,
                                              "_projection_cilia_connected.tif",
                                              sep = "")),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  # 5.3 Add cilium numbers to image ----------------------------------------
  Image_projection_numbers <- Image_projection_cilia_connected
  image_projection_numbers <- as.array(Image_projection_numbers)
  
  if(export_normalized_images){
    image_projection_numbers_normalized <- as.array(
      Image_projection_cilia_connected_normalized )
  }
  
  for(i in unique(df_cilium_all$ciliumNumber) ){
    ciliumNumber <- i
    pos_x <- df_cilium_all$pos_x[df_cilium_all$ciliumNumber == i][
      length(df_cilium_all$pos_x[df_cilium_all$ciliumNumber == i])]
    pos_y <- df_cilium_all$pos_y[df_cilium_all$ciliumNumber == i][
      length(df_cilium_all$pos_y[df_cilium_all$ciliumNumber == i])]
    
    image_projection_numbers <- addNumberToImage(
      image = image_projection_numbers,
      number = ciliumNumber,
      pos_x = pos_x,
      pos_y = pos_y,
      number_size_factor = number_size_factor,
      number_color = "red")
    
    if(export_normalized_images){
      image_projection_numbers_normalized <- addNumberToImage(
        image = image_projection_numbers_normalized,
        number = ciliumNumber,
        pos_x = pos_x,
        pos_y = pos_y,
        number_size_factor = number_size_factor,
        number_color = "red")
    }
    
    
  }
  rm(i)
  
  Image_projection_numbers <- EBImage::Image(data = image_projection_numbers,
                                             colormode = "color")
  EBImage::writeImage(x = Image_projection_numbers,
                      files = file.path(output_dir,
                                        paste(input_file_name,
                                              "_projection_cilia_all_numbers.tif",
                                              sep = "")),
                      bits.per.sample = 8,
                      type = "tiff")
  
  
  if(export_normalized_images){
    Image_projection_numbers_normalized <- EBImage::Image(
      data = image_projection_numbers_normalized,
      colormode = "color")
    EBImage::writeImage(x = Image_projection_numbers_normalized,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_projection_cilia_all_numbers_normalized.tif",
                                                sep = "")),
                        bits.per.sample = 8,
                        type = "tiff")
  }
  
  
  # 5.4 Add nuclei and cilium numbers to image -----------------------------
    
  if(!is.null(nucleus_color)){
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
          EBImage::imageData(nmask_watershed) == nuc_numbers[i], arr.ind = TRUE)
        
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
        
        image_projection_numbers <- addNumberToImage(
          image = image_projection_numbers,
          number = i,
          pos_x = df_nuclei_positions$pos_x[df_nuclei_positions$nuc_number_new == i],
          pos_y = df_nuclei_positions$pos_y[df_nuclei_positions$nuc_number_new == i],
          number_size_factor = number_size_factor,
          number_color = "green")
        
        image_projection_numbers <- addNumberToImage(
          image = image_projection_numbers,
          number = i,
          pos_x = df_nuclei_positions$pos_x[df_nuclei_positions$nuc_number_new == i],
          pos_y = df_nuclei_positions$pos_y[df_nuclei_positions$nuc_number_new == i],
          number_size_factor = number_size_factor,
          number_color = "blue")
        
        
        if(export_normalized_images){
          image_projection_numbers_normalized <- addNumberToImage(
            image = image_projection_numbers_normalized,
            number = i,
            pos_x = df_nuclei_positions$pos_x[df_nuclei_positions$nuc_number_new == i],
            pos_y = df_nuclei_positions$pos_y[df_nuclei_positions$nuc_number_new == i],
            number_size_factor = number_size_factor,
            number_color = "green")
          
          image_projection_numbers_normalized <- addNumberToImage(
            image = image_projection_numbers_normalized,
            number = i,
            pos_x = df_nuclei_positions$pos_x[df_nuclei_positions$nuc_number_new == i],
            pos_y = df_nuclei_positions$pos_y[df_nuclei_positions$nuc_number_new == i],
            number_size_factor = number_size_factor,
            number_color = "blue")
        }
        
        
      }
      
      
      rm(i)
    }
    
    # Add border of nuclei and save file
    Image_projection_numbers <- EBImage::Image(image_projection_numbers)
    EBImage::colorMode(Image_projection_numbers) <- "color"
    EBImage::colorMode(nmask_watershed) <- "gray"
    
    Image_projection_numbers <- EBImage::paintObjects(x = nmask_watershed,
                                                      tgt = Image_projection_numbers,
                                                      col='#ff00ff')
    
    # Display the number of nuclei
    print(paste("Number of nuclei: ", nucNo, sep=""))
    
    EBImage::writeImage(x = Image_projection_numbers,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_projection_cilia_all_numbers_nuclei.tif",
                                                sep = "")),
                        bits.per.sample = 8,
                        type = "tiff")
    
    
    if(export_normalized_images){
      # Add border of nuclei and save file
      Image_projection_numbers_normalized <- EBImage::Image(
        image_projection_numbers_normalized)
      EBImage::colorMode(Image_projection_numbers_normalized) <- "color"
      EBImage::colorMode(nmask_watershed) <- "gray"
      
      Image_projection_numbers_normalized <- EBImage::paintObjects(
        x = nmask_watershed,
        tgt = Image_projection_numbers_normalized,
        col='#ff00ff')
      
      EBImage::writeImage(x = Image_projection_numbers_normalized,
                          files = file.path(output_dir,
                                            paste(input_file_name,
                                                  "_projection_cilia_all_numbers_nuclei_normalized.tif",
                                                  sep = "")),
                          bits.per.sample = 8,
                          type = "tiff")
    }
    
  }
  
  
  # 5.6 Normalize and histogram equalized image ----------------------------
  
  Image_projection_histogram_equalization_normalized <- EBImage::normalize(
    Image_projection_histogram_equalization)
  
  EBImage::writeImage(x = Image_projection_histogram_equalization_normalized,
                      files = file.path(output_dir,
                                        paste(input_file_name,
                                              "_projection_histogram_equalized_normalized.tif",
                                              sep = "")),
                      bits.per.sample = 8,
                      type = "tiff")
  
  EBImage::writeImage(x = Image_projection,
                      files = file.path(output_dir,
                                        paste(input_file_name,
                                              "_projection.tif",
                                              sep = "")),
                      bits.per.sample = 8,
                      type = "tiff")
  
  if(export_normalized_images){
    Image_projection_normalized <- EBImage::normalize(
      Image_projection)
    
    EBImage::writeImage(x = Image_projection_normalized,
                        files = file.path(output_dir,
                                          paste(input_file_name,
                                                "_projection_normalized.tif",
                                                sep = "")),
                        bits.per.sample = 8,
                        type = "tiff")
  }
  
  
  # 5.7 Save function call and used parameter values -----------------------
  
  # Save all parameters in a csv
  
  # Original parameter
  function_call <- paste(deparse(match.call()), collapse = "")
  function_call <- gsub(pattern = " +", replacement = " ", x = function_call)
  df_OriginalParameterList <- data.frame(
    "Original_function_call" = function_call)
  
  # All parameter values
  parameter_names <- c("input_dir_tif", "input_file_czi", "cilium_color",
                       "nucleus_color",
                       "projection_method_for_threshold_calculation",
                       "threshold_by_density_of_cilium_pixels",
                       "threshold_find", "threshold_connect",
                       "vicinity_combine", "vicinity_connect",
                       "min_cilium_area_in_pixels", "max_cilium_area_in_pixels",
                       "number_size_factor", "pixel_size",
                       "slice_distance", "nuc_mask_width_height_in_pixels")
  
  
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
    readr::write_csv(df_parameterList,
                     file = file.path(output_dir, "parameter_list.csv"))
    
    readr::write_csv2(df_parameterList,
                      file = file.path(output_dir, "parameter_list_de.csv"))
  }
  
  # 5.9 Save the number of nuclei ------------------------------------------
  if(!is.null(nucleus_color)){
    # Add file name to tibble
    
    mean_nuclei_projection_diameter_in_um = sqrt(4 * mean_nucleus_area_in_pixels * pixel_size * pixel_size / pi)
    mean_nuclei_projection_diameter_in_um = round(x = mean_nuclei_projection_diameter_in_um, digits = 1)
    
    df_number_nuclei <- data.frame(
      "number_Of_nuclei" = nucNo,
      "mean_nuclei_projection_area_in_pixels" = mean_nucleus_area_in_pixels,
      "mean_nuclei_projection_equivalent_diameter_in_um" = mean_nuclei_projection_diameter_in_um)

    if(is.null(input_dir_tif)){
      df_number_nuclei$fileName <- basename(input_file_czi)
      df_number_nuclei <- df_number_nuclei %>% 
        dplyr::relocate(.data$fileName)
    }else if(is.null(input_file_czi)){
      df_number_nuclei$dirName <- basename(input_dir_tif)
      df_number_nuclei <- df_number_nuclei %>% 
        dplyr::relocate(.data$dirName)
    }
    
    if(!is.null(df_number_nuclei)){
      readr::write_csv(df_number_nuclei,
                       file = file.path(output_dir, "nuclei_number.csv"))
      readr::write_csv2(df_number_nuclei,
                        file = file.path(output_dir, "nuclei_number_de.csv"))
    }
  }
  
  # 5.10 Calculate and save length information of cilia --------------------
  
  # Get the length of the cilia
  df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                                 min_cilium_area_in_pixels,
                                                 pixel_size,
                                                 slice_distance,
                                                 number_of_z_stack_layers = dim(image_data)[4])
  # Add file name to tibble
  if(is.null(input_dir_tif)){
    df_cilium_summary$fileName <- basename(input_file_czi)
    df_cilium_summary <- df_cilium_summary %>% 
      dplyr::relocate(.data$fileName)
  }else if(is.null(input_file_czi)){
    df_cilium_summary$dirName <- basename(input_dir_tif)
    df_cilium_summary <- df_cilium_summary %>% 
      dplyr::relocate(.data$dirName)
  }
  
  
  if(!is.null(df_cilium_summary)){
    readr::write_csv(df_cilium_summary,
                     file = file.path(output_dir, "cilium_summary.csv"))
    
    readr::write_csv2(df_cilium_summary,
                      file = file.path(output_dir, "cilium_summary_de.csv"))
  }
  
  
  # Combine all data.frame to a list
  output_list <- list("df_parameterlist" = df_parameterList,
                      "df_cilium_information" = df_cilium_information,
                      "df_cilium_summary" = df_cilium_summary)
  
  return(output_list)
}

#TODO Nuclei_projection_area_in_pixels
