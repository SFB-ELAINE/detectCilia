#' @title detectObjects
#' @description Function for detecting colored objects in microscopy images
#' @details Input should be tif-format.
#' The output will be written in the current directory.
#' Please be aware that the coordinate system is turned by 90° to the right.
#' The origin of the x- and y-axes is in the upper-left corner of the image.
#' (The x-axis points downwards and the y-axis to the right.)
#' @aliases objectDetect detectobject
#' @author Kai Budde
#' @export detectObjects
#' @param input_dir A character
#' @param cilium_color A character
#' @param threshold A number
#' @examples
#' \dontrun{
#' detectObject(input.file = "example.tif")
#' }
#'
#'

detectObjects <- function(input_dir = NULL,
                        cilium_color = "blue",
                        threshold = 0.1) {



  # Basics and sourcing functions ------------------------------------------
  .old.options <- options()
  on.exit(options(.old.options))

  options(stringsAsFactors = FALSE, warn=-1)

  # ---------------------------------------------------------------------- #
  # ---------------------- Data acquisition ------------------------------ #
  # ---------------------------------------------------------------------- #

  # Data input -------------------------------------------------------------

  # Input directory must be submitted. If not: close function call.
  if(is.null(input_dir)){
    print(paste("Please call the function with an input directory ",
                "which contains images of cells with colored cilia",
                sep=""))
    return()
  }

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

  # Save the file names (tifs) ----------------------------------
  file_names <- list.files(path = input_dir)
  file_names <- file_names[grepl("tif", file_names)]


  # ---------------------------------------------------------------------- #
  # ---------------------- Data manipulation ----------------------------- #
  # ---------------------------------------------------------------------- #

  # i = 1 .. n(images) Go through all images (tifs) ----------------------
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
    image_cilia <- editImage(image = image, cilium_color = cilium_color,
                             threshold = threshold)

    # Save the cilia image
    save_file_name <- gsub("\\.tif+", "", file_names[i])
    save_file_name <- paste(save_file_name, "_edited.tif", sep="")

    tiff::writeTIFF(what = image_cilia,
                    where = paste(output_dir, save_file_name, sep = ""),
                    bits.per.sample = 8L, compression = "none",
                    reduce = TRUE)

    if(i == 1){
      sum_image_cilia <- image_cilia
    }else{
      sum_image_cilia <- sum_image_cilia + image_cilia
    }

  }

  # Write sum of all cilia images
  sum_image_cilia[,][sum_image_cilia[,] > 1] <- 1
  tiff::writeTIFF(what = sum_image_cilia,
                  where = paste(output_dir, "threshold",
                                threshold, "_sum_.tif", sep = ""),
                  bits.per.sample = 8L, compression = "none",
                  reduce = TRUE)

}
