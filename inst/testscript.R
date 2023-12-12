# Testscript for using the R package detectCilia +++++++++++++++++++++++++++
# Author: Kai Budde-Sagert
# Created: 2019/12/01
# Last changed: 2023/12/02


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Install packages #########################################################
groundhog.day <- "2023-01-01"
if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("BiocManager", "devtools", "dplyr", "magrittr",
          "readr", "rlang", "reticulate")
groundhog.library(pkgs, groundhog.day)


if(!("EBImage" %in% utils::installed.packages())){
  print("Installing EBImage.")
  BiocManager::install("EBImage")
}

require(EBImage)

# Install Python package for reading czi files
# (Users will be asked to install miniconda when starting for the first time)
if(! "czifile" %in% reticulate::py_list_packages()$package){
  reticulate::py_install("czifile")
}

# Install the R package for reading czi images
devtools::install_github("SFB-ELAINE/readCzi", upgrade = "ask")
require(readCzi)

# Install this R package for detecting cilia in microscopy images
devtools::install_github("SFB-ELAINE/detectCilia", upgrade = "ask")
require(detectCilia)
# Alternatively:
# devtools::load_all()
# devtools::document()
# devtools::check()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

### TIF DIRECTORY ###

# Directory of the images
input_dir <- system.file("extdata", "testImagesTif",
                          package = "detectCilia", mustWork = TRUE)

# Directory for the output
output_dir <- file.path(getwd(), "output")

# Size of a pixel in micrometer
pixel_size <- 0.219645 # in \mu m

# Distance between two layers in micrometers
slice_distance <- 0.31607# in \mu m

cilium_color <- "red"

# Manually set mask width here because image is small
nuc_mask_width_height <- 100


### CZI FILE ###

# Directory for the output
output_dir <- file.path(getwd(), "output")

# Directory of the images
input_file <- system.file("extdata", "testImageCzi", "CiliaImage.czi",
                          package = "detectCilia", mustWork = TRUE)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## 1. Example: Find cilia in TIFs of one z-stack image ---------------------

# Obtain all positions of cilia in every z-layer
detectCilia_output_list <- detectCilia::detectCilia(
  input_dir_tif = input_dir,
  output_dir = output_dir,
  cilium_color = cilium_color,
  pixel_size = pixel_size,
  slice_distance = slice_distance,
  nuc_mask_width_height = nuc_mask_width_height)

## SECOND EXAMPLE CZI FILE -------------------------------------------------

# Obtain all positions of cilia in every z-layer
detectCilia_output_list2 <- detectCilia::detectCilia(input_file_czi = input_file,
                                                     output_dir = output_dir)
