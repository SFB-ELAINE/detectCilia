# detectCilia
**detectCilia** is an R package to detect cilia (or other colored structures) in confocal fluorescence microscopy images.

## Requirements
It is necessary to load a TIF (or a directory of TIF files) or a CZI file of either a z stack or its projection. Both the nuclei and the cilia (or other small structures) need to be labelled and captured in a specific channel.

## Example code for using the R package

The following example (script) shows the capabilities of the package. The script is found in `inst/testscript.R`. The results of the detection will be written in the current directory (`output/`).

```R
# Testscript for using the R package detectCilia +++++++++++++++++++++++++++
# Author: Kai Budde-Sagert
# Created: 2019/12/01
# Last changed: 2024/03/20


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
devtools::install_github("SFB-ELAINE/readCzi@v0.4.0")
require(readCzi)

# Install this R package for detecting cilia in microscopy images
devtools::install_github("SFB-ELAINE/detectCilia", upgrade = "ask")
require(detectCilia)

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Directory for the output
output_dir <- file.path(getwd(), "output")

### TIF DIRECTORY ###

# Directory of the images
input_dir <- system.file("extdata", "testImagesTif",
                          package = "detectCilia", mustWork = TRUE)

# Size of a pixel in micrometer
pixel_size <- 0.219645 # in \mu m

# Distance between two layers in micrometers
slice_distance <- 0.31607# in \mu m

# Manually set mask width here because image is small
nuc_mask_width_height <- 100


### CZI FILE ###

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
  nuc_mask_width_height = nuc_mask_width_height,
  number_size_factor = 0.2)

## SECOND EXAMPLE CZI FILE -------------------------------------------------

# Obtain all positions of cilia in every z-layer
detectCilia_output_list2 <- detectCilia::detectCilia(input_file_czi = input_file,
                                                     output_dir = output_dir)

```
