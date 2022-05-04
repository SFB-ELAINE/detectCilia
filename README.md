# detectCilia
R package to detect cilia and other colored structures in confocal
fluorescence microscopy images. Works with R 4.0.0.

## Code for using the R package


```R
# Testscript for using the R package detectCilia +++++++++++++++++++++++++++
# Author: Kai Budde
# Last changed: 2022/05/04
# Version of detectCilia: 0.6.2


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Directory of the images
input_dir <- "images"

# Size of a pixel in micrometer
pixel_size <- 0.21964505359339307678791073625022 # in \mu m

# Distance between layer in micrometer
sclice_distance <- 0.31607# in \mu m
cilium_color <- "red"

# Use internal calculation of threshold by looking at cilium color pixel
# density
threshold_by_density_of_cilium_pixels <- TRUE

# Threshold to find cilia in stack image (max intensities of all layers)
# threshold_find <- 0.01
# <- unnecessary if threshold_by_density_of_cilium_pixels == TRUE

# Lower bound for finding pixels that belong to found cilia in every layer
# threshold_connect <- 0.005
# <- unnecessary if threshold_by_density_of_cilium_pixels == TRUE

# How many pixels to skip for joining seperate cilium pixels to one cilium
vicinity <- 2

# Minimum/Maximum size of cilia (in pixel)
min_size <- 20
max_size <- 150

# Scaling factor for digit numbers
number_size_factor <- 0.15

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

list.of.packages <- c("devtools")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(devtools)

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.11")
BiocManager::install("EBImage")
library(EBImage)
# If you encounted problems, see https://megapteraphile.wordpress.com/2019/09/29/challenges-installing-ebimage-in-r/


# Install the R package for producing stacks of the images
devtools::install_github("SFB-ELAINE/stackImages", ref = "v0.1.4")
require(stackImages)

devtools::install_github("SFB-ELAINE/detectCilia", ref = "v0.4.2") 
library(detectCilia)


## FIRST EXAMPLE DIRECRY -------------------------------------------------

# Obtain all positions of cilia in every z-layer
df_cilium_information <- detectCilia(input_dir = input_dir,
                                     cilium_color = cilium_color,
                                     threshold_by_density_of_cilium_pixels = threshold_by_density_of_cilium_pixels,
                                     vicinity = vicinity,
                                     min_size = min_size,
                                     max_size = max_size,
                                     number_size_factor = number_size_factor)

# Get the length of the cilia
df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                               pixel_size,
                                               sclice_distance)

write.csv(df_cilium_summary,
          file = paste(input_dir, "/output/cilium_summary.csv", sep=""))

write.csv2(df_cilium_summary,
          file = paste(input_dir, "/output/cilium_summary_de.csv", sep=""))

```
