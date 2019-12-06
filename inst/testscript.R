# Testscript for using the R package detectCilia +++++++++++++++++++++++++++
# Author: Kai Budde
# Last changed: 2019/12/06


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

# Threshold to find cilia in stack image (max intensities of all layers)
threshold_find <- 0.4

# Lower bound for finding pixels that belong to found cilia in every layer
threshold_connect <- 0.1

# How many pixels to skip for joining seperate cilium pixels to one cilium
vicinity <- 1

# Minimum/Maximum size of cilia (in pixel)
min_size <- 3
max_size <- 50

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

list.of.packages <- c("devtools")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(devtools)

devtools::install_github("SFB-ELAINE/stackImages") 
library(stackImages)

devtools::install_github("SFB-ELAINE/detectCilia") 
library(detectCilia)


## FIRST EXAMPLE DIRECRY -------------------------------------------------

# Obtain all positions of cilia in every z-layer
df_cilium_information <- detectCilia(input_dir = input_dir,
                                     cilium_color = cilium_color,
                                     threshold_find = threshold_find,
                                     threshold_connect = threshold_connect,
                                     vicinity = vicinity,
                                     min_size = min_size,
                                     max_size = max_size)

# Get the length of the cilia
df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                               pixel_size,
                                               sclice_distance)

write.csv(df_cilium_summary,
          file = paste(input_dir, "/output/cilium_summary.csv", sep=""))

write.csv2(df_cilium_summary,
          file = paste(input_dir, "/output/cilium_summary_de.csv", sep=""))
