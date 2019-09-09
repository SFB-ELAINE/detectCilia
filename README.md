# detectCilia
R package to detect cilia and other colored structures in confocal
fluorescence microscopy images

## Code for using the R package


```R
# Testscript V.1.1 #########################################################


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# >>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Directory of the images
input_dir <- "images"
# Size of a pixel in micrometer
pixel_size <- 0.21964505359339307678791073625022 # in \mu m
# Distance between layer in micrometer
sclice_distance <- 0.31607# in \mu m
cilia_color = "red"
# <<<<<<<<<<<<<<<<

list.of.packages <- c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(devtools)

devtools::install_github("SFB-ELAINE/stackImages") 
library(stackImages)

devtools::install_github("SFB-ELAINE/detectCilia") 
library(detectCilia)


## FIRST EXAMPLE DIRECRY -------------------------------------------------

# Threshold to find cilia
threshold_find <- 0.9

# Lower bound for finding pixels that belong to found cilia
threshold_connect <- 0.1

# Minimum size of cilia (in pixel)
min_size = 10

# Obtain all positions of cilia in every z-layer
df_cilium_information <- detectCilia(input_dir = input_dir,
                                     cilia_color = cilia_color,
                                     threshold_find = threshold_find,
                                     threshold_connect = threshold_connect)

# Get the length of the cilia
df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                               pixel_size,
                                               sclice_distance)

write.csv(df_cilium_summary,
          file = paste(input_dir, "/output/cilium_summary.csv", sep=""))

```
