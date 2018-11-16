# detectCilia
R package to detect cilia and other colored structures in confocal
fluorescence microscopy images

## Code for using the R package


```R
# Testscript V.1
rm(list = ls())
graphics.off()

# >>>>>>>>>>>>>>>>
# Please adapt the directory of the downloaded package
directory <- "~/detectCilia-master/"
# <<<<<<<<<<<<<<<<

list.of.packages <- c("tiff", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(devtools)

setwd(directory)
load_all()

## FIRST EXAMPLE DIRECTORY -------------------------------------------------

input_dir <- "inst/testImages"

# Threshold to find cilia
threshold_find <- 0.5
# Lower bound for finding pixels that belong to found cilia
threshold_connect <- 0.1
pixel_size <- 0.219647 # in \mu m
sclice_distance <- 0.20944 # in \mu m

# Obtain all positions of cilia in every z-layer
df_cilium_information <- detectCilia(input_dir = input_dir,
                                     cilia_color = "red",
                                     threshold_find = threshold_find,
                                     threshold_connect = threshold_connect)

# Get the length of the cilia
df_cilium_summary <- summarizeCiliaInformation(df_cilium_information,
                                               pixel_size,
                                               sclice_distance)

write.csv(df_cilium_summary,
          file = paste(input_dir, "/output/cilium_summary.csv", sep=""))

```
