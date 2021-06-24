# Testscript for using the R package detectCilia for development  ++++++++++
# Author: Kai Budde
# Last changed: 2021/06/23

# TODO: Cilien mit vielen Löchern, wenn man die Pixel umrandet, müssen entfernt werden


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Directory of the images
input_dir <- "E:/PhD/Daten/Cilia/allImages"
input_dir <- "E:/PhD/Daten/Cilia/test"
#input_dirs <- "tests/clemens"

#input_dirs <- c(
  #"/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_1",
  #"/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_8",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_9",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_10",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_365_x63_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_365_x63_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_365_x63_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_365_x63_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_365_x63_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_365_x63_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181005_tiff/181005_ES3_368_x63_zstack_8",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181017_tiff/181017_ES3_365_x63_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190702_tiff/190702_EV38_Kollagen_mit_WF_x40_z-stack",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190702_tiff/190702_EV38_Kollagen_mit_WF_x63_z-stack",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190730_tiff/190730_EV38_1_Kollagen mit WF_63x_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190730_tiff/190730_EV38_1_Kollagen mit WF_63x_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190730_tiff/190730_EV38_1_Kollagen mit WF_63x_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190730_tiff/190730_EV38_1_Kollagen mit WF_63x_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190731_tiff/190731_EV38_1_Kollagen mit WF_63x_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit WF_63x_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit WF_63x_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit WF_63x_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit WF_63x_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit WF_63x_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit WF_63x_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Glas mit WF_63x_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit Asc+Dexa_63x_zstack_8",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit Asc+Dexa_63x_zstack_9",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit Asc+Dexa_63x_zstack_10",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit Asc+Dexa_63x_zstack_11",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit Asc+Dexa_63x_zstack_12",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190802_tiff/190802_EV38_1_Kollagen mit Asc+Dexa_63x_zstack_13",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen nur Asc_63x_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen nur Asc_63x_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen nur Asc_63x_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen nur Asc_63x_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen nur Asc_63x_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen nur Asc_63x_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen mit FKS_63x_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen mit FKS_63x_zstack_8",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen mit FKS_63x_zstack_9",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen mit FKS_63x_zstack_11",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_1",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_2",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_3",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_4",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_5",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_6",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_7",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_8",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_9",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_10",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_11",
  # "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_12"
#  )

# Size of a pixel in micrometer
#pixel_size <- 0.21964505359339307678791073625022 # in \mu m
#pixel_size <- 0.109823521291513 # in \mu m

# Distance between layer in micrometer
#sclice_distance <- 0.436356 # in \mu m
#slice_distance <- 0.281 # in \mu m
#slice_distance <- 0.429804210436651 # in \mu m
cilium_color <- "red"
nucleus_color <- "blue"

# Use internal calculation of threshold by looking at cilium color pixel
# density
threshold_by_density_of_cilium_pixels <- TRUE

# How many pixels to skip for joining separate cilium pixels to one cilium
vicinity <- 1

# Minimum/Maximum size of cilia (in pixel)
min_size <- 10 # if no cilium is found: try out 5
max_size <- 150

# Scaling factor for digit numbers
number_size_factor <- 0.15

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load packages #############################################################

list.of.packages <- c("tiff", "dplyr", "devtools", "BiocManager", "xlsx", "zis", "reticulate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!("EBImage" %in% utils::installed.packages())){
  print("Installing EBImage.")
  BiocManager::install("EBImage")
  #BiocManager::install("MaxContrastProjection")
}

require(tiff)
require(dplyr)
require(devtools)
require(EBImage)
require(xlsx)
require(reticulate)

# Read in Python package for reading czi files
# (Users will be asked to install miniconda
# when starting for the first time)
reticulate::py_install("czifile")

# Install the R package for producing stacks of the images
if(!("stackImages" %in% installed.packages()[,"Package"])){
  #if(!installed.packages()[,"Version"][installed.packages()[,"Package"] == "stackImages"] == "0.1.4"){
    devtools::install_github("SFB-ELAINE/stackImages", ref = "v0.1.4")
  #}
}
require(stackImages)

# Install the R package for reading czi images
if(!("readCzi" %in% installed.packages()[,"Package"])){
  #if(!installed.packages()[,"Version"][installed.packages()[,"Package"] == "readCzi"] == "0.1.2"){
    devtools::install_github("SFB-ELAINE/readCzi", ref = "v0.1.2")
  #}
}
require(readCzi)

# Check package
#check()

# Document package
#document()

# Load package to use it
load_all()

## FIRST EXAMPLE DIRECTORY FOR CZI IMAGES ------------------------------------

# Data frame with certain parameter values
file_names <- list.files(path = input_dir)
file_names_czi <- file_names[grepl("czi", file_names)]
file_names_czi <- paste(input_dir, file_names_czi, sep="/")
number_of_czi_files <- length(file_names_czi)

#number_of_dirs <- length(number_of_czi_files)
df_results <- data.frame("czi file" = file_names_czi,
                         "threshold_find" = rep(-99, number_of_czi_files),
                         "threshold_connect" = rep(-99, number_of_czi_files))

# Get metadata


for(i in 1:number_of_czi_files){
  if(i==1){
    df_metadata <- readCziMetadata(input_file = file_names_czi[i])
  }else{
    df_dummy <- readCziMetadata(input_file = file_names_czi[i])
    df_metadata <- rbind(df_metadata, df_dummy)
    rm(df_dummy)
  }
}
rm(i)

write.csv(x = df_metadata,
          file = paste(input_dir,"/","summary_metadata_en.csv", sep=""),
          row.names = FALSE)
write.csv2(x = df_metadata,
           file = paste(input_dir,"/","summary_metadata_de.csv", sep=""),
           row.names = FALSE)

# Obtain all positions in every z-layer and lengths of all cilia
for(i in 1:number_of_czi_files){
  
  if(df_metadata$scaling_x[i] == df_metadata$scaling_y[i]){
    pixel_size <- df_metadata$scaling_x[i]
  }else{
    print("Sclaing is wrong.")
  }
  
  slice_distance <- df_metadata$scaling_z[i]
  

  output_list <- detectCilia(input_file_czi = file_names_czi[i],
                             cilium_color = cilium_color,
                             threshold_by_density_of_cilium_pixels = threshold_by_density_of_cilium_pixels,
                             vicinity = vicinity,
                             min_size = min_size,
                             max_size = max_size,
                             number_size_factor = number_size_factor,
                             pixel_size = pixel_size,
                             slice_distance = slice_distance)

  if(!is.null(output_list)){
    df_results$threshold_find[i] <-
      output_list$df_parameterlist$parameterValues[
        output_list$df_parameterlist$parameterNames == "threshold_find"]
    df_results$threshold_connect[i] <-
      output_list$df_parameterlist$parameterValues[
        output_list$df_parameterlist$parameterNames == "threshold_connect"]
  }

}
rm(i)


# ## FIRST EXAMPLE DIRECRY FOR TIF IMAGES ------------------------------------

# # Data frame with certain parameter values
# number_of_dirs <- length(input_dirs)
# df_results <- data.frame("Directory" = input_dirs,
#                          "threshold_find" = rep(-99, number_of_dirs),
#                          "threshold_connect" = rep(-99, number_of_dirs)
#                          )
# 
# # Obtain all positions in every z-layer and lengths of all cilia
# for(i in 1:number_of_dirs){
#   input_dir_tif <- input_dirs[i]
#   output_list <- detectCilia(input_dir_tif = input_dir_tif,
#                              cilium_color = cilium_color,
#                              threshold_by_density_of_cilium_pixels = threshold_by_density_of_cilium_pixels,
#                              vicinity = vicinity,
#                              min_size = min_size,
#                              max_size = max_size,
#                              number_size_factor = number_size_factor,
#                              pixel_size = pixel_size,
#                              slice_distance = slice_distance)
#   
#   if(!is.null(output_list)){
#     df_results$threshold_find[df_results$Directory == input_dir] <-
#       output_list$df_parameterlist$parameterValues[
#         output_list$df_parameterlist$parameterNames == "threshold_find"]
#     df_results$threshold_connect[df_results$Directory == input_dir] <-
#       output_list$df_parameterlist$parameterValues[
#         output_list$df_parameterlist$parameterNames == "threshold_connect"]
#   }
# 
# }




# ## Test script for metadata ONLY! ------------------------------------------
# number_of_dirs <- length(input_dirs)
# for(i in 1:number_of_dirs){
#   input_dir <- input_dirs[i]
#   
#   file_names <- list.files(path = input_dir)
#   file_names <- file_names[grepl("czi", file_names)]
#   number_of_czi_files <- length(file_names)
#   
#   input_files <- paste(input_dir, "/", file_names, sep="")
#   
#   for(j in 1:number_of_czi_files){
#     if(j==1){
#       df_metadata <- readCziMetadata(input_file = input_files[j])
#     }else{
#       df_dummy <- readCziMetadata(input_file = input_files[j])
#       df_metadata <- rbind(df_metadata, df_dummy)
#       rm(df_dummy)
#     }
#   }
#   
#   write.csv(x = df_metadata,
#             file = paste(input_dir,"/","summary_metadata_en.csv", sep=""),
#             row.names = FALSE)
#   write.csv2(x = df_metadata,
#              file = paste(input_dir,"/","summary_metadata_de.csv", sep=""),
#              row.names = FALSE)
# }
# 
# 
# ## Test script for czi to tif conversion ONLY! -------------------------------
# number_of_dirs <- length(input_dirs)
# for(i in 1:number_of_dirs){
#   input_dir <- input_dirs[i]
#   
#   file_names <- list.files(path = input_dir)
#   file_names <- file_names[grepl("czi", file_names)]
#   number_of_czi_files <- length(file_names)
#   
#   input_files <- paste(input_dir, "/", file_names, sep="")
#   
#   for(j in 1:number_of_czi_files){
#     if(j==1){
#       df_metadata <- readCziMetadata(input_file = input_files[j])
#     }else{
#       df_dummy <- readCziMetadata(input_file = input_files[j])
#       df_metadata <- rbind(df_metadata, df_dummy)
#       rm(df_dummy)
#     }
#   }
#   
#   write.csv(x = df_metadata,
#             file = paste(input_dir,"/","summary_metadata_en.csv", sep=""),
#             row.names = FALSE)
#   write.csv2(x = df_metadata,
#              file = paste(input_dir,"/","summary_metadata_de.csv", sep=""),
#              row.names = FALSE)
# }
# 
# 
