# Testscript for using the R package detectCilia for development  ++++++++++
# Author: Kai Budde
# Last changed: 2020/10/02


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Directory of the images
input_dirs <- c(
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_1",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/181004_tiff/181004_ES3_367_x63_zstack_2",
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
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190808_tiff/190808_EV38_1_Kollagen mit FKS_63x_zstack_11",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_1",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_2",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_3",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_4",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit WF_63x_zstack_5",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_6",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_7",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_8",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_9",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_10",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_11",
  "/home/kb/Documents/projects/2020CiliaImages/Neue_Auswertung_Cilien/190815_tiff/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_12"
  )

#input_dir <- "tests/20200915/190818_EV38_2_Kollagen mit FKS_zstack_7"
#input_dir <- "/home/kb/Unibox Rostock/Rohbilder + Export - Zen (Clemens Sehlke)/190815_EV38_2_Kollagen mit WF_63x_zstack_1_6"
#input_dir <- "/home/kb/Unibox Rostock/Rohbilder + Export - Zen (Clemens Sehlke)/190818_EV38_2_Kollagen mit FKS_zstack_7"
#input_dir <- "inst/testImages"
#input_dir <- "/home/kb/Documents/projects/2018detectCilia/20200519_BilderVonClemens/EV52_1_links_63x_zstack_Wiederholung_2"

# Size of a pixel in micrometer
pixel_size <- 0.21964505359339307678791073625022 # in \mu m

# Distance between layer in micrometer
#sclice_distance <- 0.436356# in \mu m
slice_distance <- 0.281# in \mu m
cilium_color <- "red"
nucleus_color <- "blue"

# Use internal calculation of threshold by looking at cilium color pixel
# density
threshold_by_density_of_cilium_pixels <- TRUE

# How many pixels to skip for joining seperate cilium pixels to one cilium
vicinity <- 3

# Minimum/Maximum size of cilia (in pixel)
min_size <- 10 # if no cilium is found: try out 5
max_size <- 150

# Scaling factor for digit numbers
number_size_factor <- 0.15

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load packaes #############################################################

list.of.packages <- c("tiff", "dplyr", "devtools", "EBImage", "xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(tiff)
require(dplyr)
require(devtools)
require(EBImage)
require(xlsx)

# Install the R package for producing stacks of the images
if(!("stackImages" %in% installed.packages()[,"Package"])){
  if(!installed.packages()[,"Version"][installed.packages()[,"Package"] == "stackImages"] == "0.1.4"){
    devtools::install_github("SFB-ELAINE/stackImages", ref = "v0.1.4")
  }
}
require(stackImages)

# Check package
#check()

# Document package
document()

# Load package to use it
load_all()

## FIRST EXAMPLE DIRECRY -------------------------------------------------

# Data frame with certain parameter values
number_of_dirs <- length(input_dirs)
df_results <- data.frame("Directory" = input_dirs,
                         "threshold_find" = rep(-99, number_of_dirs),
                         "threshold_connect" = rep(-99, number_of_dirs)
                         )

# Obtain all positions in every z-layer and legths of al cilia
for(i in 1:number_of_dirs){
  input_dir <- input_dirs[i]
  output_list <- detectCilia(input_dir = input_dir,
                             cilium_color = cilium_color,
                             threshold_by_density_of_cilium_pixels = threshold_by_density_of_cilium_pixels,
                             vicinity = vicinity,
                             min_size = min_size,
                             max_size = max_size,
                             number_size_factor = number_size_factor,
                             pixel_size = pixel_size,
                             slice_distance = slice_distance)
  if(!is.null(output_list)){
    df_results$threshold_find[df_results$Directory == input_dir] <-
      output_list$df_parameterlist$parameterValues[
        output_list$df_parameterlist$parameterNames == "threshold_find"]
    df_results$threshold_connect[df_results$Directory == input_dir] <-
      output_list$df_parameterlist$parameterValues[
        output_list$df_parameterlist$parameterNames == "threshold_connect"]
  }
  
}

