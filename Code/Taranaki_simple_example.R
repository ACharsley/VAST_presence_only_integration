
library(VAST)
library(tidyverse)

rm(list=ls())


##########################
#  Directories and paths #
##########################

data_taranaki_dir <- "./Data_processed/Taranaki"
model_path <- "./Models"

# Create model path
path <- file.path(model_path, "Model1a_1_ds")
dir.create(path, showWarnings = FALSE)


#############
# Load data #
#############

load(file.path(data_taranaki_dir, "Taranaki_data_1a.RData"))


#######################
# Set up model inputs #
#######################

#Set stream network
network <- Taranaki_data_1a_with_ds$network_ds
#network <- Taranaki_data_1a_with_ds$network
#network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network_aa.rds"))
#network <- readRDS(file.path(data_taranaki_dir, "testing/Taranaki_network_finaltest.rds"))


#Set data input
Data_inp <- Taranaki_data_1a_with_ds$obs_ds
#Data_inp <- Taranaki_data_1a_with_ds$obs
# set.seed(4563)
# Data_inp <- network[sample(nrow(network), 1000),c("Lat", "Lon","child_s")] %>% rename("child_i" = child_s)


Network_sz = network[,c("parent_s","child_s","dist_s")]
Network_sz_LL = network[,c("parent_s","child_s","dist_s", "Lat", "Lon")]


#Set model settings
Version = get_latest_version() ; Version #"VAST_v14_0_1"

FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) 
RhoConfig <- c("Beta1" = 0, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0)

ObsModel <- c(2,0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1)


############################
# Build extrapolation grid #
############################
input_grid <- data.frame("Lat" = Data_inp$Lat, 
                         "Lon" = Data_inp$Lon, 
                         "child_i" = Data_inp$child_i, 
                         "Area_km2" = rep(0.125,nrow(Data_inp)))

Extrapolation_List = make_extrapolation_info(Region = "stream_network", 
                                             input_grid = input_grid)


#############################
# Build spatial information #
#############################
Spatial_List = make_spatial_info(n_x = nrow(Network_sz),
                                 Lon_i = Data_inp$Lon,
                                 Lat_i = Data_inp$Lat,
                                 Extrapolation_List = Extrapolation_List,
                                 Method = "Stream_network",
                                 grid_size_km = 1,
                                 fine_scale = FALSE,
                                 Network_sz_LL = Network_sz_LL,
                                 DirPath = paste0(path, "/"),
                                 Save_Results = TRUE)
