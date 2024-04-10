

rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)
library(sf)
library(units)


#################
#  Directories  #
#################

raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network_to_sample <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE) 

##Road data
NZ_roads_data <- read_sf(dsn = raw_data_dir, layer = "nz-primary-road-parcels") #Data extracted on 14/11/22 from https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/


NZ_roads_coords <- as.data.frame(sf::st_coordinates(NZ_roads_data)) %>% select(X,Y) %>% rename("Lat"=Y, "Lon"=X)



NZ_roads_coords_old <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #Save as sf object
NZ_roads_coords_old <- st_transform(NZ_roads_coords_old, 3857) #Ensure I'm using the correct projection for distance calcs

NZ_roads_coords_new <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4167) # the crs recorded at: https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/#:~:text=polygon%20layer%20%20Multipolygon-,CRS%20as%20stored,-NZGD2000EPSG%3A4167
NZ_roads_coords_new <- st_transform(NZ_roads_coords_new, 3857) #Ensure I'm using the correct projection for distance calcs





#########################
# Set network to sample #
#########################

network_to_sample <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE) 
#I have removed lake location as I don't want any data from lakes (presence, absence or pseudo-absence).
#Lakes follow a different process so best to leave these areas for spatial interpolation and remove later
#if necessary.


#Ensure variables are the same as the customary data variables
network_to_sample <- network_to_sample %>% 
  select("nzsegment","Lat","Lon","child_s","parent_s","dist_s","CatName") %>%
  rename("child_i" = "child_s",
         "parent_i" = "parent_s",
         "dist_i" = "dist_s",
         "catchmentName" = "CatName") %>%
  mutate("nzffdRecordNumber" = NA,
         "catchmentNumber" = NA,
         # Year, Time_step and Time_step_label to be added for individual TEK data
         "org" = "Pseudo_absence",
         "institution" = "Pseudo_absence",
         "FishMethod" = "Pseudo_absence",
         "Data_source" = NA,
         "Data_value" = 0) %>%
  relocate("nzffdRecordNumber","nzsegment","Lat","Lon","catchmentName","catchmentNumber",
           "child_i","parent_i","dist_i",
           "org", "institution", "FishMethod", 
           "Data_value", "Data_source")


##Network saved as SF object for 'closest distance to road' calculations
network_to_sample_SF <- st_as_sf(network_to_sample, coords = c("Lon","Lat"), crs = 4326) #Save as sf object
network_to_sample_SF <- st_transform(network_to_sample_SF, 3857) #Ensure I'm using the correct projection for distance calcs






#########
#Calculate distance to nearest road - these take a while to run
idx_old <- st_nearest_feature(network_to_sample_SF, NZ_roads_coords_old)
network_to_sample_SF$distance_to_road_old <- st_distance(network_to_sample_SF, NZ_roads_coords_old[idx_old,], by_element = TRUE)
summary(network_to_sample_SF$distance_to_road_old/1000)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000375 0.203906 0.539856 0.841222 1.122510 8.794301 


idx_new <- st_nearest_feature(network_to_sample_SF, NZ_roads_coords_new)
network_to_sample_SF$distance_to_road_new <- st_distance(network_to_sample_SF, NZ_roads_coords_new[idx_new,], by_element = TRUE)
summary(network_to_sample_SF$distance_to_road_new/1000)


all(network_to_sample_SF$distance_to_road_old == network_to_sample_SF$distance_to_road_new)

