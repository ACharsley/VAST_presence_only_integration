###################################################
##             Examine distance to road          ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                February 2025                  ##
###################################################

# This code examines the distance the unstructured data is to
# the nearest road.

###########################################

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
fig_dir <- "./Data_processed/Figures"



###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE) 
#I have removed lake location as I don't want any data from lakes (presence, absence or pseudo-absence).
#Lakes follow a different process so best to leave these areas for spatial interpolation and remove later
#if necessary.

##encounter-only data
encounter_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

##Road data
NZ_roads_data <- read_sf(dsn = raw_data_dir, layer = "nz-primary-road-parcels") #Data extracted on 14/11/22 from https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/

#Extract coordinates
NZ_roads_coords <- as.data.frame(sf::st_coordinates(NZ_roads_data)) %>% select(X,Y) %>% rename("Lat"=Y, "Lon"=X)
#NZ_roads_coords <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #old code but results in same coordinate projections
NZ_roads_coords <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4167) # the crs recorded at: https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/#:~:text=polygon%20layer%20%20Multipolygon-,CRS%20as%20stored,-NZGD2000EPSG%3A4167

NZ_roads_coords <- st_transform(NZ_roads_coords, 3857) #Ensure I'm using the correct projection for distance calcs



#######################################
# Examine distance to registered road #
#######################################

#########
##Network saved as SF object for 'closest distance to road' calculations
network_SF <- st_as_sf(network, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #Save as sf object
network_SF <- st_transform(network_SF, 3857) #Ensure I'm using the correct projection for distance calcs

#Calculate distance to nearest road - these take a while to run
idx <- st_nearest_feature(network_SF, NZ_roads_coords)
network_SF$distance_to_road <- st_distance(network_SF, NZ_roads_coords[idx,], by_element = TRUE)
summary(network_SF$distance_to_road/1000)
#  Min.    1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000375 0.203906 0.539856 0.841222 1.122510 8.794301 

#Convert to km and drop units
network_SF$distance_to_road_km <- drop_units(network_SF$distance_to_road)/1000


network_for_plot <- network_SF %>% 
  select(c("child_s", "parent_s", "distance_to_road_km")) %>%
  rename("child_i" = "child_s",
         "parent_i" = "parent_s")

#Examine the samplying bias in the unstructured data
presence_only_sample_bias <- right_join(network_for_plot, encounter_only_lf_data)
summary(presence_only_sample_bias$distance_to_road_km)

dist_to_road_presenceonly_hist <- ggplot(presence_only_sample_bias, aes(x=distance_to_road_km)) +
  geom_histogram(binwidth = 0.1, boundary = 0)  +
  scale_x_continuous(breaks = seq(0,35,by=2)/10) +
  scale_y_continuous(breaks = seq(0,180,by=20)) +
  #geom_vline(xintercept = median(presence_only_sample_bias$distance_to_road_km)) +
  #geom_vline(xintercept = mean(presence_only_sample_bias$distance_to_road_km)) +
  #ggtitle("Distance to road (km) of unstructured data") +
  xlab("Distance to road (km)") + ylab("Count") +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(1)))
ggsave(file.path(fig_dir, "Distance_to_road_unstructured_data.png"), dist_to_road_presenceonly_hist, height = 12, width = 15)

# dist_cut_off <- summary(presence_only_sample_bias$distance_to_road_km)["3rd Qu."] #0.6573748
summary(presence_only_sample_bias$distance_to_road_km)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0020  0.1347  0.3503  0.5260  0.6574  3.0661 
#########




