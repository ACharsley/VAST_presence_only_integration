###################################################
##           Generate pseudo-absence data        ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                January 2023                   ##
###################################################

# This code generates pseudo-absence data.

# Generate pseudo-absence data by:
#   
#   1. Random generation (at locations in the stream network without presence observations)
#   2. Spatially biased generation to simulate 'true' absence data (within 2km of a road)
#   3. Habitat unsuitability (at locations not suitable or less suitable for eels)
# 
# For each year of data, generate:
#   a. As many as the presence-only data
#   b. Twice as many as the presence-only data
#   c. 5x as many as the presence-only data
#   d. 10x as many as the presence-only data (we want this to be ~10,000 data points (recommended by Barbet-Massin et al (2012))


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

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw/Eel_application_Taranaki"
data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- "./Data_processed/Taranaki/Figures"

HSM_dir <- "./Eel_HSM_taranaki"

pseudoabsence_data_dir <- file.path(data_taranaki_dir, "Pseudo_absence_data")
dir.create(pseudoabsence_data_dir, showWarnings = F)


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network_to_sample <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE)

##NZFFD presence/absence data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_pa_data.rds"))

##presence-only data
presence_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_presence_only_lf_data.rds"))

years_to_sample <- unique(presence_only_lf_data$Year)[order(unique(presence_only_lf_data$Year))]

##Load habitat suitability data
load(file.path(HSM_dir, "HSM_encounter_prob.RData"))

##Road data
NZ_roads_data <- read_sf(dsn = raw_data_dir, layer = "nz-primary-road-parcels") #Data extracted on 14/11/22 from https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/

#Extract coordinates
NZ_roads_coords <- as.data.frame(sf::st_coordinates(NZ_roads_data)) %>% select(X,Y) %>% rename("Lat"=Y, "Lon"=X)
NZ_roads_coords <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #Save as sf object
NZ_roads_coords <- st_transform(NZ_roads_coords, 3857) #Ensure I'm using the correct projection for distance calcs


##############################
#  Format network_to_sample  #
##############################

#Ensure variables are the same as presence-only/NZFFD variables
network_to_sample <- network_to_sample %>% 
  select("nzsegment","Lat","Lon","child_s","parent_s","dist_s","CatName") %>%
  rename("child_i" = "child_s",
         "parent_i" = "parent_s",
         "dist_i" = "dist_s",
         "catchmentName" = "CatName") %>%
  mutate("nzffdRecordNumber" = NA,
         "catchmentNumber" = NA,
         #Year to be added in sampling procedure below
         "org" = "pseudo_absence",
         "institution" = "Pseudo absence",
         "FishMethod" = NA,
         "Anguilla dieffenbachii" = 0, 
         "Anguilla australis" = 0) %>%
  relocate("nzffdRecordNumber","nzsegment","Lat","Lon","catchmentName","catchmentNumber","child_i","parent_i","dist_i",
           "org", "institution", "FishMethod", "Anguilla dieffenbachii", "Anguilla australis")


##################################
#  Generate pseudo-absence data  #
##################################

########################
# 1. Random generation
########################

sample_1a = sample_1b = sample_1c = sample_1d = list()

set.seed(141122)

#Loop over all the years in the presence-only data
for(i in c(1:length(years_to_sample))){
  
  print(i)
  
  
  #Remove NZFFD locations for year of interest
  ##to_remove <- NZFFD_data$child_i[NZFFD_data$Year == years_to_sample[i] & NZFFD_data$`Anguilla dieffenbachii` == 1]
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove presence-only locations for all years except the year of interest
  ##to_remove2 <- presence_only_lf_data$child_i[presence_only_lf_data$Year == years_to_sample[i]] #no need to filter out lf==1 as all are ==1
  to_remove2 <- presence_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #browser()
  
  #Add together
  to_remove <- c(to_remove1, to_remove2)
  
  #If there is data in that year take a sample, if not no pseudo-absence data for that year
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of presence-only data for year of interest 
    n <- nrow(presence_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    
    ## a. As many as the presence-only data
    sample_1a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. Twice as many as the presence-only data
    sample_1b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*2),]
    
    ## c. 5x as many as the presence-only data
    sample_1c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## d. 10x as many as the presence-only data
    sample_1d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

#Combine data sets
sample_1a <- do.call(rbind, sample_1a)
sample_1b <- do.call(rbind, sample_1b)
sample_1c <- do.call(rbind, sample_1c)
sample_1d <- do.call(rbind, sample_1d)




#########################################################################
# 2. Random generation at locations with unsuitable longfin eel habitat #
#########################################################################

sample_2a = sample_2b = sample_2c = sample_2d = list()

set.seed(280223)

# 
# ##Network saved as SF object for 'closest distance to road' calculations
# network_to_sample_SF <- st_as_sf(network_to_sample, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #Save as sf object for use later
# network_to_sample_SF <- st_transform(network_to_sample_SF, 3857) #Ensure I'm using the correct projection for distance calcs
# 
# #Calculate distance to nearest road
# idx <- st_nearest_feature(network_to_sample_SF, NZ_roads_coords)
# network_to_sample_SF$distance_to_road <- st_distance(network_to_sample_SF, NZ_roads_coords[idx,], by_element = TRUE)
# 
# summary(network_to_sample_SF$distance_to_road/1000)
# 
# #Convert to km and drop units
# network_to_sample_SF$distance_to_road_km <- drop_units(network_to_sample_SF$distance_to_road)/1000
# 
# #Subset data by distance to road, must be less than 2km away from road
# network_to_sample_SF <- network_to_sample_SF %>% filter(distance_to_road_km < 2)
# 
# ## Set final dataset I want to sample from
# network_to_sample_roadbias <- network_to_sample %>% filter(child_i %in% network_to_sample_SF$child_i)
# 
# 
# set.seed(141122)
# 
# ## a. As many as the presence-only data
# sample_2a <- network_to_sample_roadbias[sample(c(1:nrow(network_to_sample_roadbias)), n),]
# 
# ## b. Twice as many as the presence-only data
# sample_2b <- network_to_sample_roadbias[sample(c(1:nrow(network_to_sample_roadbias)), n*2),]
# 
# ## c. 5x as many as the presence-only data
# sample_2c <- network_to_sample_roadbias[sample(c(1:nrow(network_to_sample_roadbias)), n*5),]
# 
# ## d. 10x as many as the presence-only data
# sample_2d <- network_to_sample_roadbias[sample(c(1:nrow(network_to_sample_roadbias)), n*10),]


#####################################################################
# 3. Random generation at locations within 2km of a registered road #
#####################################################################



#####################################################################
# 4. Random generation at locations within 2km of a registered road #
#    and with unsuitable longfin eel habitat                        #
#####################################################################





##############################
#  Save pseudo-absence data  #
##############################

#Save randomly generated data
#a.
saveRDS(sample_1a, file.path(pseudoabsence_data_dir, "Random_sample_1a.rds"))

#b.
saveRDS(sample_1b, file.path(pseudoabsence_data_dir, "Random_sample_1b.rds"))

#c.
saveRDS(sample_1c, file.path(pseudoabsence_data_dir, "Random_sample_1c.rds"))

#d.
saveRDS(sample_1d, file.path(pseudoabsence_data_dir, "Random_sample_1d.rds"))


# #Save spatially biased data
# 
# #a.
# saveRDS(sample_2a, file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2a.rds"))
# 
# #b.
# saveRDS(sample_2b, file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2b.rds"))
# 
# #c.
# saveRDS(sample_2c, file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2c.rds"))
# 
# #d.
# saveRDS(sample_2d, file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2d.rds"))
