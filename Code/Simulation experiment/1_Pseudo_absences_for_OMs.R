###################################################
##           Generate pseudo-absence data        ##
##         for OMs for the Taranaki region       ##
##                                               ##
##               Anthony Charsley                ##
##                September 2024                 ##
###################################################

# This code generates pseudo-absence data for the OMs of the simulation study.

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

HSM_dir <- "./Eel_HSM_taranaki"

pseudoabsence_data_dir <- file.path(data_taranaki_dir, "Pseudo_absence_data")



###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network_to_sample <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE) 
#I have removed lake location as I don't want any data from lakes (presence, absence or pseudo-absence).
#Lakes follow a different process so best to leave these areas for spatial interpolation and remove later
#if necessary.

##NZFFD encounter/non-encounter data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))

##encounter-only data
encounter_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

years_to_sample <- unique(encounter_only_lf_data$Year)[order(unique(encounter_only_lf_data$Year))]

##Load habitat suitability data
load(file.path(HSM_dir, "HSM_encounter_prob.RData"))

##Set unsuitable habitat as anything less than mean
summary(HSM_encounter_prob$POE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.237   0.416   0.476   0.483   0.549   0.717
unsuit_hab_cutoff <- summary(HSM_encounter_prob$POE)["Mean"] # 0.48 - mean probability of encounter 

##Road data
NZ_roads_data <- read_sf(dsn = raw_data_dir, layer = "nz-primary-road-parcels") #Data extracted on 14/11/22 from https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/

#Extract coordinates
NZ_roads_coords <- as.data.frame(sf::st_coordinates(NZ_roads_data)) %>% select(X,Y) %>% rename("Lat"=Y, "Lon"=X)
#NZ_roads_coords <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #old code but results in same coordinate projections
NZ_roads_coords <- st_as_sf(NZ_roads_coords, coords = c("Lon","Lat"), crs = 4167) # the crs recorded at: https://data.linz.govt.nz/layer/50796-nz-primary-road-parcels/#:~:text=polygon%20layer%20%20Multipolygon-,CRS%20as%20stored,-NZGD2000EPSG%3A4167

NZ_roads_coords <- st_transform(NZ_roads_coords, 3857) #Ensure I'm using the correct projection for distance calcs



##############################
#  Format network_to_sample  #
##############################

#Ensure variables are the same as encounter-only/NZFFD variables
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
         "FishMethod" = "Other",
         "Data_source" = "Unstructured",
         "Anguilla dieffenbachii" = 0) %>%
  relocate("nzffdRecordNumber","nzsegment","Lat","Lon","catchmentName","catchmentNumber","child_i","parent_i","dist_i",
           "org", "institution", "FishMethod", "Data_source", "Anguilla dieffenbachii")



##################################
#  Generate pseudo-absence data  #
##################################

########################
# 1. Random generation
########################

sample_OM_1a = sample_OM_1b = sample_OM_1c = sample_OM_1d = list()

set.seed(110924)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(years_to_sample[i])
  
  
  #Remove structured data presence locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove presence-only locations for all years except the year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #Add together
  to_remove <- c(to_remove1, to_remove2)
  to_remove <- unique(to_remove)
  
  #I'm looping over years of data from the presence-only data (years_to_sample). 
  #So, if there is data in that year, take a sample, if not no pseudo-absence data for that year.
  if(length(to_remove)>0){ #not really needed as we loop over "years_to_sample" so length(to_remove) will always be >0
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    # Operating model data
    ## a. As many as the encounter-only data
    sample_OM_1a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_OM_1b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_OM_1c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
    sample_OM_1d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
    
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

sample_OM_1a <- do.call(rbind, sample_OM_1a)
sample_OM_1b <- do.call(rbind, sample_OM_1b)
sample_OM_1c <- do.call(rbind, sample_OM_1c)
sample_OM_1d <- do.call(rbind, sample_OM_1d)



#########################################################################
# 2. Random generation at locations with unsuitable longfin eel habitat #
#########################################################################

#Identify locations that are 'unsuitable' for longfin eel - as habitat suitability models of 
#longfin eel predict probability of encounter as low throughout the Taranaki region, we will
#assume that locations with a probability of encounter below the median value has 'unsuitable'
#habitat for longfin eel

##Load unique 'child' nodes denoting longfin eel suitability habitat locations to remove
suitable_hab_to_remove <- readRDS(file.path(data_taranaki_dir, "Child_s_suitable_hab_to_remove.rds"))

sample_OM_2a = sample_OM_2b = sample_OM_2c = sample_OM_2d = list()

set.seed(110924)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(years_to_sample[i])
  
  
  #Remove structured data presence locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove presence-only locations for year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #Add together
  to_remove <- c(to_remove1, to_remove2, suitable_hab_to_remove)
  to_remove <- unique(to_remove)
  
  #I'm looping over years of data from the presence-only data (years_to_sample). 
  #So, if there is data in that year, take a sample, if not no pseudo-absence data for that year.
  if(length(to_remove)>0){#not really needed as we loop over "years_to_sample" so length(to_remove) will always be >0
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    # Operating model data
    ## a. As many as the encounter-only data
    sample_OM_2a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_OM_2b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_OM_2c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
    sample_OM_2d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

sample_OM_2a <- do.call(rbind, sample_OM_2a)
sample_OM_2b <- do.call(rbind, sample_OM_2b)
sample_OM_2c <- do.call(rbind, sample_OM_2c)
sample_OM_2d <- do.call(rbind, sample_OM_2d)



#######################################################################
# 3. Random generation at locations within 500 m of a registered road #
#######################################################################

#Identify locations that are further than 500 m away from a road. This locations will
#be excluded from the sampling

#########
##Load unique 'child' nodes denoting locations far from roads to remove
locations_far_from_roads <- readRDS(file.path(data_taranaki_dir, "Child_s_locations_far_from_roads.rds"))
#########

sample_OM_3a = sample_OM_3b = sample_OM_3c = sample_OM_3d = list()

set.seed(110924)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(years_to_sample[i])
  
  
  #Remove structured data presence locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove encounter-only locations for year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #Add together
  to_remove <- c(to_remove1, to_remove2, locations_far_from_roads)
  to_remove <- unique(to_remove)
  
  #I'm looping over years of data from the presence-only data (years_to_sample). 
  #So, if there is data in that year, take a sample, if not no pseudo-absence data for that year.
  if(length(to_remove)>0){#not really needed as we loop over "years_to_sample" so length(to_remove) will always be >0
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    # Operating model data
    ## a. As many as the encounter-only data
    sample_OM_3a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_OM_3b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_OM_3c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
    sample_OM_3d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

sample_OM_3a <- do.call(rbind, sample_OM_3a)
sample_OM_3b <- do.call(rbind, sample_OM_3b)
sample_OM_3c <- do.call(rbind, sample_OM_3c)
sample_OM_3d <- do.call(rbind, sample_OM_3d)



######################################################################
# 4. Random generation at locations within 500m of a registered road #
#    and with unsuitable longfin eel habitat                         #
######################################################################

sample_OM_4a = sample_OM_4b = sample_OM_4c = sample_OM_4d = list()

set.seed(110924)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(years_to_sample[i])
  
  
  #Remove structured data presence locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove encounter-only locations for year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #Add together
  to_remove <- c(to_remove1, to_remove2, locations_far_from_roads, suitable_hab_to_remove)
  to_remove <- unique(to_remove)
  
  #I'm looping over years of data from the presence-only data (years_to_sample). 
  #So, if there is data in that year, take a sample, if not no pseudo-absence data for that year.
  if(length(to_remove)>0){#not really needed as we loop over "years_to_sample" so length(to_remove) will always be >0
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    # Operating model data
    ## a. As many as the encounter-only data
    sample_OM_4a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_OM_4b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_OM_4c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
    sample_OM_4d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

sample_OM_4a <- do.call(rbind, sample_OM_4a)
sample_OM_4b <- do.call(rbind, sample_OM_4b)
sample_OM_4c <- do.call(rbind, sample_OM_4c)
sample_OM_4d <- do.call(rbind, sample_OM_4d)



##############################
#  Save pseudo-absence data  #
##############################

## Save randomly generated data ##
#OM - a.
saveRDS(sample_OM_1a, file.path(pseudoabsence_data_dir, "Sample_OM_1a.rds"))

#OM - b.
saveRDS(sample_OM_1b, file.path(pseudoabsence_data_dir, "Sample_OM_1b.rds"))

#OM - c.
saveRDS(sample_OM_1c, file.path(pseudoabsence_data_dir, "Sample_OM_1c.rds"))

#OM - d.
saveRDS(sample_OM_1d, file.path(pseudoabsence_data_dir, "Sample_OM_1d.rds"))
####


## Save randomly generated data at locations with unsuitable longfin eel habitat ##
#OM - a.
saveRDS(sample_OM_2a, file.path(pseudoabsence_data_dir, "Sample_OM_2a.rds"))

#OM - b.
saveRDS(sample_OM_2b, file.path(pseudoabsence_data_dir, "Sample_OM_2b.rds"))

#OM - c.
saveRDS(sample_OM_2c, file.path(pseudoabsence_data_dir, "Sample_OM_2c.rds"))

#OM - d.
saveRDS(sample_OM_2d, file.path(pseudoabsence_data_dir, "Sample_OM_2d.rds"))
####


## Save randomly generated data at locations within 2km of a registered road ##
#OM - a.
saveRDS(sample_OM_3a, file.path(pseudoabsence_data_dir, "Sample_OM_3a.rds"))

#OM - b.
saveRDS(sample_OM_3b, file.path(pseudoabsence_data_dir, "Sample_OM_3b.rds"))

#OM - c.
saveRDS(sample_OM_3c, file.path(pseudoabsence_data_dir, "Sample_OM_3c.rds"))

#OM - d.
saveRDS(sample_OM_3d, file.path(pseudoabsence_data_dir, "Sample_OM_3d.rds"))
####


## Save randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat ##
#OM - a.
saveRDS(sample_OM_4a, file.path(pseudoabsence_data_dir, "Sample_OM_4a.rds"))

#OM - b.
saveRDS(sample_OM_4b, file.path(pseudoabsence_data_dir, "Sample_OM_4b.rds"))

#OM - c.
saveRDS(sample_OM_4c, file.path(pseudoabsence_data_dir, "Sample_OM_4c.rds"))

#OM - d.
saveRDS(sample_OM_4d, file.path(pseudoabsence_data_dir, "Sample_OM_4d.rds"))
####
