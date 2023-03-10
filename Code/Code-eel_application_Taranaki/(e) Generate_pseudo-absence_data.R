###################################################
##           Generate pseudo-absence data        ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                January 2023                   ##
###################################################

# This code generates pseudo-absence data.

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

##NZFFD encounter/non-encounter data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))

##encounter-only data
encounter_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

years_to_sample <- unique(encounter_only_lf_data$Year)[order(unique(encounter_only_lf_data$Year))]

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

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(i)
  
  
  #Remove NZFFD locations for year of interest
  ##to_remove <- NZFFD_data$child_i[NZFFD_data$Year == years_to_sample[i] & NZFFD_data$`Anguilla dieffenbachii` == 1]
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove encounter-only locations for all years except the year of interest
  ##to_remove2 <- encounter_only_lf_data$child_i[encounter_only_lf_data$Year == years_to_sample[i]] #no need to filter out lf==1 as all are ==1
  to_remove2 <- encounter_only_lf_data %>%
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
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    
    ## a. As many as the encounter-only data
    sample_1a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. Twice as many as the encounter-only data
    sample_1b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 5x as many as the encounter-only data
    sample_1c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 10x as many as the encounter-only data
    sample_1d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
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

#Identify locations that are 'unsuitable' for longfin eel - as habitat suitability models of 
#longfin eel predict probability of encounter as low throughout the Taranaki region, we will
#assume that locations with a probability of encounter below the median value has 'unsuitable'
#habitat for longfin eel

suitable_hab_to_remove <- HSM_encounter_prob %>%
  filter(POE >= median(POE)) %>% pull(child_s)

sample_2a = sample_2b = sample_2c = sample_2d = list()

set.seed(100323)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(i)
  
  
  #Remove NZFFD locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove encounter-only locations for year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #browser()
  
  #Add together
  to_remove <- c(to_remove1, to_remove2, suitable_hab_to_remove)
  
  #If there is data in that year take a sample, if not no pseudo-absence data for that year
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    
    ## a. As many as the encounter-only data
    sample_2a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. Twice as many as the encounter-only data
    sample_2b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 5x as many as the encounter-only data
    sample_2c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 10x as many as the encounter-only data
    sample_2d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

#Combine data sets
sample_2a <- do.call(rbind, sample_2a)
sample_2b <- do.call(rbind, sample_2b)
sample_2c <- do.call(rbind, sample_2c)
sample_2d <- do.call(rbind, sample_2d)



#####################################################################
# 3. Random generation at locations within 2km of a registered road #
#####################################################################

#Identify locations that are further than 2km away from a road. This locations will
#be excluded from the sampling

#########
##Network saved as SF object for 'closest distance to road' calculations
network_to_sample_SF <- st_as_sf(network_to_sample, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #Save as sf object
network_to_sample_SF <- st_transform(network_to_sample_SF, 3857) #Ensure I'm using the correct projection for distance calcs

#Calculate distance to nearest road - these take a while to run
idx <- st_nearest_feature(network_to_sample_SF, NZ_roads_coords)
network_to_sample_SF$distance_to_road <- st_distance(network_to_sample_SF, NZ_roads_coords[idx,], by_element = TRUE)

summary(network_to_sample_SF$distance_to_road/1000)

#Convert to km and drop units
network_to_sample_SF$distance_to_road_km <- drop_units(network_to_sample_SF$distance_to_road)/1000
#########

## Extract locations that are further than 2km from a road
locations_further_than_2km <- network_to_sample_SF %>%
  filter(distance_to_road_km > 2) %>% pull(child_i)

sample_3a = sample_3b = sample_3c = sample_3d = list()

set.seed(100323)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(i)
  
  
  #Remove NZFFD locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove encounter-only locations for year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #browser()
  
  #Add together
  to_remove <- c(to_remove1, to_remove2, locations_further_than_2km)
  
  #If there is data in that year take a sample, if not no pseudo-absence data for that year
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    
    ## a. As many as the encounter-only data
    sample_3a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. Twice as many as the encounter-only data
    sample_3b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 5x as many as the encounter-only data
    sample_3c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 10x as many as the encounter-only data
    sample_3d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

#Combine data sets
sample_3a <- do.call(rbind, sample_3a)
sample_3b <- do.call(rbind, sample_3b)
sample_3c <- do.call(rbind, sample_3c)
sample_3d <- do.call(rbind, sample_3d)


#####################################################################
# 4. Random generation at locations within 2km of a registered road #
#    and with unsuitable longfin eel habitat                        #
#####################################################################

sample_4a = sample_4b = sample_4c = sample_4d = list()

set.seed(100323)

#Loop over all the years in the encounter-only data
for(i in c(1:length(years_to_sample))){
  
  print(i)
  
  
  #Remove NZFFD locations for year of interest
  to_remove1 <- NZFFD_data %>%
    filter(Year == years_to_sample[i] & `Anguilla dieffenbachii` == 1) %>%
    pull(child_i)
  
  #Remove encounter-only locations for year of interest
  to_remove2 <- encounter_only_lf_data %>%
    filter(Year == years_to_sample[i]) %>%
    pull(child_i)
  
  #browser()
  
  #Add together
  to_remove <- c(to_remove1, to_remove2, locations_further_than_2km, suitable_hab_to_remove)
  
  #If there is data in that year take a sample, if not no pseudo-absence data for that year
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    
    ## a. As many as the encounter-only data
    sample_4a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. Twice as many as the encounter-only data
    sample_4b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 5x as many as the encounter-only data
    sample_4c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 10x as many as the encounter-only data
    sample_4d[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*20),]
    
  }
  
  #Remove
  rm(to_remove) ; rm(to_remove1) ; rm(to_remove2) ; rm(data_to_sample)
  
}

#Combine data sets
sample_4a <- do.call(rbind, sample_4a)
sample_4b <- do.call(rbind, sample_4b)
sample_4c <- do.call(rbind, sample_4c)
sample_4d <- do.call(rbind, sample_4d)



##############################
#  Save pseudo-absence data  #
##############################

#Save randomly generated data
#a.
saveRDS(sample_1a, file.path(pseudoabsence_data_dir, "Sample_1a.rds"))

#b.
saveRDS(sample_1b, file.path(pseudoabsence_data_dir, "Sample_1b.rds"))

#c.
saveRDS(sample_1c, file.path(pseudoabsence_data_dir, "Sample_1c.rds"))

#d.
saveRDS(sample_1d, file.path(pseudoabsence_data_dir, "Sample_1d.rds"))


#Save randomly generated data at locations with unsuitable longfin eel habitat

#a.
saveRDS(sample_2a, file.path(pseudoabsence_data_dir, "Sample_2a.rds"))

#b.
saveRDS(sample_2b, file.path(pseudoabsence_data_dir, "Sample_2b.rds"))

#c.
saveRDS(sample_2c, file.path(pseudoabsence_data_dir, "Sample_2c.rds"))

#d.
saveRDS(sample_2d, file.path(pseudoabsence_data_dir, "Sample_2d.rds"))


#Save randomly generated data at locations within 2km of a registered road

#a.
saveRDS(sample_3a, file.path(pseudoabsence_data_dir, "Sample_3a.rds"))

#b.
saveRDS(sample_3b, file.path(pseudoabsence_data_dir, "Sample_3b.rds"))

#c.
saveRDS(sample_3c, file.path(pseudoabsence_data_dir, "Sample_3c.rds"))

#d.
saveRDS(sample_3d, file.path(pseudoabsence_data_dir, "Sample_3d.rds"))


#Save randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat 

#a.
saveRDS(sample_4a, file.path(pseudoabsence_data_dir, "Sample_4a.rds"))

#b.
saveRDS(sample_4b, file.path(pseudoabsence_data_dir, "Sample_4b.rds"))

#c.
saveRDS(sample_4c, file.path(pseudoabsence_data_dir, "Sample_4c.rds"))

#d.
saveRDS(sample_4d, file.path(pseudoabsence_data_dir, "Sample_4d.rds"))

