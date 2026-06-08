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

raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"
fig_dir <- "./Data_processed/Figures"

HSM_dir <- "./Eel_HSM_taranaki"

pseudoabsence_data_dir <- file.path(data_taranaki_dir, "Pseudo_absence_data")
dir.create(pseudoabsence_data_dir, showWarnings = F)


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

# ##Set unsuitable habitat as anything less than 1st quartile
# summary(HSM_encounter_prob$POE)
# unsuit_hab_cutoff <- summary(HSM_encounter_prob$POE)["1st Qu."]

##Set unsuitable habitat as anything less than 0.5
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

sample_1a = sample_1b = sample_1c = sample_1d = list()

set.seed(141122)

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
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    
    ## a. As many as the encounter-only data
    sample_1a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_1b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_1c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
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
  filter(POE >= unsuit_hab_cutoff) %>% pull(child_s)

#Save
saveRDS(suitable_hab_to_remove, file.path(data_taranaki_dir, "Child_s_suitable_hab_to_remove.rds"))


sample_2a = sample_2b = sample_2c = sample_2d =  list()

set.seed(100323)

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
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    
    ## a. As many as the encounter-only data
    sample_2a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_2b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_2c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
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


#######################################################################
# 3. Random generation at locations within 500 m of a registered road #
#######################################################################

#Identify locations that are further than 500 m away from a road. This locations will
#be excluded from the sampling

#########
##Network saved as SF object for 'closest distance to road' calculations
network_to_sample_SF <- st_as_sf(network_to_sample, coords = c("Lon","Lat"), crs = 4326, agr = "constant") #Save as sf object
network_to_sample_SF <- st_transform(network_to_sample_SF, 3857) #Ensure I'm using the correct projection for distance calcs

#Calculate distance to nearest road - these take a while to run
idx <- st_nearest_feature(network_to_sample_SF, NZ_roads_coords)
network_to_sample_SF$distance_to_road <- st_distance(network_to_sample_SF, NZ_roads_coords[idx,], by_element = TRUE)
summary(network_to_sample_SF$distance_to_road/1000)
#  Min.    1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000375 0.203906 0.539856 0.841222 1.122510 8.794301 

#Convert to km and drop units
network_to_sample_SF$distance_to_road_km <- drop_units(network_to_sample_SF$distance_to_road)/1000

#Examine distance to road in stream network
dist_road_sn_plot <- ggplot(network_to_sample_SF, aes(distance_to_road_km)) +
  geom_histogram(alpha=0.5, bins=30) +
  ggtitle("Distance to road (km) for each segment of the stream network")
ggsave(file.path(fig_dir, "Distance_to_road_stream_network.png"), dist_road_sn_plot, height = 12, width = 15)

#Examine distance to road in the structured data
network_for_plot <- network_to_sample_SF %>% 
  select(c("child_i", "parent_i", "distance_to_road_km"))

NZFFD_sample_bias <- right_join(network_for_plot, NZFFD_data)
summary(NZFFD_sample_bias$distance_to_road_km)
NZFFD_sample_bias$Presence_or_absence <- factor(NZFFD_sample_bias$`Anguilla dieffenbachii`)
distance_to_road_NZFFD_hist <- ggplot(NZFFD_sample_bias, aes(distance_to_road_km, fill=Presence_or_absence)) +
  geom_histogram(alpha=0.5, bins=30) +
  ggtitle("Distance to road (km) of structured data")
ggsave(file.path(fig_dir, "Distance_to_road_structured_data.png"), distance_to_road_NZFFD_hist, height = 12, width = 15)

#Examine the samplying bias in the unstructured data
presence_only_sample_bias <- right_join(network_for_plot, encounter_only_lf_data)
summary(presence_only_sample_bias$distance_to_road_km)
dist_to_road_presenceonly_hist <- ggplot(presence_only_sample_bias, aes(distance_to_road_km)) +
  geom_histogram(alpha=0.5, bins=30) +
  ggtitle("Distance to road (km) of unstructured data")
ggsave(file.path(fig_dir, "Distance_to_road_unstructured_data.png"), dist_to_road_presenceonly_hist, height = 12, width = 15)

# dist_cut_off <- summary(presence_only_sample_bias$distance_to_road_km)["3rd Qu."] #0.6573748
summary(presence_only_sample_bias$distance_to_road_km)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0020  0.1347  0.3503  0.5260  0.6574  3.0661 
#########

## Extract locations that are further than 500 m from a road
locations_far_from_roads <- network_to_sample_SF %>%
  filter(distance_to_road_km > 0.5) %>% pull(child_i)

#Save
saveRDS(locations_far_from_roads, file.path(data_taranaki_dir, "Child_s_locations_far_from_roads.rds"))

sample_3a = sample_3b = sample_3c = sample_3d = list()

set.seed(100323)

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
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    ## a. As many as the encounter-only data
    sample_3a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_3b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_3c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
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


######################################################################
# 4. Random generation at locations within 500m of a registered road #
#    and with unsuitable longfin eel habitat                         #
######################################################################

sample_4a = sample_4b = sample_4c = sample_4d = list()

set.seed(100323)

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
  if(length(to_remove)>0){
    
    data_to_sample <- network_to_sample %>% 
      filter(!(child_i %in% to_remove)) %>%
      mutate("Year" = years_to_sample[i])
    
    #length of encounter-only data for year of interest 
    n <- nrow(encounter_only_lf_data %>% filter(Year == years_to_sample[i]))
    
    if(n*20 > nrow(data_to_sample)){stop("n*20 is greater than the data to sample from")}
    
    ## a. As many as the encounter-only data
    sample_4a[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
    
    ## b. 5x as many as the encounter-only data
    sample_4b[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
    
    ## c. 10x as many as the encounter-only data
    sample_4c[[i]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
    
    ## d. 20x as many as the encounter-only data
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

## Save randomly generated data ##
#a.
saveRDS(sample_1a, file.path(pseudoabsence_data_dir, "Sample_1a.rds"))

#b.
saveRDS(sample_1b, file.path(pseudoabsence_data_dir, "Sample_1b.rds"))

#c.
saveRDS(sample_1c, file.path(pseudoabsence_data_dir, "Sample_1c.rds"))

#d.
saveRDS(sample_1d, file.path(pseudoabsence_data_dir, "Sample_1d.rds"))
####


## Save randomly generated data at locations with unsuitable longfin eel habitat ##
#a.
saveRDS(sample_2a, file.path(pseudoabsence_data_dir, "Sample_2a.rds"))

#b.
saveRDS(sample_2b, file.path(pseudoabsence_data_dir, "Sample_2b.rds"))

#c.
saveRDS(sample_2c, file.path(pseudoabsence_data_dir, "Sample_2c.rds"))

#d.
saveRDS(sample_2d, file.path(pseudoabsence_data_dir, "Sample_2d.rds"))
####


## Save randomly generated data at locations within 500m of a registered road ##
#a.
saveRDS(sample_3a, file.path(pseudoabsence_data_dir, "Sample_3a.rds"))

#b.
saveRDS(sample_3b, file.path(pseudoabsence_data_dir, "Sample_3b.rds"))

#c.
saveRDS(sample_3c, file.path(pseudoabsence_data_dir, "Sample_3c.rds"))

#d.
saveRDS(sample_3d, file.path(pseudoabsence_data_dir, "Sample_3d.rds"))
####


## Save randomly generated data at locations within 500m of a registered road and with unsuitable longfin eel habitat ##
#a.
saveRDS(sample_4a, file.path(pseudoabsence_data_dir, "Sample_4a.rds"))

#b.
saveRDS(sample_4b, file.path(pseudoabsence_data_dir, "Sample_4b.rds"))

#c.
saveRDS(sample_4c, file.path(pseudoabsence_data_dir, "Sample_4c.rds"))

#d.
saveRDS(sample_4d, file.path(pseudoabsence_data_dir, "Sample_4d.rds"))
####


###############
#  Plot data  #
###############

# Taranaki catchment
l2 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
  return(out)
})
l2 <- do.call(rbind, l2)


##Plot locations of unsuitable longfin eel habitat
Data_to_plot <- HSM_encounter_prob %>% 
  mutate("Habitat" = ifelse(POE >= unsuit_hab_cutoff, "Suitable", "Unsuitable"))

catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Habitat), size=3, alpha = 0.6) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("Longfin eel suitable and unsuitable habitat") +
  
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + 
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("#39B600", "#E41A1C")) +
  theme_bw(base_size = 14) +
  #theme(axis.text = element_text(size = rel(0.8)))
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(fig_dir, "Lf_habitat_suitability.png"), catchmap, height = 12, width = 15)



##Plot distance to nearest road
Data_to_plot2 <- left_join(network_to_sample_SF, network_to_sample)

catchmap2 <- ggplot(Data_to_plot2) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = round(distance_to_road_km, 2)), size=3, alpha = 0.6) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("Distance to nearest road (km)") +
  
  labs(colour = "") +
  #guides(col = guide_legend(title = "")) + 
  scale_color_distiller(palette = "RdYlGn", limits = c(0,9), direction = 1) +
  theme_bw(base_size = 14) +
  #theme(axis.text = element_text(size = rel(0.8)))
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))
ggsave(file.path(fig_dir, "Distance_to_road.png"), catchmap2, height = 12, width = 15)


