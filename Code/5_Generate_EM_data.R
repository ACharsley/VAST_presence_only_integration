###################################################
##               Generate EM data                ##
##                                               ##
##               Anthony Charsley                ##
##                   May 2023                    ##
###################################################


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

# Set data input path
VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")

# Set model path
model_path <- paste0(getwd(), "/Models")

# Set EM directory
EM_path <- file.path(model_path, "EMs")
dir.create(EM_path, showWarnings = F)

# EM data path
EM_data_path <- file.path(EM_path, "EM_data")
dir.create(EM_data_path, showWarnings = F)

# Directories for data needed to simulate pseudo-absences
raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"
HSM_dir <- "./Eel_HSM_taranaki"



###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network_to_sample <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE)

##NZFFD encounter/non-encounter data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))

##Load unique 'child' nodes denoting longfin eel suitability habitat locations to remove
suitable_hab_to_remove <- readRDS(file.path(data_taranaki_dir, "Child_s_suitable_hab_to_remove.rds"))

##Load unique 'child' nodes denoting locations far from roads to remove
locations_far_from_roads <- readRDS(file.path(data_taranaki_dir, "Child_s_locations_far_from_roads.rds"))


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


##########################
#  Load VAST input data  #
##########################

VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))


########################################################
########################################################


##############
# Build data #
##############

scenarios <- c("OM_1a", "OM_1b",
               "OM_2a", "OM_2b",
               "OM_3a", "OM_3b",
               "OM_4a", "OM_4b")

set.seed(300523)
for(sce in scenarios){ #sce = "OM_1a"
  
  print(sce)
  
  # Set path to simulated data
  sim_data_path <- file.path(model_path, paste0("Model_", sce, "/Simulated_data"))
  
  # Set data from scenario
  Fish_data <- VAST_input_data[[sce]]$Data_Geostat
  
  
  ############################
  # Loop over simulated data #
  ############################
  
  for(i in 1:20){ #i <- 1 
    
    # Load simulated data
    Data_sim <- readRDS(file.path(sim_data_path, paste0("Data_sim_", i, ".RDS")))
    
    b_i <- as.numeric(Data_sim$b_i > 0)
    
    Data_sim_df <-data.frame("Lon" = Fish_data$Lon,
                             "Lat" = Fish_data$Lat,
                             "child_i" = Fish_data$Child_i,
                             "Year" = Fish_data$Year,
                             "Catch_KG" = b_i,
                             "Data_source" = Fish_data$Data_source,
                             "Length" = Fish_data$Length)
    
    #Simulated structured (EF and Net/Trap) data
    Data_sim_structured <- Data_sim_df %>% 
      filter(Data_source %in% c("Structured_EF", "Structured_NetTrap"))
    
    #Simulated unstructured presence-only data
    Data_sim_unstructured <- Data_sim_df %>% filter(Data_source == "Unstructured")
    Data_sim_unstructured <- Data_sim_unstructured %>% filter(Catch_KG == 1)
    
    years_to_sample <- unique(Data_sim_unstructured$Year)[order(unique(Data_sim_unstructured$Year))]
    
    ################################
    # Simulate pseudo-absence data #
    ################################
    
    sample_1 = sample_2 = sample_3 = sample_4 = sample_5 = sample_6 = sample_7 = sample_8 =list()
    
    # 1. Random generation
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove NZFFD locations for year of interest
      to_remove1 <- NZFFD_data %>%
        filter(Year == years_to_sample[yr] & `Anguilla dieffenbachii` == 1) %>%
        pull(child_i)
      
      #Remove encounter-only locations for all years except the year of interest
      to_remove2 <- Data_sim_unstructured %>%
        filter(Year == years_to_sample[yr]) %>%
        pull(child_i)
      
      #Add together
      to_remove <- c(to_remove1, to_remove2)
      to_remove <- unique(to_remove)
      
      #If there is data in that year take a sample, if not no pseudo-absence data for that year
      if(length(to_remove)>0){
        
        data_to_sample <- network_to_sample %>% 
          filter(!(child_i %in% to_remove)) %>%
          mutate("Year" = years_to_sample[yr])
        
        #length of unstructured data for year of interest 
        n <- nrow(Data_sim_unstructured %>% filter(Year == years_to_sample[yr]))
        
        if(n > 0){
          ## a. As many as the encounter-only data
          sample_1[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 10x as many as the encounter-only data
          sample_2[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
        }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_1 <- do.call(rbind, sample_1)
    sample_2 <- do.call(rbind, sample_2)
    
    
    # 2. Random generation at locations with unsuitable longfin eel habitat
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove NZFFD locations for year of interest
      to_remove1 <- NZFFD_data %>%
        filter(Year == years_to_sample[yr] & `Anguilla dieffenbachii` == 1) %>%
        pull(child_i)
      
      #Remove encounter-only locations for all years except the year of interest
      to_remove2 <- Data_sim_unstructured %>%
        filter(Year == years_to_sample[yr]) %>%
        pull(child_i)
      
      #Add together
      to_remove <- c(to_remove1, to_remove2, suitable_hab_to_remove)
      to_remove <- unique(to_remove)
      
      #If there is data in that year take a sample, if not no pseudo-absence data for that year
      if(length(to_remove)>0){
        
        data_to_sample <- network_to_sample %>% 
          filter(!(child_i %in% to_remove)) %>%
          mutate("Year" = years_to_sample[yr])
        
        #length of unstructured data for year of interest 
        n <- nrow(Data_sim_unstructured %>% filter(Year == years_to_sample[yr]))
        
        if(n > 0){
          ## a. As many as the encounter-only data
          sample_3[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 10x as many as the encounter-only data
          sample_4[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
      }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_3 <- do.call(rbind, sample_3)
    sample_4 <- do.call(rbind, sample_4)
    
    
    
    # 3. Random generation at locations within 2km of a registered road
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove NZFFD locations for year of interest
      to_remove1 <- NZFFD_data %>%
        filter(Year == years_to_sample[yr] & `Anguilla dieffenbachii` == 1) %>%
        pull(child_i)
      
      #Remove encounter-only locations for all years except the year of interest
      to_remove2 <- Data_sim_unstructured %>%
        filter(Year == years_to_sample[yr]) %>%
        pull(child_i)
      
      #Add together
      to_remove <- c(to_remove1, to_remove2, locations_far_from_roads)
      to_remove <- unique(to_remove)
      
      #If there is data in that year take a sample, if not no pseudo-absence data for that year
      if(length(to_remove)>0){
        
        data_to_sample <- network_to_sample %>% 
          filter(!(child_i %in% to_remove)) %>%
          mutate("Year" = years_to_sample[yr])
        
        #length of unstructured data for year of interest 
        n <- nrow(Data_sim_unstructured %>% filter(Year == years_to_sample[yr]))
        
        if(n > 0){
          ## a. As many as the encounter-only data
          sample_5[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 10x as many as the encounter-only data
          sample_6[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
      }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_5 <- do.call(rbind, sample_5)
    sample_6 <- do.call(rbind, sample_6)
    
    
    
    # 4. Random generation at locations within 2km of a registered road
    #    and with unsuitable longfin eel habitat 
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove NZFFD locations for year of interest
      to_remove1 <- NZFFD_data %>%
        filter(Year == years_to_sample[yr] & `Anguilla dieffenbachii` == 1) %>%
        pull(child_i)
      
      #Remove encounter-only locations for all years except the year of interest
      to_remove2 <- Data_sim_unstructured %>%
        filter(Year == years_to_sample[yr]) %>%
        pull(child_i)
      
      #Add together
      to_remove <- c(to_remove1, to_remove2, locations_far_from_roads, suitable_hab_to_remove)
      to_remove <- unique(to_remove)
      
      #If there is data in that year take a sample, if not no pseudo-absence data for that year
      if(length(to_remove)>0){
        
        data_to_sample <- network_to_sample %>% 
          filter(!(child_i %in% to_remove)) %>%
          mutate("Year" = years_to_sample[yr])
        
        #length of unstructured data for year of interest 
        n <- nrow(Data_sim_unstructured %>% filter(Year == years_to_sample[yr]))
        
        if(n > 0){
          ## a. As many as the encounter-only data
          sample_7[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 10x as many as the encounter-only data
          sample_8[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*10),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
      }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_7 <- do.call(rbind, sample_7)
    sample_8 <- do.call(rbind, sample_8)
    
    
    
    
    ##########################
    # Build model input data #
    ##########################
    
    # Need to add together Data_sim_df structured data, Data_sim_unstructured, sampled data
    samples <- paste0("sample_", c(1:8))
    Data_inp_list <- list()
    for(s in samples){
      dat <- get(s)
      
      Data_inp_list[[s]] <- data.frame("Lon" = c(Data_sim_structured$Lon, Data_sim_unstructured$Lon, dat$Lon),
                                       "Lat" = c(Data_sim_structured$Lat, Data_sim_unstructured$Lat, dat$Lat),
                                       "Child_i" = c(Data_sim_structured$child_i, Data_sim_unstructured$child_i, dat$child_i),
                                       "Year" = c(Data_sim_structured$Year, Data_sim_unstructured$Year, dat$Year),
                                       "Catch_KG" = c(Data_sim_structured$Catch_KG, Data_sim_unstructured$Catch_KG, dat$`Anguilla dieffenbachii`),
                                       "Data_source" = c(Data_sim_structured$Data_source, rep("Unstructured", nrow(Data_sim_unstructured) + nrow(dat))),
                                       "Length" = c(Data_sim_structured$Length, Data_sim_unstructured$Length, dat$dist_i))
      
      set.seed(22)
      Data_inp_list[[s]]$Catch_KG = Data_inp_list[[s]]$Catch_KG * exp(1e-3*rnorm(nrow(Data_inp_list[[s]])))
      
    }
    saveRDS(Data_inp_list, file.path(EM_data_path, paste0("EM_data_",sce,"_rep",i, ".rds")))
  }

  
}# Takes about 10mins to run

