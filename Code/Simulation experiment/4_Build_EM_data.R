###################################################
##               Generate EM data                ##
##                                               ##
##               Anthony Charsley                ##
##                September 2024                 ##
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

# Processed data path
data_taranaki_dir <- "./Data_processed"

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



###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
network_to_sample <- network %>% filter(parent_s!=0, FWENZ_isLake!=TRUE)

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

VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_OMs.rds"))



########################################################
########################################################


##############
# Build data #
##############

scenarios <- c("OM_1a", #"OM_1b", #these didn't converge
               "OM_2a", #"OM_2b",
               "OM_3a", "OM_3b",
               "OM_4a", "OM_4b")

set.seed(120924)
for(sce in scenarios){ #sce = "OM_1a"
  
  print(sce)
  
  # Set path to simulated data
  sim_data_path <- file.path(model_path, paste0("Model_", sce, "/Simulated_data"))
  
  # Set data from scenario
  Fish_data <- VAST_input_data[[sce]]
  
  
  ############################
  # Loop over simulated data #
  ############################
  
  for(i in 1:10){ #i <- 1 
    
    # Load simulated data
    Data_sim <- readRDS(file.path(sim_data_path, paste0("Data_sim_", i, ".RDS")))
    
    b_i <- as.numeric(Data_sim$b_i > 0)
    
    # Simulated b_i align with data used to generate OM
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
    
    sample_rand1n = sample_rand5n = list()
    sample_unsuithab1n = sample_unsuithab5n = list()
    sample_nearroads1n = sample_nearroads5n = list()
    sample_unsuithab_nearroads1n = sample_unsuithab_nearroads5n =list()
    
    # 1. Random generation
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove structured data locations for year of interest
      to_remove1 <- Data_sim_structured %>%
        filter(Year == years_to_sample[yr] & Catch_KG == 1) %>%
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
          sample_rand1n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 5x as many as the encounter-only data
          sample_rand5n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
        }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_rand1n <- do.call(rbind, sample_rand1n)
    sample_rand5n <- do.call(rbind, sample_rand5n)
    
    
    # 2. Random generation at locations with unsuitable longfin eel habitat
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove structured data locations for year of interest
      to_remove1 <- Data_sim_structured %>%
        filter(Year == years_to_sample[yr] & Catch_KG == 1) %>%
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
          sample_unsuithab1n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 5x as many as the encounter-only data
          sample_unsuithab5n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
      }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_unsuithab1n <- do.call(rbind, sample_unsuithab1n)
    sample_unsuithab5n <- do.call(rbind, sample_unsuithab5n)
    
    
    
    # 3. Random generation at locations within 2km of a registered road
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove structured data locations for year of interest
      to_remove1 <- Data_sim_structured %>%
        filter(Year == years_to_sample[yr] & Catch_KG == 1) %>%
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
          sample_nearroads1n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 5x as many as the encounter-only data
          sample_nearroads5n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
      }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_nearroads1n <- do.call(rbind, sample_nearroads1n)
    sample_nearroads5n <- do.call(rbind, sample_nearroads5n)
    
    
    
    # 4. Random generation at locations within 2km of a registered road
    #    and with unsuitable longfin eel habitat 
    #Loop over all the years in the encounter-only data
    for(yr in c(1:length(years_to_sample))){ #yr = 33
      
      #print(years_to_sample[yr])
      
      #Remove structured data locations for year of interest
      to_remove1 <- Data_sim_structured %>%
        filter(Year == years_to_sample[yr] & Catch_KG == 1) %>%
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
          sample_unsuithab_nearroads1n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n),]
          
          ## b. 5x as many as the encounter-only data
          sample_unsuithab_nearroads5n[[yr]] <- data_to_sample[sample(c(1:nrow(data_to_sample)), n*5),]
        }
        
        #Remove
        rm(n) ; rm(data_to_sample)
      }
      
      #Remove
      rm(to_remove) ; rm(to_remove1) ; rm(to_remove2)
      
    }
    
    #Combine data sets
    sample_unsuithab_nearroads1n <- do.call(rbind, sample_unsuithab_nearroads1n)
    sample_unsuithab_nearroads5n <- do.call(rbind, sample_unsuithab_nearroads5n)
    
    
    
    
    ##########################
    # Build model input data #
    ##########################
    
    # Need to add together Data_sim_df structured data, Data_sim_unstructured, sampled data
    samples <- c("sample_rand1n", "sample_rand5n",
                 "sample_unsuithab1n", "sample_unsuithab5n",
                 "sample_nearroads1n", "sample_nearroads5n",
                 "sample_unsuithab_nearroads1n", "sample_unsuithab_nearroads5n")
    Data_inp_list <- list()
    for(s in samples){ #s = "sample_rand1n"
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

