###################################################
##              Build VAST inputs for            ##
##                Operating Models               ##
##                                               ##
##               Anthony Charsley                ##
##                September 2024                 ##
###################################################

# This code generates VAST input data for OMs of the simulation study.

###########################################

rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)



#################
#  Directories  #
#################

raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"
VAST_input_data_dir <- "./Data_processed/VAST_input_data"
pseudoabsence_data_dir <- "./Data_processed/Pseudo_absence_data"



#################################
# Scenarios to build inputs for #
#################################

scenarios <- c("OM_1a", "OM_1b", "OM_1c", "OM_1d", #random generation
               "OM_2a", "OM_2b", "OM_2c", "OM_2d", #unsuitable habitat
               "OM_3a", "OM_3b", "OM_3c", "OM_3d", #near roads
               "OM_4a", "OM_4b", "OM_4c", "OM_4d") #unsuitable habitat and near roads

VAST_input_data <- list()



####################
# Build input data #
####################

## Network data ##
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
VAST_input_data[["network"]] <- network


## Covariate data ##
load(file.path(data_taranaki_dir, "Covariate_data.RData"))
covariate_df <- covariate_df %>% filter(Year %in% c(1978:2022)) # This ensures that if any years are dropped then covariate_df has the right years still

VAST_input_data[["covariate_data"]] <- covariate_df


## Presence/absence data and presence/pseudo-absences data ##
## NZFFD observations ##
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))

## encounter-only data ##
encounter_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

# Loop over all scenarios to combine presence/absence data and presence/pseudo-absences data, and 
# add data to VAST data input list
for(sce in scenarios){ #sce = "OM_1a"
  
  print(sce)
  
  
  ###################################################
  # Load pseudo-absences and build all observations #
  ###################################################
  
  pseudo_absences <- readRDS(file.path(pseudoabsence_data_dir, paste0("Sample_",sce,".rds")))
  obs <- rbind(NZFFD_data, encounter_only_lf_data, pseudo_absences)
  
  
  ########################
  # Set data up for VAST #
  ########################
  
  ## 1. Examine data type variable
  fct_count(obs$Data_source)
  
  ## 2. Examine fishing method
  fct_count(obs$FishMethod)
  
  ## 3. Examine fish samplers
  fct_count(obs$org) #Will assume catchability is constant between organisations
  
  #Examine fishing method and fish samplers together
  addmargins(table(obs$FishMethod, obs$org, useNA = "ifany"))
  
  ## 4. Examine Years of data
  addmargins(table(obs$`Anguilla dieffenbachii`, obs$Year))
  #addmargins(table(obs$Year, obs$`Anguilla dieffenbachii`))
  
  addmargins(table(obs$Year, obs$Data_source)) #Some years, data type doesn't overlap. Is that an issue?
  
  ## 5. Create final dataset
  #Data for longfin eel catch
  Data_Geostat = data.frame(Lon=obs$Lon,
                            Lat=obs$Lat,
                            Child_i=obs$child_i,
                            Year=obs$Year,
                            Catch_KG=obs$`Anguilla dieffenbachii`, 
                            Data_source=obs$Data_source,
                            Sampler = obs$org,
                            FishMethod = obs$FishMethod,
                            Length = obs$dist_i) 
  
  set.seed(22)
  Data_Geostat[,'Catch_KG'] = Data_Geostat[,'Catch_KG'] * exp(1e-3*rnorm(nrow(Data_Geostat)))
  
  ##Final data set
  #pander::pandoc.table( Data_Geostat[1:6,], digits=6 ) #table of the first 6 observations
  
  ###############
  ###############
  
  ## Save data ##
  VAST_input_data[[sce]]<- Data_Geostat
  
}

saveRDS(VAST_input_data, file.path(VAST_input_data_dir, "VAST_input_data_OMs.rds"))
