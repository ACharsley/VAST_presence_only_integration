###################################################
##        Assemble habitat covariate data        ##
##           for the Taranaki region             ##
##                                               ##
##               Anthony Charsley                ##
##                November 2022                  ##
###################################################

# This code assembles habitat covariates using
# REC data ...

###################################################

rm(list=ls())


#################
#  Directories  #
#################

raw_data <- "./Data/raw_data"
data_taranaki_dir <- "./Data/Taranaki"

fig_dir <- file.path(data_taranaki_dir, "Figures")

covariate_plot_dir <- file.path(fig_dir, "Covariate_plots")
dir.create(covariate_plot_dir, showWarnings=FALSE)


##############
#  Packages  #
##############

library(tidyverse)
library(sf)


######################
# Load stream network
######################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))
#network <- network %>% filter(parent_s!=0)


#######################
# Habitat coviariates #
#######################

REC_covs <- c('Shade', 'Substrate', 'Slope', 'AveTWarm', 'Dist2Coast', 'DSDist2Lake')

#Set years
yrs <- c(1978:2021)

###############################

## Set up raw REC covariates ##

## select REC data from network
hab_REC_full <- network %>% 
  mutate("Year" = NA) %>%
  select(Year, Lat, Lon, child_s, FWENZ_isLake, all_of(REC_covs))

#Habitat covariate values at lakes should be set to NA for REC covariates
#i.e. the segment is included but observations / habitat values are not
hab_REC_full[hab_REC_full$FWENZ_isLake==TRUE, c('Shade', 'Substrate', 'Slope', 'AveTWarm', 'Dist2Coast')] <- NA

#Distance to lake is set to zero
hab_REC_full[hab_REC_full$FWENZ_isLake==TRUE, 'DSDist2Lake'] <- 0

#Remove FWENZ_isLake variable
hab_REC_full <- hab_REC_full %>% select(-c("FWENZ_isLake"))

#Some missing in Shade covariate
sapply(1:ncol(hab_REC_full), function(x) length(which(is.na(hab_REC_full[,x])))/nrow(hab_REC_full))

###############################

## Set up raw barrier covariate ##

## select barrier data to then set correctly
raw_barrier_data <- network %>% 
  select(Lat, Lon, child_s, "Year_barrier_finished")


barrier_data_list <- list()

#Loop over all the years and set the areas which are affected
for(y in yrs) {
  
  #Counter
  i <- which(yrs %in% y)
  
  #Set data for each year
  raw_barrier_data_yr <- raw_barrier_data %>%
    mutate("Year" = y, "Years_since_barrier" = y - Year_barrier_finished) 
  
  #Save to a list
  barrier_data_list[[i]] <- raw_barrier_data_yr
  
  #Remove as no longer needed
  rm(raw_barrier_data_yr)
  
}

#bind together
hab_barrier_full <- do.call(rbind, barrier_data_list)

#Fix Years_since_barrier variable
hab_barrier_full$Years_since_barrier <- ifelse(is.na(hab_barrier_full$Years_since_barrier), 0, #if NA, set to 0
                                               ifelse(hab_barrier_full$Years_since_barrier<0, 0, #if less than 0, set to 0
                                                      hab_barrier_full$Years_since_barrier)) #else remain the same

hab_barrier_full$Barrier_present <- ifelse(hab_barrier_full$Years_since_barrier > 0, 1, 0)

#Remove Year_barrier_finished column
hab_barrier_full <- hab_barrier_full %>% select(-c(Year_barrier_finished))

#Set name of barrier covariates
barrier_covs <- c("Years_since_barrier", "Barrier_present")


#Check missing 
sapply(1:ncol(hab_barrier_full), function(x) length(which(is.na(hab_barrier_full[,x])))/nrow(hab_barrier_full))


# ###############################
# 
# ## Set up raw barrier covariate ##
# 
# lu_data <- read_sf(dsn = file.path(raw_data,"Land_use_data"), layer = "lucas-nz-land-use-map-1990-2008-2012-2016-v011") #Data extracted on 7/12/22 from https://data.mfe.govt.nz/layer/52375-lucas-nz-land-use-map-1990-2008-2012-2016-v011/data/
# 
# #Extract coordinates
# lu_data_coords <- as.data.frame(sf::st_coordinates(lu_data)) %>% select(X,Y) %>% rename("Lat"=Y, "Lon"=X)
# 
# 
# ###############################


#########################################################
# Examine habitat covariates and transform as necessary #
#########################################################

## Examine REC covariates ##

# Look at raw data
summary(hab_REC_full[,REC_covs]) # mean and median look approximately similar for all except Dist2Coast_FromMid and DSDIST2LAK

# Raw covariate plots
jpeg(paste0(covariate_plot_dir,"/REC_raw.jpeg"), height=8, width=8,units="in", res=600)
par(mfrow=c(2,3))
boxplot(hab_REC_full$Shade, main="Distribution of shade")

boxplot(hab_REC_full$Substrate, main="Distribution of substrate type")

boxplot(hab_REC_full$Slope, main="Distribution of slope")

boxplot(hab_REC_full$AveTWarm, main="Distribution of average summer temp.")

boxplot(hab_REC_full$Dist2Coast, main="Distribution of distance to coast")

boxplot(hab_REC_full$DSDist2Lake, main="Distribution of distance to lake")
dev.off()

## Examining a log transformation of distance to coast
summary(log(hab_REC_full$Dist2Coast)) #mean and median much closer now

jpeg(paste0(covariate_plot_dir,"/Log_distance_to_coast.jpeg"), height=8, width=8,units="in", res=600)
boxplot(log(hab_REC_full$Dist2Coast), main="Distribution of log distance to coast")
dev.off()

#log
hab_REC_full$log_Dist2Coast <- log(hab_REC_full$Dist2Coast)

#remove raw values
hab_REC_full <- hab_REC_full %>% select(-c("Dist2Coast"))

#Change cov names
REC_covs[REC_covs=="Dist2Coast"] = "log_Dist2Coast"



## Examining a log+0.1 transformation of distance to lake 
summary(log(hab_REC_full$DSDist2Lake+0.1)) #mean and median much closer now

jpeg(paste0(covariate_plot_dir,"/Log_distance_to_lake.jpeg"), height=8, width=8,units="in", res=600)
boxplot(log(hab_REC_full$DSDist2Lake+0.1), main="Distribution of log distance to lake")
dev.off()


#Standardising covariates and log distance to lake should fix up issues

#log
hab_REC_full$log_DSDist2Lake <- log(hab_REC_full$DSDist2Lake+0.1)

#remove raw values
hab_REC_full <- hab_REC_full %>% select(-c("DSDist2Lake"))

#Change cov names
REC_covs[REC_covs=="DSDist2Lake"] = "log_DSDist2Lake"


# Plot fixed covariates
jpeg(paste0(covariate_plot_dir,"/REC_standardised.jpeg"), height=8, width=8,units="in", res=600)
par(mfrow=c(2,3))
boxplot(scale(hab_REC_full$Shade), main="Distribution of std. shade")

boxplot(scale(hab_REC_full$Substrate), main="Distribution of std. substrate type")

boxplot(scale(hab_REC_full$Slope), main="Distribution of std. slope")

boxplot(scale(hab_REC_full$AveTWarm), main="Distribution of std. average summer temp.")

boxplot(scale(hab_REC_full$log_Dist2Coast), main="Distribution of std. distance to coast")

boxplot(scale(hab_REC_full$log_DSDist2Lake), main="Distribution of std. log distance to lake")
dev.off()


###########################################

## Examine years since barrier covariate ##

summary(hab_barrier_full$Years_since_barrier)

summary(scale(hab_barrier_full$Years_since_barrier))


#Raw covariate
jpeg(paste0(covariate_plot_dir,"/Raw_YearsSinceDam.jpeg"), height=8, width=8,units="in", res=600)
boxplot(hab_barrier_full$Years_since_barrier, main="Distribution of years since barrier")
dev.off()

#Standardised covariate
jpeg(paste0(covariate_plot_dir,"/Standardised_YearsSinceDam.jpeg"), height=8, width=8,units="in", res=600)
boxplot(scale(hab_barrier_full$Years_since_barrier), main="Distribution of std. years since barrier")
dev.off()


###########################################



#####################################
# Add covariate data to X_gctp grid #
#####################################

covars_all <- c(REC_covs, barrier_covs)

#Grid of covariates to fill
X_gctp <- array(NA, dim=c(nrow(network),1, length(yrs), length(covars_all)), dimnames=list(network$child_s,NULL,yrs,covars_all))
#NOTE: I'll create X_itp when setting up the model

#Loop over REC covariates to add to grid
for(cov in REC_covs){
  
  print(cov)
  
  for(t in yrs){
    
    #Set REC covariate (same for each year)
    X_gctp[hab_REC_full$child_s,1,as.character(t),as.character(cov)] <- hab_REC_full %>% pull(cov)
    
    
  }
  
  
}


#Loop over time for the barrier covariates to add to grid
for(t in yrs){
  
  print(t)
  
  #Set barrier covariate
  X_gctp[hab_REC_full$child_s,1,as.character(t),"Years_since_barrier"] <- hab_barrier_full %>% filter(Year == t) %>% pull(Years_since_barrier)
   
  #Set barrier covariate
  X_gctp[hab_REC_full$child_s,1,as.character(t),"Barrier_present"] <- hab_barrier_full %>% filter(Year == t) %>% pull(Barrier_present) 

}


############################
# Interpolate missing data #
############################

# Loop to perform nearest neighbour algorithm and apply to matrix - not for dam variable
for(covar in REC_covs){
  
  #Rename so that hab_REC_full is not lost
  Data_NN <- hab_REC_full
  
  #IF any is missing then proceed
  some_missing <- any(is.na(Data_NN[,covar]))
  
  if(some_missing){
    
    #Set the complete and missing data sets for a particular covar
    Data_complete <- Data_NN[!is.na(Data_NN[,covar]),]
    Data_missing <- Data_NN[which(is.na(Data_NN[,covar])), c("Lat","Lon")]
    
    #Find the nearest neighbour of the missing data using coordinates
    NN = RANN::nn2(data=Data_complete[,c("Lat","Lon")], query=Data_missing, k=1)
    
    #Input the data with the nearest neighbour data
    Data_NN[is.na(Data_NN[,covar]), covar] <- Data_complete[NN$nn.idx, covar]
    
  }
  
  #Order Data_NN by child_s so that it's added to the right row of X_gctp
  Data_NN <- Data_NN[order(Data_NN$child_s),]
  
  #Input this into a grid to be used in VAST
  X_gctp[Data_NN$child_s,,,which(covars_all==covar)] <- Data_NN[,covar]
  
}


###############################################################
#    Standardise, check missingness, update names and save    #
###############################################################


#Standardise
for(p in covars_all[!(covars_all == "Barrier_present")]){
  
  print(p)
  
  X_gctp[,1,,as.character(p)] <- (X_gctp[,1,,as.character(p)] - mean(X_gctp[,1,,as.character(p)]))/sd(X_gctp[,1,,as.character(p)])
}

#update X_gctp dimnames
dimnames(X_gctp)[[4]] <- c(paste0("std_", covars_all[!(covars_all == "Barrier_present")]), "Barrier_present")

#any(is.na(X_gctp))

for(p in dimnames(X_gctp)[[4]]){print(any(is.na(X_gctp[,,,p])))}

## Save ##
save(X_gctp, file=file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))



