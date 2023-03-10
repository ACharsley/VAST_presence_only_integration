###################################################
##        Assemble habitat covariate data        ##
##           for the Taranaki region             ##
##                                               ##
##               Anthony Charsley                ##
##                 March 2022                    ##
###################################################

# This code assembles habitat covariates using
# REC data.

###################################################

rm(list=ls())


#################
#  Directories  #
#################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw/Eel_application_Taranaki"
data_taranaki_dir <- "./Data_processed/Taranaki"

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

# REC_covs <- c('Shade', 'Substrate', 'Slope', 'AveTWarm', 'Dist2Coast', 'DSDist2Lake')
# REC_covs <- c("Dist2Coast", "StreamOrder", "sinuosity", "segslpmean", "seg_ro_mm", "FWENZ_usHard",
#               "loc_elev", "us_slope", "loc_penpet", "loc_rnvar", "loc_rd100", "lc_phos", "us_phos",
#               "loc_psize", "local_twarm", "DSDIST2LAK", "FWENZ_dsMaxSlope", "FWENZ_dsAveSlope", "us_ind",
#               "FWENZ_USCalcium", "FWENZ_USLakePC", "FWENZ_segShade", "MeanFlowCumecs", "FWENZ_usLowFlow",
#               "FWENZ_SegFlowStability", "FWENZ_SegRipShade", "FWENZ_segSubstrate", "loc_slope", "FWENZ_segAveTWarm",
#               "Dist2Coast_FromMid")
REC_covs <- c("loc_elev", "FWENZ_SegRipShade", "MeanFlowCumecs", "FWENZ_segSubstrate", "local_twarm")

#Identify covariates to use through expert opinion

#Set years
yrs <- c(1966:2022) #Use all these years for now but will remove some later

###############################

## Set up raw REC covariates ##

## select REC data from network
hab_REC_full <- network %>% 
  mutate("Year" = NA) %>%
  select(Year, Lat, Lon, child_s, FWENZ_isLake, all_of(REC_covs))

# #Habitat covariate values at lakes should be set to NA for REC covariates
# #i.e. the segment is included but observations / habitat values are not
# hab_REC_full[hab_REC_full$FWENZ_isLake==TRUE, REC_covs[!REC_covs == "DSDIST2LAK"]] <- NA
# 
# #Distance to lake is set to zero
# hab_REC_full[hab_REC_full$FWENZ_isLake==TRUE, 'DSDIST2LAK'] <- 0
# 
# #Remove FWENZ_isLake variable
# hab_REC_full <- hab_REC_full %>% select(-c("FWENZ_isLake"))

#Some missing
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



#########################################################
# Examine habitat covariates and transform as necessary #
#########################################################

## Examine REC covariates ##

for(j in c(1:length(REC_covs))){
  
  # Raw covariate plots
  jpeg(paste0(covariate_plot_dir,"/REC_raw_",REC_covs[j],".jpeg"), height=8, width=8,units="in", res=600)
  hist(hab_REC_full[,REC_covs[j]], main=paste0("Distribution of ",REC_covs[j]))
  abline(v=mean(hab_REC_full[,REC_covs[j]], na.rm = T),col="red")
  abline(v=median(hab_REC_full[,REC_covs[j]], na.rm = T),col="blue")
  legend("topright", c("Mean", "Median"), col = c("red", "blue"), lty = 1, lwd=3)
  dev.off()
  
  
}

# Look at raw data
summary(hab_REC_full[,REC_covs]) 

# The variables loc_elev, FWENZ_SegRipShade, MeanFlowCumecs and local_twarm are either
# skewed or have mean/medians that aren't close



##################################################
# Examine possible transformations of covariates #
##################################################

# log transform these
summary(log(hab_REC_full$loc_elev+1)) #add 1 because a few elevation values are below zero, mean and median much closer now
# hist(log(hab_REC_full$loc_elev+1))
summary((hab_REC_full$FWENZ_SegRipShade)^2) #mean and median much closer using square
# hist((hab_REC_full$FWENZ_SegRipShade)^2) #distribution is still not great
summary(log(hab_REC_full$MeanFlowCumecs)) #mean and median much closer now
# hist(log(hab_REC_full$MeanFlowCumecs)) #log does the best job

# I will leave local_twarm as is because the skew is mostly due to a few extreme values (that are possible and therefore not
# outliers)


#Perform transformation
hab_REC_full$log_loc_elev <- log(hab_REC_full$loc_elev+1)
hab_REC_full$SegRipShade_sqrd <- (hab_REC_full$FWENZ_SegRipShade)^2
hab_REC_full$log_MeanFlowCumecs <- log(hab_REC_full$MeanFlowCumecs)

#remove raw values
hab_REC_full <- hab_REC_full %>% select(-c("loc_elev", "FWENZ_SegRipShade", "MeanFlowCumecs"))

#Change cov names
REC_covs[REC_covs=="loc_elev"] = "log_loc_elev"
REC_covs[REC_covs=="FWENZ_SegRipShade"] = "SegRipShade_sqrd"
REC_covs[REC_covs=="MeanFlowCumecs"] = "log_MeanFlowCumecs"


# Plot covariates transformed and standardised
for(j in c(1:length(REC_covs))){
  
  std_data <- as.vector(scale(hab_REC_full[,REC_covs[j]]))
  
  # Standardised covariate plots
  jpeg(paste0(covariate_plot_dir,"/REC_std_",REC_covs[j],".jpeg"), height=8, width=8,units="in", res=600)
  hist(std_data, main=paste0("Distribution of std. ",REC_covs[j]))
  abline(v=mean(std_data, na.rm = T),col="red")
  abline(v=median(std_data, na.rm = T),col="blue")
  legend("topright", c("Mean", "Median"), col = c("red", "blue"), lty = 1, lwd=3)
  dev.off()
  
  
}

## Covariate data is certainly not perfect but improves significantly

###########################################

## Examine years since barrier covariate ##

summary(hab_barrier_full$Years_since_barrier)
summary(scale(hab_barrier_full$Years_since_barrier))


#Raw covariate
jpeg(paste0(covariate_plot_dir,"/Raw_YearsSinceDam.jpeg"), height=8, width=8,units="in", res=600)
hist(hab_barrier_full$Years_since_barrier, main="Distribution of years since barrier")
dev.off()

#Standardised covariate
jpeg(paste0(covariate_plot_dir,"/Standardised_YearsSinceDam.jpeg"), height=8, width=8,units="in", res=600)
hist(scale(hab_barrier_full$Years_since_barrier), main="Distribution of std. years since barrier")
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



