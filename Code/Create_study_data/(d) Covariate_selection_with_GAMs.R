###################################################
##      Perform habitat covariate selection      ##
##                   with GAMs                   ##
##                                               ##
##               Anthony Charsley                ##
##                  July 2023                    ##
###################################################

# This code builds species distribution models with GAMs
# to perform habitat covariate selection.

###################################################

rm(list=ls())



#################
#  Directories  #
#################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"

fig_dir <- file.path(data_taranaki_dir, "Figures")


##############
#  Packages  #
##############

library(tidyverse)
library(mgcv)



#############
# Load data #
#############

#NZFFD presence/absence data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))

#Covariate data
load(file.path(data_taranaki_dir, "Covariate_data.RData"))



########################
# Format for modelling #
########################

# #Presence/absence data
# PA_data <- NZFFD_data[,"Anguilla dieffenbachii"]
# 
# #Coordinates
# coords <- as.matrix(NZFFD_data[,c("Lat","Lon")])



#Covariates
covariate_names <- c("std_Dist2Coast", #Distance to coast
                     "std_log_loc_elev", #Elevation
                     "std_log_seg_ro_mm", #Segment rain
                     "std_FWENZ_SegRipShade", #riparian shade
                     "std_log_MeanFlowCumecs", #mean flow in cumecs
                     "std_FWENZ_segSubstrate", #river substrate
                     #"std_local_twarm", #average January temperature
                     "Barrier_present" #barrier present
)

hab_data <- covariate_df %>% select("Lat","Lon","Year",all_of(covariate_names))

#All covariates have low correlations
cor(covariate_df[,covariate_names]) 

#Add NZFFD data with catchability data and therefore restrict hab data to that cooresponding to NZFFD data:
data_full <- left_join(NZFFD_data[,c("Anguilla dieffenbachii","Lat","Lon","Year","FishMethod", "org")], hab_data) #hab_data is ordered by NZFFD_data$child_i, so can simply attach
data_full$Encounter <- data_full$`Anguilla dieffenbachii`

summary(data_full) #no missing

cor(data_full[,covariate_names]) 

data_full$Barrier_present <- factor(data_full$Barrier_present)



###########
# Fit GAM #
###########

# bingam1 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_log_seg_ro_mm, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_log_MeanFlowCumecs, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  s(std_local_twarm, k=4, bs="ts") +
#                  Barrier_present,
#               family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam1)
# 
# bingam2 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_log_MeanFlowCumecs, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  s(std_local_twarm, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam2)
# 
# bingam3 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_log_MeanFlowCumecs, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam3)
# 
# 
# bingam4 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam4)
# 
# bingam5 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam5)
# 
# bingam6 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts"),
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam6)
# 
# bingam7 = gam( Encounter ~ Year + FishMethod + org +
#                  s(std_Dist2Coast, k=4, bs="ts"),
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam7)




## Removing org as it is assumed that catchability doesn't change between organisation for the more reliable 'structured data'


# bingam1b = gam( Encounter ~ Year + FishMethod + #org + 
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_log_seg_ro_mm, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") + 
#                  s(std_log_MeanFlowCumecs, k=4, bs="ts") + 
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  s(std_local_twarm, k=4, bs="ts") + 
#                  Barrier_present, 
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam1b)
# 
# bingam2b = gam( Encounter ~ Year + FishMethod + #org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_log_seg_ro_mm, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  s(std_local_twarm, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam2b)
# 
# bingam3b = gam( Encounter ~ Year + FishMethod + #org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
#                  s(std_local_twarm, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam3b)
# 
# bingam4b = gam( Encounter ~ Year + FishMethod + #org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  s(std_local_twarm, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam4b)
# 
# bingam5b = gam( Encounter ~ Year + FishMethod + #org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  s(std_FWENZ_SegRipShade, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam5b)
# 
# bingam6b = gam( Encounter ~ Year + FishMethod + #org +
#                  s(std_Dist2Coast, k=4, bs="ts") +
#                  Barrier_present,
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam6b)
# 
# bingam7b = gam( Encounter ~ Year + FishMethod + #org + 
#                  s(std_Dist2Coast, k=4, bs="ts"), 
#                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)
# 
# summary(bingam7b)


bingam1b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts") +
                  s(std_log_loc_elev, k=4, bs="ts") +
                  s(std_log_seg_ro_mm, k=4, bs="ts") +
                  s(std_FWENZ_SegRipShade, k=4, bs="ts") + 
                  s(std_log_MeanFlowCumecs, k=4, bs="ts") + 
                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
                  Barrier_present, 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam1b)

#Remove std_log_MeanFlowCumecs
bingam2b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts") +
                  s(std_log_loc_elev, k=4, bs="ts") +
                  s(std_log_seg_ro_mm, k=4, bs="ts") +
                  s(std_FWENZ_SegRipShade, k=4, bs="ts") + 
                  s(std_FWENZ_segSubstrate, k=4, bs="ts") +
                  Barrier_present, 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam2b)

#Remove std_FWENZ_segSubstrate
bingam3b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts") +
                  s(std_log_loc_elev, k=4, bs="ts") +
                  s(std_log_seg_ro_mm, k=4, bs="ts") +
                  s(std_FWENZ_SegRipShade, k=4, bs="ts") + 
                  Barrier_present, 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam3b)

#Remove Barrier_present
bingam4b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts") +
                  s(std_log_loc_elev, k=4, bs="ts") +
                  s(std_log_seg_ro_mm, k=4, bs="ts") +
                  s(std_FWENZ_SegRipShade, k=4, bs="ts"), 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam4b)

#Remove std_log_seg_ro_mm
bingam5b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts") +
                  s(std_log_loc_elev, k=4, bs="ts") +
                  s(std_FWENZ_SegRipShade, k=4, bs="ts"), 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam5b)

#Remove std_FWENZ_SegRipShade
bingam6b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts") +
                  s(std_log_loc_elev, k=4, bs="ts"), 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam6b)

#Remove std_log_loc_elev as edf less than 0.9 and smallest between remaining variables
bingam7b = gam( Encounter ~ Year + FishMethod + #org + 
                  s(std_Dist2Coast, k=4, bs="ts"), 
                family=binomial(link="logit"), select=TRUE, method='REML', data=data_full)

summary(bingam7b) ## small p-value and EDF greater than 0.9
