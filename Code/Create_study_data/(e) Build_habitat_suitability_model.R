####################################################
##        Build a habitat suitability model       ##
##                                                ##
##               Anthony Charsley                 ##
##                  March 2023                    ##
####################################################


rm(list=ls())


##############
#  Packages  #
##############

library(biomod2)
library(terra)
library(tidyverse)

#################
#  Directories  #
#################

primary_directory <- getwd()

data_taranaki_dir <- file.path(getwd(), "Data_processed")

model_dir <- file.path(primary_directory, "Eel_HSM_taranaki")
dir.create(model_dir, showWarnings=FALSE)


###################
#  Load datasets  #
###################

##NZFFD presence/absence data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))
NZFFD_data$FishMethod <- ifelse(NZFFD_data$FishMethod == "Electric fishing", "Electric fishing", "NetTrap")
NZFFD_data$FishMethod <- as.factor(NZFFD_data$FishMethod)

##Habitat data
#load(file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))
load(file.path(data_taranaki_dir, "Covariate_data.RData"))

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))


############################
# Change working directory #
############################

setwd(model_dir)


########################
# Format for modelling #
########################

#Presence/absence data
PA_data <- NZFFD_data %>%  
  select(c("Anguilla dieffenbachii")) %>% 
  pull()

#Coordinates
coords <- as.matrix(NZFFD_data[,c("Lat","Lon")])

#Covariates
# covariate_names <- c("std_Dist2Coast", #Distance to coast
#                      "std_log_seg_ro_mm", #Segment rain
#                      "std_FWENZ_SegRipShade", #riparian shade
#                      "std_log_MeanFlowCumecs", #mean flow in cumecs
#                      "std_FWENZ_segSubstrate", #river substrate
#                      "std_local_twarm", #average January temperature
#                      "Barrier_present" #barrier present
# )

#Only using distance to coast as identified in (d)
covariate_names <- c("std_log_loc_elev", "std_FWENZ_SegRipShade","std_FWENZ_segSubstrate")

#hab_data <- X_gctp[NZFFD_data$child_i,,"2022",covariate_names]
#hab_data <- data.frame(hab_data)

hab_data <- covariate_df %>% select("Lat","Lon","Year",all_of(covariate_names))
#hab_data$Barrier_present <- factor(hab_data$Barrier_present)

# #Add NZFFD data with catchability data and therefore restrict hab data to that cooresponding to NZFFD data:
hab_data <- right_join(hab_data, NZFFD_data[,c("Lat","Lon","Year","FishMethod")])


summary(hab_data) #no missing

#Remove lat, long, year
hab_data <- hab_data %>% select(-c("Lat","Lon","Year"))


#Data set to make projections on to
pred_data <- covariate_df %>%
  filter(Year == 2022) %>%
  select(all_of(covariate_names)) %>%
  mutate("FishMethod" = factor("Electric fishing"))

#pred_data$Barrier_present <- factor(pred_data$Barrier_present)

pred_coords <- covariate_df %>%
  filter(Year == 2022) %>%
  select("Lat","Lon")

myBiomodData <- BIOMOD_FormatingData(resp.var = PA_data,
                                     expl.var = hab_data, 
                                     resp.xy = coords,
                                     resp.name = "longfin_eel")

myBiomodData
plot(myBiomodData)


#############
# Run Model #
#############

set.seed(280223)
#Can take a few mins when using 10-fold CV
# myBiomodModelOut <- BIOMOD_Modeling(data = myBiomodData,
#                                     models = c("GLM", "GAM", "ANN", 
#                                                "MARS", "RF", "MAXENT.Phillips"),
#                                     models.options = myBiomodOptions #, DataSplitTable = myBiomodCV
#                                     )
# won't use GBM, SRE and MAXENT.Phillips.2 as they can't use factor variables. 
#Flexible Discriminant Analysis (FDA) model fails to make projections, so removed.
#Classification tree analysis (CTA) model makes strange predictions (same across the catchment), so removed.

myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    models = c("GLM", "GAM", "ANN", 
                                               "MARS", "RF", "MAXENT.Phillips",
                                               "FDA", "CTA"),
                                    CV.strategy = "random",
                                    CV.perc = 0.8
                                    #models.options = myBiomodOptions #, DataSplitTable = myBiomodCV
)

myBiomodModelOut


#####################
# Get model outputs #
#####################

# # Get evaluation scores
# model_cv_outputs <- get_evaluations(myBiomodModelOut)


# Project models
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  proj.name = 'Habitat_suitability',
                                  new.env = pred_data,
                                  new.env.xy = pred_coords#, binary.meth = "TSS"
)
myBiomodProj

#evaluate projections
plot(myBiomodProj) 
# View(myBiomodProj@proj.out@val)

# Ensemble predictions
ensemblemod <- BIOMOD_EnsembleModeling(myBiomodModelOut,
                                       metric.eval = c("KAPPA", 'TSS', "ROC"))

ensembleproj <- BIOMOD_EnsembleForecasting(bm.em = ensemblemod, 
                                           bm.proj = myBiomodProj)


#View(ensembleproj@proj.out@val[ensembleproj@proj.out@val$filtered.by=="ROC",])

#Set encounter probabilities and save. NOTE: will keep the predictions from metric ROC
HSM_encounter_prob <- data.frame("POE" = as.vector(ensembleproj@proj.out@val[ensembleproj@proj.out@val$filtered.by=="ROC","pred"]/1000), 
                                 "Lat" = ensembleproj@coord$Lat, 
                                 "Lon" = ensembleproj@coord$Lon)

#Join back to network
network_to_join <- network %>% select(nzsegment, Lat, Lon, parent_s, child_s, dist_s)
HSM_encounter_prob <- full_join(HSM_encounter_prob, network_to_join)

#save encounter probabilities
save(HSM_encounter_prob, file = file.path(model_dir, "HSM_encounter_prob.RData"))



####################################################
# Build catchment plot with ensemble model results #
####################################################

l1 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
  return(out)
})
l1 <- do.call(rbind, l1)

catchmap <- ggplot() +
  geom_point(data=network, aes(x = Lon, y = Lat), col="gray") +
  geom_segment(data=l1, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(data=HSM_encounter_prob, aes(x = Lon, y = Lat, col=POE), alpha=0.6) +
  scale_color_distiller(palette = "RdYlGn", limits = c(0,1), direction = 1) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel habitat suitability in Taranaki, NZ") +
  theme_bw(base_size = 14)
ggsave(file.path(model_dir, "Habitat_suitability_map.png"), catchmap)



#################################
# Change working directory back #
#################################

setwd(primary_directory)


