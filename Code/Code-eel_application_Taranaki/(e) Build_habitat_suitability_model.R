####################################################
##        Build a habitat suitability model       ##
##                                                ##
##               Anthony Charsley                 ##
##                February 2023                   ##
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

data_taranaki_dir <- file.path(getwd(), "Data_processed/Taranaki")

model_dir <- file.path(primary_directory, "Eel_HSM_taranaki")
dir.create(model_dir, showWarnings=FALSE)


###################
#  Load datasets  #
###################

##NZFFD presence/absence data
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_pa_data.rds"))

##Habitat data
load(file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))

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
PA_data <- NZFFD_data[,"Anguilla dieffenbachii"]

#Coordinates
coords <- as.matrix(NZFFD_data[,c("Lat","Lon")])

#Covariates
covariate_names <- c("std_log_Dist2Coast", #distance to coast
                     "std_StreamOrder", #stream order
                     "std_log_seg_ro_mm", #rainfall runoff
                     "std_FWENZ_SegRipShade", #riparian shade
                     "std_MeanFlowCumecs", #mean flow in cumecs
                     "std_FWENZ_segSubstrate", #river substrate
                     "std_Years_since_barrier" #years since a barrier was installed
                     )

hab_data <- X_gctp[NZFFD_data$child_i,,"2021",covariate_names]

#Data set to make projections on to
pred_data <- X_gctp[network$child_s,,"2021",covariate_names]
pred_coords <- network[,c("Lat","Lon")]


myBiomodData <- BIOMOD_FormatingData(resp.var = PA_data,
                                     expl.var = hab_data, 
                                     resp.xy = coords,
                                     resp.name = "longfin_eel")

myBiomodData
plot(myBiomodData)

# Create default modeling options
myBiomodOptions <- BIOMOD_ModelingOptions(GAM = list(k = 3)) #Change k as error occurs when k=-1
myBiomodOptions
#Print_Default_ModelingOptions()

# ####################################
# # Create cross-validation datasets #
# ####################################
# 
# myBiomodCV <- BIOMOD_cv(data = myBiomodData,
#                         k=10,
#                         repetition = 1)
# head(myBiomodCV)
# 
# 

#############
# Run Model #
#############

set.seed(280223)
#Can take a few mins when using 10-fold CV
myBiomodModelOut <- BIOMOD_Modeling(data = myBiomodData,
                                    models.options = myBiomodOptions #, DataSplitTable = myBiomodCV
                                    )

myBiomodModelOut


#####################
# Get model outputs #
#####################

# # Get evaluation scores
# model_cv_outputs <- get_evaluations(myBiomodModelOut)


# Project models
myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                  proj.name = 'Habitat_suitability',
                                  new.env = pred_data,
                                  xy.new.env = pred_coords#, binary.meth = "TSS"
                                  )
myBiomodProj

#evaluate projections
plot(myBiomodProj) 
#View(myBiomodProj@proj@val[,,1,1]/1000) 
summary((myBiomodProj@proj@val[,,1,1]/1000)) #NOTE: SRE predictions are binary

# Ensemble predictions
ensemblemod <- BIOMOD_EnsembleModeling(myBiomodModelOut,
                                       eval.metric = c('TSS'))

ensembleproj <- BIOMOD_EnsembleForecasting(ensemblemod, 
                                           projection.output = myBiomodProj)

#Set encounter probabilities and save
HSM_encounter_prob <- data.frame("POE" = as.vector(ensembleproj@proj@val/1000), 
                                 "Lat" = ensembleproj@xy.coord[,"Lat"], 
                                 "Lon" = ensembleproj@xy.coord[,"Lon"])

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


