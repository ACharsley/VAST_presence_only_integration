###################################################
##                 Combining data                ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                  March 2023                   ##
###################################################




###########################################

rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)

################################
#  Call 'downstream' function  #
################################

source("./Code/funcs.R")


#################
#  Directories  #
#################

raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"
fig_dir <- "./Data_processed/Figures"
pseudoabsence_data_dir <- "./Data_processed/Pseudo_absence_data"

covariate_plot_dir <- file.path(fig_dir, "Covariate_plots")


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))

##Habitat data
#load(file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))
load(file.path(data_taranaki_dir, "Covariate_data.RData"))

##NZFFD observations
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_ene_data.rds"))

##encounter-only data
encounter_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_encounter_only_lf_data.rds"))

##Pseudo-absence data
#Randomly generated data
#a.
Sample_1a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_1a.rds"))

#b.
Sample_1b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_1b.rds"))

#c.
Sample_1c <- readRDS(file.path(pseudoabsence_data_dir, "Sample_1c.rds"))

#d.
Sample_1d <- readRDS(file.path(pseudoabsence_data_dir, "Sample_1d.rds"))

#OM - a.
Sample_OM_1a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_1a.rds"))

#OM - b.
Sample_OM_1b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_1b.rds"))
####


#Randomly generated data at locations with unsuitable longfin eel habitat
#a.
Sample_2a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_2a.rds"))

#b.
Sample_2b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_2b.rds"))

#c.
Sample_2c <- readRDS(file.path(pseudoabsence_data_dir, "Sample_2c.rds"))

#d.
Sample_2d <- readRDS(file.path(pseudoabsence_data_dir, "Sample_2d.rds"))

#OM - a.
Sample_OM_2a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_2a.rds"))

#OM - b.
Sample_OM_2b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_2b.rds"))
####


#Randomly generated data at locations within 2km of a registered road
#a.
Sample_3a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_3a.rds"))

#b.
Sample_3b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_3b.rds"))

#c.
Sample_3c <- readRDS(file.path(pseudoabsence_data_dir, "Sample_3c.rds"))

#d.
Sample_3d <- readRDS(file.path(pseudoabsence_data_dir, "Sample_3d.rds"))

#OM - a.
Sample_OM_3a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_3a.rds"))

#OM - b.
Sample_OM_3b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_3b.rds"))
####


#Randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat 
#a.
Sample_4a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_4a.rds"))

#b.
Sample_4b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_4b.rds"))

#c.
Sample_4c <- readRDS(file.path(pseudoabsence_data_dir, "Sample_4c.rds"))

#d.
Sample_4d <- readRDS(file.path(pseudoabsence_data_dir, "Sample_4d.rds"))

#OM - a.
Sample_OM_4a <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_4a.rds"))

#OM - b.
Sample_OM_4b <- readRDS(file.path(pseudoabsence_data_dir, "Sample_OM_4b.rds"))
####



######################
#  Combine datasets  #
######################

#Data set for habitat unsuitability model
Taranaki_data <- list()
Taranaki_data$network <- network
Taranaki_data$covariate_df <- covariate_df
Taranaki_data$obs <- NZFFD_data 

#Randomly generated pseudo-absence data
#a.
Taranaki_data_1a <- list()
Taranaki_data_1a$network <- network
Taranaki_data_1a$covariate_df <- covariate_df
Taranaki_data_1a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1a)

#b.
Taranaki_data_1b <- list()
Taranaki_data_1b$network <- network
Taranaki_data_1b$covariate_df <- covariate_df
Taranaki_data_1b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1b)

#c.
Taranaki_data_1c <- list()
Taranaki_data_1c$network <- network
Taranaki_data_1c$covariate_df <- covariate_df
Taranaki_data_1c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1c)

#d.
Taranaki_data_1d <- list()
Taranaki_data_1d$network <- network
Taranaki_data_1d$covariate_df <- covariate_df
Taranaki_data_1d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1d)

#OM - a.
Taranaki_data_OM_1a <- list()
Taranaki_data_OM_1a$network <- network
Taranaki_data_OM_1a$covariate_df <- covariate_df
Taranaki_data_OM_1a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_1a)

#OM - b.
Taranaki_data_OM_1b <- list()
Taranaki_data_OM_1b$network <- network
Taranaki_data_OM_1b$covariate_df <- covariate_df
Taranaki_data_OM_1b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_1b)


#Randomly generated data at locations with unsuitable longfin eel habitat
#a.
Taranaki_data_2a <- list()
Taranaki_data_2a$network <- network
Taranaki_data_2a$covariate_df <- covariate_df
Taranaki_data_2a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2a)

#b.
Taranaki_data_2b <- list()
Taranaki_data_2b$network <- network
Taranaki_data_2b$covariate_df <- covariate_df
Taranaki_data_2b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2b)

#c.
Taranaki_data_2c <- list()
Taranaki_data_2c$network <- network
Taranaki_data_2c$covariate_df <- covariate_df
Taranaki_data_2c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2c)

#d.
Taranaki_data_2d <- list()
Taranaki_data_2d$network <- network
Taranaki_data_2d$covariate_df <- covariate_df
Taranaki_data_2d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2d)

#OM - a.
Taranaki_data_OM_2a <- list()
Taranaki_data_OM_2a$network <- network
Taranaki_data_OM_2a$covariate_df <- covariate_df
Taranaki_data_OM_2a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_2a)

#OM - b.
Taranaki_data_OM_2b <- list()
Taranaki_data_OM_2b$network <- network
Taranaki_data_OM_2b$covariate_df <- covariate_df
Taranaki_data_OM_2b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_2b)


#Randomly generated data at locations within 2km of a registered road
#a.
Taranaki_data_3a <- list()
Taranaki_data_3a$network <- network
Taranaki_data_3a$covariate_df <- covariate_df
Taranaki_data_3a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3a)

#b.
Taranaki_data_3b <- list()
Taranaki_data_3b$network <- network
Taranaki_data_3b$covariate_df <- covariate_df
Taranaki_data_3b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3b)

#c.
Taranaki_data_3c <- list()
Taranaki_data_3c$network <- network
Taranaki_data_3c$covariate_df <- covariate_df
Taranaki_data_3c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3c)

#d.
Taranaki_data_3d <- list()
Taranaki_data_3d$network <- network
Taranaki_data_3d$covariate_df <- covariate_df
Taranaki_data_3d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3d)

#OM - a.
Taranaki_data_OM_3a <- list()
Taranaki_data_OM_3a$network <- network
Taranaki_data_OM_3a$covariate_df <- covariate_df
Taranaki_data_OM_3a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_3a)

#OM - b.
Taranaki_data_OM_3b <- list()
Taranaki_data_OM_3b$network <- network
Taranaki_data_OM_3b$covariate_df <- covariate_df
Taranaki_data_OM_3b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_3b)


#Randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat
#a.
Taranaki_data_4a <- list()
Taranaki_data_4a$network <- network
Taranaki_data_4a$covariate_df <- covariate_df
Taranaki_data_4a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4a)

#b.
Taranaki_data_4b <- list()
Taranaki_data_4b$network <- network
Taranaki_data_4b$covariate_df <- covariate_df
Taranaki_data_4b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4b)

#c.
Taranaki_data_4c <- list()
Taranaki_data_4c$network <- network
Taranaki_data_4c$covariate_df <- covariate_df
Taranaki_data_4c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4c)

#d.
Taranaki_data_4d <- list()
Taranaki_data_4d$network <- network
Taranaki_data_4d$covariate_df <- covariate_df
Taranaki_data_4d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4d)

#OM - a.
Taranaki_data_OM_4a <- list()
Taranaki_data_OM_4a$network <- network
Taranaki_data_OM_4a$covariate_df <- covariate_df
Taranaki_data_OM_4a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_4a)

#OM - b.
Taranaki_data_OM_4b <- list()
Taranaki_data_OM_4b$network <- network
Taranaki_data_OM_4b$covariate_df <- covariate_df
Taranaki_data_OM_4b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_4b)



#########################
# Build downstream data #
#########################

#Save data for habitat unsuitability model
Taranaki_data_with_ds <- create_ds_data(Taranaki_data)

#Randomly generated pseudo-absence data
Taranaki_data_1a_with_ds <- create_ds_data(Taranaki_data_1a)
Taranaki_data_1b_with_ds <- create_ds_data(Taranaki_data_1b)
Taranaki_data_1c_with_ds <- create_ds_data(Taranaki_data_1c)
Taranaki_data_1d_with_ds <- create_ds_data(Taranaki_data_1d)

Taranaki_data_OM_1a_with_ds <- create_ds_data(Taranaki_data_OM_1a)
Taranaki_data_OM_1b_with_ds <- create_ds_data(Taranaki_data_OM_1b)


#Randomly generated data at locations with unsuitable longfin eel habitat
Taranaki_data_2a_with_ds <- create_ds_data(Taranaki_data_2a)
Taranaki_data_2b_with_ds <- create_ds_data(Taranaki_data_2b)
Taranaki_data_2c_with_ds <- create_ds_data(Taranaki_data_2c)
Taranaki_data_2d_with_ds <- create_ds_data(Taranaki_data_2d)

Taranaki_data_OM_2a_with_ds <- create_ds_data(Taranaki_data_OM_2a)
Taranaki_data_OM_2b_with_ds <- create_ds_data(Taranaki_data_OM_2b)


#Randomly generated data at locations within 2km of a registered road
Taranaki_data_3a_with_ds <- create_ds_data(Taranaki_data_3a)
Taranaki_data_3b_with_ds <- create_ds_data(Taranaki_data_3b)
Taranaki_data_3c_with_ds <- create_ds_data(Taranaki_data_3c)
Taranaki_data_3d_with_ds <- create_ds_data(Taranaki_data_3d)

Taranaki_data_OM_3a_with_ds <- create_ds_data(Taranaki_data_OM_3a)
Taranaki_data_OM_3b_with_ds <- create_ds_data(Taranaki_data_OM_3b)


#Randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat
Taranaki_data_4a_with_ds <- create_ds_data(Taranaki_data_4a)
Taranaki_data_4b_with_ds <- create_ds_data(Taranaki_data_4b)
Taranaki_data_4c_with_ds <- create_ds_data(Taranaki_data_4c)
Taranaki_data_4d_with_ds <- create_ds_data(Taranaki_data_4d)

Taranaki_data_OM_4a_with_ds <- create_ds_data(Taranaki_data_OM_4a)
Taranaki_data_OM_4b_with_ds <- create_ds_data(Taranaki_data_OM_4b)



########################
#  Save combined data  #
########################

#Save data for habitat unsuitability model
saveRDS(Taranaki_data_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data.rds"))

#Randomly generated pseudo-absence data
saveRDS(Taranaki_data_1a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1a.rds"))
saveRDS(Taranaki_data_1b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1b.rds"))
saveRDS(Taranaki_data_1c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1c.rds"))
saveRDS(Taranaki_data_1d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1d.rds"))

saveRDS(Taranaki_data_OM_1a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_1a.rds"))
saveRDS(Taranaki_data_OM_1b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_1b.rds"))


#Randomly generated data at locations with unsuitable longfin eel habitat
saveRDS(Taranaki_data_2a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2a.rds"))
saveRDS(Taranaki_data_2b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2b.rds"))
saveRDS(Taranaki_data_2c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2c.rds"))
saveRDS(Taranaki_data_2d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2d.rds"))

saveRDS(Taranaki_data_OM_2a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_2a.rds"))
saveRDS(Taranaki_data_OM_2b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_2b.rds"))


#Randomly generated data at locations within 2km of a registered road
saveRDS(Taranaki_data_3a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_3a.rds"))
saveRDS(Taranaki_data_3b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_3b.rds"))
saveRDS(Taranaki_data_3c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_3c.rds"))
saveRDS(Taranaki_data_3d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_3d.rds"))

saveRDS(Taranaki_data_OM_3a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_3a.rds"))
saveRDS(Taranaki_data_OM_3b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_3b.rds"))


#Randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat
saveRDS(Taranaki_data_4a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_4a.rds"))
saveRDS(Taranaki_data_4b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_4b.rds"))
saveRDS(Taranaki_data_4c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_4c.rds"))
saveRDS(Taranaki_data_4d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_4d.rds"))

saveRDS(Taranaki_data_OM_4a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_4a.rds"))
saveRDS(Taranaki_data_OM_4b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_OM_4b.rds"))



# #######################
# # Plot covariate data #
# #######################
# 
# #Covariates
# # covariate_names <- c("std_log_loc_elev", #Elevation
# #                      "std_FWENZ_SegRipShade", #riparian shade
# #                      "std_log_MeanFlowCumecs", #mean flow in cumecs
# #                      "std_FWENZ_segSubstrate", #river substrate
# #                      "std_local_twarm", #average January temperature
# #                      "std_Years_since_barrier", #years since barrier installed
# #                      "Barrier_present" #barrier present
# # )
# covariate_names <- c("std_Dist2Coast")
# 
# for(cov in covariate_names){# cov = "loc_elev"
#   
#   data_to_plot <- Taranaki_data_with_ds$covariate_df_ds %>%
#     mutate("Covariate" = Taranaki_data_with_ds$covariate_df_ds[,cov])
#   
#   if(cov %in% c("std_Years_since_barrier", "Barrier_present")){
#     
#     catchmap1 <- ggplot(data_to_plot) +
#       geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
#       facet_wrap(~Year) +
#       scale_colour_distiller(palette = "RdYlGn") +
#       xlab("Longitude") + ylab("Latitude") +
#       ggtitle(paste0("Catchment map of ", cov)) + 
#       #theme_bw(base_size = 14)
#       theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
#             axis.text.y=element_text(size=6))
#     ggsave(file.path(covariate_plot_dir, paste0("Final_covariate_map_ds - ", cov,".png")), catchmap1)
#     
#   }else{
#     
#     catchmap2 <- ggplot(data_to_plot) +
#       geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
#       scale_colour_distiller(palette = "RdYlGn") +
#       xlab("Longitude") + ylab("Latitude") +
#       ggtitle(paste0("Catchment map of ", cov)) + 
#       theme_bw(base_size = 14)
#     ggsave(file.path(covariate_plot_dir, paste0("Final_covariate_map_ds - ", cov,".png")), catchmap2)
#     
#   }
#   
# }
# 
