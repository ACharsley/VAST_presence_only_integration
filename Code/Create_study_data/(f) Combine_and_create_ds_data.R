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


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))

##Habitat data
load(file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))

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
Taranaki_data$X_gctp <- X_gctp
Taranaki_data$obs <- NZFFD_data 

#Randomly generated pseudo-absence data
#a.
Taranaki_data_1a <- list()
Taranaki_data_1a$network <- network
Taranaki_data_1a$X_gctp <- X_gctp
Taranaki_data_1a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1a)

#b.
Taranaki_data_1b <- list()
Taranaki_data_1b$network <- network
Taranaki_data_1b$X_gctp <- X_gctp
Taranaki_data_1b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1b)

#c.
Taranaki_data_1c <- list()
Taranaki_data_1c$network <- network
Taranaki_data_1c$X_gctp <- X_gctp
Taranaki_data_1c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1c)

#d.
Taranaki_data_1d <- list()
Taranaki_data_1d$network <- network
Taranaki_data_1d$X_gctp <- X_gctp
Taranaki_data_1d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_1d)

#OM - a.
Taranaki_data_OM_1a <- list()
Taranaki_data_OM_1a$network <- network
Taranaki_data_OM_1a$X_gctp <- X_gctp
Taranaki_data_OM_1a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_1a)

#OM - b.
Taranaki_data_OM_1b <- list()
Taranaki_data_OM_1b$network <- network
Taranaki_data_OM_1b$X_gctp <- X_gctp
Taranaki_data_OM_1b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_1b)


#Randomly generated data at locations with unsuitable longfin eel habitat
#a.
Taranaki_data_2a <- list()
Taranaki_data_2a$network <- network
Taranaki_data_2a$X_gctp <- X_gctp
Taranaki_data_2a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2a)

#b.
Taranaki_data_2b <- list()
Taranaki_data_2b$network <- network
Taranaki_data_2b$X_gctp <- X_gctp
Taranaki_data_2b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2b)

#c.
Taranaki_data_2c <- list()
Taranaki_data_2c$network <- network
Taranaki_data_2c$X_gctp <- X_gctp
Taranaki_data_2c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2c)

#d.
Taranaki_data_2d <- list()
Taranaki_data_2d$network <- network
Taranaki_data_2d$X_gctp <- X_gctp
Taranaki_data_2d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_2d)

#OM - a.
Taranaki_data_OM_2a <- list()
Taranaki_data_OM_2a$network <- network
Taranaki_data_OM_2a$X_gctp <- X_gctp
Taranaki_data_OM_2a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_2a)

#OM - b.
Taranaki_data_OM_2b <- list()
Taranaki_data_OM_2b$network <- network
Taranaki_data_OM_2b$X_gctp <- X_gctp
Taranaki_data_OM_2b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_2b)


#Randomly generated data at locations within 2km of a registered road
#a.
Taranaki_data_3a <- list()
Taranaki_data_3a$network <- network
Taranaki_data_3a$X_gctp <- X_gctp
Taranaki_data_3a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3a)

#b.
Taranaki_data_3b <- list()
Taranaki_data_3b$network <- network
Taranaki_data_3b$X_gctp <- X_gctp
Taranaki_data_3b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3b)

#c.
Taranaki_data_3c <- list()
Taranaki_data_3c$network <- network
Taranaki_data_3c$X_gctp <- X_gctp
Taranaki_data_3c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3c)

#d.
Taranaki_data_3d <- list()
Taranaki_data_3d$network <- network
Taranaki_data_3d$X_gctp <- X_gctp
Taranaki_data_3d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_3d)

#OM - a.
Taranaki_data_OM_3a <- list()
Taranaki_data_OM_3a$network <- network
Taranaki_data_OM_3a$X_gctp <- X_gctp
Taranaki_data_OM_3a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_3a)

#OM - b.
Taranaki_data_OM_3b <- list()
Taranaki_data_OM_3b$network <- network
Taranaki_data_OM_3b$X_gctp <- X_gctp
Taranaki_data_OM_3b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_3b)


#Randomly generated data at locations within 2km of a registered road and with unsuitable longfin eel habitat
#a.
Taranaki_data_4a <- list()
Taranaki_data_4a$network <- network
Taranaki_data_4a$X_gctp <- X_gctp
Taranaki_data_4a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4a)

#b.
Taranaki_data_4b <- list()
Taranaki_data_4b$network <- network
Taranaki_data_4b$X_gctp <- X_gctp
Taranaki_data_4b$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4b)

#c.
Taranaki_data_4c <- list()
Taranaki_data_4c$network <- network
Taranaki_data_4c$X_gctp <- X_gctp
Taranaki_data_4c$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4c)

#d.
Taranaki_data_4d <- list()
Taranaki_data_4d$network <- network
Taranaki_data_4d$X_gctp <- X_gctp
Taranaki_data_4d$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_4d)

#OM - a.
Taranaki_data_OM_4a <- list()
Taranaki_data_OM_4a$network <- network
Taranaki_data_OM_4a$X_gctp <- X_gctp
Taranaki_data_OM_4a$obs <- rbind(NZFFD_data, encounter_only_lf_data, Sample_OM_4a)

#OM - b.
Taranaki_data_OM_4b <- list()
Taranaki_data_OM_4b$network <- network
Taranaki_data_OM_4b$X_gctp <- X_gctp
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



############################
# Check habitat data is ok #
############################

#Check with whole network
network_check <- Taranaki_data_1a_with_ds$network
hab_check <- data.frame("Lon"=network_check$Lon, "Lat"=network_check$Lat, 
                        "Elevation"=Taranaki_data_1a_with_ds$X_gctp[,1,"2022","std_log_loc_elev"])

l1 <- lapply(1:nrow(network_check), function(x){
  parent <- network_check$parent_s[x]
  find <- network_check %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network_check[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network_check[x,], 'Lon2'=NA, 'Lat2'=NA)
  return(out)
})
l1 <- do.call(rbind, l1)

catchmap <- ggplot() +
  geom_point(data=network_check, aes(x = Lon, y = Lat), col="gray") +
  geom_segment(data=l1, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(data=hab_check, aes(x = Lon, y = Lat, col=Elevation), alpha=0.6) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Taranaki river network map - Elevation") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Taranaki_elevation.png"), catchmap)





#Check with downstream network
network_sub <- Taranaki_data_1a_with_ds$network_ds
hab_sub <- data.frame("Lon"=network_sub$Lon, "Lat"=network_sub$Lat, 
                      "Elevation"=Taranaki_data_1a_with_ds$X_gctp_ds[,1,"2022","std_log_loc_elev"])

l2 <- lapply(1:nrow(network_sub), function(x){
  parent <- network_sub$parent_s[x]
  find <- network_sub %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network_sub[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network_sub[x,], 'Lon2'=NA, 'Lat2'=NA)
  return(out)
})
l2 <- do.call(rbind, l2)

catchmap <- ggplot() +
  geom_point(data=network_sub, aes(x = Lon, y = Lat), col="gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(data=hab_sub, aes(x = Lon, y = Lat, col=Elevation), alpha=0.6) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Taranaki river network map - Elevation") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Taranaki_elevation_ds.png"), catchmap)


