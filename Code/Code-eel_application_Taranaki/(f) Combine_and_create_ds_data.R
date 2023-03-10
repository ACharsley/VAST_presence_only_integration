###################################################
##                 Combining data                ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                January 2023                   ##
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

source("./Code/Code-eel_application_Taranaki/funcs.R")


#################
#  Directories  #
#################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw/Eel_application_Taranaki"
data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- "./Data_processed/Taranaki/Figures"
pseudoabsence_data_dir <- "./Data_processed/Taranaki/Pseudo_absence_data"


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))

##Habitat data
load(file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))

##NZFFD observations
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_pa_data.rds"))

##Presence-only data
presence_only_lf_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_presence_only_lf_data.rds"))

##Pseudo-absence data
#Randomly generated data
#a.
sample_1a <- readRDS(file.path(pseudoabsence_data_dir, "Random_sample_1a.rds"))

#b.
sample_1b <- readRDS(file.path(pseudoabsence_data_dir, "Random_sample_1b.rds"))

#c.
sample_1c <- readRDS(file.path(pseudoabsence_data_dir, "Random_sample_1c.rds"))

#d.
sample_1d <- readRDS(file.path(pseudoabsence_data_dir, "Random_sample_1d.rds"))


# #Spatially biased data
# #a.
# sample_2a <- readRDS(file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2a.rds"))
# 
# #b.
# sample_2b <- readRDS(file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2b.rds"))
# 
# #c.
# sample_2c <- readRDS(file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2c.rds"))
# 
# #d.
# sample_2d <- readRDS(file.path(pseudoabsence_data_dir, "Spatially_biased_sample_2d.rds"))


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
Taranaki_data_1a$obs <- rbind(NZFFD_data, presence_only_lf_data, sample_1a)

#b.
Taranaki_data_1b <- list()
Taranaki_data_1b$network <- network
Taranaki_data_1b$X_gctp <- X_gctp
Taranaki_data_1b$obs <- rbind(NZFFD_data, presence_only_lf_data, sample_1b)

#c.
Taranaki_data_1c <- list()
Taranaki_data_1c$network <- network
Taranaki_data_1c$X_gctp <- X_gctp
Taranaki_data_1c$obs <- rbind(NZFFD_data, presence_only_lf_data, sample_1c)

#d.
Taranaki_data_1d <- list()
Taranaki_data_1d$network <- network
Taranaki_data_1d$X_gctp <- X_gctp
Taranaki_data_1d$obs <- rbind(NZFFD_data, presence_only_lf_data, sample_1d)


# #Spatially biased pseudo-absence data
# #a.
# Taranaki_data_2a <- list()
# Taranaki_data_2a$network <- network
# Taranaki_data_2a$X_gctp <- X_gctp
# Taranaki_data_2a$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
# Taranaki_data_2a$pseudo_absence_data <- sample_2a
# 
# #b.
# Taranaki_data_2b <- list()
# Taranaki_data_2b$network <- network
# Taranaki_data_2b$X_gctp <- X_gctp
# Taranaki_data_2b$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
# Taranaki_data_2b$pseudo_absence_data <- sample_2b
# 
# #c.
# Taranaki_data_2c <- list()
# Taranaki_data_2c$network <- network
# Taranaki_data_2c$X_gctp <- X_gctp
# Taranaki_data_2c$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
# Taranaki_data_2c$pseudo_absence_data <- sample_2c
# 
# #d.
# Taranaki_data_2d <- list()
# Taranaki_data_2d$network <- network
# Taranaki_data_2d$X_gctp <- X_gctp
# Taranaki_data_2d$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
# Taranaki_data_2d$pseudo_absence_data <- sample_2d


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

# #Spatially biased pseudo-absence data
# Taranaki_data_2a_with_ds <- create_ds_data(Taranaki_data_2a)
# Taranaki_data_2b_with_ds <- create_ds_data(Taranaki_data_2b)
# Taranaki_data_2c_with_ds <- create_ds_data(Taranaki_data_2c)
# Taranaki_data_2d_with_ds <- create_ds_data(Taranaki_data_2d)




########################
#  Save combined data  #
########################

#Save data for habitat unsuitability model
save(Taranaki_data_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data.RData"))

#Randomly generated pseudo-absence data
save(Taranaki_data_1a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1a.RData"))
save(Taranaki_data_1b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1b.RData"))
save(Taranaki_data_1c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1c.RData"))
save(Taranaki_data_1d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1d.RData"))

# #Spatially biased pseudo-absence data
# save(Taranaki_data_2a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2a.RData"))
# save(Taranaki_data_2b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2b.RData"))
# save(Taranaki_data_2c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2c.RData"))
# save(Taranaki_data_2d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2d.RData"))



############################
# Check habitat data is ok #
############################

#Check with whole network
network_check <- Taranaki_data_1a_with_ds$network
hab_check <- data.frame("Lon"=network_check$Lon, "Lat"=network_check$Lat, 
                        "Dist2Coast"=Taranaki_data_1a_with_ds$X_gctp[,1,"2021","std_log_Dist2Coast"])

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
  geom_point(data=hab_check, aes(x = Lon, y = Lat, col=Dist2Coast), alpha=0.6) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Taranaki river network map - Dist2Coast") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Taranaki_Dist2Coast.png"), catchmap)





#Check with downstream network
network_sub <- Taranaki_data_1a_with_ds$network_ds
hab_sub <- data.frame("Lon"=network_sub$Lon, "Lat"=network_sub$Lat, 
                      "Dist2Coast"=Taranaki_data_1a_with_ds$X_gctp_ds[,1,"2021","std_log_Dist2Coast"])

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
  geom_point(data=hab_sub, aes(x = Lon, y = Lat, col=Dist2Coast), alpha=0.6) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Taranaki river network map - Dist2Coast") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Taranaki_Dist2Coast_ds.png"), catchmap)


