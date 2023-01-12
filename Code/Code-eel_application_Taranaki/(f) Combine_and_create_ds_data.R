###################################################
##                 Combining data                ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                November 2022                  ##
###################################################




###########################################

rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)


#################
#  Directories  #
#################

data_dir <- "./Data"
raw_data <- "./Data/raw_data"
data_taranaki_dir <- "./Data/Taranaki"
fig_dir <- "./Data/Taranaki/Figures"
pseudoabsence_data_dir <- file.path(data_taranaki_dir, "Pseudo_absence_data")


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))

##Habitat data
load(file.path(data_taranaki_dir, "Taranaki_X_gctp.RData"))

##NZFFD observations
NZFFD_data <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_obs.rds"))

##Presence-only data
# ADD CODE HERE

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
Taranaki_data_1a$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
Taranaki_data_1a$pseudo_absence_data <- sample_1a

#b.
Taranaki_data_1b <- list()
Taranaki_data_1b$network <- network
Taranaki_data_1b$X_gctp <- X_gctp
Taranaki_data_1b$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
Taranaki_data_1b$pseudo_absence_data <- sample_1b

#c.
Taranaki_data_1c <- list()
Taranaki_data_1c$network <- network
Taranaki_data_1c$X_gctp <- X_gctp
Taranaki_data_1c$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
Taranaki_data_1c$pseudo_absence_data <- sample_1c

#d.
Taranaki_data_1d <- list()
Taranaki_data_1d$network <- network
Taranaki_data_1d$X_gctp <- X_gctp
Taranaki_data_1d$obs <- NZFFD_data #rbind presence only / pseudo-absence data here
Taranaki_data_1d$pseudo_absence_data <- sample_1d


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



################################
#  Call 'downstream' function  #
################################

source("./Code/funcs.R")


#########################
# Build downstream data #
#########################

#Save data for habitat unsuitability model
Taranaki_data_with_ds <- create_ds_data(Taranaki_data)

#Need to fix function so that it takes the presence / pseeudo-absence locations

# #Randomly generated pseudo-absence data
# Taranaki_data_1a_with_ds <- create_ds_data(Taranaki_data_1a)
# Taranaki_data_1b_with_ds <- create_ds_data(Taranaki_data_1b)
# Taranaki_data_1c_with_ds <- create_ds_data(Taranaki_data_1c)
# Taranaki_data_1d_with_ds <- create_ds_data(Taranaki_data_1d)

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

# #Randomly generated pseudo-absence data
# save(Taranaki_data_1a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1a.RData"))
# save(Taranaki_data_1b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1b.RData"))
# save(Taranaki_data_1c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1c.RData"))
# save(Taranaki_data_1d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1d.RData"))

# #Spatially biased pseudo-absence data
# save(Taranaki_data_2a_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2a.RData"))
# save(Taranaki_data_2b_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2b.RData"))
# save(Taranaki_data_2c_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2c.RData"))
# save(Taranaki_data_2d_with_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2d.RData"))

