###################################################
##             Create 'downstream' data          ##
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

#Data for habitat unsuitability modelling
load(file.path(data_taranaki_dir, "Taranaki_data.RData"))

# #Randomly generated pseudo-absence data
# load(file.path(data_taranaki_dir, "Taranaki_data_1a.RData"))
# load(file.path(data_taranaki_dir, "Taranaki_data_1b.RData"))
# load(file.path(data_taranaki_dir, "Taranaki_data_1c.RData"))
# load(file.path(data_taranaki_dir, "Taranaki_data_1d.RData"))

################################
#  Call 'downstream' function  #
################################

source("./Code/funcs.R")


#########################
# Build downstream data #
#########################

#Save data for habitat unsuitability model
Taranaki_data_ds <- create_ds_data(Taranaki_data)

# #Randomly generated pseudo-absence data
# Taranaki_data_1a_ds <- create_ds_data(Taranaki_data_1a)
# Taranaki_data_1b_ds <- create_ds_data(Taranaki_data_1b)
# Taranaki_data_1c_ds <- create_ds_data(Taranaki_data_1c)
# Taranaki_data_1d_ds <- create_ds_data(Taranaki_data_1d)

# #Spatially biased pseudo-absence data
# Taranaki_data_2a_ds <- create_ds_data(Taranaki_data_2a)
# Taranaki_data_2b_ds <- create_ds_data(Taranaki_data_2b)
# Taranaki_data_2c_ds <- create_ds_data(Taranaki_data_2c)
# Taranaki_data_2d_ds <- create_ds_data(Taranaki_data_2d)


#############
# Save data #
#############

#Save data for habitat unsuitability model
save(Taranaki_data_ds, file=file.path(data_taranaki_dir, "Taranaki_data_with_ds_data.RData"))

# #Randomly generated pseudo-absence data
# save(Taranaki_data_1a_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1a_with_ds_data.RData"))
# save(Taranaki_data_1b_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1b_with_ds_data.RData"))
# save(Taranaki_data_1c_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1c_with_ds_data.RData"))
# save(Taranaki_data_1d_ds, file=file.path(data_taranaki_dir, "Taranaki_data_1d_with_ds_data.RData"))

# #Spatially biased pseudo-absence data
# save(Taranaki_data_2a_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2a_with_ds_data.RData"))
# save(Taranaki_data_2b_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2b_with_ds_data.RData"))
# save(Taranaki_data_2c_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2c_with_ds_data.RData"))
# save(Taranaki_data_2d_ds, file=file.path(data_taranaki_dir, "Taranaki_data_2d_with_ds_data.RData"))


