
rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)
library(VAST)



#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")


###################################
# Set modelling scenario and path #
###################################


scenario <-   "3a"

#Set model path
model_path <- paste0(getwd(), "/Models")

#Set path
path <- file.path(model_path, paste0("Model_", scenario))

#Figures path
path_figs <- file.path(path, "Figures")



#######################################
#  Load VAST input data and model fit #
#######################################

VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
load(file.path(path, "Fit.RData"))



##################
# Set parameters #
##################

Opt <- Fit$parameter_estimates

TmbList <- Fit$tmb_list
Obj <- TmbList[["Obj"]]


###################
# Sample variable #
###################

samples <- sample_variable(Sdreport = Opt$SD, 
                           Obj = Obj, 
                           variable_name = "R1_gct", 
                           n_samples = 100, 
                           seed = 23524)
