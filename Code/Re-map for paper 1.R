###################################################
##  Species distribution maps for longfin eel    ##
##              in the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                 August 2023                   ##
###################################################

rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)
library(plyr)



#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



########################
#  Modelling scenario  #
########################

network_type <- "full"
SE_switch <- "SE_off"

#scenario <- "Taranaki data" #"1a" "1b" "2a" "2b" "3a" "3b" "4a" "4b"
scenario <- c("Taranaki data", "1a", "1b", "2a", "2b", "3a", "3b", "4a", "4b")


for(sce in scenario){
  
  print(sce)
  
  ##############
  # Model path #
  ##############
  
  #Set model path
  model_path <- paste0(getwd(), "/Models")
  
  #Set path
  if(sce == "Taranaki data"){
    if(network_type == "downstream"){
      path <- file.path(model_path, "Taranaki_data_model_ds")
    }
    if(network_type == "full"){
      path <- file.path(model_path, "Taranaki_data_model")
    }
  }else{
    if(network_type == "downstream"){
      path <- file.path(model_path, paste0("Model_", sce, "_ds"))
    }
    if(network_type == "full"){
      path <- file.path(model_path, paste0("Model_", sce))
    }
  }
  
  #If building uncertainty maps then add to path
  if(SE_switch == "SE_on"){
    path <- paste0(path, "_SE")
  }
  
  path_figs <- file.path(path, "Figures")
  
  #path ; path_figs
  
  
  
  ##################
  # Load model fit #
  ##################
  
  load(file.path(path, "Fit.RData"))
  
  #################################
  # Plot probability of encounter #
  #################################
  
  print(summary(Fit$Report$R1_gct))
  
  zlim_inp <- c(0, round_any(max(Fit$Report$R1_gct), 0.1, f=ceiling))
  
  ## Yearly
  plot_maps_network(plot_set = c(1), 
                    fit = Fit, 
                    Sdreport = Fit$parameter_estimates$SD, 
                    TmbData = Fit$data_list, 
                    spatial_list = Fit$spatial_list, 
                    DirName = path_figs, 
                    Panel = "category", 
                    PlotName = "POE_lf_yearly_v2",
                    PlotTitle = "",
                    cex = 0.5, 
                    Zlim = zlim_inp, 
                    arrows=T, 
                    pch=15)
  
  
  ## Across time
  plot_maps_network(plot_set = c(1), 
                    fit = Fit, 
                    Sdreport = Fit$parameter_estimates$SD, 
                    TmbData = Fit$data_list, 
                    spatial_list = Fit$spatial_list, 
                    DirName = path_figs, 
                    Panel = "Year", 
                    PlotName = "POE_lf_v2",
                    PlotTitle = "",
                    cex = 0.75, 
                    Zlim = zlim_inp, 
                    arrows=T, 
                    pch=15)
  
  
}






