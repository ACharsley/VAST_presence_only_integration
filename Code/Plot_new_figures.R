###################################################
##              Generate new figures             ##
##                                               ##
##               Anthony Charsley                ##
##                  June 2023                    ##
###################################################



rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)
#library(DHARMa)



#################
#  Directories  #
#################

model_path <- paste0(getwd(), "/Models")



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



########################
#  Modelling scenario  #
########################

# "Taranaki data", 
# "1a", "1b", "1c", "1d", 
# "2a", "2b", "2c", "2d",
# "3a", "3b", "3c", "3d",
# "4a", "4b", "4c", "4d"
#scenario <-   "1a"

# "OM_1a", "OM_1b"
# "OM_2a", "OM_2b"
# "OM_3a", "OM_3b"
# "OM_4a", "OM_4b"
scenario <-   "Taranaki data" #"Taranaki data", "1d", "3c", "OM_1b", "OM_3b"


network_type <- "full"



##############
# Model path #
##############

#Set path
if(scenario == "Taranaki data"){
  if(network_type == "downstream"){
    path <- file.path(model_path, "Taranaki_data_model_ds")
  }
  if(network_type == "full"){
    path <- file.path(model_path, "Taranaki_data_model")
  }
}else{
  if(network_type == "downstream"){
    path <- file.path(model_path, paste0("Model_", scenario, "_ds"))
  }
  if(network_type == "full"){
    path <- file.path(model_path, paste0("Model_", scenario))
  }
}

path_figs <- file.path(path, "Figures")




# ##########################
# #  Load VAST input data  #
# ##########################
# 
# if(network_type == "downstream"){
#   VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))
# }
# if(network_type == "full"){
#   VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
# }



##############
# Set inputs #
##############

# network = VAST_input_data[[scenario]]$network
# Network_sz = network %>% select(parent_s,child_s,dist_s)
# Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)


load(file.path(path, "Fit.RData"))



#################################
# Plot probability of encounter #
#################################

log_POE_array <- log(Fit$Report$R1_gct)

zlim_inp <- c(floor(summary(log_POE_array)["Min."]),ceiling(summary(log_POE_array)["Max."]))

## Yearly
plot_maps_network(Array_xct = log_POE_array, 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "category", 
                  PlotName = "Log_POE_lf_yearly",
                  PlotTitle = "Longfin eel yearly log probability of encounter in Taranaki, NZ",
                  cex = 0.5, 
                  Zlim = zlim_inp, 
                  arrows=T, 
                  pch=15)


## Across time
plot_maps_network(Array_xct = log_POE_array, 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "Year", 
                  PlotName = "Log_POE_lf",
                  PlotTitle = "Longfin eel yearly log P.O.E in Taranaki, NZ",
                  cex = 0.75, 
                  Zlim = zlim_inp, 
                  arrows=T, 
                  pch=15)



# #######################################
# # Percentage of river length occupied #
# #######################################
# 
# Effective_area <- plot_range_index_SN(Sdreport = Fit$parameter_estimates$SD,
#                                       Report = Fit$Report,
#                                       TmbData = Fit$data_list,
#                                       year_labels = as.numeric(Fit$year_labels),
#                                       Znames = colnames(Fit$data_list$Z_gm),
#                                       PlotDir = path_figs,
#                                       use_biascorr = TRUE,
#                                       category_names = "",
#                                       total_river_length = (sum(network$length)))
# saveRDS(Effective_area, file.path(path, paste0("Effective_area.rds")))


