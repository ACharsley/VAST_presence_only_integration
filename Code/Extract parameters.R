#############################################
# Extract model and CV parameters for table #
#############################################


rm(list=ls())


##############
#  Scenario  #
##############

scenarios <-   c("Taranaki data",
                 "1a", "1b", "1c", "1d", 
                 "2a", "2b", "2c", "2d",
                 "3a", "3b", "3c", "3d",
                 "4a", "4b", "4c", "4d")

network_type <- "full"

mod_params_list <- list()



######################
# Extract parameters #
######################

for(sce in 1:length(scenarios)){
  
  #################
  #  Directories  #
  #################
  
  model_path <- paste0(getwd(), "/Models")
  
  if(scenarios[sce] == "Taranaki data"){
    if(network_type == "downstream"){
      path <- file.path(model_path, "Taranaki_data_model_ds")
    }
    if(network_type == "full"){
      path <- file.path(model_path, "Taranaki_data_model")
    }
  }else{
    if(network_type == "downstream"){
      path <- file.path(model_path, paste0("Model_", scenarios[sce], "_ds"))
    }
    if(network_type == "full"){
      path <- file.path(model_path, paste0("Model_", scenarios[sce]))
    }
  }
  
  #Load parameters
  load(file.path(path, "parameter_estimates.RData"))
  
  mod_params_list[[scenarios[sce]]] <- parameter_estimates$SD
  
  rm(parameter_estimates)
}


mod_params_list

###########################

# #Extract parameters for CV models
# CV_params_list <- list()
# 
# for(sce in 1:length(scenarios)){
#   
#   #################
#   #  Directories  #
#   #################
#   
#   model_path <- paste0(getwd(), "/Models")
#   
#   if(scenarios[sce] == "Taranaki data"){
#     if(network_type == "downstream"){
#       path <- file.path(model_path, "Taranaki_data_model_ds")
#     }
#     if(network_type == "full"){
#       path <- file.path(model_path, "Taranaki_data_model")
#     }
#   }else{
#     if(network_type == "downstream"){
#       path <- file.path(model_path, paste0("Model_", scenarios[sce], "_ds"))
#     }
#     if(network_type == "full"){
#       path <- file.path(model_path, paste0("Model_", scenarios[sce]))
#     }
#   }
#   
#   
#   #Set a cv path
#   path_cv <- file.path(path, "CV")
#   
#   n_fold <- 10
#   i <- 1 #counter
#   for(k in 1:n_fold){
#     #Set cv k-fold path
#     path_cv_k <- file.path(path_cv, paste0("Fold_", k))
#     
#     #If file exists, save
#     if(file.exists(file.path(path_cv_k, paste0("Save.rds")))){
#       CV_failed[[scenarios[sce]]][i] <- k
#       i <- i + 1
#     }
#     
#   }
#   
#   
#   
#   
#   
#   #Load parameters
#   load(file.path(path, "parameter_estimates.RData"))
#   
#   mod_params_list[[scenarios[sce]]] <- parameter_estimates$SD
#   
#   rm(parameter_estimates)
# }




